<#
Runs VoronoiExample.m in the installed Excel Power Query / Mashup engine and
writes WGS84 GeoJSON containing both source sites and their Voronoi cells.
Also writes an offline SVG preview from those exported coordinates.

Run: pwsh -NoProfile -File .\scripts\GenerateVoronoiExample.ps1 -Resolution 10
Requires Windows and an installed Power Query engine, just like tests/TestAll.ps1.
The PowerShell SVG rendering only displays the result; all geometry construction
and reprojection are performed by mgis in Mashup.
#>
[CmdletBinding()]
param(
    [ValidateRange(3,100)][int]$Resolution = 10,
    [string]$InputGeoJSONPath,
    [ValidatePattern('^[A-Za-z0-9][A-Za-z0-9_-]*$')][string]$OutputName = 'voronoi-example',
    [string]$OutputDirectory = (Join-Path $PSScriptRoot 'output'),
    [string]$LibraryPath = (Join-Path (Split-Path $PSScriptRoot -Parent) 'mgis.m'),
    [string]$EnginePath,
    [string]$ScriptDirectory = $PSScriptRoot
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

try {
    $LibraryPath = (Resolve-Path -LiteralPath $LibraryPath).Path
    if ($InputGeoJSONPath) { $InputGeoJSONPath = (Resolve-Path -LiteralPath $InputGeoJSONPath).Path }
    $examplePath = Join-Path $ScriptDirectory 'VoronoiExample.m'
    if (-not $EnginePath) {
        $relativeEngine = 'Microsoft Office\root\Office16\ADDINS\Microsoft Power Query for Excel Integrated\bin\Microsoft.MashupEngine.dll'
        $EnginePath = @($env:ProgramFiles, ${env:ProgramFiles(x86)}) | Where-Object { $_ } |
            ForEach-Object { Join-Path $_ $relativeEngine } | Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
        if (-not $EnginePath) { throw 'Power Query engine not found. Supply -EnginePath pointing to Microsoft.MashupEngine.dll from Excel.' }
    }
    $EnginePath = (Resolve-Path -LiteralPath $EnginePath).Path
    $OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
    if ($PSVersionTable.PSEdition -eq 'Core') {
        $scriptPath = $PSCommandPath.Replace("'", "''")
        $libraryArgument = $LibraryPath.Replace("'", "''")
        $engineArgument = $EnginePath.Replace("'", "''")
        $outputArgument = $OutputDirectory.Replace("'", "''")
        $scriptDirectoryArgument = $ScriptDirectory.Replace("'", "''")
        $inputArgument = ([string]$InputGeoJSONPath).Replace("'", "''")
        # Match the test runner's .NET Framework host: evaluate the saved script
        # text without changing the machine's PowerShell execution policy.
        $command = "& ([scriptblock]::Create([IO.File]::ReadAllText('$scriptPath'))) -Resolution $Resolution -InputGeoJSONPath '$inputArgument' -OutputName '$OutputName' -OutputDirectory '$outputArgument' -LibraryPath '$libraryArgument' -EnginePath '$engineArgument' -ScriptDirectory '$scriptDirectoryArgument'"
        $encodedCommand = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($command))
        & (Join-Path $env:WINDIR 'System32\WindowsPowerShell\v1.0\powershell.exe') -NoProfile -OutputFormat Text -EncodedCommand $encodedCommand
        exit $LASTEXITCODE
    }

    [void][Reflection.Assembly]::LoadFrom($EnginePath)
    $engine = [Microsoft.Mashup.Engine1.Engine]::Instance
    $hostApi = [Microsoft.Mashup.Evaluator.MinimalEngineHost]::Instance
    $library = [Microsoft.Mashup.Engine.Interface.IEngine].GetMethod('GetLibrary').Invoke($engine, @($hostApi, $null))
    $source = [IO.File]::ReadAllText($LibraryPath)
    $example = [IO.File]::ReadAllText($examplePath)
    # MinimalEngineHost has no File.Contents access. Supply the actual input
    # bytes as a binary literal; JSON parsing still takes place inside Mashup.
    $inputExpression = 'null'
    if ($InputGeoJSONPath) {
        $encodedInput = [Convert]::ToBase64String([IO.File]::ReadAllBytes($InputGeoJSONPath))
        $inputExpression = 'Json.Document(Binary.FromText("' + $encodedInput + '", BinaryEncoding.Base64))'
    }
    $query = 'let mgis = (' + $source + '), voronoiResolution = ' + $Resolution + ', voronoiInputGeoJSON = ' + $inputExpression + ', result = (' + $example + ') in Text.FromBinary(Json.FromValue(result), TextEncoding.Utf8)'
    $json = [Microsoft.Mashup.Engine1.Language.LanguageLibrary]::Evaluate($query, $library).ToString()
    $collection = ConvertFrom-Json -InputObject $json
    $siteCount = @($collection.features | Where-Object { $_.properties.role -eq 'site' }).Count
    $cellCount = @($collection.features | Where-Object { $_.properties.role -eq 'cell' }).Count
    if ($collection.type -ne 'FeatureCollection' -or $siteCount -eq 0 -or $cellCount -ne $siteCount) { throw 'Unexpected GeoJSON result.' }

    [void][IO.Directory]::CreateDirectory($OutputDirectory)
    $geojsonPath = Join-Path $OutputDirectory ($OutputName + '.geojson')
    [IO.File]::WriteAllText($geojsonPath, $json, (New-Object Text.UTF8Encoding($false)))

    # Display the exported lon/lat in the original Web Mercator plane, so curves
    # and distances have the same appearance as in the planar construction.
    function ConvertTo-Plane($coordinate) {
        $lon = [double]$coordinate[0] * [Math]::PI / 180
        $lat = [double]$coordinate[1] * [Math]::PI / 180
        return @((6378137 * $lon), (6378137 * [Math]::Log([Math]::Tan([Math]::PI / 4 + $lat / 2))))
    }
    function Format-Number([double]$number) { $number.ToString('0.######', [Globalization.CultureInfo]::InvariantCulture) }
    $lower = ConvertTo-Plane @($collection.bbox[0], $collection.bbox[1])
    $upper = ConvertTo-Plane @($collection.bbox[2], $collection.bbox[3])
    $plotScale = [Math]::Min(760 / ($upper[0] - $lower[0]), 460 / ($upper[1] - $lower[1]))
    function ConvertTo-Screen($coordinate) {
        $xy = ConvertTo-Plane $coordinate
        return @((70 + ($xy[0] - $lower[0]) * $plotScale), (570 - ($xy[1] - $lower[1]) * $plotScale))
    }
    function Get-Path($rings) {
        $paths = foreach ($ring in $rings) {
            $points = foreach ($coordinate in $ring) {
                $xy = ConvertTo-Screen $coordinate
                (Format-Number $xy[0]) + ',' + (Format-Number $xy[1])
            }
            'M ' + ($points -join ' L ') + ' Z'
        }
        return $paths -join ' '
    }
    $svg = New-Object Text.StringBuilder
    [void]$svg.AppendLine('<svg xmlns="http://www.w3.org/2000/svg" width="900" height="660" viewBox="0 0 900 660"><rect width="900" height="660" fill="white"/><g font-family="Arial,sans-serif" fill="#172033">')
    [void]$svg.AppendLine('<text x="70" y="35" font-size="22" font-weight="bold">Generalized Voronoi: source sites and cells</text>')
    [void]$svg.AppendLine('<text x="70" y="61" font-size="13">Actual mgis output evaluated in Mashup · resolution ' + $Resolution + ' points per curved edge</text>')
    $position = 70
    foreach ($feature in @($collection.features | Where-Object { $_.properties.role -eq 'site' })) {
        $color = $feature.properties.stroke
        $name = [Security.SecurityElement]::Escape($feature.properties.name)
        [void]$svg.AppendLine('<rect x="' + $position + '" y="78" width="14" height="14" fill="' + $color + '"/><text x="' + ($position + 22) + '" y="90" font-size="13">' + $name + '</text>')
        $position += 235
    }
    foreach ($feature in $collection.features) {
        $g = $feature.geometry
        $color = $feature.properties.stroke
        $isSite = $feature.properties.role -eq 'site'
        $strokeWidth = if ($isSite) { 3 } else { 1.5 }
        $opacity = if ($isSite) { 0.45 } else { 0.16 }
        if ($g.type -eq 'Polygon' -or $g.type -eq 'MultiPolygon') {
            $polygons = @(if ($g.type -eq 'Polygon') { ,$g.coordinates } else { $g.coordinates })
            foreach ($rings in $polygons) {
                [void]$svg.AppendLine('<path d="' + (Get-Path $rings) + '" fill="' + $color + '" fill-opacity="' + $opacity + '" stroke="' + $color + '" stroke-width="' + $strokeWidth + '" fill-rule="evenodd"/>')
            }
        } elseif ($g.type -eq 'Point') {
            $xy = ConvertTo-Screen $g.coordinates
            [void]$svg.AppendLine('<circle cx="' + (Format-Number $xy[0]) + '" cy="' + (Format-Number $xy[1]) + '" r="6" fill="' + $color + '" stroke="white" stroke-width="2"/>')
        } elseif ($g.type -eq 'LineString') {
            $points = foreach ($coordinate in $g.coordinates) { $xy = ConvertTo-Screen $coordinate; (Format-Number $xy[0]) + ',' + (Format-Number $xy[1]) }
            [void]$svg.AppendLine('<polyline points="' + ($points -join ' ') + '" fill="none" stroke="' + $color + '" stroke-width="3"/>')
        }
    }
    $plotWidth = ($upper[0] - $lower[0]) * $plotScale
    $plotHeight = ($upper[1] - $lower[1]) * $plotScale
    [void]$svg.AppendLine('<rect x="70" y="' + (Format-Number (570 - $plotHeight)) + '" width="' + (Format-Number $plotWidth) + '" height="' + (Format-Number $plotHeight) + '" fill="none" stroke="#64748b"/>')
    [void]$svg.AppendLine('<text x="70" y="596" font-size="12">0 m</text><text x="' + (Format-Number (70 + $plotWidth - 45)) + '" y="596" font-size="12">' + (Format-Number ($upper[0] - $lower[0])) + ' m</text>')
    [void]$svg.AppendLine('<text x="70" y="622" font-size="12">Easting offset (m) · northing increases upwards · EPSG:3857 · shaded regions are cells</text>')
    $sourceLabel = if ($InputGeoJSONPath) { [IO.Path]::GetFileName($InputGeoJSONPath) } else { 'synthetic sites near London' }
    [void]$svg.AppendLine('<text x="70" y="643" font-size="12">Source: ' + [Security.SecurityElement]::Escape($sourceLabel) + ' · scripts/VoronoiExample.m · GeoJSON exported in WGS84</text></g></svg>')
    $svgPath = Join-Path $OutputDirectory ($OutputName + '.svg')
    [IO.File]::WriteAllText($svgPath, $svg.ToString(), (New-Object Text.UTF8Encoding($false)))
    Write-Output ('Mashup export: ' + $geojsonPath)
    Write-Output ('Preview: ' + $svgPath)
    Write-Output ($siteCount.ToString() + ' source sites + ' + $cellCount.ToString() + ' cells; resolution=' + $Resolution)
} catch {
    $failure = $_.Exception
    while ($failure.InnerException) { $failure = $failure.InnerException }
    Write-Error $failure.Message
    exit 1
}
