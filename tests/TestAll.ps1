<#
Runs every Test*.m query using the Power Query engine installed with Excel.
No workbook or Excel process is created. Query results are fully serialized to
force lazy M values and surface errors in table cells as well as assertions.

Run: pwsh -NoProfile -File .\tests\TestAll.ps1
PowerShell 7 automatically delegates to Windows PowerShell for the .NET
Framework engine. Override -EnginePath for another compatible installation.
Use -LibraryPath to validate a different copy of mgis.m (e.g. a regression).
#>
[CmdletBinding()]
param(
    [string]$TestsDirectory = $PSScriptRoot,
    [string]$LibraryPath,
    [string]$EnginePath
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

try {
    $TestsDirectory = (Resolve-Path -LiteralPath $TestsDirectory).Path
    if (-not $LibraryPath) {
        $LibraryPath = Join-Path (Split-Path $TestsDirectory -Parent) 'mgis.m'
    }
    $LibraryPath = (Resolve-Path -LiteralPath $LibraryPath).Path
    if (-not $EnginePath) {
        $relativeEngine = 'Microsoft Office\root\Office16\ADDINS\Microsoft Power Query for Excel Integrated\bin\Microsoft.MashupEngine.dll'
        $installations = @($env:ProgramFiles, ${env:ProgramFiles(x86)}) | Where-Object { $_ }
        $EnginePath = $installations | ForEach-Object { Join-Path $_ $relativeEngine } |
            Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
        if (-not $EnginePath) {
            throw 'Power Query engine not found. Supply -EnginePath pointing to Microsoft.MashupEngine.dll from Excel.'
        }
    }
    $EnginePath = (Resolve-Path -LiteralPath $EnginePath).Path

    if ($PSVersionTable.PSEdition -eq 'Core') {
        $windowsPowerShell = Join-Path $env:WINDIR 'System32\WindowsPowerShell\v1.0\powershell.exe'
        # Single-quote paths as PowerShell literals, then encode the command to
        # avoid native argument quoting issues. No execution policy is changed.
        $scriptFile = (Join-Path $TestsDirectory 'TestAll.ps1').Replace("'", "''")
        $testsArgument = $TestsDirectory.Replace("'", "''")
        $libraryArgument = $LibraryPath.Replace("'", "''")
        $engineArgument = $EnginePath.Replace("'", "''")
        $command = "& ([scriptblock]::Create([IO.File]::ReadAllText('$scriptFile'))) -TestsDirectory '$testsArgument' -LibraryPath '$libraryArgument' -EnginePath '$engineArgument'"
        $encodedCommand = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($command))
        & $windowsPowerShell -NoProfile -OutputFormat Text -EncodedCommand $encodedCommand
        exit $LASTEXITCODE
    }

    [void][Reflection.Assembly]::LoadFrom($EnginePath)
    $engine = [Microsoft.Mashup.Engine1.Engine]::Instance
    $hostApi = [Microsoft.Mashup.Evaluator.MinimalEngineHost]::Instance
    $library = [Microsoft.Mashup.Engine.Interface.IEngine].GetMethod('GetLibrary').Invoke($engine, @($hostApi, $null))
    $source = [IO.File]::ReadAllText($LibraryPath)
    $tests = @(Get-ChildItem -LiteralPath $TestsDirectory -Filter 'Test*.m' -File | Sort-Object Name)
    if ($tests.Count -eq 0) { throw 'No Test*.m queries found.' }

    $failures = 0
    foreach ($test in $tests) {
        $testSource = [IO.File]::ReadAllText($test.FullName)
        $query = 'let mgis = (' + $source + '), result = (' + $testSource + ') in Binary.Length(Json.FromValue(result))'
        try {
            $value = [Microsoft.Mashup.Engine1.Language.LanguageLibrary]::Evaluate($query, $library)
            Write-Output ($test.Name + ': PASS (' + $value.ToString() + ' serialized bytes)')
        } catch {
            $failures++
            $failure = $_.Exception
            while ($failure.InnerException) { $failure = $failure.InnerException }
            Write-Output ($test.Name + ': FAIL - ' + $failure.Message)
        }
    }
    Write-Output ("{0}/{1} tests passed." -f ($tests.Count - $failures), $tests.Count)
    if ($failures -gt 0) { exit 1 }
    exit 0
} catch {
    Write-Error $_ -ErrorAction Continue
    exit 1
}
