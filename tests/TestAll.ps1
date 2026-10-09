<#
Runs every Test*.m query and the consolidated UnitTests.m suite using the Power Query engine installed with Excel.
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
    # Numerical fixtures are committed M literals; no reference-engine dependency
    # is needed to run the tests or use the library.
    $referencePath = Join-Path $TestsDirectory 'data\projection-references.m'
    $projectionReferences = if (Test-Path -LiteralPath $referencePath) {
        [IO.File]::ReadAllText($referencePath)
    } else { 'null' }
    # MinimalEngineHost blocks external resources. Supply the committed fixture
    # bytes to File.Contents so the real M loaders/parsers run without a workbook.
    # Paths outside tests/data are deliberately absent from this fixture record.
    $fixtureFields = @(Get-ChildItem -LiteralPath (Join-Path $TestsDirectory 'data') -File -Recurse |
        Where-Object { $_.Extension -ne '.m' } | Sort-Object FullName | ForEach-Object {
            $fixtureName = $_.FullName.Replace('\', '/').Replace('"', '""')
            $fixtureBytes = [Convert]::ToBase64String([IO.File]::ReadAllBytes($_.FullName))
            '#"' + $fixtureName + '" = Binary.FromText("' + $fixtureBytes + '", BinaryEncoding.Base64)'
        })
    $fixtureSource = '[' + ($fixtureFields -join ', ') + ']'
    $testRepositoryRoot = (Split-Path $TestsDirectory -Parent).Replace('"', '""')
    $tests = @(Get-ChildItem -LiteralPath $TestsDirectory -File |
        Where-Object { $_.Name -like 'Test*.m' -or $_.Name -eq 'UnitTests.m' } | Sort-Object Name)
    if ($tests.Count -eq 0) { throw 'No Test*.m queries found.' }

    $failures = 0
    foreach ($test in $tests) {
        $testSource = [IO.File]::ReadAllText($test.FullName)
        $query = 'let mgis = (' + $source + '), projectionReferences = (' + $projectionReferences + '), testRepositoryRoot = "' + $testRepositoryRoot + '", testFiles = ' + $fixtureSource + ', #"File.Contents" = (path as text, optional options as nullable record) as binary => Record.Field(testFiles, Text.Replace(path, "\", "/")), result = (' + $testSource + '), validated = if Value.Is(result, type table) then if Table.HasColumns(result, "passed") then if Table.MatchesAllRows(result, each [passed] = true) then result else error Error.Record("TestFailure", "Suite returned failed assertions.", Table.SelectRows(result, each [passed] <> true)) else result else result in Binary.Length(Json.FromValue(validated))'
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
