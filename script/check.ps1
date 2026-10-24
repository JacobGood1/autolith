# Run script/check.lisp the way script/check does, under the checked runtime.
#
# Usage: check.ps1 [--list | --source | --fix] [--suite NAME]... [--test NAME]... [--jobs N] [--timeout SECONDS]

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$sourceRoot = Split-Path -Parent $PSScriptRoot
& (Join-Path $sourceRoot 'bin\autolith-runtime.ps1') --script (Join-Path $sourceRoot 'script\check.lisp') @args
exit $LASTEXITCODE
