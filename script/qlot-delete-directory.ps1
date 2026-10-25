param([Parameter(Mandatory = $true)][string]$Path)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

# Qlot's SBCL cleanup follows directory links on Windows. Remove each reparse
# point itself, including dangling links, before considering directory recursion.
function Remove-QlotDirectoryEntry([string]$Entry) {
  try {
    $attributes = [IO.File]::GetAttributes($Entry)
  } catch [IO.FileNotFoundException] {
    return
  } catch [IO.DirectoryNotFoundException] {
    return
  }
  $directory = ($attributes -band [IO.FileAttributes]::Directory) -ne 0
  if (($attributes -band [IO.FileAttributes]::ReparsePoint) -ne 0) {
    if ($directory) { [IO.Directory]::Delete($Entry) }
    else { [IO.File]::Delete($Entry) }
    return
  }
  if ($directory) {
    foreach ($child in @([IO.Directory]::EnumerateFileSystemEntries($Entry))) {
      Remove-QlotDirectoryEntry $child
    }
  }
  if (($attributes -band [IO.FileAttributes]::ReadOnly) -ne 0) {
    [IO.File]::SetAttributes($Entry, ($attributes -band (-bnot [IO.FileAttributes]::ReadOnly)))
  }
  if ($directory) { [IO.Directory]::Delete($Entry) }
  else { [IO.File]::Delete($Entry) }
}

$fullPath = [IO.Path]::GetFullPath($Path).TrimEnd([char[]]'\/')
if ($fullPath -eq [IO.Path]::GetPathRoot($fullPath).TrimEnd([char[]]'\/')) {
  throw 'Refusing to remove a filesystem root.'
}
Remove-QlotDirectoryEntry $fullPath
