if (-not (Get-Module SimplySql -ErrorAction Ignore) -and -not (Import-Module SimplySql -ErrorAction Ignore)) {
    Install-Module SimplySql -Scope CurrentUser -Force
    Import-Module SimplySql
}

Open-SQLiteConnection -DataSource .\data\musiorder.db
$DbMembers = Invoke-SqlQuery -query "SELECT * FROM Member"
$NewKeyCodes = Get-Content "1.txt" `
    | ConvertFrom-Csv -Delimiter "`t" -Header CardType,Empty1,KeyNumber,Name,Empty2,Empty3,Empty4,KeyCount,Empty5,Empty6,KeyCode `
    | ForEach-Object {
        $Name = $_.Name.Split([string[]]@(",", " "), [System.StringSplitOptions]::TrimEntries + [System.StringSplitOptions]::RemoveEmptyEntries)
        [PSCustomObject]@{
            LastName = $Name[-1]
            FirstName = if ($Name.Length -gt 1) { $Name[0..($Name.Length - 2)] -join " " } else { "" }
            KeyCode = $_.KeyCode
        }
    } `
    | Where-Object { $_.KeyCode }
$MembersWithNewKeyCode = $DbMembers `
    | ForEach-Object {
        $DbMember = $_
        $NewKeyCode = $NewKeyCodes | Where-Object { $_.LastName -eq $DbMember.lastName -and $_.FirstName -eq $DbMember.firstName }
        if (-not $NewKeyCode) {
            return $null
        }
        [PSCustomObject]@{
            Id = $DbMember.id
            Name = "$($DbMember.lastName) $($DbMember.firstName)"
            KeyCode = $NewKeyCode.KeyCode
        }
    } `
    | Where-Object { $_ }
Write-Host "=== Updating key codes of $($MembersWithNewKeyCode.Length)/$($DbMembers.Length) members ==="
foreach ($Item in $MembersWithNewKeyCode) {
    Write-Host "* $($Item.Name)"
    Invoke-SqlUpdate "UPDATE Member SET keyCode = @KeyCode WHERE id = @Id" -Parameters @{ KeyCode = $Item.KeyCode; Id = $Item.Id } | Out-Null
}

$MembersWithoutNewKeyCode = $DbMembers `
    | ForEach-Object {
        $DbMember = $_
        $NewKeyCode = $NewKeyCodes | Where-Object { $_.LastName -eq $DbMember.lastName -and $_.FirstName -eq $DbMember.firstName }
        if ($NewKeyCode) {
            return $null
        }
        [PSCustomObject]@{
            Id = $DbMember.id
            Name = "$($DbMember.lastName) $($DbMember.firstName)"
        }
    } `
    | Where-Object { $_ }
Write-Host "`n=== $($MembersWithoutNewKeyCode.Length)/$($DbMembers.Length) members don't have a new key code yet ==="
foreach ($Item in $MembersWithoutNewKeyCode) {
    Write-Host "* $($Item.Name)"
}

$NewMembers = $NewKeyCodes `
    | ForEach-Object {
        $NewKeyCode = $_
        $DbMember = $DbMembers | Where-Object { $_.lastName -eq $NewKeyCode.LastName -and $_.firstName -eq $NewKeyCode.FirstName }
        if ($DbMember) {
            return $null
        }
        [PSCustomObject]@{
            Id = "$([System.Guid]::NewGuid())"
            LastName = $NewKeyCode.LastName
            FirstName = $NewKeyCode.FirstName
            KeyCode = $NewKeyCode.KeyCode
        }
    } `
    | Where-Object { $_ }
Write-Host "`n=== Adding $($NewMembers.Length) members ==="
foreach ($Item in $NewMembers) {
    Write-Host "* $($Item.LastName) $($Item.FirstName)"
    Invoke-SqlScalar "INSERT INTO Member (id, firstName, lastName, keyCode, role) VALUES (@Id, @FirstName, @LastName, @KeyCode, 'user')" -Parameters @{ Id = $Item.Id; FirstName = $Item.FirstName; LastName = $Item.LastName; KeyCode = $Item.KeyCode }
}

Close-SqlConnection