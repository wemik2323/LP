if ($args.Length -eq 0) {
    Write-Host "Использование: $MyInvocation.MyCommand"
    Write-Host "Введите хотя бы одно слово"
    exit 1
}

foreach ($var in $args) {
    if ($var -like "*ая*" -or $var -like "*яя*" -or $var -like "*ой*" -or $var -like "*ей*" -or $var -like "*ую*" -or $var -like "*юю*" -or $var -like "*ой*") {
        Write-Host "$var - adjective."
    }
    elseif ($var -like "*ый*" -or $var -like "*ий*" -or $var -like "*ого*" -or $var -like "*его*" -or $var -like "*ому*" -or $var -like "*ему*" -or $var -like "*ым*") {
        Write-Host "$var - adjective."
    }
    elseif ($var -like "*им*" -or $var -like "*ом*" -or $var -like "*ем*" -or $var -like "*ое*" -or $var -like "*ее*" -or $var -like "*ые*" -or $var -like "*ие*") {
        Write-Host "$var - adjective."
    }
    elseif ($var -like "*их*" -or $var -like "*ых*" -or $var -like "*ым*" -or $var -like "*им*" -or $var -like "*ых*" -or $var -like "*их*" -or $var -like "*ыми*" -or $var -like "*ими*") {
        Write-Host "$var - adjective."
    }
    elseif ($var -like "*у*" -or $var -like "*ю*" -or $var -like "*ешь*" -or $var -like "*ем*" -or $var -like "*ете*" -or $var -like "*ет*" -or $var -like "*ут*" -or $var -like "*ют*" -or $var -like "*ть*") {
        Write-Host "$var - verb."
    }
    elseif ($var -like "*им*" -or $var -like "*ишь*" -or $var -like "*ите*" -or $var -like "*и*" -or $var -like "*ат*" -or $var -like "*ят*" -or $var -like "*еть*" -or $var -like "*ить*" -or $var -like "*ать*" -or $var -like "*ла*" -or $var -like "*ло*" -or $var -like "*ли*" -or $var -like "*ил*" -or $var -like "*ел*") {
        Write-Host "$var - verb."
    }
    elseif ($var -like "*а*" -or $var -like "*я*" -or $var -like "*о*" -or $var -like "*е*" -or $var -like "*ы*" -or $var -like "*и*" -or $var -like "*и*" -or $var -like "*е*" -or $var -like "*б*") {
        Write-Host "$var - noun."
    }
    else {
        Write-Host "$var - nonparseble."
    }
}