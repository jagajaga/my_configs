#!/usr/bin/env bash

# author: yulya3102

# FIXME: "$year" variable is so weird
usage="Usage: de_reload.sh -l login -p password -s semester -g group [-c config_file] | de_reload.sh [-c config_file]"
config=".de_config"

while getopts l:p:s:g:c: option
do
    case $option in
        l) 
            login="$OPTARG"
            new_config=1;;
        p) 
            password="$OPTARG"
            new_config=1;;
        s) 
            semester="$OPTARG"
            new_config=1;;
        g) 
            group="$OPTARG"
            new_config=1;;
        c) 
            config="$OPTARG";;
        ?) 
            echo "$usage"
            exit 1;;
    esac
done

if [ -z "$new_config" ]; then
    if [ -e "$config" ]; then
        source "$config"
    else
        echo "$usage"
        exit 1
    fi
fi
if [ -z "$login" ]; then
    echo "Option -l required"
    echo "$usage"
    exit 1
elif [ -z "$password" ]; then
    echo "Option -p required"
    echo "$usage"
    exit 1
elif [ -z "$semester" ]; then
    echo "Option -s required"
    echo "$usage"
    exit 1
elif [ -z "$group" ]; then
    echo "Option -g required"
    echo "$usage"
    exit 1
elif [ -n "$config" ]; then
    echo "login=$login" > "$config"
    echo "password=$password" >> "$config"
    echo "semester=$semester" >> "$config"
    echo "group=$group" >> "$config"
fi

if [ -z "$year" ]; then
    year=2012%2F2013
fi
if [ -z "$de_file" ]; then
    de_file=".de"
fi

reload_from_server() {
    local login="$1"
    local password="$2"
    local semester="$3"
    local group="$4"
    local year="$5"
    curl --cookie ~/.cookie --cookie-jar ~/.cookie -d "Rule=LOGON&LOGIN=$login&PASSWD=$password&loginbutton=%E2%EE%E9%F2%E8" https://de.ifmo.ru/servlet/ > /dev/null
    local de=`curl --cookie ~/.cookie -d "Rule=eRegisterGetStudentAllProgram&PERSONID=$login&PROGRAMID=&SEMID=$semester&ST_GRP=$group&APPRENTICESHIP=$year&UNIVER=1&BACK=%0D%0A++++++++++%3CBack+Rule%3D%22ERegister%22%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22ST_GRP%22+Value%3D%22$group%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22PERSONID%22+Value%3D%22$login%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22UNIVER%22+Value%3D%221%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22PROGRAMID%22+Value%3D%22-%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22APPRENTICESHIP%22+Value%3D%22$year%22%2F%3E%0D%0A+++++++++++++++%0D%0A++++++++++%3C%2FBack%3E%0D%0A+++++++&CURRENT=%0D%0A++++++++++%3CBack+Rule%3D%22ERegister%22%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22ST_GRP%22+Value%3D%22$group%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22PERSONID%22+Value%3D%22$login%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22UNIVER%22+Value%3D%221%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22PROGRAMID%22+Value%3D%22-%22%2F%3E%0D%0A++++++++++++++%0D%0A++++++++++++++++++%3CParameter+Name%3D%22APPRENTICESHIP%22+Value%3D%22$year%22%2F%3E%0D%0A+++++++++++++++%0D%0A++++++++++%3C%2FBack%3E%0D%0A+++++++" https://de.ifmo.ru/servlet/distributedCDE`

    #code this Windows-1251 shit to UTF-8
    local de=`iconv -f WINDOWS-1251 -t UTF-8 <( echo "$de" )`

    echo "$de"
}

name_column_width=55
rating_column_width=5
exam_column_width=15

process_line() {
    local line="$1"
    local line=`echo "$line" | sed -e 's/<\/td>/&\n/g'`
    local name=`echo "$line" | sed -n '3 p' | sed -e 's/<td>//;s/<\/td>//;s/<\/a>.*//;s/.*">//'`
    local rating=`echo "$line" | sed -n '5 p' | sed -e 's/<td>//;s/<\/td>//'`
    local exam=`echo "$line" | sed -n '6 p' | sed -e 's/<td>//;s/<\/td>//'`
    local name=`iconv -f UTF-8 -t WINDOWS-1251 <( echo "$name" )`
    local exam=`iconv -f UTF-8 -t WINDOWS-1251 <( echo "$exam" )`
    printf %-"$name_column_width"s%-"$rating_column_width"s%-"$exam_column_width"s "$name" "$rating" "$exam"
    echo
}

de=`reload_from_server $login $password $semester $group $year | tr -d '\n' | sed -e 's/.*<form name="IntermediateExams" id="IntermediateExams">//' | sed -e 's/<\/form>.*//' | sed -e 's/<\/tr>/&\n/g' | sed -n '2,$ p' | { while read i; do process_line "$i"; done }`
iconv -f WINDOWS-1251 -t UTF-8 <( echo "$de" ) > "$de_file"
echo -e "\n" >> "$de_file"
echo `date` >> "$de_file"
