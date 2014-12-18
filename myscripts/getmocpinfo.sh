#!/usr/bin/env bash

if [ "$(pidof mocp)" ] 
then
    INFO=`mocp --info`

    if [ $? == 2 ]
    then
        echo -n ""
        exit
    fi

    if [ "$INFO" == "State: STOP" ];
    then
        echo -n "Stopped"
    else
        Artist=`mocp --info | grep Artist | cut -f2 -d ":"`
        Song=`mocp --info | grep SongTitle | cut -f2 -d ":"`
        Album=`mocp --info | grep Album | cut -f2 -d ":"`
        STATE=`mocp -i | grep State | cut -f2 -d ":" | cut -f2 -d " "`
        while getopts "asb" opt; do
            case "$opt" in
                a)     
                        echo "${Artist:1}"
                        exit 0
                        ;;
                s)
                        echo "${Song:1}"
                        exit 0
                        ;;
                b) 
                        echo "${Album:1}"
                        exit 0
                        ;;
            esac
        done

        if [  "$STATE" == "PAUSE" ]
        then
            echo -n `expr substr "◼ $Artist - $Song" 1 200`
            exit
        fi

        echo -n `expr substr "\► $Artist - $Song" 2 200`

    fi
else
    echo -n ""
    exit 1
fi

