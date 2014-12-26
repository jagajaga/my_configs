#!/usr/bin/env bash

if [ "$(pidof mpd)" ] 
then
    INFO=`mpc`

    if [ $? == 2 ]
    then
        echo -n ""
        exit
    fi

    Artist=`mpc --format \"[%artist%]\" | head -n 1`
    Song=`mpc --format \"[%title%]\" | head -n 1`
    Album=`mpc --format \"[%album%]\" | head -n 1`
    STATE=`mpc | head -n 2 | tail -n 1 | sed -e 's/\[\(.*\)\].*/\1/g'`
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

    if [ ! "$STATE" == "playing" ]
    then
        echo -n `expr substr "◼ Music paused" 1 200`
        exit
    fi

    echo -n `expr substr "\► $Artist - $Song" 2 200`

else
    echo -n ""
    exit 1
fi

