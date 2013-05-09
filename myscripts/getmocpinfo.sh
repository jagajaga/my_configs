#!/bin/bash

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

	if [  "$STATE" == "PAUSE" ]
	then
		expr substr "(Paused) $Artist - $Song |" 1 200
		exit
	fi

	expr substr "$Artist - $Song |" 2 200

fi
