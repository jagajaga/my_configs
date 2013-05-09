#!/bin/bash
volume=$(amixer get Master | grep % | cut -d ' ' -f 6 | grep -Eo '[0-9]*')
state=$(amixer get Master | grep off | cut -d ' ' -f 8 | grep -Eo '[a-z]*')
if [ "$state" = "off" ]
then
	echo "$state"
	exit
fi
echo "$volume""%"
if [ $1 == "-s" ]
then
	bar=""
	for i in $(seq 1 $(($volume / 10)))
	do
		bar=${bar}"|"
	done
	if [ "$bar" == "" ]
	then
		bar="Muted"
	fi
		killall xfce4-notifyd
		notify-send -u critical -t 1050 Volume " $bar"
fi
