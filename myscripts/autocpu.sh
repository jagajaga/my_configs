#!/usr/bin/env bash -e

COUNT=`cat /proc/cpuinfo | grep -c ^processor`
currentGov=`cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor`
FLAG="false"
while getopts "sopchn" opt; do
    FLAG="true"
    case "$opt" in
        s)     
                sudo cpupower frequency-set -g powersave
                #notify-send powersave 
                ;;
        o)
				sudo cpupower frequency-set -g ondemand 
                #notify-send ondemand 
                ;;
        p) 
	     		sudo cpupower frequency-set -g performance
                #notify-send performance
                ;;
        c)      
                echo $currentGov 
                ;;
        n)      
                echo $currentGov 
                notify-send $currentGov
                ;;
		*)
                echo -e  "autocpu 0.1\nA simple bash script to change your CPUs governors with 'cpupower'.\n\nUsage:\n-h   Show this help\n-c   Report current governor\n-s   Powersave mode\n-o   Ondemand mode\n-p   Performance mode\n\nWith no switches it just changes your active governor to the next one\npowersave -> ondemand -> performance -> powersave etc."
                ;;

    esac
done

if [ $FLAG == "true" ] 
then
    exit
fi

if [ "$currentGov" == "powersave" ]
then
    #sudo cpupower frequency-set -g ondemand; //ondemand doesn't work
    sudo cpupower frequency-set -g performance;
    #notify-send ondemand
    exit
fi
if [ "$currentGov" == "ondemand" ] 
then
    sudo cpupower frequency-set -g performance;
    #notify-send performance
    exit
fi
if [ "$currentGov" == "performance" ]
then
    sudo cpupower frequency-set -g powersave;
    #notify-send powersave
fi
exit
