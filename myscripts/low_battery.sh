#!/bin/bash
# notify if battery is low

battery_int=$(cat /sys/class/power_supply/BAT0/capacity)
battery_state=$(cat /sys/class/power_supply/BAT0/status)
#battery_int=$(echo "($battery_float+0.5)/1" | bc )
if [ "$battery_int" -le "9" ]
then
    if [ "$battery_state" == "Discharging" ]
    then
        zenity --info --title "Low battery" --text "Battery is $battery_int%. Enable charging!"
    fi
fi

