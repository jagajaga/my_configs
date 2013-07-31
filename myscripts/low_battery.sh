#!/bin/bash
# notify if battery is low

battery_float=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage | cut -d ':' -f 2 | sed -e 's/^[ \t]*//' | cut -d '%' -f 1)
battery_int=$(echo "($battery_float+0.5)/1" | bc )
if [ "$battery_int" -le "9" ]
then
    zenity --info --title "Low battery" --text "Battery is $battery_int%. Enable charging!"
fi
