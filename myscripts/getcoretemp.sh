#!/usr/bin/env bash
temp1=`cat /sys/devices/platform/coretemp.0/temp2_input`
temp2=`cat /sys/devices/platform/coretemp.0/temp3_input`
echo `expr $temp1 / 1000`C `expr $temp2 / 1000`C

