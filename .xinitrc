#!/bin/sh
xsetroot -cursor_name left_ptr

skype &
dropboxd &
mount-tray &
nm-applet &
sudo mount /dev/sda2 &
sh /home/jaga/myscripts/brightness.sh
home/jaga/myscripts/autocpu.sh -s &
smbnetfs /media/samba &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype percent --width 5 --transparent true --tint 0x000000 --height 12 &
exec xmonad 
