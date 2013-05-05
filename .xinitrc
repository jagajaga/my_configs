#!/bin/sh
<<<<<<< HEAD

exec startkde
=======
xsetroot -cursor_name left_ptr

skype &
dropboxd &
mount-tray &
nm-applet &
sudo mount /dev/sda2 &
sh /home/jaga/myscripts/brightness.sh
home/jaga/myscripts/autocpu.sh -s &
smbnetfs /media/samba &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype percent --width 5 --transparent true --alpha 0 --tint 0x000000 --height 12 &
feh --bg-scale ~/Dropbox/Darius\'\ Dare.png
exec xmonad 
>>>>>>> db356515e850388db660145ec43cc694580ba734
