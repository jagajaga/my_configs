#!/bin/sh

killall dropbox
sleep 40
sudo -u jaga dropboxd &
