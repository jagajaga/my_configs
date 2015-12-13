#!/usr/bin/env bash
vpn=`systemctl is-active openvpn-JJ`
echo ${vpn:0:1}
