#!/bin/sh
sudo ifconfig enx606405fc91a2 192.168.7.1
sudo sysctl net.ipv4.ip_forward=1
sudo iptables --append FORWARD --in-interface enx606405fc91a2 -j ACCEPT
sudo iptables --table nat --append POSTROUTING --out-interface wlp1s0 -j MASQUERADE

# Then, on the bb:
# route add default gw 192.168.7.1
# echo "nameserver 8.8.8.8" >> /etc/resolv.conf

