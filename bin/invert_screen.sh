#!/bin/bash
# xrandr|grep -q 'eDP1 connected 2400x1600+0+0 inverted ('
xrandr|grep -q 'eDP1 connected 2400x1600+0+0 ('
if [ $? -eq 0 ]; then
	xrandr -o inverted
else
	xrandr -o normal
fi
