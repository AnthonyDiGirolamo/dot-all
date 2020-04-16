#!/bin/bash
! xfconf-query --channel xsettings --property /Xft/DPI 2>/dev/null 1>/dev/null \
  && xfconf-query --channel xsettings --property /Xft/DPI --create --type int --set 96

CURRENT_DPI=$(xfconf-query -c xsettings -p /Xft/DPI)

case "$CURRENT_DPI" in
    "96")
        echo Currently $CURRENT_DPI
        echo Setting 120 DPI
        xfconf-query -c xsettings -p /Xft/DPI -s 120
        xfconf-query -c xfce4-panel -p /panels/panel-1/size -s 32
        ;;
    "120")
        echo Currently $CURRENT_DPI
        echo Setting 144 DPI
        xfconf-query -c xsettings -p /Xft/DPI -s 144
        xfconf-query -c xfce4-panel -p /panels/panel-1/size -s 32
        ;;
    "144")
        echo Currently $CURRENT_DPI
        echo Setting 192 DPI
        xfconf-query -c xsettings -p /Xft/DPI -s 192
        xfconf-query -c xfce4-panel -p /panels/panel-1/size -s 64
        ;;
    "192")
        echo Currently $CURRENT_DPI
        echo Setting 240 DPI
        xfconf-query -c xsettings -p /Xft/DPI -s 240
        xfconf-query -c xfce4-panel -p /panels/panel-1/size -s 64
        ;;
    "240")
        echo Currently $CURRENT_DPI
        echo Setting 96 DPI
        xfconf-query -c xsettings -p /Xft/DPI -s 96
        xfconf-query -c xfce4-panel -p /panels/panel-1/size -s 32
        ;;
    *)
        echo Currently unknown $CURRENT_DPI
        echo Setting 96 DPI
        xfconf-query -c xsettings -p /Xft/DPI -s 96
        xfconf-query -c xfce4-panel -p /panels/panel-1/size -s 32
        ;;
esac


