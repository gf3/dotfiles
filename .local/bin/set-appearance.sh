#!/bin/bash

set -Eeuo pipefail

if [ "$#" -eq 0 ]; then 
  echo "You must either explicitly set the appearance to light or dark. See --help" >&2
  exit 1
fi

options=$(getopt -o a:th --long appearance:,toggle,help -- "$@")
eval set -- "$options"

usage="$(basename "$0") [-h] [-a light|dark] -- Change the appearance to light/dark mode.

where:
    -h,--help        show this help text
    -a,--appearance  explicitly set the appearance to light/dark"

while true; do
  case "$1" in
    -h)
      echo "$usage"; exit;;
    -a|--appearance)
      mode=$2; shift 2;;
    --)
      shift; break;;
    *)
      echo "Error parsing options" >&2; exit 1;;
  esac
done

if [[ "$mode" == "light" ]]; then
  gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
  gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
  lookandfeeltool -a org.manjaro.breath-light.desktop
  dbus-send --session --dest=org.kde.GtkConfig --type=method_call /GtkConfig org.kde.GtkConfig.setGtkTheme "string:Default"
else
  gsettings set org.gnome.desktop.interface gtk-theme "Adwaita-dark"
  gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
  lookandfeeltool -a org.manjaro.breath-dark.desktop
  dbus-send --session --dest=org.kde.GtkConfig --type=method_call /GtkConfig org.kde.GtkConfig.setGtkTheme "string:Breeze-dark-gtk"
fi
