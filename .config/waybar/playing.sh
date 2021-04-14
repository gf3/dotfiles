#!/bin/sh

if [ "$(playerctl status 2> /dev/null)" = "Playing" ]; then
	exit 0
else
	exit 1
fi
