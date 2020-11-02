#!/bin/bash

me=`whoami`
me_name=`getent passwd ${me} | cut -d ':' -f 5 | cut -d ',' -f 1`
class=powermenu

echo -e "{\"text\":\""$me_name"\", \"class\":\""$class"\"}"
