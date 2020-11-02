#!/bin/bash

class=cpu_speed
speed_mhz=$(lscpu | grep "CPU MHz" | sed --expression "s/CPU MHz:[[:space:]]*//g" | xargs printf "%.*f\n" 0)

# speed_ghz=`echo $(($speed_mhz / 1000))`

speed_ghz=`bc -l <<< "$speed_mhz / 1000"`

info=$(echo $speed_ghz | xargs printf "%.*f\n" 2)

echo -e "{\"text\":\""$info GHz"\", \"class\":\""$class"\"}"
