#!/bin/sh

mount="/"
warning=20
critical=10

df -h -P -l "$mount" | awk -v warning=$warning -v critical=$critical '
/\/.*/ {
  text=$4
  tooltip="Filesystem: "$1"\rSize: "$2"\rUsed: "$3"\rAvail: "$4"\rUse%: "$5"\rMounted on: "$6
  use=$5
  exit 0
}
END {
  class=""
  gsub(/%$/,"",use)
  if ((100 - use) < critical) {
    class="critical"
  } else if ((100 - use) < warning) {
    class="warning"
  }
  print "{\"text\":\""text"\", \"percentage\":"use",\"tooltip\":\""tooltip"\", \"class\":\""class"\"}"
}
'
