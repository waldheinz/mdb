#!/bin/sh

# parameters:
# 1 - source file
# 2 - start time
# 3 - length
# 4 - video bitrate
# 5 - vertical resulution

ffmpeg -nostdin -loglevel quiet -y -seek_timestamp 1 -ss "$2"  -i "$1" -to "$3" -an -c:v copy -copyts -f mpegts - |

ffmpeg -nostdin -loglevel quiet -y -i - \
    -c:v libx264 -preset veryfast -b:v $4k \
    -vf "select=between(t\, $2\, $3),scale=-2:$5" \
    -f mpegts -copyts \
    - 2>/dev/null
