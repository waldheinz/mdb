#!/usr/bin/env python

import json
import os
from subprocess import check_output, run
import sys

VIDEO_PARAMETERS = \
    [ ( 416     , 234   , 200 )
#    , ( 640     , 360   , 600 )
    , ( 640     , 360   , 1200 )
#    , ( 960     , 540   , 3500 )
    , ( 1280    , 720   , 5000 )
#    , ( 1920    , 1080  , 8500 )
    ]

SEGMENT_LEN = 5

def probe_file(fname):
    probe = check_output(
        [ "ffprobe"
        , "-v",  "quiet"
        , "-print_format",  "json"
        ,  "-show_format"
        ,  "-show_streams"
        , fname
        ])

    return json.loads(probe.decode("utf-8"))

def best_video_stream(finfo):
    result = None

    for stream in finfo["streams"]:
        if stream["codec_type"] == "video":
            result = stream
            break

    return result

def transcode(fname):
    finfo = probe_file(fname)
    vstream = best_video_stream(finfo)

    vw = int(vstream["coded_width"])
    vh = int(vstream["coded_height"])
    vb = int(finfo["format"]["bit_rate"]) * 0.9

    if "bit_rate" in vstream:
        vb = int(vstream["bit_rate"])

    muxopts = []
    vtargets = []
    vouts = []
    tmpfiles = []

    for tgt in VIDEO_PARAMETERS:
        (tw, th, tb) = tgt
        if tw <= vw and th <= vh:
            vtargets.append(tgt)

    for tgt in vtargets:
        (w, h, b) = tgt
        oname = "video_" + str(h) + "_" + str(b) + ".mp4"
        vouts.extend(
            [ "-s", str(w) + "x" + str(h)
            , "-vsync", "passthrough"
            , "-c:v", "libx264"
            , "-force_key_frames", "expr:gte(t,n_forced*" + str(SEGMENT_LEN) + ")"
            , "-b:v", str(b) + "k"
            , "-an"
            , oname
            ])
        tmpfiles.append(oname)
        muxopts.append(oname + "#video")

    atargets = [ ("aac", "64"), ("aac", "128") ]
    aouts = []

    for tgt in atargets:
        (codec, b) = tgt
        oname = "audio_" + codec + "_" + str(b) + ".mp4"
        aouts.extend([ "-c:a", codec , "-b:a", str(b) + "k" , "-vn" , oname ])
        tmpfiles.append(oname)
        muxopts.append(oname + "#audio")

    print(vtargets)

    run(["ffmpeg", "-i", fname] + vouts + aouts)
    run(
        [ "MP4Box"
        , "-dash", str(SEGMENT_LEN * 1000)
        , "-profile", "dashavc264:live"
        , "-bs-switching", "no"
        , "-segment-name", "r_$RepresentationID$_s_$Number$$init=play$.m4s"
        , "-out", "index.mpd"
        ] + muxopts)

    for tf in tmpfiles:
        os.remove(tf)

def main():
    fname = sys.argv[1]
    transcode(fname)

if __name__ == "__main__":
    main()
