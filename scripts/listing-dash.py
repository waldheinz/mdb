#!/usr/bin/env python

import json
import os
import subprocess
import sys

DASH_SCRIPT = "/home/trem/Arbeitsplatz/Meins/mdb/scripts/to-dash.py"

def local_path(fp):
    if (fp.startswith("pr0n/")):
        return "/mnt/nas/" + fp
    else:
        print("no local path for \"" + fp + "\"")
        return None

def transcode(item):
    if not item["fileMime"].startswith("video/"):
        return

    src_file = local_path(item["filePath"])
    if src_file == None:
        return

    print(src_file)

    tgt_dir = str(item["fileId"])

    if os.path.exists(tgt_dir):
        return

    os.makedirs(tgt_dir)
    os.chdir(tgt_dir)
    subprocess.run([DASH_SCRIPT, src_file])

def main():
    # read stdin until empty line
    jsons = ""
    while True:
        line = sys.stdin.readline()

        if line.strip():
            jsons += line
        else:
            break

    listing = json.loads(jsons)
    base_path = os.getcwd()

    for item in listing["items"]:
        transcode(item)
        os.chdir(base_path)

if __name__ == "__main__":
    main()
