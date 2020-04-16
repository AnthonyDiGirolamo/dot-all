#!/usr/bin/env python3
import subprocess
import sys
import signal
import time
# import pdb
# pdb.set_trace()

awaketime = sys.argv[1]
sleeptime = sys.argv[2]

get = subprocess.check_output(["xrandr"]).decode("utf-8").split()
screens = [get[i-1] for i in range(len(get)) if get[i] == "connected"]

def signal_handler(signal, frame):
  print('Exiting')
  reset_rotation()
  sys.exit(0)

signal.signal(signal.SIGINT, signal_handler)
# print('Press Ctrl+C')
# signal.pause()

def invert_rotation():
  for scr in screens:
    # subprocess.call(["xrandr", "--output", scr, "--brightness", "0"])
    subprocess.call(["xrandr", "--output", scr, "--rotate", "inverted"])

def reset_rotation():
  for scr in screens:
    # subprocess.call(["xrandr", "--output", scr, "--brightness", "1"])
    subprocess.call(["xrandr", "--output", scr, "--rotate", "normal"])

def main():
  while True:
    print('Computer Time! Next break in {} minutes'.format(awaketime))
    time.sleep(int(float(awaketime)*60))
    invert_rotation()
    print('Break Time! Computer time resumes in {} minutes'.format(sleeptime))
    time.sleep(int(float(sleeptime)*60))
    reset_rotation()

if __name__ == '__main__':
    main()
