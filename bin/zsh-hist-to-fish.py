#!/usr/bin/env python
# coding=utf-8
import re
import string
import pprint
pp = pprint.PrettyPrinter(indent=4, width=110).pprint

with open('.zhistory') as f:
    # for i, line in enumerate(f):
    #     print(line)
    for timestamp, command in re.findall(r'^:\s*(\d+):\d+;(.*?)(?=\n:)', f.read(), flags=re.MULTILINE|re.DOTALL):
        print("- cmd: {0}".format(
            string.replace(command, '\\\n', '\\n')))
        print("  when: {0}".format(timestamp))
