#!/usr/bin/env python2.7

import sys
import os

print 's3cmd-placeholder:', sys.argv
os.system('mkdir -p ~/public_html/pkg-index-static')
os.system('rsync -av --delete %s ~/public_html/pkg-index-static' % \
          (sys.argv[-2],))
