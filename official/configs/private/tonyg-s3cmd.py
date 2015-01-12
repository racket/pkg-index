#!/usr/bin/env python2.7

import sys
import os

print 's3cmd-placeholder:', sys.argv
os.system('rsync -av --delete %s ~/public_html/pkg-catalog-static' % \
          (sys.argv[-2],))
