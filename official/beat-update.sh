#!/bin/sh
RACKET=~/local/new-plt/racket/bin

exec ${RACKET}/racket -l- plt-service-monitor/beat heartbeat.racket-lang.org pkgd-update
