#!/bin/sh
exec racket -l- plt-service-monitor/beat heartbeat.racket-lang.org pkgd-update
