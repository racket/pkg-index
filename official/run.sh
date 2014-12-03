#!/bin/bash

set -o xtrace

RACKET=~/local/new-plt/racket/bin

while true ; do
    ${RACKET}/raco make dynamic.rkt
    ${RACKET}/racket -t dynamic.rkt
    sleep 1
done
