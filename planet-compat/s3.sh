#!/bin/zsh
set -e

. ~/.profile

export PC_ROOT="${PC_ROOT:-~/Dev/scm/plt/extra-pkgs/pkg-index/planet-compat}"
export S3CFG_PLT="${S3CFG_PLT:-~/.s3cfg-plt}"
export S3_BUCKET="${S3_BUCKET:-planet-compats.racket-lang.org}"
export S3CMD="${S3CMD:-s3cmd}"

cd $(dirname $0)
raco make update.rkt static.rkt
racket update.rkt
racket static.rkt
${S3CMD} -c ${S3CFG_PLT} sync --recursive --delete-removed ${PC_ROOT}/cache/ s3://${S3_BUCKET}/
