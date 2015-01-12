#lang racket/base

(provide main)

(require "config.rkt")

(define (main [configuration (hash)])
  (config configuration)
  ((dynamic-require "dynamic.rkt" 'go)))
