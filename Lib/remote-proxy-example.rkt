#lang racket

(require "remote-proxy.rkt")

(struct data [v] #:transparent)

(define (data->jsexpr d) (-> data? integer?)
  (data-v d))

(define/contract (jsexpr->data j) (-> integer? data?)
  (data j))

(define-remote-datatypes
  [data/r data->jsexpr jsexpr->data])

(define adder%
  (class object%
    (super-new)
    (define/public (add1 d)
      (match d
        [(data v)
         (+ 1 v)]))))

(define-remote-proxy (remote-adder% adder-driver)
  [add1 (-> data/r integer?)])

(thread
 (lambda ()
   (define l (tcp-listen 8000 4 #t))
   (define-values (in out) (tcp-accept l))
   (define o (new remote-adder% [in in] [out out]))
   (displayln (send o add1 (data 1)))
   (displayln (send o add1 (data 5)))))

(sleep 1)

(thread
 (lambda ()
   (define-values (in out) (tcp-connect "127.0.0.1" 8000))
   (adder-driver in out (new adder%))))
   
   