#!r6rs

(import (rnrs (6))
        (otf)
        (xunit))

(define-assert-predicate otff?)

(define-syntax assert-otf-file
  (syntax-rules ()
    ((_ path)
     (call-with-port (open-file-input-port path)
       (lambda (iport)
         (assert-otff? (read-otff iport)))))))

(assert-otf-file "IPAfont00302/ipag.ttf")
(assert-otf-file "IPAfont00302/ipagp.ttf")
(assert-otf-file "IPAfont00302/ipam.ttf")
(assert-otf-file "IPAfont00302/ipamp.ttf")

(report)
