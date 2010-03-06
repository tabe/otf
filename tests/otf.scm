#!r6rs

(import (rnrs (6))
        (otf)
        (xunit))

(define-assert-predicate otff?)
(define-assert-predicate head?)
(define-assert-predicate hhea?)
(define-assert-predicate hmtx?)
(define-assert-predicate maxp?)
(define-assert-predicate name?)
(define-assert-predicate OS/2?)
(define-assert-predicate post?)
(define-assert-predicate glyf?)
(define-assert-predicate loca?)

(define-syntax assert-otf-file
  (syntax-rules ()
    ((_ path
        (*head-Table-version-number*
         *head-fontRevision*
         *head-checkSumAdjustment*
         *head-magicNumber*
         *head-flags*
         *head-unitsPerEm*
         *head-created*
         *head-modified*
         *head-xMin*
         *head-yMin*
         *head-xMax*
         *head-yMax*
         *head-macStyle*
         *head-lowestRecPPEM*
         *head-fontDirectionHint*
         *head-indexToLocFormat*
         *head-glyphDataFormat*
         )
        )
     (call-with-port (open-file-input-port path)
       (lambda (iport)
         (let ((o (read-otff iport)))
           (assert-otff? o)
           (assert-list? (otff-cmap o))

           (let ((*head* (otff-head o)))
             (assert-head? *head*)
             (assert-equal? *head-Table-version-number* (head-Table-version-number *head*))
             (assert-equal? *head-fontRevision* (head-fontRevision *head*))
             (assert-equal? *head-checkSumAdjustment* (head-checkSumAdjustment *head*))
             (assert-equal? *head-magicNumber* (head-magicNumber *head*))
             (assert-equal? *head-flags* (head-flags *head*))
             (assert-equal? *head-unitsPerEm* (head-unitsPerEm *head*))
             (assert-equal? *head-created* (head-created *head*))
             (assert-equal? *head-modified* (head-modified *head*))
             (assert-equal? *head-xMin* (head-xMin *head*))
             (assert-equal? *head-yMin* (head-yMin *head*))
             (assert-equal? *head-xMax* (head-xMax *head*))
             (assert-equal? *head-yMax* (head-yMax *head*))
             (assert-equal? *head-macStyle* (head-macStyle *head*))
             (assert-equal? *head-lowestRecPPEM* (head-lowestRecPPEM *head*))
             (assert-equal? *head-fontDirectionHint* (head-fontDirectionHint *head*))
             (assert-equal? *head-indexToLocFormat* (head-indexToLocFormat *head*))
             (assert-equal? *head-glyphDataFormat* (head-glyphDataFormat *head*)))

           (assert-hhea? (otff-hhea o))

           (assert-hmtx? (otff-hmtx o))

           (assert-maxp? (otff-maxp o))

           (assert-name? (otff-name o))

           (assert-OS/2? (otff-OS/2 o))

           (assert-post? (otff-post o))

           (let ((v (otff-glyf o)))
             (assert-vector? v)
             (vector-for-each
              (lambda (x)
                (or (boolean? x)
                    (assert-glyf? x)))
              v))

           (let ((*loca* (otff-loca o)))
             (assert-loca? *loca*)
             (assert-vector? (loca-offsets *loca*)))
           ))))))

(assert-otf-file
 "IPAfont00302/ipag.ttf"
 (#vu8(0 1 0 0)
  #vu8(0 3 5 30)
  255933086
  1594834165
  15
  2048
  3337912800
  3340778542
  -932
  -571
  2048
  1905
  0
  6
  2
  1
  0
  )
)
(assert-otf-file
 "IPAfont00302/ipagp.ttf"
 (#vu8(0 1 0 0)
  #vu8(0 3 5 30)
  1659655152
  1594834165
  15
  2048
  3337912800
  3340709000
  -932
  -571
  2048
  1905
  0
  6
  2
  1
  0
  )
)
(assert-otf-file
 "IPAfont00302/ipam.ttf"
 (#vu8(0 1 0 0)
  #vu8(0 3 5 30)
  2123025922
  1594834165
  15
  2048
  3337912800
  3340778678
  -870
  -571
  2048
  1876
  0
  6
  2
  1
  0
  )
)
(assert-otf-file
 "IPAfont00302/ipamp.ttf"
 (#vu8(0 1 0 0)
  #vu8(0 3 5 30)
  2542507371
  1594834165
  15
  2048
  3337912800
  3340708978
  -870
  -571
  2048
  1876
  0
  6
  2
  1
  0
  )
)

(report)
