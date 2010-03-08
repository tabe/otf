;;
;;   Copyright (c) 2010 Takeshi Abe. All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the authors nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (otf)
  (export otff?
          otff-cmap
          otff-head
          otff-hhea
          otff-hmtx
          otff-maxp
          otff-name
          otff-OS/2
          otff-post
          otff-cvt
          otff-fpgm
          otff-glyf
          otff-loca
          otff-prep
          otff-CFF
          otff-VORG
          otff-EBDT
          otff-EBLC
          otff-EBSC
          otff-BASE
          otff-GDEF
          otff-GPOS
          otff-GSUB
          otff-JSTF
          otff-DSIG
          otff-gasp
          otff-hdmx
          otff-kern
          otff-LTSH
          otff-PCLT
          otff-VDMX
          otff-vhea
          otff-vmtx
          offset-table?
          offset-table-version
          offset-table-num-tables
          offset-table-search-range
          offset-table-entry-selector
          offset-table-range-shift
          record-table?
          record-table-tag
          record-table-check-sum
          record-table-offset
          record-table-length
          cmap-header?
          cmap-header-version
          cmap-header-numTables
          cmap-encoding-record?
          cmap-encoding-record-platformID
          cmap-encoding-record-encodingID
          cmap-encoding-record-offset
          cmap-subtable?
          cmap-subtable-format
          cmap-subtable-length
          cmap-subtable-language
          cmap-subtable-format-0?
          cmap-subtable-format-0-glyphIdArray
          head?
          head-Table-version-number
          head-fontRevision
          head-checkSumAdjustment
          head-magicNumber
          head-flags
          head-unitsPerEm
          head-created
          head-modified
          head-xMin
          head-yMin
          head-xMax
          head-yMax
          head-macStyle
          head-lowestRecPPEM
          head-fontDirectionHint
          head-indexToLocFormat
          head-glyphDataFormat
          hhea?
          hhea-Table-version-number
          hhea-Ascender
          hhea-Descender
          hhea-LineGap
          hhea-advanceWidthMax
          hhea-minLeftSideBearing
          hhea-minRightSideBearing
          hhea-xMaxExtent
          hhea-caretSlopeRise
          hhea-caretSlopeRun
          hhea-caretOffset
          hhea-metricDataFormat
          hhea-numberOfHMetrics
          hmtx?
          hmtx-hMetrics
          hmtx-leftSideBearing
          maxp?
          maxp-Table-version-number
          maxp-numGlyphs
          name?
          name-format
          name-count
          name-stringOffset
          name-nameRecord
          name-langTagCount
          name-langTagRecord
          lang-tag-record?
          lang-tag-record-length
          lang-tag-record-offset
          name-record?
          name-record-platformID
          name-record-encodingID
          name-record-languageID
          name-record-nameID
          name-record-length
          name-record-offset
          OS/2?
          OS/2-version
          OS/2-xAvgCharWidth
          OS/2-usWeightClass
          OS/2-usWidthClass
          OS/2-fsType
          OS/2-ySubscriptXSize
          OS/2-ySubscriptYSize
          OS/2-ySubscriptXOffset
          OS/2-ySubscriptYOffset
          OS/2-ySuperscriptXSize
          OS/2-ySuperscriptYSize
          OS/2-ySuperscriptXOffset
          OS/2-ySuperscriptYOffset
          OS/2-yStrikeoutSize
          OS/2-yStrikeoutPosition
          OS/2-sFamilyClass
          OS/2-panose
          OS/2-ulUnicodeRange1
          OS/2-ulUnicodeRange2
          OS/2-ulUnicodeRange3
          OS/2-ulUnicodeRange4
          OS/2-achVendID
          OS/2-fsSelection
          OS/2-usFirstCharIndex
          OS/2-usLastCharIndex
          OS/2-sTypoAscender
          OS/2-sTypoDescender
          OS/2-sTypoLineGap
          OS/2-usWinAscent
          OS/2-usWinDescent
          OS/2-ulCodePageRange1
          OS/2-ulCodePageRange2
          OS/2-sxHeight
          OS/2-sCapHeight
          OS/2-usDefaultChar
          OS/2-usBreakChar
          OS/2-usMaxContext
          post?
          post-Version
          post-italicAngle
          post-underlinePosition
          post-underlineThickness
          post-isFixedPitch
          post-minMemType42
          post-maxMemType42
          post-minMemType1
          post-maxMemType1
          glyf?
          glyf-numberOfContours
          glyf-xMin
          glyf-yMin
          glyf-xMax
          glyf-yMax
          loca?
          loca-offsets
          read-otff)
  (import (rnrs (6)))

  ;; http://www.microsoft.com/typography/otspec/otff.htm
  (define-record-type otff
    (fields
     ;; Required Tables
     cmap head hhea hmtx maxp name OS/2 post
     ;; Tables Related to TrueType Outlines
     cvt fpgm glyf loca prep
     ;; Tables Related to PostScript Outlines
     CFF VORG
     ;; Tables Related to Bitmap Glyphs
     EBDT EBLC EBSC
     ;; Advanced Typographic Tables
     BASE GDEF GPOS GSUB JSTF
     ;; Other OpenType Tables
     DSIG gasp hdmx kern LTSH PCLT VDMX vhea vmtx
     ))

  (define-record-type offset-table
    (fields version num-tables search-range entry-selector range-shift))

  (define-record-type record-table
    (fields tag check-sum offset length))

  ;; http://www.microsoft.com/typography/otspec/cmap.htm
  (define-record-type cmap-header
    (fields version numTables))

  (define-record-type cmap-encoding-record
    (fields platformID encodingID offset))

  (define-record-type cmap-subtable
    (fields format length language))

  (define-record-type cmap-subtable-format-0
    (parent cmap-subtable)
    (fields glyphIdArray))

  ;; http://www.microsoft.com/typography/otspec/head.htm
  (define-record-type head
    (fields Table-version-number
            fontRevision
            checkSumAdjustment
            magicNumber
            flags
            unitsPerEm
            created
            modified
            xMin
            yMin
            xMax
            yMax
            macStyle
            lowestRecPPEM
            fontDirectionHint
            indexToLocFormat
            glyphDataFormat))

  ;; http://www.microsoft.com/typography/otspec/hhea.htm
  (define-record-type hhea
    (fields Table-version-number
            Ascender
            Descender
            LineGap
            advanceWidthMax
            minLeftSideBearing
            minRightSideBearing
            xMaxExtent
            caretSlopeRise
            caretSlopeRun
            caretOffset
            metricDataFormat
            numberOfHMetrics))

  ;; http://www.microsoft.com/typography/otspec/hmtx.htm
  (define-record-type hmtx
    (fields hMetrics        ; longHorMetric[numberOfHMetrics]
            leftSideBearing ; SHORT[]
            ))

  ;; http://www.microsoft.com/typography/otspec/maxp.htm
  (define-record-type maxp
    (fields Table-version-number numGlyphs))

  ;; http://www.microsoft.com/typography/otspec/name.htm
  (define-record-type name
    (fields format
            count
            stringOffset
            nameRecord
            langTagCount
            langTagRecord))

  (define-record-type lang-tag-record
    (fields length
            offset))

  (define-record-type name-record
    (fields platformID
            encodingID
            languageID
            nameID
            length
            offset))

  ;; http://www.microsoft.com/typography/otspec/os2.htm
  (define-record-type OS/2
    (fields version ; USHORT
            xAvgCharWidth ; SHORT
            usWeightClass ; USHORT
            usWidthClass ; USHORT
            fsType ; USHORT
            ySubscriptXSize ; SHORT
            ySubscriptYSize ; SHORT
            ySubscriptXOffset ; SHORT
            ySubscriptYOffset ; SHORT
            ySuperscriptXSize ; SHORT
            ySuperscriptYSize ; SHORT
            ySuperscriptXOffset ; SHORT
            ySuperscriptYOffset ; SHORT
            yStrikeoutSize ; SHORT
            yStrikeoutPosition ; SHORT
            sFamilyClass ; SHORT
            panose ; BYTE[10]
            ulUnicodeRange1 ; ULONG ; Bits 0-31
            ulUnicodeRange2 ; ULONG ; Bits 32-63
            ulUnicodeRange3 ; ULONG ; Bits 64-95
            ulUnicodeRange4 ; ULONG ; Bits 96-127
            achVendID ; CHAR[4]
            fsSelection ; USHORT
            usFirstCharIndex ; USHORT
            usLastCharIndex ; USHORT
            sTypoAscender ; SHORT
            sTypoDescender ; SHORT
            sTypoLineGap ; SHORT
            usWinAscent ; USHORT
            usWinDescent ; USHORT
            ulCodePageRange1 ; ULONG ; Bits 0-31
            ulCodePageRange2 ; ULONG ; Bits 32-63
            sxHeight ; SHORT
            sCapHeight ; SHORT
            usDefaultChar ; USHORT
            usBreakChar ; USHORT
            usMaxContext ; USHORT
            ))

  ;; http://www.microsoft.com/typography/otspec/post.htm
  (define-record-type post
    (fields Version
            italicAngle
            underlinePosition
            underlineThickness
            isFixedPitch
            minMemType42
            maxMemType42
            minMemType1
            maxMemType1))

  ;; http://www.microsoft.com/typography/otspec/glyf.htm
  (define-record-type glyf
    (fields numberOfContours
            xMin
            yMin
            xMax
            yMax))

  ;; http://www.microsoft.com/typography/otspec/loca.htm
  (define-record-type loca
    (fields offsets))

  (define (read-otff iport)

    (define-syntax get-byte
      (syntax-rules ()
        ((_)
         (bytevector-u8-ref (get-bytevector-n iport 1) 0))))

    (define-syntax get-char
      (syntax-rules ()
        ((_)
         (bytevector-s8-ref (get-bytevector-n iport 1) 0))))

    (define-syntax get-short
      (syntax-rules ()
        ((_)
         (bytevector-s16-ref (get-bytevector-n iport 2) 0 (endianness big)))))

    (define-syntax get-ushort
      (syntax-rules ()
        ((_)
         (bytevector-u16-ref (get-bytevector-n iport 2) 0 (endianness big)))))

    (define-syntax get-fixed
      (syntax-rules ()
        ((_)
         (get-bytevector-n iport 4))))

    (define-syntax get-longdatetime
      (syntax-rules ()
        ((_)
         (bytevector-s64-ref (get-bytevector-n iport 8) 0 (endianness big)))))

    (define-syntax get-ulong
      (syntax-rules ()
        ((_)
         (bytevector-u32-ref (get-bytevector-n iport 4) 0 (endianness big)))))

    (define-syntax get-fword
      (syntax-rules ()
        ((_)
         (bytevector-s16-ref (get-bytevector-n iport 2) 0 (endianness big)))))

    (define-syntax get-ufword
      (syntax-rules ()
        ((_)
         (bytevector-u16-ref (get-bytevector-n iport 2) 0 (endianness big)))))

    (assert (binary-port? iport))

    (let* ((*offset-table* (let* ((version (get-bytevector-n iport 4))
                                  (num-tables (get-ushort))
                                  (search-range (get-ushort))
                                  (entry-selector (get-ushort))
                                  (range-shift (get-ushort)))
                             (make-offset-table version num-tables search-range entry-selector range-shift)))
           (*record-tables* (let* ((n (offset-table-num-tables *offset-table*))
                                   (ht (make-eq-hashtable n)))
                              (let lp ((i 0))
                                (cond ((= i n)
                                       ht)
                                      (else
                                       (let* ((tag (utf8->string (get-bytevector-n iport 4)))
                                              (check-sum (get-ulong))
                                              (offset (get-ulong))
                                              (length (get-ulong)))
                                         (hashtable-set! ht (string->symbol tag) (make-record-table tag check-sum offset length)))
                                       (lp (+ i 1))))))))

      (define (tag->record-table tag)
        (hashtable-ref *record-tables* tag #f))

      (define (go-to-record-table rt)
        (set-port-position! iport (record-table-offset rt)))

      (let ((*table-cmap* (tag->record-table 'cmap))
            (*table-head* (tag->record-table 'head))
            (*table-hhea* (tag->record-table 'hhea))
            (*table-hmtx* (tag->record-table 'hmtx))
            (*table-OS/2* (tag->record-table 'OS/2))
            (*table-maxp* (tag->record-table 'maxp))
            (*table-name* (tag->record-table 'name))
            (*table-post* (tag->record-table 'post))
            (*table-glyf* (tag->record-table 'glyf))
            (*table-loca* (tag->record-table 'loca)))

        (assert *table-cmap*)
        (assert *table-head*)
        (assert *table-hhea*)
        (assert *table-hmtx*)
        (assert *table-OS/2*)
        (assert *table-maxp*)
        (assert *table-name*)
        (assert *table-post*)
        (assert *table-glyf*)
        (assert *table-loca*)

        (let ()

          (define (read-cmap)
            (go-to-record-table *table-cmap*)
            (let* ((*cmap-header* (let* ((version (get-ushort))
                                         (numTables (get-ushort)))
                                    (make-cmap-header version numTables)))
                   (*cmap-encoding-records* (let ((v (make-vector (cmap-header-numTables *cmap-header*))))
                                              (let lp ((i 0))
                                                (cond ((= i (vector-length v))
                                                       v)
                                                      (else
                                                       (let* ((platformID (get-ushort))
                                                              (encodingID (get-ushort))
                                                              (offset (get-ulong)))
                                                         (vector-set! v i (make-cmap-encoding-record platformID encodingID offset)))
                                                       (lp (+ i 1)))))))
                   (*cmap-format-0* (vector-ref *cmap-encoding-records* 2)))
              (set-port-position! iport (+ (record-table-offset *table-cmap*) (cmap-encoding-record-offset *cmap-format-0*)))
              (let ((*cmap-subtable-format-0* (let* ((format (get-ushort))
                                                     (length (get-ushort))
                                                     (language (get-ushort)))
                                                (case format
                                                  ((0)
                                                   (make-cmap-subtable-format-0 format length language (get-bytevector-n iport 256)))
                                                  (else
                                                   format)))))
                (list *cmap-header* *cmap-encoding-records* *cmap-format-0* *cmap-subtable-format-0*))))

          (define (read-head)
            (go-to-record-table *table-head*)
            (let* ((Table-version-number (get-fixed))
                   (fontRevision (get-fixed))
                   (checkSumAdjustment (get-ulong))
                   (magicNumber (get-ulong))
                   (flags (get-ushort))
                   (unitsPerEm (get-ushort))
                   (created (get-longdatetime))
                   (modified (get-longdatetime))
                   (xMin (get-short))
                   (yMin (get-short))
                   (xMax (get-short))
                   (yMax (get-short))
                   (macStyle (get-ushort))
                   (lowestRecPPEM (get-ushort))
                   (fontDirectionHint (get-short))
                   (indexToLocFormat (get-short))
                   (glyphDataFormat (get-short)))
              (make-head Table-version-number
                         fontRevision
                         checkSumAdjustment
                         magicNumber
                         flags
                         unitsPerEm
                         created
                         modified
                         xMin
                         yMin
                         xMax
                         yMax
                         macStyle
                         lowestRecPPEM
                         fontDirectionHint
                         indexToLocFormat
                         glyphDataFormat)))

          (define (read-hhea)
            (go-to-record-table *table-hhea*)
            (let* ((Table-version-number (get-fixed))
                   (Ascender (get-fword))
                   (Descender (get-fword))
                   (LineGap (get-fword))
                   (advanceWidthMax (get-ufword))
                   (minLeftSideBearing (get-fword))
                   (minRightSideBearing (get-fword))
                   (xMaxExtent (get-fword))
                   (caretSlopeRise (get-short))
                   (caretSlopeRun (get-short))
                   (caretOffset (get-short)))
              (assert (= 0 (get-short)))
              (assert (= 0 (get-short)))
              (assert (= 0 (get-short)))
              (assert (= 0 (get-short)))
              (let* ((metricDataFormat (get-short))
                     (numberOfHMetrics (get-ushort)))
                (make-hhea Table-version-number
                           Ascender
                           Descender
                           LineGap
                           advanceWidthMax
                           minLeftSideBearing
                           minRightSideBearing
                           xMaxExtent
                           caretSlopeRise
                           caretSlopeRun
                           caretOffset
                           metricDataFormat
                           numberOfHMetrics))))

          (define (read-hmtx numberOfHMetrics numGlyphs)
            (go-to-record-table *table-hmtx*)
            (let ((hMetrics (make-vector numberOfHMetrics))
                  (leftSideBearing (make-vector (- numGlyphs numberOfHMetrics))))
              (let lp1 ((i 0))
                (cond ((= i numberOfHMetrics)
                       (let lp2 ((i 0))
                         (cond ((= i (- numGlyphs numberOfHMetrics))
                                (make-hmtx hMetrics leftSideBearing))
                               (else
                                (vector-set! leftSideBearing i (get-short))
                                (lp2 (+ i 1))))))
                      (else
                       (let* ((advanceWidth (get-ushort))
                              (lsb (get-short)))
                         (vector-set! hMetrics i (cons advanceWidth lsb))
                         (lp1 (+ i 1))))))))

          (define (read-maxp)
            (go-to-record-table *table-maxp*)
            (let* ((Table-version-number (get-fixed))
                   (numGlyphs (get-ushort)))
              (make-maxp Table-version-number numGlyphs)))

          (define (read-name)
            (go-to-record-table *table-name*)
            (let* ((format (get-ushort))
                   (count (get-ushort))
                   (stringOffset (get-ushort)))

              (assert (<= 0 format 1))

              (let ((nameRecord (make-vector count)))
                (let lp1 ((i 0))

                  (define (read-name-record)
                    (let* ((platformID (get-ushort))
                           (encodingID (get-ushort))
                           (languageID (get-ushort))
                           (nameID (get-ushort))
                           (length (get-ushort))
                           (offset (get-ushort)))
                      (make-name-record platformID
                                        encodingID
                                        languageID
                                        nameID
                                        length
                                        offset)))

                  (cond ((= i count)
                         (if (= format 0)
                             (make-name format count stringOffset nameRecord #f #f)
                             (let* ((langTagCount (get-ushort))
                                    (langTagRecord (make-vector langTagCount)))
                               (let lp2 ((i 0))
                                 (cond ((= i langTagCount)
                                        (make-name format count stringOffset nameRecord langTagCount langTagRecord))
                                       (else
                                        (let* ((length (get-ushort))
                                               (offset (get-ushort)))
                                          (vector-set! langTagRecord i (make-lang-tag-record length offset))
                                          (lp2 (+ i 1)))))))))
                        (else
                         (vector-set! nameRecord i (read-name-record))
                         (lp1 (+ i 1))))))))

          (define (read-OS/2)
            (go-to-record-table *table-OS/2*)
            (let* ((version (get-ushort))
                   (xAvgCharWidth (get-short))
                   (usWeightClass (get-ushort))
                   (usWidthClass (get-ushort))
                   (fsType (get-ushort))
                   (ySubscriptXSize (get-short))
                   (ySubscriptYSize (get-short))
                   (ySubscriptXOffset (get-short))
                   (ySubscriptYOffset (get-short))
                   (ySuperscriptXSize (get-short))
                   (ySuperscriptYSize (get-short))
                   (ySuperscriptXOffset (get-short))
                   (ySuperscriptYOffset (get-short))
                   (yStrikeoutSize (get-short))
                   (yStrikeoutPosition (get-short))
                   (sFamilyClass (get-short))
                   (panose (get-bytevector-n iport 10))
                   (ulUnicodeRange1 (get-ulong))
                   (ulUnicodeRange2 (get-ulong))
                   (ulUnicodeRange3 (get-ulong))
                   (ulUnicodeRange4 (get-ulong))
                   (achVendID (get-bytevector-n iport 4))
                   (fsSelection (get-ushort))
                   (usFirstCharIndex (get-ushort))
                   (usLastCharIndex (get-ushort))
                   (sTypoAscender (get-short))
                   (sTypoDescender (get-short))
                   (sTypoLineGap (get-short))
                   (usWinAscent (get-ushort))
                   (usWinDescent (get-ushort))
                   (ulCodePageRange1 (get-ulong))
                   (ulCodePageRange2 (get-ulong))
                   (sxHeight (get-short))
                   (sCapHeight (get-short))
                   (usDefaultChar (get-ushort))
                   (usBreakChar (get-ushort))
                   (usMaxContext (get-ushort)))
              (assert (<= version 4))
              (make-OS/2 version
                         xAvgCharWidth
                         usWeightClass
                         usWidthClass
                         fsType
                         ySubscriptXSize
                         ySubscriptYSize
                         ySubscriptXOffset
                         ySubscriptYOffset
                         ySuperscriptXSize
                         ySuperscriptYSize
                         ySuperscriptXOffset
                         ySuperscriptYOffset
                         yStrikeoutSize
                         yStrikeoutPosition
                         sFamilyClass
                         panose
                         ulUnicodeRange1
                         ulUnicodeRange2
                         ulUnicodeRange3
                         ulUnicodeRange4
                         achVendID
                         fsSelection
                         usFirstCharIndex
                         usLastCharIndex
                         sTypoAscender
                         sTypoDescender
                         sTypoLineGap
                         usWinAscent
                         usWinDescent
                         ulCodePageRange1
                         ulCodePageRange2
                         sxHeight
                         sCapHeight
                         usDefaultChar
                         usBreakChar
                         usMaxContext)))

          (define (read-post)
            (go-to-record-table *table-post*)
            (let* ((Version (get-bytevector-n iport 4))
                   (italicAngle (get-bytevector-n iport 4))
                   (underlinePosition (get-fword))
                   (underlineThickness (get-fword))
                   (isFixedPitch (get-ulong))
                   (minMemType42 (get-ulong))
                   (maxMemType42 (get-ulong))
                   (minMemType1 (get-ulong))
                   (maxMemType1 (get-ulong)))
              (make-post Version
                         italicAngle
                         underlinePosition
                         underlineThickness
                         isFixedPitch
                         minMemType42
                         maxMemType42
                         minMemType1
                         maxMemType1)))

          (define (read-glyf *loca*)
            (go-to-record-table *table-glyf*)
            (let* ((base (record-table-offset *table-glyf*))
                   (offsets (loca-offsets *loca*))
                   (n (vector-length offsets))
                   (v (make-vector (- n 1))))
              (let lp ((i 0))
                (if (= i (vector-length v))
                    v
                    (let ((off0 (vector-ref offsets i))
                          (off1 (vector-ref offsets (+ i 1))))
                      (cond ((= off0 off1)
                             (vector-set! v i #f)
                             (lp (+ i 1)))
                            (else
                             (set-port-position! iport (+ off0 base))
                             (let* ((numberOfContours (get-short))
                                    (xMin (get-short))
                                    (yMin (get-short))
                                    (xMax (get-short))
                                    (yMax (get-short)))
                               (vector-set! v i (make-glyf numberOfContours
                                                           xMin
                                                           yMin
                                                           xMax
                                                           yMax)))
                             (lp (+ i 1)))))))))

          (define (read-loca *head* *maxp*)
            (go-to-record-table *table-loca*)
            (let* ((n (+ 1 (maxp-numGlyphs *maxp*)))
                   (v (make-vector n)))

              (define-syntax fill-v!
                (syntax-rules ()
                  ((_ x)
                   (let lp ((i 0)
                            (prev 0))
                     (if (= i n)
                         v
                         (let ((u (x)))
                           (assert (<= prev u))
                           (vector-set! v i u)
                           (lp (+ i 1) u)))))))

              (if (= 0 (head-indexToLocFormat *head*))
                  (fill-v! get-ushort)
                  (fill-v! get-ulong))
              (make-loca v)))

          (let* ((h (read-head))
                 (hh (read-hhea))
                 (m (read-maxp))
                 (l (read-loca h m))
                 (g (read-glyf l)))
            (make-otff (read-cmap)
                       h
                       hh
                       (read-hmtx (hhea-numberOfHMetrics hh) (maxp-numGlyphs m))
                       m
                       (read-name)
                       (read-OS/2)
                       (read-post)
                       #f ; cvt
                       #f ; fpgm
                       g
                       l
                       #f ; prep
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f
                       ))))))

)
