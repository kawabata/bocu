;;; bocu.el --- BOCU decoding software -*- lexical-binding: t -*-

;; Copyright (C) 2024 Taichi Kawabata

;; Author: Taichi Kawabata <kawabata.taichi@gmail.com>
;; Keywords: i18n, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; BOCU (Binary Ordered Compression for Unicode) implementation.

;;; Code:

(define-ccl-program bocu-normalize-code-point
  '(1
    (if (r1 < #x20) (r4 = #x40) ; C0 → Reset NCP to #x40
      (if (r1 == #x20) () ; (r4 = r4) space → Keep NCP
        (if (r1 < #x80) (r4 = #x40) ; ASCII → Reset NCP to #x40
          ((r5 = (r1 >= #x3040)) ; Hiragana
           (r6 = (r1 <= #x309f))
           (if (r5 & r6) (r4 = #x3070)
             ((r5 = (r1 >= #x4e00)) ; CJK Ideographs
              (r6 = (r1 <= #x9fa5))
              (if (r5 & r6) (r4 = #x7711)
                ((r5 = (r1 >= #xac00)) ; Hangul
                 (r6 = (r1 <= #xd7a3))
                 (if (r5 & r6) (r4 = #xc1d1)
                   ((r5 = (r1 & #xff)) ; Other characters
                    ;; (r6 = (r1 & #xffffff00))
                    (r6 = (r1 & -256))
                    (if (r5 < #x80) (r4 = (r6 + #x40))
                      (r4 = (r6 + #xc0))))))))))))))
  "Calculate normal code point (r4) from previous character (r1).")

(define-ccl-program bocu-read-r2
  '(1
    (read-if (r2 > #x20) (r2 -= 13)
	     (if (r2 >= #x1c) (r2 -= 12)
	       (if (r2 >= #x10) (r2 -= 10)
		 (r2 -= 1)))))
  "Bytes 00 07 08 09 0A 0B 0C 0D 0E 0F 1A 1B 20 are skipped.")

(define-ccl-program decode-bocu
  `(4
    ((r4 = #x40)
     (r3 = ,(charset-id-internal 'unicode))
     (loop
      (read r0)
      ;; Diff calculation phase
      (if (r0 <= #x20) (r1 = r0)
        (if (r0 == #x21)
            ((r1 = -14536567)     (call bocu-read-r2)
             (r1 += (r2 * 59049)) (call bocu-read-r2)
             (r1 += (r2 * 243))   (call bocu-read-r2)
             (r1 += r2))
          (if (r0 < #x25)
              ((r1 = (((r0 - #x25) * 59049) - 10513)) (call bocu-read-r2)
               (r1 += (r2 * 243))                     (call bocu-read-r2)
               (r1 += r2))
            (if (r0 < #x50)
                ((r1 = (((r0 - #x50) * 243) - 64)) (call bocu-read-r2)
                 (r1 += r2))
              (if (r0 < #xd0)
                  (r1 = (r0 - #x90))
                (if (r0 < #xfb)
                    ((r1 = (((r0 - #xd0) * 243) + 64)) (call bocu-read-r2)
                     (r1 += r2))
                  (if (r0 < #xfe)
                      ((r1 = (((r0 - #xfb) * 59049) + 10513)) (call bocu-read-r2)
                       (r1 += (r2 * 243))                     (call bocu-read-r2)
                       (r1 += r2))
                    (if (r0 == #xfe)
                        ((r1 = 187660)        (call bocu-read-r2)
                         (r1 += (r2 * 59049)) (call bocu-read-r2)
                         (r1 += (r2 * 243))   (call bocu-read-r2)
                         (r1 += r2)
                         ;; ignore case: `r0 = #xff'
                         )))))))))
      ;; output stage
      (if (r0 <= #x20)
          ((if (r0 != 13) (write r0))
           (if (r0 < #x20) (r4 = #x40)))
        (if (r0 < #xff)
            ((r1 += r4)
             (if (r1 < 0) (r1 = 0)) ; error recovery
             (write-multibyte-character r3 r1)
             ;; Normalized CP renewal stage
             (call bocu-normalize-code-point)
             )))
      (repeat)))))

;;###autoload
(defun bocu-to-string (string)
  "Decode BOCU STRING to Emacs String."
  (ccl-execute-on-string 'decode-bocu (vector 0 0 0 0 0 0 0 0 0) string))

(define-ccl-program bocu-write-r0
  '(1
    (if (r0 >= #x14) (r0 += 13)
      (if (r0 >= #x10) (r0 += 12)
	(if (r0 >= #x06) (r0 += 10)
	  (r0 += 1))))
    (write r0))
  "Bytes 00 07 08 09 0A 0B 0C 0D 0E 0F 1A 1B 20 are skipped.")

(define-ccl-program encode-bocu
  `(4
    ((r4 = #x40) ; default diff
     (loop
      (read r1) ; unicode
      (if (r1 <= #x20) (write r1)
        ;; diff and output
        ((r2 = (r1 - r4))
         (if (r2 <= #x-2dd0d)
             ((r2 = (r2 + 301514707)) ; 300400692 [21 F0 58 D9] + #x10ff9f
              (r0 = (r2 / 14348907))      (call bocu-write-r0)
              (r0 = ((r2 / 59049) % 243)) (call bocu-write-r0)
              (r0 = ((r2 / 243) % 243))   (call bocu-write-r0)
              (r0 = (r2 % 243))           (call bocu-write-r0))
           (if (r2 <= #x-2912)
               ((r2 = (r2 + 1427689)) ; 1240029 [22 01 01] + #x2DD0C
                (r0 = ((r2 / 59049) % 243)) (call bocu-write-r0)
                (r0 = ((r2 / 243) % 243))   (call bocu-write-r0)
                (r0 = (r2 % 243))           (call bocu-write-r0))
             (if (r2 <= #x-41)
                 ((r2 = (r2 + 16345)) ; 5832 [25 01] + #x2911
                  (r0 = ((r2 / 243) % 243)) (call bocu-write-r0)
                  (r0 = (r2 % 243))         (call bocu-write-r0))
               (if (r2 <= #x3F)
                   ((r2 = (r2 + 131)) ; 67 [50] + #x40
                    (r0 = (r2 % 243)) (call bocu-write-r0))
                 (if (r2 <= #x2910)
                     ((r2 = (r2 + 47321)) ; 47385 [D0 01] + #x-40
                      (r0 = ((r2 / 243) % 243))   (call bocu-write-r0)
                      (r0 = (r2 % 243))           (call bocu-write-r0))
                   (if (r2 <= #x2DD0B)
                       ((r2 = (r2 + 14043149)) ;14053662 [FB 01 01] + #x-2911
                        (r0 = ((r2 / 59049) % 243)) (call bocu-write-r0)
                        (r0 = ((r2 / 243) % 243))   (call bocu-write-r0)
                        (r0 = (r2 % 243))           (call bocu-write-r0))
                     ((r2 = (r2 + 3457898927)) ; 3458086587 [FE 01 01 01] + #x-2dd0c
                      (r0 = (r2 / 14348907))      (call bocu-write-r0)
                      (r0 = ((r2 / 59049) % 243)) (call bocu-write-r0)
                      (r0 = ((r2 / 243) % 243))   (call bocu-write-r0)
                      (r0 = (r2 % 243))           (call bocu-write-r0))))))))
         ))
      ;; calculate normalized code point
      (call bocu-normalize-code-point)
      (repeat)
      ))))

(defun bocu-from-string (string)
  "Encode BOCU STRING from Emacs String."
  (ccl-execute-on-string 'encode-bocu (vector 0 0 0 0 0 0 0 0 0) string))

(define-coding-system 'bocu-1
  "BOCU-1 encoding system."
  :coding-type 'ccl
  :mnemonic ?B
  :character-list '(unicode)
  :mime-charset 'bocu-1
  :ccl-encoder 'encode-bocu
  :ccl-decoder 'decode-bocu)

;;(define-coding-system 'bocu-1-with-signature
;;  "BOCU-1 encoding system."
;;  :coding-type 'ccl
;;  :mnemonic ?B
;;  :character-list '(unicode)
;;  :mime-charset 'bocu-1
;;  :ccl-encoder 'encode-bocu
;;  :ccl-decoder 'decode-bocu
;;  :bom t)

(provide 'bocu)

;;; bocu-test.el ends here
