;;
;;  matcher-stream  -  Character matcher classes for cl-stream
;;
;;  Copyright 2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :common-lisp)

(defpackage :matcher-stream
  (:use :cl-stream
        :common-lisp)
  #.(cl-stream:shadowing-import-from)
  (:export
   #:match
   #:match-not
   #:match-option
   #:match-sequence
   #:match-times
   #:match-until
   #:matcher
   #:matcher-buffer
   #:matcher-char
   #:matcher-eof-p
   #:matcher-input
   #:matcher-input-column
   #:matcher-input-ended
   #:matcher-input-line
   #:matcher-input-n
   #:matcher-start
   #:matcher-stream
   ))
