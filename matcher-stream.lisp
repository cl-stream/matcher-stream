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

(in-package :matcher-stream)

(defclass matcher-stream (super-stream buffered-input-stream)
  ((line :initarg :input-line
         :initform 0
         :accessor matcher-input-line
         :type fixnum+)
   (column :initarg :input-column
           :initform 0
           :accessor matcher-input-column
           :type fixnum+)
   (input-ended :initform nil
                :accessor matcher-input-ended
                :type boolean)))

(defgeneric matcher-input (matcher-stream)
  (:documentation "Read a character from underlying stream and put it
 in input buffer."))

(defgeneric matcher-input-n (matcher-stream n)
  (:documentation "Ensure at least N characters are in input buffer."))

(defgeneric matcher-char (matcher-stream index)
  (:documentation "Ensure input and return the character from input
buffer for matching at index."))

(defgeneric matcher-start (matcher-stream)
  (:documentation "Return match start for STREAM."))

(defgeneric (setf matcher-start) (value matcher-stream)
  (:documentation "Set match start for STREAM."))

(defgeneric matcher-push-extend (stream character)
  (:documentation "Put a character into input buffer, extending it by
*BUFFER-SIZE* if necessary."))

(defgeneric match (matcher-stream thing)
  (:documentation "Advance match start if THING is matched in
input buffer."))

(defgeneric match-not (matcher-stream thing))

(defgeneric match-option (matcher-stream thing))

(defgeneric match-times (matcher-stream thing min max))

(defgeneric match-until (matcher-stream thing)
  (:documentation "Advance match start until THING is matched in
input buffer."))

;;  Input

(defmethod make-stream-input-buffer ((mh matcher-stream))
  (let* ((in (stream-underlying-stream mh))
         (element-type (stream-element-type in)))
    (make-array `(,(stream-input-buffer-size mh))
                :element-type element-type
                :fill-pointer 0)))

(defmethod matcher-push-extend ((mh matcher-stream) item)
  (let* ((buffer (stream-input-buffer mh))
         (ibs (stream-input-buffer-size mh))
         (fp (fill-pointer buffer))
         (new-fp (1+ fp)))
    (declare (type fixnum ibs fp new-fp))
    (if (= fp (the fixnum (array-dimension buffer 0)))
        (let ((new-buffer (adjust-array buffer
                                        (the fixnum (+ fp ibs))
                                        :fill-pointer new-fp)))
          (setf (stream-input-buffer mh) new-buffer))
        (setf (fill-pointer buffer) new-fp))
    (locally (declare (optimize (safety 0)))
      (setf (aref buffer fp) item))
    fp))

(defmethod matcher-input ((mh matcher-stream))
  (let ((in (stream-underlying-stream mh)))
    (multiple-value-bind (item state) (stream-read in)
      (ecase state
        ((nil) (let* ((pos (the fixnum (matcher-push-extend mh item)))
                      (buf (stream-input-buffer mh)))
                 (declare (type vector buf))
                 (cond ((or (and (char= #\Newline item)
                                 (or (not (< 0 pos))
                                     (char/= #\Return
                                             (char buf (1- pos)))))
                            (char= #\Return item))
                        (setf (matcher-input-column mh) 0)
                        (incf (the fixnum (matcher-input-line mh))))
                       (t
                        (incf (the fixnum (matcher-input-column mh)))))
                 (values item nil)))
        ((:eof) (setf (matcher-input-ended mh) t)
         (values nil :eof))
        ((:non-blocking)
         (signal (make-condition 'non-blocking :stream mh)))))))

(defmethod matcher-start ((mh matcher-stream))
  (stream-input-index mh))

(defmethod (setf matcher-start) ((value integer) (mh matcher-stream))
  (setf (stream-input-index mh) (the fixnum value)))

(defmethod matcher-input-n ((mh matcher-stream) (n integer))
  (declare (type fixnum n))
  (loop
     (let ((length (- (the fixnum (fill-pointer (stream-input-buffer mh)))
                      (the fixnum (matcher-start mh)))))
       (declare (type fixnum length))
       (when (or (matcher-input-ended mh)
                 (<= n length))
         (return))
       (matcher-input mh))))

(defmethod matcher-char ((mh matcher-stream) (index integer))
  (declare (type fixnum index))
  (matcher-input-n mh (the fixnum (1+ index)))
  (let ((buf (stream-input-buffer mh))
        (match-index (+ (the fixnum (matcher-start mh))
                        index)))
    (declare (type (vector character) buf)
             (type fixnum match-index))
    (char buf match-index)))

;;  Matcher

(defmethod match ((mh matcher-stream) (s string))
  (let* ((length (length s))
         (match-start (matcher-start mh))
         (match-end (+ match-start length))
         (buffer (stream-input-buffer mh)))
    (declare (type fixnum length match-start match-end)
             (type (vector character) buffer))
    (matcher-input-n mh length)
    (when (and (<= match-end (length buffer))
               (string= s buffer :start2 match-start :end2 match-end))
      (incf (the fixnum (matcher-start mh)) length))))

(defmethod match ((mh matcher-stream) (c character))
  (when (char= c (matcher-char mh 0))
    (incf (the fixnum (matcher-start mh)))))

(defmethod match-until ((mh matcher-stream) (s string))
  (matcher-input-n mh (length s))
  (loop
     (let ((match (match mh s)))
       (when match
         (return match)))
     (when (matcher-input-ended mh)
       (return))
     (matcher-input mh)
     (incf (the fixnum (matcher-start mh)))))

(defmethod match-until ((mh matcher-stream) (f function))
  (loop
     (let ((match (funcall f mh)))
       (when match
         (return match)))
     (when (matcher-input-ended mh)
       (return))
     (matcher-input mh)
     (incf (the fixnum (matcher-start mh)))))

(defmethod match-option ((mh matcher-stream) (f function))
  (or (funcall f mh)
      (matcher-start mh)))

(defmethod match-not ((mh matcher-stream) (f function))
  (let ((match-start (matcher-start mh)))
    (cond ((or (funcall f mh)
               (matcher-input-ended mh))
           (setf (matcher-start mh) match-start)
           nil)
          (t
           (incf (the fixnum (matcher-start mh)))))))

(defmacro match-sequence (matcher-stream &body body)
  (let ((mh (gensym "MH-"))
	(match-start (gensym "MATCH-START-"))
	(result (gensym "RESULT-")))
    `(let* ((,mh ,matcher-stream)
	    (,match-start (matcher-start ,mh))
	    (,result (progn ,@body)))
       (cond (,result
	      ,result)
	     (t
	      (setf (matcher-start ,mh) ,match-start)
	      nil)))))

(defmethod match-times ((mh matcher-stream) (f function) (min integer) (max integer))
  (declare (type fixnum min max))
  (match-sequence mh
    (let ((n 0))
      (loop
         (unless (< n max)
           (return (matcher-start mh)))
         (unless (funcall f mh)
           (if (< n min)
               (return nil)
               (return (matcher-start mh))))
         (incf n)))))

(defmethod match-times ((mh matcher-stream) (f function) (min integer) (max null))
  (declare (type fixnum min))
  (match-sequence mh
    (let ((n 0))
      (declare (type fixnum n))
      (loop
         (unless (funcall f mh)
           (if (< n min)
               (return nil)
               (return (matcher-start mh))))
         (incf n)))))
