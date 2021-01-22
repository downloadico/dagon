
(defpackage dagon
  (:use :common-lisp))

(in-package :dagon)

(ql:quickload :cl-fad)

;;(declaim (optimize (space 3) (debug 1)))
(declaim (optimize (debug 1) (space 2) (speed 3)))

(defmacro with-file-stat ((stat-var file) &body body)
  `(let ((,stat-var (sb-posix:stat ,file)))
     ,@body))

(defvar *chunk-size* 600)
(defvar *ht-size* 4096)
(defvar *ht* (make-hash-table :size *ht-size*))

(defun reset ()
  (setf *chunk-size* 600)
  (setf *ht-size* 4096)
  (setf *ht* (make-hash-table :size *ht-size*))
  (sb-ext:gc :full t))

(defun round-time (n)
  (- n (mod n *chunk-size*)))

(defun condense-table (ht chunk-size)
  (warn "condensing ht chunk-size: ~A" chunk-size)
  (let ((new-ht (make-hash-table :size *ht-size*)))
    (loop for k being the hash-keys in ht
       do
	 (incf (gethash
		(- k (mod k chunk-size))
		new-ht 0)
	       (gethash k ht)))
    new-ht))

(defun maybe-squeeze (ht)
  (when (< .8 (/ (hash-table-count ht) (hash-table-size ht)))
    (progn
      (format t "current usage: ~A~%" (/ (hash-table-count ht) (hash-table-size ht)))
      (format t "before condense ~A/~A~%" (hash-table-count ht) (hash-table-size ht))
      
      (setf ht
	    (condense-table ht *chunk-size*))
      (format t "Now ~A entries of ~A~%chunk-size: ~A~%"
	      (hash-table-count ht)
	      (hash-table-size ht)
	      *chunk-size*)
      (when  (< .8 (/ (hash-table-count ht) (hash-table-size ht)))
	(setf *chunk-size* (* 10 *chunk-size*))
	(format t "larger chunk ~A~%" *chunk-size*)
	(setf ht
	      (condense-table ht *chunk-size*))
	)
      
      (when  (< .5 (/ (hash-table-count ht) (hash-table-size ht)))
	(setf *ht-size* (* 2 *ht-size*))
	(format t "larger size ~A~%" *ht-size*)
	(setf ht
	      (condense-table ht *chunk-size*))
	)
      
	      
      (format t "Now ~A entries of ~A~%chunk-size: ~A~%"
	      (hash-table-count ht)
	      (hash-table-size ht)
	      *chunk-size*)
      (sb-ext:gc :full t)
      ;(room)
      ))
  ht)

(defun store-size-and-atime (file &optional (time-transform #'round-time))
  (ignore-errors
    (with-file-stat (stat file)
      (with-slots ((size sb-posix::size)
		   (time sb-posix::atime))
	  stat
	(when (< .5 (/ (hash-table-count *ht*) (hash-table-size *ht*)))
	  (setf *ht*  (maybe-squeeze *ht*)))
	(let ((transformed-time (funcall time-transform time)))
	  (incf (gethash transformed-time *ht* 0) size))))))

(defun process-file (file &key (fn #'store-size-and-atime))
  "Apply fn to named file"
  (funcall fn file))

(defun keys (ht)
  (loop for k being the hash-keys in ht collect k))

(let ((*unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0)))
  (defun universal-to-unix-time (universal-time)
    (- universal-time *unix-epoch-difference*))

  (defun unix-to-universal-time (unix-time)
    (+ unix-time *unix-epoch-difference*))

  (defun get-unix-time ()
    (universal-to-unix-time (get-universal-time)))
)
