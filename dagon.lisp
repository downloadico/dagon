
(defpackage dagon
  (:use :common-lisp))

(in-package :dagon)

(ql:quickload :cl-fad)

(declaim (optimize (space 3) (debug 1)))
;;(declaim (optimize (debug 1) (space 2) (speed 3)))

(defun zero-size-p (l)
  "does the file info list represent a zero-sized file?"
  (eql 0 (car (last l))))

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
      (room)))
  ht)

(defun store-size-and-atime (file &optional (time-transform #'round-time))
  (ignore-errors
    (with-file-stat (stat file)
      (with-slots ((size sb-posix::size)
		   (time sb-posix::atime))
	  stat
	(if (< .5 (/ (hash-table-count *ht*) (hash-table-size *ht*)))
	    (setf *ht*  (maybe-squeeze *ht*)))
	(let ((transformed-time (funcall time-transform time)))
	  (incf (gethash transformed-time *ht* 0) size))))))

(defvar *i* 0)
(defvar *max* 10000)
(defun process-file (file &key (fn #'store-size-and-atime))
  "Apply fn to named file"
  (when (<= *max* (incf *i*))
    (format t "*i* is <= ~A~%" *max*)
    (sb-ext:gc :full t)
    (setf *i* 0))
  (ignore-errors (funcall fn file)))

(defun process-dir (directory &key (file-fn #'process-file))
  (multiple-value-bind (files directories)
      (loop  for dir-entry
	       in (cl-fad:list-directory directory :follow-symlinks nil)
	     when  (cl-fad:directory-exists-p dir-entry)
	       collect dir-entry into dirs
	     else collect dir-entry into files
	     finally (return (values files dirs)))
    (loop for file in files
       do (funcall file-fn file))
    (loop for found-dir in directories do
	 (process-dir found-dir
		      :file-fn file-fn))))

(defun keys (ht)
  (loop for k being the hash-keys in ht collect k))


