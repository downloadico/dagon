
(defpackage dagon
  (:use :common-lisp))

(in-package :dagon)

(ql:quickload :cl-fad)

;;(declaim (optimize (space 3) (debug 3)))
(declaim (optimize (debug 1) (space 2) (speed 3)))

(defun zero-size-p (l)
  "does the file info list represent a zero-sized file?"
  (eql 0 (car (last l))))

(defun lf (file &key (time-type 'atime))
  "return a list of file time and size"
  (ignore-errors         ; in case there is an error finding the file
    (let ((stat (sb-posix:stat file))
	  (time-type (intern (symbol-name time-type) (find-package 'sb-posix))))
      (list
       (slot-value stat time-type) 
       (slot-value stat 'sb-posix::size)))))

(defun list-file (file &key (time-type 'atime))
  "return a list of filename, its time, and size"
  (let ((found (lf file :time-type time-type)))
    (if found
	(cons file found))))

(defun list-dir (directory &key (filenames t) (time-type 'atime))
  (multiple-value-bind (files directories)
	(loop  for e in (cl-fad:list-directory directory)
	   when (cl-fad:directory-exists-p e) collect e into dirs
	   else collect e into files
	   finally (return (values files dirs)))
      (nconc
       (remove nil
	       (if filenames
		   (loop for file in files
		      collect (list-file file :time-type time-type))
		   (loop for file in files collect (lf file :time-type time-type))))
       (loop for found-dir in directories nconc
	    (nconc (list-dir found-dir :time-type time-type :filenames filenames))))))

(defun tot-size (l)
  (loop for f in l sum (car (last f))))
