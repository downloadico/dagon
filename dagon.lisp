(mapcar #'ql:quickload '( :hunchentoot :cl-who :parenscript :cl-fad :cl-json))

(defpackage dagon
  (:use :common-lisp :hunchentoot :cl-who :parenscript))

(in-package :dagon)

;; (declaim (optimize (space 3) (debug 1)))
;; (declaim (optimize (debug 3) (speed 0)))
(declaim (optimize (debug 1) (speed 3)))

(defmacro with-file-stat ((stat-var file) &body body)
  `(let ((,stat-var (sb-posix:stat ,file)))
     ,@body))

(defmacro with-unix-stat (file &body body)
   `(multiple-value-bind (found-p st_dev st_ino st_mode st_nlink st_uid st_gid st_rdev st_size st_atime st_mtime st_ctime st_blksize st_blocks st_flags)
	(sb-unix:unix-stat ,file)
      ,@body))

(defclass my-stat ()
  ((file :initarg :file)
   (uid :initarg :uid)
   (gid  :initarg :gid)
   (size  :initarg :size)
   (ctime :initarg :ctime)
   (mtime :initarg :mtime)))

(defun file-info (file)
  (with-unix-stat (format nil "~A" file)
    (if found-p
	`(,(make-instance 'my-stat :file (format nil "~A" file) :uid st_uid  :gid st_gid :size st_size :mtime st_mtime :ctime st_ctime)))))

(defun resolve-pathname (path)
  (uiop/filesystem:resolve-symlinks path))

(defun dagon (path &optional (output "/tmp/dagon.out"))
  (with-open-file (*standard-output* output
				     :direction :output
				     :if-exists :overwrite
				     :if-does-not-exist :create)
    (process-dir path)))

(let ((crumbs (make-hash-table :test #'equal)))
  (defun process-dir (directory)
    (let ((directory (resolve-pathname directory)))
      (if (gethash directory crumbs)
	(warn "not entering ~A~%" directory)
	(progn
	  (setf (gethash directory crumbs) t)
	  (warn "entering dir ~A" directory)
	  (let ((dirents (cl-fad:list-directory
			  directory
			  :follow-symlinks nil)))
	    (loop for ent in dirents when (and (cl-fad:directory-pathname-p ent)
					       (not (gethash (resolve-pathname ent) crumbs)))
		  do (process-dir ent))
	    (loop for js
		    in
		    (loop for ent in dirents nconc (file-info ent)) do
		      (format t "~A~%" (json:encode-json-to-string js))
		  )))))))
