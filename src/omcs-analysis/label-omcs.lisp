;; Simple code to label omcs sentences

(in-package :conceptminer)

(defun number-omcs-w-uid (inputfile outputfile &optional version)
  (with-open-file (in inputfile :direction :input)
    (with-open-file (out outputfile :direction :output :if-exists :supersede)
      (when version (format t ";; Version: ~A~%" version))
      (do-count-contentful-lines (line count in)
;;	(format t "~A ~A~%" count line)
	(awhen (split-omcs-w-uid line)
	  (format out "~A ~A~%" count (cdr it)))))))

(defun split-omcs-w-uid (line)
  (let ((words (conceptnet::split-into-words line)))
    (when (string= (first words) "ip:")
      (cons 
       (unsplit-words (subseq words 0 4))
       (unsplit-words (subseq words 4))))))

(defun unsplit-words (list)
  (apply #'concatenate 'string
	 (shuffle list
		  (when (> (length list) 1) (repeat " " (1- (length list)))))))


