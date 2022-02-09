(ql:quickload :drakma)
(ql:quickload :cl-json)
 
(defvar *target* "http://10.10.241.214:3000")
 
(defun get-json (target)
  (map 'string #'code-char (drakma:http-request target)))
 
(defun decode-json (json-string)
  (with-input-from-string (s json-string)
    (json:decode-json s)))
 
(defun decode-page (page)
  (decode-json (get-json (concatenate 'string *target* "/" page))))
 
(defun capture-flag ()
  (with-output-to-string (s)
    (do* ((next "" (cdr (assoc :next map)))
          (map #1=(decode-page next) #1#)
          (val #2=(cdr (assoc :value map)) #2#))
         ((equal val "end"))
      (princ val s))))
 
(print (capture-flag))