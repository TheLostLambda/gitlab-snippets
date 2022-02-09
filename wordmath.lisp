(defpackage wordmath
  (:use :cl))
(in-package :wordmath)

(defmacro generate-numbers (limit)
  `(progn ,@(loop :for n :from 0 :to limit :collect
                  `(defmacro ,(intern (format nil "~@:(~R~)" n)) (&rest op)
                     (if op (funcall (eval op) ,n) ,n)))))

(defmacro generate-operators (&rest ops)
  `(progn ,@(loop :for op :in ops :collect
                  `(defmacro ,(car op) (&rest rhs)
                     (lambda (lhs) (,(cdr op) lhs (eval rhs)))))))

(generate-numbers 99)
(generate-operators (plus . +) (minus . -) (times . *) (divided-by . /))

;; For generating test cases
;; (in-package :cl-user)
;; (defun generate-tests (n op)
;;   (dotimes (_ n)
;;     (let ((a (random 100)) (b (random 100)))
;;       (print `(is (= ,(funcall (cdr op) a b) (,(numsym a) ,(car op) ,(numsym b))))))))

;; (defun numsym (x)
;;   (intern (format nil "~@:(~R~)" x)))

;; (let ((*print-case* :downcase))
;;   (generate-tests 10 '(plus . +))
;;   (generate-tests 10 '(minus . -))
;;   (generate-tests 10 '(times . *))
;;   (generate-tests 10 '(divided-by . /)))
