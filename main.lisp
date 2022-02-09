(defpackage common-lisp-1-a
  (:use :cl)
  (:export :make-pokemon :battle))
(in-package :common-lisp-1-a)

(defparameter *matchups*
  '((:fire . :grass)
    (:grass . :water)
    (:water . :fire)))

(defconstant +attack-weakness-factor+ 2)

(defstruct pokemon name type atk hp)

(defun battle-turn (defender attacker)
  (let* ((dfn-type (pokemon-type defender))
	 (atk-type (pokemon-type attacker))
	 (dfn-weak (assoc atk-type *matchups*))
	 (atk-weak (rassoc dfn-type *matchups*))
         (damage (* (pokemon-atk attacker)
                    (cond ((eql dfn-type (cdr dfn-weak))
                           +attack-weakness-factor+)
                          ((eql dfn-type (cdr atk-weak))
                           (/ 1 +attack-weakness-factor+))
                          (t 1)))))
    (decf (pokemon-hp defender)
          damage)))

(defun battle (p1 p2)
  (loop
     for order = (list p2 p1) then (reverse order)
     until (or (> 1 (pokemon-hp p1))
               (> 1 (pokemon-hp p2)))
     do (battle-turn (first order)
                     (second order))
     finally (return (pokemon-name
               (car (sort (list p1 p2)
                          #'>
                          :key #'pokemon-hp))))))
