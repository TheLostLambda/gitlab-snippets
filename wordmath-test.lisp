(load "wordmath.lisp")
(ql:quickload :fiveam)
(defpackage wordmath-test
  (:use :cl :fiveam :wordmath)
  (:export :run-tests))
(in-package :wordmath-test)

(def-suite wordmath-suite)
(in-suite wordmath-suite)

;; This is so the student solution doesn't need to export more than
;; one hundred symbols. We will just import everything in that package.
(do-symbols (sym :wordmath)
  (import sym))

(test small-nums "Test numbers without operators less than 5"
  (is (= 0 (zero)))
  (is (= 1 (one)))
  (is (= 2 (two)))
  (is (= 3 (three)))
  (is (= 4 (four)))
  (is (= 5 (five))))

(test small-add "Test addition with numbers less than 5"
  (is (= 1 (one plus zero)))
  (is (= 2 (one plus one)))
  (is (= 3 (two plus one)))
  (is (= 4 (two plus two)))
  (is (= 5 (one plus four)))
  (is (= 6 (three plus three)))
  (is (= 7 (four plus three)))
  (is (= 8 (five plus three)))
  (is (= 9 (four plus five)))
  (is (= 10 (five plus five))))

(test small-sub "Test subtraction with numbers less than 5"
  (is (= 0 (three minus three)))
  (is (= 1 (three minus two)))
  (is (= 2 (four minus two)))
  (is (= 3 (five minus two)))
  (is (= 4 (five minus one)))
  (is (= 5 (five minus zero))))

(test numbers "Test numbers without operators"
  (is (= 72 (seventy-two))) 
  (is (= 84 (eighty-four))) 
  (is (= 40 (forty))) 
  (is (= 26 (twenty-six)))
  (is (= 43 (forty-three))) 
  (is (= 12 (twelve))) 
  (is (= 18 (eighteen))) 
  (is (= 9 (nine))) 
  (is (= 14 (fourteen))) 
  (is (= 63 (sixty-three))))

(test add "Test addition (0-99)"
  (is (= 75 (fifty-one plus twenty-four))) 
  (is (= 125 (thirty-one plus ninety-four))) 
  (is (= 16 (four plus twelve))) 
  (is (= 177 (ninety-six plus eighty-one))) 
  (is (= 99 (ninety-one plus eight))) 
  (is (= 20 (three plus seventeen))) 
  (is (= 80 (twenty-one plus fifty-nine))) 
  (is (= 37 (twenty-six plus eleven))) 
  (is (= 60 (forty-seven plus thirteen))) 
  (is (= 22 (nineteen plus three))))

(test subtract "Test subtraction (0-99)"
  (is (= 66 (ninety-three minus twenty-seven))) 
  (is (= -5 (eighty-three minus eighty-eight))) 
  (is (= 29 (sixty-five minus thirty-six))) 
  (is (= 14 (fifty-four minus forty))) 
  (is (= 27 (eighty-one minus fifty-four))) 
  (is (= 1 (ninety-six minus ninety-five))) 
  (is (= 59 (seventy-eight minus nineteen))) 
  (is (= 21 (sixty-nine minus forty-eight))) 
  (is (= -32 (thirty-nine minus seventy-one))) 
  (is (= 47 (fifty-two minus five))))

(test multiply "Test multiplication (0-99)"
  (is (= 190 (ninety-five times two))) 
  (is (= 1674 (eighteen times ninety-three))) 
  (is (= 5796 (sixty-nine times eighty-four))) 
  (is (= 806 (thirteen times sixty-two))) 
  (is (= 380 (thirty-eight times ten))) 
  (is (= 3096 (seventy-two times forty-three))) 
  (is (= 3168 (forty-eight times sixty-six))) 
  (is (= 4056 (fifty-two times seventy-eight))) 
  (is (= 470 (ten times forty-seven))) 
  (is (= 1372 (twenty-eight times forty-nine))))

(test division "Test division (0-99)"
  (is (= 20/89 (twenty divided-by eighty-nine))) 
  (is (= 4/11 (thirty-two divided-by eighty-eight))) 
  (is (= 6/13 (forty-two divided-by ninety-one))) 
  (is (= 95/23 (ninety-five divided-by twenty-three))) 
  (is (= 71/63 (seventy-one divided-by sixty-three))) 
  (is (= 97/35 (ninety-seven divided-by thirty-five))) 
  (is (= 13/3 (seventy-eight divided-by eighteen))) 
  (is (= 57/91 (fifty-seven divided-by ninety-one))) 
  (is (= 42/19 (forty-two divided-by nineteen))) 
  (is (= 45/4 (ninety divided-by eight))))

(defun run-tests (&optional (explain t))
  (let ((tests (run 'wordmath-suite)))
    (if explain (explain! tests) tests)))
