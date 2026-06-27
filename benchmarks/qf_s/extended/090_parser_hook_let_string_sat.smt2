; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(assert (= (let ((y (str.++ "a" "b"))) y) x))
(assert (= x "ab"))
(check-sat)
