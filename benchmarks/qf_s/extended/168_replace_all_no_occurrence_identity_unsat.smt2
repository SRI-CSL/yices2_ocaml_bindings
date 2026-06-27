; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (not (str.contains x "a")))
(assert (= y (str.replace_all x "a" "bb")))
(assert (not (= y x)))
(check-sat)
