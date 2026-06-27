; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (not (str.contains x "a")))
(assert (= y (str.replace_all x "a" "bb")))
(assert (not (= (str.len y) (str.len x))))
(check-sat)
