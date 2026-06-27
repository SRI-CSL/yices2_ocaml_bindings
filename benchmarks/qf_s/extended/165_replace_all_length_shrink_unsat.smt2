; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "ab" "")))
(assert (> (str.len y) (str.len x)))
(check-sat)
