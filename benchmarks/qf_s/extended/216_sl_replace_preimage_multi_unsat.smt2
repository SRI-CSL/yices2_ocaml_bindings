; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace x "ab" "c")))
(assert (str.in_re y (str.to_re "cc")))
(assert (str.in_re x (str.to_re "abcc")))
(check-sat)
