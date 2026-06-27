; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "b"))
(assert (str.in_re x (re.range "a" "a")))
(check-sat)
