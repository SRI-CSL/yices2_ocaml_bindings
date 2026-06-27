; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.++ (str.to_re "abc") re.all)))
(assert (= (str.len x) 2))
(check-sat)
