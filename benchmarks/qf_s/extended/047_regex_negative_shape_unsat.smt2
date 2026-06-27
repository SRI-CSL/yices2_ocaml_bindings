; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.prefixof "a" x))
(assert (not (str.in_re x (re.++ (str.to_re "a") re.all))))
(check-sat)
