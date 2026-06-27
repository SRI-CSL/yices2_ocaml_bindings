; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (not (str.in_re x (re.++ re.all (str.to_re "a") re.all))))
(assert (str.contains x "a"))
(check-sat)
