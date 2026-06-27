; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.++ re.all (str.to_re "z") re.all)))
(assert (not (str.contains x "z")))
(check-sat)
