; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x y))
(assert (str.in_re y (re.++ re.all (str.to_re "m") re.all)))
(assert (not (str.contains x "m")))
(check-sat)
