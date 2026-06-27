; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace x "a" "b")))
(assert (str.in_re y (re.union (str.to_re "bb") (str.to_re "bc"))))
(assert (str.in_re x (str.to_re "ca")))
(check-sat)
