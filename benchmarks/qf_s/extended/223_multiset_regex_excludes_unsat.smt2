; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.union (str.to_re "ab") (str.to_re "bc"))))
(assert (str.contains x "bb"))
(check-sat)
