; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "a" "b")))
(assert (str.in_re y (re.* (str.to_re "bb"))))
(assert (= (str.len x) 1))
(check-sat)
