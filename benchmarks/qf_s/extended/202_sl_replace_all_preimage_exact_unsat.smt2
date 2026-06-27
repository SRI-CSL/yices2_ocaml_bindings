; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "a" "b")))
(assert (str.in_re y (str.to_re "bb")))
(assert (str.in_re x (re.++ (re.range "c" "d") (re.range "c" "d"))))
(check-sat)
