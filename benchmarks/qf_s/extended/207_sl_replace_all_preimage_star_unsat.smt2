; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "a" "b")))
(assert (str.in_re y (re.* (str.to_re "b"))))
(assert (str.in_re x (re.++ (re.range "c" "d") (re.range "c" "d"))))
(check-sat)
