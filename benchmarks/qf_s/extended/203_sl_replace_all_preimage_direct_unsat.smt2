; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re (str.replace_all x "a" "b") (str.to_re "bb")))
(assert (str.in_re x (re.++ (re.range "c" "d") (re.range "c" "d"))))
(check-sat)
