; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "a" "b")))
(assert (str.in_re y (re.range "b" "b")))
(assert (str.in_re x (re.range "c" "d")))
(check-sat)
