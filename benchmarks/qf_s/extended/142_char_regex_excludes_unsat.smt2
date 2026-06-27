; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (re.range "a" "c"))))
(assert (str.contains x "z"))
(check-sat)
