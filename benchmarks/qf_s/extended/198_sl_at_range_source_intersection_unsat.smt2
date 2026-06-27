; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const c String)
(assert (= c (str.at x 0)))
(assert (str.in_re c (re.range "a" "c")))
(assert (str.in_re x (re.range "d" "f")))
(check-sat)
