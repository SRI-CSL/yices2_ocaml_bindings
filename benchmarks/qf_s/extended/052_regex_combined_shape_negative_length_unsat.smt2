; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.union (str.to_re "a") (str.to_re "ab") (str.to_re "ba"))))
(assert (str.prefixof "a" x))
(assert (not (str.in_re x (str.to_re "ab"))))
(assert (= (str.len x) 2))
(check-sat)
