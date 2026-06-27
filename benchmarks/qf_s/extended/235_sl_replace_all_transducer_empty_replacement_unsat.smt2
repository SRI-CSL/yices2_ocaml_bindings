; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= y (str.replace_all x "na" "")))
(assert (str.in_re y (str.to_re "ba")))
(assert (str.in_re x (str.to_re "ban")))
(check-sat)
