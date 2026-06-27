; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x re.all))
(assert (not (str.in_re x re.all)))
(check-sat)
