; EXPECT: sat
(set-logic QF_S)
(declare-const |strange name| String)
(assert (= |strange name| "\u{61}"))
(check-sat)
