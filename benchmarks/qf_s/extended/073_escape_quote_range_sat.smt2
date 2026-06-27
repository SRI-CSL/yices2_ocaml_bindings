; EXPECT: sat
(set-logic QF_S)
(assert (str.in_re """" (re.range "\u{22}" "$")))
(check-sat)
