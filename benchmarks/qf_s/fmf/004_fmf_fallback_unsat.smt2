; EXPECT: unsat
; FMF_MAX_TOTAL_LENGTH: 2
; FMF_MAX_ROUNDS: 3
; FMF_EXPECT_TRACE: falling back after all bounded rounds through total length 2
(set-logic QF_SLIA)
(declare-const x String)
(assert (= (str.len x) 1))
(assert (= (str.len x) 2))
(check-sat)
