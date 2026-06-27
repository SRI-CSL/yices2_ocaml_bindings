; EXPECT: sat
; FMF_MAX_TOTAL_LENGTH: 1
; FMF_MAX_ROUNDS: 2
; FMF_EXPECT_TRACE: falling back after all bounded rounds through total length 1
(set-logic QF_S)
(declare-const x String)
(assert (= x "abc"))
(check-sat)
