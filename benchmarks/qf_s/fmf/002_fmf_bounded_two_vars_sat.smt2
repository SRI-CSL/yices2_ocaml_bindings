; EXPECT: sat
; FMF_MAX_TOTAL_LENGTH: 3
; FMF_MAX_ROUNDS: 4
; FMF_EXPECT_TRACE: bounded sat at total length 3
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x "a"))
(assert (= y "bc"))
(check-sat)
