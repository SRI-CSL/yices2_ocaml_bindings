; EXPECT: sat
; FMF_MAX_TOTAL_LENGTH: 2
; FMF_MAX_ROUNDS: 3
; FMF_EXPECT_TRACE: bounded sat at total length 2
(set-logic QF_S)
(declare-const x String)
(assert (= x "ab"))
(check-sat)
