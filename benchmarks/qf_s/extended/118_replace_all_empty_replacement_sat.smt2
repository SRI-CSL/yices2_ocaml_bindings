; EXPECT: sat
(set-logic QF_S)
(assert (= (str.replace_all "banana" "na" "") "ba"))
(check-sat)
