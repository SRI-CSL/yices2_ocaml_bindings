; EXPECT: sat
(set-logic QF_S)
(assert
  (let ((middle "b"))
    (str.in_re "abc" (re.++ (str.to_re "a") (str.to_re middle) (str.to_re "c")))))
(check-sat)
