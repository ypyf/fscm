;(the "rain" in "spain" falls mainly on the "plain")
(let ((x "rain")
(y "spain")
 (z "plain"))
 (quasiquote
 (the (unquote x)
 in (unquote y)
 falls mainly on the (unquote z))))