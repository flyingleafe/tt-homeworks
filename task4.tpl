true = \a.\b.a
false = \a.\b.b
c0 = \s.\z.z
c1 = \s.\z.s z
c5 = \s.\z.s (s (s (s (s z))))
c2 = \s.\z.s (s z)
inc = \a.\s.\z.s (a s z)
mul = \a.\b.\s.a (b s)
plus = \a.\b.\s.\z.a (b s z)
c10 = mul c5 c2
pair = \a.\b.\s.s a b
fst = \p.p true
snd = \p.p false
iszero = \a.a (\x.false) true
dec = \n.snd (n (\p.pair (inc (fst p)) (fst p)) (pair c0 c0))
fix = \f. (\x. f (\y. x x y)) (\x. f (\y. x x y))
facg = \fct. \n. (iszero n) c1 (mul n (fct (dec n)))
factorial = fix facg
out = factorial c2
