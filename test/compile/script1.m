a1 = 1;
b1 = 3;
eval ("c1 = 4;")
eval ("d1 = 5;")

script11 ();

assert (a11 == 1)
assert (b11 == 3)
eval ("assert (c11 == 4)")
eval ("assert (d11 == 5)")