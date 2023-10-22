% Check that variables from script one are accessable
% and have the correct value.
assert (a1 == 1);
assert (b1 == 3);
assert (c1 == 4);
eval ("assert (d1 == 5)");
assert (a2 == 3);
