a1 = 1;
b1 = 3;
eval ("c1 = 4;")
eval ("d1 = 5;")

% Test function definitions inlined in scripts
function inlinefn1 (a)
  assert (!exist ("a1")) % Ensure inlinefn1 has its own scope
  b = inlinefn2 (a);
  assert (b == a + 1);
end

inlinefn2 = 3; % Assure a function can be defined when id exists with that name

function b = inlinefn2 (a)
  b = a + 1;
end

assert (inlinefn2 (3) == 4);
inlinefn1 (123);

script11 ();

assert (a11 == 1)
assert (b11 == 3)
eval ("assert (c11 == 4)")
eval ("assert (d11 == 5)")
