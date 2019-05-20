function s = transpose (s)

  x = double (s);

  s = Snork (x.' + 2);  % +2 To make sure some non-standard tranpose is called.

end
