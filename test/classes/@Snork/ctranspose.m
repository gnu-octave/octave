function s = ctranspose (s)

  x = double (s);

  s = Snork (x' + 4);  % +4 To make sure some non-standard tranpose is called.

end
