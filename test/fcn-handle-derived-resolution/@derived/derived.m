function r = derived (n)
  s.a = n;
  p = parent (n);
  r = class (s, 'derived', p);
end
