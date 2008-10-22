function y = mtimes (a, b)
  ap = double (a);
  bp = double (b);
  y = polynomial (filter (ap, 1, [bp(:).', zeros(1, length(bp) - 1)]));
endfunction