function y = f (x)
  y = x .* sin (1 ./ x) .* sqrt (abs (1 - x));
endfunction
[v, ier, nfun, err] = quad ("f", 0, 3);
(ier == 0 || ier == 1) && abs (v - 1.98194122455795) < sqrt (eps) &&
