function y = f (x) y = x + 1; endfunction
[v, ier, nfun, err] = quad ("f", 0, 5);
ier == 0 && abs (v - 17.5) < sqrt (eps) && nfun > 0 && err < sqrt (eps)
