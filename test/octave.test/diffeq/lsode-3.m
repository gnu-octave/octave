function xdot = f (x, t)
  xdot = x;
endfunction

x0 = 1;
t = [0; 1];
tol = 100 * dassl_options ("absolute tolerance");

x = lsode ("f", x0, t);

y = [1; e];

all (all (abs (x - y) < tol))
