function xdotdot = f (x, t)
  xdotdot = [x(2); -x(1)];
endfunction

x0 = [1; 0];
t = [0; 2*pi];
tol = 100 * dassl_options ("absolute tolerance");

x = lsode ("f", x0, t);

y = [1, 0; 1, 0];

all (all (abs (x - y) < tol))
