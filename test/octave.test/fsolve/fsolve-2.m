x_opt = [ -0.767297326653401;
           0.590671081117440;
           1.47190018629642;
          -1.52719341133957 ];

tol = 1.0e-5;

function retval = f (p)

  x = p(1);
  y = p(2);
  z = p(3);
  w = p(4);

  retval = zeros (4, 1);

  retval(1) = 3*x + 4*y + exp (z + w) - 1.007;
  retval(2) = 6*x - 4*y + exp (3*z + w) - 11;
  retval(3) = x^4 - 4*y^2 + 6*z - 8*w - 20;
  retval(4) = x^2 + 2*y^3 + z - w - 4;

end

[x, info] = fsolve ("f", [-1, 1, 2, -1]);

val = f (x);

info_bad = (info != 1);
solution_bad = sum (abs (x - x_opt) > tol);
value_bad = sum (abs (val) > tol);

if (info_bad)
  printf ("info bad\n");
else
  printf ("info good\n");
endif

if (solution_bad)
  printf ("solution bad\n");
else
  printf ("solution good\n");
endif

if (value_bad)
  printf ("value bad\n");
else
  printf ("value good\n");
endif
