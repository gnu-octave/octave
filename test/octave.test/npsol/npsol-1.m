# A test from Reklaitis, Ravindran and Ragsdell

tol = 1.0e-5;

x_opt = [1; 2];

phi_opt = 5;

function phi = f (x)
  phi = 6*x(1)/x(2) + x(2)/x(1)/x(1);
end

function nlc = g (x)
  nlc = x(1)*x(2) - 2;
end

c = [1, 1];

x0 = [2; 2];

[x, phi, inform] = npsol (x0, 'f', 1, c, 100, 0, 'g', 0);

info_bad = (inform != 0 && inform != 1 && inform != 6);
solution_bad = sum (abs (x - x_opt) > tol);
value_bad = sum (abs (phi - phi_opt) > tol);

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
