# Rosenbrock's famouns function:

tol = 1.0e-5;

x_opt = [1; 1];

phi_opt = 0.0;

function obj = phi (x)
  obj = 100 * (x(2) - x(1)^2)^2 + (1 - x(1))^2;
end

x0 = [-1.2; 1];

[x, phi, inform] = npsol (x0, 'phi');

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
