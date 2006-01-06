%% Automatically generated from DejaGNU files

%% test/octave.test/nonlin/fsolve-1.m
%!function retval = f (p) 
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  retval = zeros (3, 1);
%!  retval(1) = sin(x) + y**2 + log(z) - 7;
%!  retval(2) = 3*x + 2**y -z**3 + 1;
%!  retval(3) = x + y + z - 5;
%!test
%! x_opt = [ 0.599054;
%! 2.395931;
%! 2.005014 ];
%! tol = 1.0e-5;
%! [x, info] = fsolve ("f", [ 0.5, 2.0, 2.5 ]);
%! val = f (x);
%! info_bad = (info != 1);
%! solution_bad = sum (abs (x - x_opt) > tol);
%! value_bad = sum (abs (val) > tol);
%! if (info_bad)
%!   printf_assert ("info bad\n");
%! else
%!   printf_assert ("info good\n");
%! endif
%! if (solution_bad)
%!   printf_assert ("solution bad\n");
%! else
%!   printf_assert ("solution good\n");
%! endif
%! if (value_bad)
%!   printf_assert ("value bad\n");
%! else
%!   printf_assert ("value good\n");
%! endif
%! assert(prog_output_assert("info good\nsolution good\nvalue good"));

%% test/octave.test/nonlin/fsolve-2.m
%!function retval = f (p)
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  w = p(4);
%!  retval = zeros (4, 1);
%!  retval(1) = 3*x + 4*y + exp (z + w) - 1.007;
%!  retval(2) = 6*x - 4*y + exp (3*z + w) - 11;
%!  retval(3) = x^4 - 4*y^2 + 6*z - 8*w - 20;
%!  retval(4) = x^2 + 2*y^3 + z - w - 4;
%!test
%! x_opt = [ -0.767297326653401;
%! 0.590671081117440;
%! 1.47190018629642;
%! -1.52719341133957 ];
%! tol = 1.0e-5;
%! [x, info] = fsolve ("f", [-1, 1, 2, -1]);
%! val = f (x);
%! info_bad = (info != 1);
%! solution_bad = sum (abs (x - x_opt) > tol);
%! value_bad = sum (abs (val) > tol);
%! if (info_bad)
%!   printf_assert ("info bad\n");
%! else
%!   printf_assert ("info good\n");
%! endif
%! if (solution_bad)
%!   printf_assert ("solution bad\n");
%! else
%!   printf_assert ("solution good\n");
%! endif
%! if (value_bad)
%!   printf_assert ("value bad\n");
%! else
%!   printf_assert ("value good\n");
%! endif
%! assert(prog_output_assert("info good\nsolution good\nvalue good"));

%% test/octave.test/nonlin/fsolve_options-1.m
%!test
%! fsolve_options ("tolerance", eps);
%! assert(fsolve_options ("tolerance") == eps);

%% test/octave.test/nonlin/fsolve_options-2.m
%!error <... fsolve_options:.*> fsolve_options ();

%% test/octave.test/nonlin/fsolve_options-3.m
%!error <... fsolve_options:.*> fsolve_options ("foo", 1, 2);

