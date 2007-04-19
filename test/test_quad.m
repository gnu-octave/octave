%% Automatically generated from DejaGNU files

%% test/octave.test/quad/quad-1.m
%!function y = f (x) 
%! y = x + 1;
%!test
%! [v, ier, nfun, err] = quad ("f", 0, 5);
%! assert(ier == 0 && abs (v - 17.5) < sqrt (eps) && nfun > 0 && 
%!        err < sqrt (eps))

%% test/octave.test/quad/quad-2.m
%!function y = f (x)
%!  y = x .* sin (1 ./ x) .* sqrt (abs (1 - x));
%!test
%!  [v, ier, nfun, err] = quad ("f", 0.001, 3);
%! assert((ier == 0 || ier == 1) && abs (v - 1.98194120273598) < sqrt (eps) && nfun > 0);

%% test/octave.test/quad/quad-3.m
%!error <Invalid call to quad.*> quad ();

%% test/octave.test/quad/quad-4.m
%!error <Invalid call to quad.*> quad ("f", 1, 2, 3, 4, 5);

%% test/octave.test/quad/quad_options-1.m
%!test
%! quad_options ("absolute tolerance", eps);
%! assert(quad_options ("absolute tolerance") == eps);

%% test/octave.test/quad/quad_options-2.m
%!error <Invalid call to quad_options.*> quad_options ();

%% test/octave.test/quad/quad_options-3.m
%!error <Invalid call to quad_options.*> quad_options (1, 2, 3);

