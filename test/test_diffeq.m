%% Automatically generated from DejaGNU files

%% test/octave.test/diffeq/lsode-1.m
%% dassl-1.m
%%
%% Test lsode() function
%%
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         20 May 1998
%%
%% Problem
%%
%%    y1' = -y2,   y1(0) = 1
%%    y2' =  y1,   y2(0) = 0
%%
%% Solution
%%
%%    y1(t) = cos(t)
%%    y2(t) = sin(t)
%!function xdot = f (x, t)
%!  xdot = [-x(2); x(1)];
%!test
%! 
%! x0 = [1; 0];
%! xdot0 = [0; 1];
%! t = (0:1:10)';
%! 
%! tol = 500 * lsode_options ("relative tolerance");
%! 
%! 
%! x = lsode ("f", x0, t);
%! 
%! y = [cos(t), sin(t)];
%! 
%! assert(all (all (abs (x - y) < tol)));

%% test/octave.test/diffeq/lsode-2.m
%!function xdotdot = f (x, t)
%!  xdotdot = [x(2); -x(1)];
%!test
%! 
%! x0 = [1; 0];
%! t = [0; 2*pi];
%! tol = 100 * dassl_options ("relative tolerance");
%! 
%! x = lsode ("f", x0, t);
%! 
%! y = [1, 0; 1, 0];
%! 
%! assert(all (all (abs (x - y) < tol)));

%% test/octave.test/diffeq/lsode-3.m
%!function xdot = f (x, t)
%!  xdot = x;
%!test
%! 
%! x0 = 1;
%! t = [0; 1];
%! tol = 100 * dassl_options ("relative tolerance");
%! 
%! x = lsode ("f", x0, t);
%! 
%! y = [1; e];
%! 
%! assert(all (all (abs (x - y) < tol)));

%% test/octave.test/diffeq/lsode_options-1.m
%!test
%! lsode_options ("absolute tolerance", eps);
%! assert(lsode_options ("absolute tolerance") == eps);

%% test/octave.test/diffeq/lsode_options-2.m
%!error <Invalid call to lsode_options.*> lsode_options ();

%% test/octave.test/diffeq/lsode_options-3.m
%!error <Invalid call to lsode_options.*> lsode_options ("foo", 1, 2);

%% test/octave.test/diffeq/dassl-1.m
%% dassl-1.m
%%
%% Test dassl() function
%%
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         20 May 1998
%%
%% Problem
%%
%%    y1' = -y2,   y1(0) = 1
%%    y2' =  y1,   y2(0) = 0
%%
%% Solution
%%
%%    y1(t) = cos(t)
%%    y2(t) = sin(t)
%!function res = f (x, xdot, t)
%!  res = [xdot(1)+x(2); xdot(2)-x(1)];
%!test
%! 
%! x0 = [1; 0];
%! xdot0 = [0; 1];
%! t = (0:1:10)';
%! 
%! tol = 100 * dassl_options ("relative tolerance");
%! 
%! 
%! [x, xdot] = dassl ("f", x0, xdot0, t);
%! 
%! y = [cos(t), sin(t)];
%! 
%! assert(all (all (abs (x - y) < tol)));

%% test/octave.test/diffeq/dassl-2.m
%% dassl-2.m
%%
%% Test dassl() function
%%
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         20 May 1998
%%
%% Based on SLATEC quick check for DASSL by Linda Petzold
%%
%% Problem
%%
%%   x1' + 10*x1 = 0,   x1(0) = 1
%%   x1  + x2    = 1,   x2(0) = 0
%% 
%%
%% Solution
%%
%%  x1(t) = exp(-10*t)
%%  x2(t) = 1 - x(1)
%!function res = f (x, xdot, t)
%!  res = [xdot(1)+10*x(1); x(1)+x(2)-1];
%!test
%! 
%! x0 = [1; 0];
%! xdot0 = [-10; 10];
%! t = (0:0.2:1)';
%! 
%! tol = 500 * dassl_options ("relative tolerance");
%! 
%! 
%! [x, xdot] = dassl ("f", x0, xdot0, t);
%! 
%! y = [exp(-10*t), 1-exp(-10*t)];
%! 
%! assert(all (all (abs (x - y) < tol)));

%% test/octave.test/diffeq/dassl_options-1.m
%!test
%! dassl_options ("absolute tolerance", eps);
%! assert(dassl_options ("absolute tolerance") == eps);

%% test/octave.test/diffeq/dassl_options-2.m
%!error <Invalid call to dassl_options.*> dassl_options ();

%% test/octave.test/diffeq/dassl_options-3.m
%!error <Invalid call to dassl_options.*> dassl_options ("foo", 1, 2);

