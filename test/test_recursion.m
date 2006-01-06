%% Automatically generated from DejaGNU files

%% test/octave.test/recursion/recursion-1.m
%!function y = f (x)
%!  if (x == 1)
%!    y = x;
%!    return;
%!  else
%!    y = x * f (x-1);
%!  endif
%!test
%! assert(f (5),120);

%% test/octave.test/recursion/recursion-2.m
%!function y = f (x)
%!  if (x == 1)
%!    y = x;
%!    return;
%!  else
%!    y = f (x-1) * x;
%!  endif
%!test
%! assert(f (5),120);

