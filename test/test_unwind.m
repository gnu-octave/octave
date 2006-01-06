%% Automatically generated from DejaGNU files

%% test/octave.test/unwind/unwind-1.m
%!function y = f (x)
%!  global g;
%!  save_g = g;
%!  unwind_protect
%!    g = 0;
%!    y = g;
%!    [1,2;x];
%!    g = 1;
%!    y = [y, g];
%!  unwind_protect_cleanup
%!    g = save_g;
%!    y = [y, g];
%!  end_unwind_protect
%!test
%! global g = -1;
%! y = f ([3,4]);
%! assert(y,[0,1,-1]);

%% test/octave.test/unwind/unwind-2.m
%!function y = f (x)
%!  global g;
%!  save_g = g;
%!  unwind_protect
%!    g = 0;
%!    y = g;
%!    [1,2;x];
%!    g = 1;
%!    y = [y, g];
%!  unwind_protect_cleanup
%!    g = save_g;
%!    y = [y, g];
%!    assert(y,[0,-1]);
%!  end_unwind_protect
%!test
%! global g = -1;
%! fail("y = f (3);","number of columns must match");

