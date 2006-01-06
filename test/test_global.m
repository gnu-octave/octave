%% Automatically generated from DejaGNU files

%% test/octave.test/global/global-1.m
%!test
%! global G = 1;
%! assert(G,1)

%% test/octave.test/global/global-2.m
%!function f ()
%!  global G;
%!  assert(G,1);
%!test
%! global G = 1;
%! f;

%% test/octave.test/global/global-3.m
%!function f ()
%!  fail("G");
%!test
%! global G = 1;
%! f();

%% test/octave.test/global/global-4.m
%!function f ()
%!  global H = 1;
%!test
%!  f;
%!  fail("H");

%% test/octave.test/global/global-5.m
%!function f ()
%!  global H = 1;
%!test
%!function g ()
%!  fail("H");
%!test
%! g();

%% test/octave.test/global/global-6.m
%!function f ()
%!  global H = 1;
%!function g ()
%!  global H;
%!  assert(H,1);
%!test
%! f();
%! g();

%% test/octave.test/global/global-7.m
%!test
%!function f ()
%!  global H = 1;
%!test
%! fail("H");

%% test/octave.test/global/global-8.m
%!function f ()
%!  global H = 1;
%!function g ()
%!  global H;
%!  assert(H,1)
%!test
%! f;
%! clear H
%! g;

