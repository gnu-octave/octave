%% Automatically generated from DejaGNU files

%% test/octave.test/infnan/infnan-1.m
%!test
%! a = Inf;
%! assert(!(finite (a)));

%% test/octave.test/infnan/infnan-2.m
%!test
%! a = Inf;
%! assert(isinf (a));

%% test/octave.test/infnan/infnan-3.m
%!test
%! a = Inf;
%! assert(!(isnan (a)));

%% test/octave.test/infnan/infnan-4.m
%!test
%! b = NaN;
%! assert(!(finite (b)));

%% test/octave.test/infnan/infnan-5.m
%!test
%! b = NaN;
%! assert(!(isinf (b)));

%% test/octave.test/infnan/infnan-6.m
%!test
%! b = NaN;
%! assert(isnan (b));

%% test/octave.test/infnan/infnan-7.m
%!test
%! c = rand ();
%! assert(finite (c));

%% test/octave.test/infnan/infnan-8.m
%!test
%! c = rand ();
%! assert(!(isinf (c)));

%% test/octave.test/infnan/infnan-9.m
%!test
%! c = rand ();
%! assert(!(isnan (c)));

