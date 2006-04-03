%% Automatically generated from DejaGNU files

%% test/octave.test/number/ismatrix-1.m
%!assert(ismatrix (1));

%% test/octave.test/number/ismatrix-2.m
%!assert(ismatrix ([1, 2, 3]));

%% test/octave.test/number/ismatrix-3.m
%% Yes, this is right, ismatrix() checks for non-empty matrices.
%!assert(
%! ismatrix ([]) == 0);

%% test/octave.test/number/ismatrix-4.m
%!assert(ismatrix ([1, 2; 3, 4]));

%% test/octave.test/number/ismatrix-5.m
%!test
%! warn_str_to_num = 0;
%! assert(!(ismatrix ("t")));

%% test/octave.test/number/ismatrix-6.m
%!test
%! warn_str_to_num = 0;
%! assert(!(ismatrix ("test")));

%% test/octave.test/number/ismatrix-7.m
%!test
%! warn_str_to_num = 0;
%! assert(!(ismatrix (["test"; "ing"])));

%% test/octave.test/number/ismatrix-8.m
%!test
%! s.a = 1;
%! assert(!(ismatrix (s)));

%% test/octave.test/number/ismatrix-9.m
%!error <... ismatrix:.*> ismatrix ();

%% test/octave.test/number/ismatrix-10.m
%!error <... ismatrix:.*> ismatrix ([1, 2; 3, 4], 2);

%% test/octave.test/number/isvector-1.m
%!assert(isvector (1));

%% test/octave.test/number/isvector-2.m
%!assert(isvector ([1; 2; 3]));

%% test/octave.test/number/isvector-3.m
%!assert(!(isvector ([])));

%% test/octave.test/number/isvector-4.m
%!assert(!(isvector ([1, 2; 3, 4])));

%% test/octave.test/number/isvector-5.m
%!test
%! warn_str_to_num = 0;
%! assert((isvector ("t")));

%% test/octave.test/number/isvector-6.m
%!test
%! warn_str_to_num = 0;
%! assert((isvector ("test")));

%% test/octave.test/number/isvector-7.m
%!assert(!(isvector (["test"; "ing"])));

%% test/octave.test/number/isvector-8.m
%!test
%! s.a = 1;
%! assert((isvector (s)));

%% test/octave.test/number/isvector-9.m
%!error isvector ();

%% test/octave.test/number/isvector-10.m
%!error isvector ([1, 2], 2);

%% test/octave.test/number/isscalar-1.m
%!assert(isscalar (1));

%% test/octave.test/number/isscalar-2.m
%!assert(!(isscalar ([1, 2])));

%% test/octave.test/number/isscalar-3.m
%!assert(!(isscalar ([])));

%% test/octave.test/number/isscalar-4.m
%!assert(!(isscalar ([1, 2; 3, 4])));

%% test/octave.test/number/isscalar-5.m
%!test
%! warn_str_to_num = 0;
%! assert((isscalar ("t")));

%% test/octave.test/number/isscalar-6.m
%!assert(!(isscalar ("test")));

%% test/octave.test/number/isscalar-7.m
%!assert(!(isscalar (["test"; "ing"])));

%% test/octave.test/number/isscalar-8.m
%!test
%! s.a = 1;
%! assert((isscalar (s)));

%% test/octave.test/number/isscalar-9.m
%!error isscalar ();

%% test/octave.test/number/isscalar-10.m
%!error isscalar (1, 2);

%% test/octave.test/number/issquare-1.m
%!assert(issquare (1));

%% test/octave.test/number/issquare-2.m
%!assert(!(issquare ([1, 2])));

%% test/octave.test/number/issquare-3.m
%!assert(!(issquare ([])));

%% test/octave.test/number/issquare-4.m
%!assert(issquare ([1, 2; 3, 4]) == 2);

%% test/octave.test/number/issquare-5.m
%!test
%! warn_str_to_num = 0;
%! assert(!(issquare ("t")));

%% test/octave.test/number/issquare-6.m
%!assert(!(issquare ("test")));

%% test/octave.test/number/issquare-7.m
%!test
%! warn_str_to_num = 0;
%! assert(!(issquare (["test"; "ing"; "1"; "2"])));

%% test/octave.test/number/issquare-8.m
%!test
%! s.a = 1;
%! assert(!(issquare (s)));

%% test/octave.test/number/issquare-9.m
%!assert(!(issquare ([1, 2; 3, 4; 5, 6])));

%% test/octave.test/number/issquare-10.m
%!error issquare ();

%% test/octave.test/number/issquare-11.m
%!error issquare ([1, 2; 3, 4], 2);

%% test/octave.test/number/issymmetric-1.m
%!assert(issymmetric (1));

%% test/octave.test/number/issymmetric-2.m
%!assert(!(issymmetric ([1, 2])));

%% test/octave.test/number/issymmetric-3.m
%!assert(!(issymmetric ([])));

%% test/octave.test/number/issymmetric-4.m
%!assert(issymmetric ([1, 2; 2, 1]) == 2);

%% test/octave.test/number/issymmetric-5.m
%!test
%! warn_str_to_num = 0;
%! assert(!(issymmetric ("t")));

%% test/octave.test/number/issymmetric-6.m
%!assert(!(issymmetric ("test")));

%% test/octave.test/number/issymmetric-7.m
%!test
%! warn_str_to_num = 0;
%! assert(!(issymmetric (["te"; "et"])));

%% test/octave.test/number/issymmetric-8.m
%!test
%! s.a = 1;
%! assert(!(issymmetric (s)));

%% test/octave.test/number/issymmetric-9.m
%!assert(issymmetric ([1, 2.1; 2, 1.1], 0.2) == 2);

%% test/octave.test/number/issymmetric-10.m
%!error issymmetric ([1, 2; 2, 1], 0, 0);

%% test/octave.test/number/issymmetric-11.m
%!error issymmetric ();

