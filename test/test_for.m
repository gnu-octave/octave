%% Automatically generated from DejaGNU files

%% test/octave.test/for/for-1.m
%!test
%! for i = 1
%! printf_assert ("%d", i);
%! end
%! printf_assert ("\n");
%! assert(prog_output_assert("1"));

%% test/octave.test/for/for-2.m
%!test
%! for i = 1:4
%! printf_assert ("%d", i);
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("1234"));

%% test/octave.test/for/for-3.m
%!test
%! for i = [1,2,3,4]
%! printf_assert ("%d", i);
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("1234"));

%% test/octave.test/for/for-4.m
%!test
%! for i = [1,2;3,4]
%! printf_assert ("%d", i(1,1));
%! printf_assert ("%d", i(2,1));
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("1324"));

%% test/octave.test/for/for-5.m
%!test
%! for i = I
%! printf_assert ("%d", imag (i));
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("1"));

%% test/octave.test/for/for-6.m
%!test
%! for i = [1,2,3,4]*I
%! printf_assert ("%d", imag (i));
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("1234"));

%% test/octave.test/for/for-7.m
%!test
%! for i = [1,2;3,4]*I
%! printf_assert ("%d", imag (i(1,1)));
%! printf_assert ("%d", imag (i(2,1)));
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("1324"));

%% test/octave.test/for/for-8.m
%!test
%! for i = [1,2,3,4]
%! if (i > 2)
%! break;
%! endif
%! printf_assert ("%d", i);
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("12"));

%% test/octave.test/for/for-9.m
%!test
%! for i = [1,2,3,4]
%! if (i < 3)
%! continue;
%! endif
%! printf_assert ("%d", i);
%! endfor
%! printf_assert ("\n");
%! assert(prog_output_assert("34"));

%!test
%! a = [1,3;2,4];
%! j = 0;
%! for i = cat (3, a, 4 + a)
%!   assert (i, [1;2] + 2*j++)
%! endfor

%!test
%! a = {1,3;2,4};
%! j = 0;
%! for i = cat (3, a, cellfun(@(x) 4 + x, a, 'UniformOutput', 0))
%!   assert (i, {1 + 2*j; 2 + 2*j++})
%! endfor
