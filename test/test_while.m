%% Automatically generated from DejaGNU files

%% test/octave.test/while/while-1.m
%!test
%! i = 0;
%! while (eye (2))
%! i++;
%! printf_assert ("%d\n", i);
%! endwhile;
%! assert(prog_output_assert(""));

%% test/octave.test/while/while-2.m
%!test
%! i = 5;
%! while (--i)
%! printf_assert ("%d", i);
%! endwhile
%! printf_assert ("\n");
%! assert(prog_output_assert("4321"));

%% test/octave.test/while/while-3.m
%!test
%! i = 5;
%! while (i)
%! i--;
%! printf_assert ("%d", i);
%! endwhile
%! printf_assert ("\n");
%! assert(prog_output_assert("43210"));

%% test/octave.test/while/while-4.m
%!test
%! i = 0;
%! while (i++ < 20)
%! if (i > 2)
%! break;
%! endif
%! printf_assert ("%d", i);
%! endwhile;
%! printf_assert ("\n");
%! assert(prog_output_assert("12"));

%% test/octave.test/while/while-5.m
%!test
%! i = 0;
%! while (++i < 5)
%! if (i < 3)
%! continue;
%! endif
%! printf_assert ("%d", i);
%! endwhile
%! printf_assert ("\n");
%! assert(prog_output_assert("34"));

