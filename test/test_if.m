%% Automatically generated from DejaGNU files

%% test/octave.test/if/if-1.m
%!test
%! i = 0;
%! if (i == 0)
%! i++;
%! printf_assert ("%d\n", i);
%! endif
%! assert(prog_output_assert("1"));

%% test/octave.test/if/if-2.m
%!test
%! if (eye (2))
%! printf_assert ("fail\n");
%! else
%! printf_assert ("pass\n");
%! end
%! assert(prog_output_assert("pass"));

%% test/octave.test/if/if-3.m
%!test
%! x = 2;
%! if (eye (2))
%! printf_assert ("fail\n");
%! elseif (x)
%! printf_assert ("pass\n");
%! endif
%! assert(prog_output_assert("pass"));

%% test/octave.test/if/if-4.m
%!test
%! x = 0;
%! y = -2;
%! if (eye (2))
%! printf_assert ("fail\n");
%! elseif (x)
%! printf_assert ("fail\n");
%! elseif (y)
%! printf_assert ("pass\n");
%! end
%! assert(prog_output_assert("pass"));

%% test/octave.test/if/if-5.m
%!test
%! x = 0;
%! y = -2;
%! if (eye (2))
%! printf_assert ("fail\n");
%! elseif (x)
%! printf_assert ("fail\n");
%! elseif (x)
%! printf_assert ("fail\n");
%! else
%! printf_assert ("pass\n");
%! endif
%! assert(prog_output_assert("pass"));

%% test/octave.test/if/if-6.m
%!test
%! x = 0;
%! y = -2;
%! if (y)
%! printf_assert ("pass\n");
%! elseif (x)
%! printf_assert ("fail\n");
%! elseif (x)
%! printf_assert ("fail\n");
%! end
%! assert(prog_output_assert("pass"));

