## Test display due to no ";" at eol
##
## We are overloading display for double so we place this test
## in its own folder to not mess double up for the other
## tests.

%!test
%! % Overloading of class-methods seems to stick so we need to clear them since we overload
%! % double's display. Is this a bug ???
%! clear classes
%! key = "ans = 1 . ans = 5 . . ans = 0 . ans = 8 . ans = 3 . x = 3 . x = 1 y = 2 . x = 1 . . x = 1 . y = 2 . x = 1 ";
%! __compile__ bytecode_disp clear;
%! bytecode_disp;
%! assert (__prog_output_assert__ (key));
%!
%! assert (__compile__ ("bytecode_disp"));
%! bytecode_disp;
%! assert (__prog_output_assert__ (key));
