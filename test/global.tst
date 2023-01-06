########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

%!test
%! global G = 1;
%! assert (G,1);
%! clear -global G;  # cleanup after test

%!function f ()
%!  global G;
%!  assert (G,1);
%!endfunction
%!test
%! global G = 1;
%! f;
%! clear -global G;  # cleanup after test

%!function f ()
%!  fail ("G");
%!endfunction
%!test
%! global G = 1;
%! f ();
%! clear -global G;  # cleanup after test

%!function f ()
%!  global H = 1;
%!endfunction
%!test
%! f;
%! fail ("H");
%! clear -global H;  # cleanup after test

%!function f ()
%!  global H = 1;
%!endfunction
%!test
%!function g ()
%!  fail ("H");
%!test
%! g ();
%! clear -global H;  # cleanup after test

%!function f ()
%!  global H = 1;
%!endfunction
%!function g ()
%!  global H;
%!  assert (H,1);
%!endfunction
%!test
%! f ();
%! g ();
%! clear -global H;  # cleanup after test

%!test
%!function f ()
%!  global H = 1;
%!endfunction
%!test
%! fail ("H");
%! clear -global H;  # cleanup after test

%!function f ()
%!  global H = 1;
%!endfunction
%!function g ()
%!  global H;
%!  assert (H,1);
%!endfunction
%!test
%! f;
%! clear H;
%! g;
%! clear -global H;  # cleanup after test

%!function r = f ()
%!  x = 1;
%!  global x;
%!  r = x;
%!endfunction
%!test
%! warning ("off", "Octave:global-local-conflict", "local");
%! clear global x      ## clears global and local value
%! global x;
%! x = 0;
%! assert (f (), 0);
%! global x;
%! assert (x, 0);
%!test
%! warning ("off", "Octave:global-local-conflict", "local");
%! clear global x      ## clears global and local value
%! assert (f (), 1);
%! global x;
%! assert (x, 1);
%! clear -global x;  # cleanup after test

%!function r = f ()
%!  x = 1;
%!  global x = 3;
%!  r = x;
%!endfunction
%!test
%! warning ("off", "Octave:global-local-conflict", "local");
%! clear global x      ## clears global and local value
%! global x;
%! x = 0;
%! assert (f (), 0);
%! global x;
%! assert (x, 0);
%!test
%! warning ("off", "Octave:global-local-conflict", "local");
%! clear global x
%! assert (f (), 1);
%! global x;
%! assert (x, 1);
%! clear -global x;  # cleanup after test

%!test
%! warning ("off", "Octave:global-local-conflict", "local");
%! clear global x      ## clears global and local value
%! x = 42;             ## local value
%! global x;           ## link to undefined global, global gets local value
%! assert (x, 42);
%! clear x             ## clears local; global still defined
%! x = 13;             ## new local value
%! global x;           ## link to existing global, local gets global value
%! assert (x, 42);
%! clear global x      ## clears global and local value
%! assert (exist ("x"), 0);
%! clear -global x;  # cleanup after test
