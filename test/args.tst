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

########################################
## No inputs or no outputs

## no input or output arguments
%!function f ()
%!  assert (nargin, 0);
%!  assert (nargout, 0);
%!endfunction
%!test
%! f;

## one input with two possible inputs
%!function f (x, y)
%!  assert (nargin, 1);
%!  assert (nargout, 0);
%!endfunction
%!test
%! f (1);

## no inputs, one of multiple outputs
%!function [x, y] = f ()
%!  assert (nargin, 0);
%!  assert (nargout, 1);
%!  x = 2;
%!endfunction
%!test
%! assert (f (), 2);

## one of multiple inputs, one of multiple outputs
%!function [x, y] = f (a, b)
%!  assert (nargin, 1);
%!  assert (nargout, 1);
%!  x = a;
%!endfunction
%!test
%! assert (f (1), 1);

########################################
## Varargin, varargout

## varargin and varargout with no inputs or outputs
%!function [varargout] = f (varargin)
%!  assert (nargin, 0);
%!  assert (nargout, 0);
%!endfunction
%!test
%! f;

## varargin and varargout with one input
%!function [varargout] = f (x, varargin)
%!  assert (nargin, 1);
%!  assert (nargout, 0);
%!endfunction
%!test
%! f (1);

## varargin and varargout with one output
%!function [x, varargout] = f (varargin)
%!  assert (nargin, 0);
%!  assert (nargout, 1);
%!  x = 2;
%!endfunction
%!test
%! assert (f (), 2);

## varargin and varargout with one input and output
%!function [varargout] = f (varargin)
%!  assert (nargin, 1);
%!  assert (nargout, 1);
%!  varargout{1} = varargin{1};
%!endfunction
%!test
%! assert (f (1), 1);

## multiple inputs, multiple outputs, but not all of either
## WARNING: The original test did not assign the outputs, it just
## requested them, and I think that is supposed to be an error.  It also
## still has a non-assigned output argument.
%!function [x, y, z] = f (a, b, c, d, e)
%!  assert (nargin, 4);
%!  assert (nargout, 2);
%!  x = a;
%!  y = b;
%!endfunction
%!test
%! [s, t] = f (1, 2, 3, 4);
%! assert ([s t], [1 2]);

## Fully used varargin and varargout
%!function [varargout] = f (varargin)
%!  assert (nargin, 3);
%!  assert (nargout, 4);
%!  varargout{1} = varargin{1};
%!  varargout{2} = varargin{2};
%!  varargout{3} = varargin{3};
%!  varargout{4} = 4;
%!endfunction
%!test
%! [s, t, u, v] = f (1, 2, 3);
%! assert ([s t u v], [1 2 3 4]);

## Wrapper functions
%!function [x, y, z] = f (varargin)
%!  assert (nargin, 0);
%!  assert (nargout, 0);
%!  x = 3;
%!  y = 2;
%!  z = 1;
%!endfunction
%!function varargout = wrapper_1 (varargin)
%!  assert (nargout, 0);
%!  [varargout{1:nargout}] = f ();
%!endfunction
%!function varargout = wrapper_2 (varargin)
%!  assert (nargout, 0);
%!  varargout = cell (1, nargout);
%!  [varargout{1:nargout}] = f ();
%!endfunction
%!function varargout = wrapper_3 (varargin)
%!  assert (nargout, 0);
%!  varargout = cell (1, nargout);
%!  [varargout{:}] = f ();
%!endfunction
%!test
%! wrapper_1 ();
%! assert (ans, 3);
%! wrapper_2 ();
%! assert (ans, 3);
%! wrapper_3 ();
%! assert (ans, 3);

## Test default arguments
## numeric
%!function f (x = 0)
%!  assert (x, 0);
%!endfunction
%!test
%! f()

## numeric vector (spaces)
%!function f (x = [0 1 2])
%!  assert (x, [0 1 2]);
%!endfunction
%!test
%! f()

## numeric vector (range)
%!function f (x = 1:3)
%!  assert (x, 1:3);
%!endfunction
%!test
%! f()

## numeric vector (commas)
%!function f (x = [0,1,2])
%!  assert (x, [0 1 2]);
%!endfunction
%!test
%! f()

## numeric vector (commas and spaces)
%!function f (x = [0, 1, 2])
%!  assert (x, [0 1 2]);
%!endfunction
%!test
%! f()

## numeric matrix
%!function f (x = [0, 1, 2;3, 4, 5])
%!  assert (x, [0 1 2;3 4 5]);
%!endfunction
%!test
%! f()

## empty cell
%!function f (x = {})
%!  assert (x, {});
%!endfunction
%!test
%! f()

## full cell
%!function f (x = {1})
%!  assert (x, {1});
%!endfunction
%!test
%! f()

## many cells
%!function f (x = {1 'a' "b" 2.0 struct("a", 3)})
%!  assert (x, {1 'a' "b" 2.0 struct("a", 3)});
%!endfunction
%!test
%! f()

## struct
%!function f (x = struct ("a", 3))
%!  assert (x, struct ("a", 3));
%!endfunction
%!test
%! f()

## char (double quotes)
%!function f (x = "a")
%!  assert (x, "a");
%!endfunction
%!test
%! f()

## char (single quotes)
%!function f (x = 'a')
%!  assert (x, "a");
%!endfunction
%!test
%! f()

## char (string, double quotes)
%!function f (x = "abc123")
%!  assert (x, "abc123");
%!endfunction
%!test
%! f()

## char (string, double quotes, punctuation)
%!function f (x = "abc123`1234567890-=~!@#$%^&*()_+[]{}|;':\",./<>?\\")
%!  assert (x, "abc123`1234567890-=~!@#$%^&*()_+[]{}|;':\",./<>?\\");
%!endfunction
%!test
%! f()

## Function handle (builtin)
%!function f (x = @sin)
%!  finfo = functions (x);
%!  fname = finfo.function;
%!  assert (is_function_handle (x) && strcmp (fname, "sin"));
%!endfunction
%!test
%! f()

## Function handle (anonymous)
%!function f (x = @(x) x.^2)
%!  finfo = functions (x);
%!  ftype = finfo.type;
%!  assert (is_function_handle (x) && strcmp (ftype, "anonymous"));
%!endfunction
%!test
%! f()
