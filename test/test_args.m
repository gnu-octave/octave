## Copyright (C) 2006, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%% Automatically generated from DejaGNU files

%% test/octave.test/args/args-1.m
%!function f ()
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!test
%! f;
%! assert(prog_output_assert("nargin: 0, nargout: 0"));

%% test/octave.test/args/args-2.m
%!function f (x, y)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!test
%! f (1);
%! assert(prog_output_assert("nargin: 1, nargout: 0"));

%% test/octave.test/args/args-3.m
%!function [x, y] = f ()
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!  x = 2;
%!test
%! x = f ();
%! assert(prog_output_assert("nargin: 0, nargout: 1"));

%% test/octave.test/args/args-4.m
%!function [x, y] = f (a, b)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!  x = a;
%!test
%! z = f (1);
%! assert(prog_output_assert("nargin: 1, nargout: 1"));

%% test/octave.test/args/args-5.m
%!function [varargout] = f (varargin)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!test
%! f;
%! assert(prog_output_assert("nargin: 0, nargout: 0"));

%% test/octave.test/args/args-6.m
%!function [varargout] = f (x, varargin)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!test
%! f (1);
%! assert(prog_output_assert("nargin: 1, nargout: 0"));

%% test/octave.test/args/args-7.m
%!function [x, varargout] = f (varargin)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!  x = 2;
%!test
%! x = f ();
%! assert(prog_output_assert("nargin: 0, nargout: 1"));

%% test/octave.test/args/args-8.m
%!function [varargout] = f (varargin)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!  varargout{1} = (varargin{1});
%!test
%! z = f (1);
%! assert(prog_output_assert("nargin: 1, nargout: 1"));

%% test/octave.test/args/args-9.m
%!function [x, y, z] = f (a, b, c, d, e)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!test
%! [s, t] = f (1, 2, 3, 4);
%! assert(prog_output_assert("nargin: 4, nargout: 2"));

%% test/octave.test/args/args-10.m
%!function [varargout] = f (varargin)
%!  printf_assert ("nargin: %d, nargout: %d\n", nargin, nargout);
%!  varargout{1} = 1;
%!  varargout{2} = 2;
%!  varargout{3} = 3;
%!  varargout{4} = 4;
%!test
%! [s, t, u, v] = f (1, 2, 3);
%! assert(prog_output_assert("nargin: 3, nargout: 4"));

