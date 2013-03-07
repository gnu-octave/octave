## Copyright (C) 2006-2012 John W. Eaton
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

## Test %!error usage

%% test/octave.test/error/error-1.m
%!function g () 
%!  error ("foo");
%!endfunction
%!function f () 
%!  g (); 
%!endfunction
%!error <foo> f ()

%% test/octave.test/error/error-2.m
%!function g () 
%!  error ("foo\n");
%!endfunction
%!function f () 
%!  g 
%!endfunction
%!error <foo> f ()

%% test/octave.test/error/error-3.m
%!error error ()

%% test/octave.test/error/error-4.m
%!error <foo> error ("foo\n")

## Test %!warning usage

%% test/octave.test/error/warning-1.m
%!function g ()
%!  warning ("foo");
%!endfunction
%!function f ()
%!  g;
%!endfunction
%!warning <foo> f ()

%% test/octave.test/error/warning-2.m
%!test
%! st.identifier = "backtrace";
%! ws = warning ("query", "backtrace");
%! warning ("on", "backtrace");
%! st.state = "on";
%! assert (warning ("query", "backtrace"), st);
%! warning ("off", "backtrace");
%! st.state = "off";
%! assert (warning ("query", "backtrace"), st);
%! warning (ws.state, "backtrace");

## Test usage() function

%% test/octave.test/error/usage-1.m
%!function g () 
%!  usage ("foo");
%!endfunction
%!function f () 
%!  g (); 
%!endfunction
%!error <foo> f ()

%% test/octave.test/error/usage-2.m
%!function g () 
%!  usage ("foo");
%!endfunction
%!function f () 
%!  g 
%!endfunction
%!error <foo> f ()

%% test/octave.test/error/usage-3.m
%!error usage ()

%% test/octave.test/error/usage-4.m
%!error <foo> usage ("foo\n")

