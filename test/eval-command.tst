########################################################################
##
## Copyright (C) 2013-2020 The Octave Project Developers
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

%!function r = sigma (opt)
%!  global sigma_call;
%!  if (nargin == 0)
%!    sigma_call = "function";
%!    r = 1;
%!  elseif (ischar (opt))
%!    sigma_call = "command";
%!    r = 1;
%!  else
%!    sigma_call = "unexpected";
%!  endif
%!endfunction
%!
%!function f1 ()
%!  ## ERROR; sigma used as variable and later parsed as command
%!  sigma = svd (1);
%!  eval ("sigma -1;");
%!endfunction
%!
%!function f1a ()
%!  ## Assignment of eval result means eval code is not parsed as command.
%!  sigma = svd (1);
%!  val = eval ("sigma -1;");
%!endfunction
%!
%!function f2 ()
%!  ## ERROR; sigma used as variable and later parsed as command
%!  [u, sigma, v] = svd (1);
%!  eval ("sigma -1;");
%!endfunction
%!
%!function f2a ()
%!  ## Assignment of eval result means eval code is not parsed as command.
%!  [u, sigma, v] = svd (1);
%!  val = eval ("sigma -1;");
%!endfunction
%!
%!function f3 (sigma)
%!  ## No assignment of eval result means eval code is parsed as command.
%!  ## If f3 is called with a value for sigma, it will be used.  Otherwise,
%!  ## search for the function sigma and call with no arguments.
%!  eval ("sigma -1;");
%!endfunction
%!
%!function f3a (sigma)
%!  ## Assignment of eval result means eval code is not parsed as command.
%!  val = eval ("sigma -1;");
%!endfunction
%!
%!function f4 ()
%!  ## No assignment of eval result means eval code is parsed as command.
%!  eval ("sigma -1;");
%!endfunction
%!
%!function f4a ()
%!  ## Assignment of eval result means eval code is not parsed as command.
%!  val = eval ("sigma -1;");
%!endfunction
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! ## Matlab complains about sigma previously being used as a variable
%! ## before being used as a command.
%! fail ("f1 ()", "used as variable and later as function");
%! assert (sigma_call, "none");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! f1a ();
%! assert (sigma_call, "none");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! ## Matlab complains about sigma previously being used as a variable
%! ## before being used as a command.
%! fail ("f2 ()", "used as variable and later as function");
%! assert (sigma_call, "none");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! f2a ();
%! assert (sigma_call, "none");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! f3 ();
%! assert (sigma_call, "command");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! f3a ();
%! assert (sigma_call, "function");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! ## NOTE: this result disagrees with Matlab, which evaluates sigma
%! ## as a command-style function even though there is a variable named
%! ## sigma defined in the workspace prior to evaluating the function
%! ## call (compare with f1() and f2() above).
%! fail ("f3 (1)", "used as variable and later as function");
%! assert (sigma_call, "none");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! f3a (1);
%! assert (sigma_call, "none");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! f4 ();
%! assert (sigma_call, "command");
%! clear -global sigma_call
%!
%!test <55610>
%! global sigma_call;
%! sigma_call = "none";
%! f4a ();
%! assert (sigma_call, "function");
%! clear -global sigma_call
