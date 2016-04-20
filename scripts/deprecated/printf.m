## Copyright (C) 1993-2016 John W. Eaton
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

## -*- texinfo -*-
## @deftypefn {} {} fprintf (@var{template}, @dots{})
##
## @code{printf} is deprecated and will be removed in Octave version 4.6.
## Use @code{fprintf} for the equivalent functionality.
##
## Print optional arguments under the control of the template string
## @var{template} to the stream @code{stdout} and return the number of
## characters printed.
## @ifclear OCTAVE_MANUAL
##
## See the Formatted Output section of the GNU Octave manual for a
## complete description of the syntax of the template string.
## @end ifclear
##
## Implementation Note: For compatibility with @sc{matlab}, escape sequences in
## the template string (e.g., @qcode{\"@xbackslashchar{}n\"} => newline) are
## expanded even when the template string is defined with single quotes.
##
## @seealso{fprintf, sprintf, fscanf, sscanf}
## @end deftypefn

## Deprecated in version 4.2

function varargout = printf (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "printf is obsolete and will be removed from a future version of Octave, please use fprintf instead");
  endif

  if (nargin < 1)
    print_usage ();
  endif

  varargout = fprintf (varargin);

endfunction
