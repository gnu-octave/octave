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
## @deftypefn  {} {[@var{val}, @var{count}, @var{errmsg}] =} scanf (@var{template}, @var{size})
## @deftypefnx {} {[@var{v1}, @var{v2}, @dots{}, @var{count}, @var{errmsg}]] =} scanf (@var{template}, \"C\")
##
## @code{scanf} is deprecated and will be removed in Octave version 4.6.
## Use @code{fscanf (stdin, @dots{})} for the equivalent functionality.
##
## This is equivalent to calling @code{fscanf} with @var{fid} = @code{stdin}.
##
## It is currently not useful to call @code{scanf} in interactive programs.
## @seealso{fscanf, sscanf, fprintf, sprintf}
## @end deftypefn

## Deprecated in version 4.2

function varargout = scanf (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "scanf is obsolete and will be removed from a future version of Octave, please use fscanf instead");
  endif

  if (nargin < 1)
    print_usage ();
  endif

  varargout = fscanf (stdin (), varargin);

endfunction
