## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn  {Function File} {@var{r} =} colon (@var{a}, @var{b})
## @deftypefnx {Function File} {@var{r} =} colon (@var{a}, @var{b}, @var{c})
## Method of a class to construct a range with the @code{:} operator.  For
## example:
##
## @example
## @group
## a = myclass (@dots{});
## b = myclass (@dots{});
## c = a : b
## @end group
## @end example
##
## @seealso{class, subsref, subsasgn}
## @end deftypefn

function r = colon (varargin)
  if (nargin != 0)
    error ("colon: not defined for class \"%s\"", class(varargin{1}));
  endif
endfunction

%!error colon (1)

## FIXME -- what does colon () mean since it doesn't set a return value?
