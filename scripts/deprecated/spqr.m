## Copyright (C) 2008, 2009 David Bateman
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
## @deftypefn {Loadable Function} {@var{r} =} spqr (@var{a})
## @deftypefnx {Loadable Function} {@var{r} =} spqr (@var{a},0)
## @deftypefnx {Loadable Function} {[@var{c}, @var{r}] =} spqr (@var{a},@var{b})
## @deftypefnx {Loadable Function} {[@var{c}, @var{r}] =} spqr (@var{a},@var{b},0)
## This function has been deprecated.  Use @code{qr} instead.
## @end deftypefn

## Deprecated in version 3.2

function varargout = spqr (varargin)
  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "spqr is obsolete and will be removed from a future version of Octave; please use qr instead");
  endif

  varargout = cell (nargout, 1);
  [ varargout{:} ] = qr (varargin{:});

endfunction
