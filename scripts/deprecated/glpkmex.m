## Copyright (C) 2005-2012 Nicolo' Giorgetti
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
## @deftypefn {Function File} {[@var{xopt}, @var{fmin}, @var{status}, @var{extra}] =} glpkmex (@var{sense}, @var{c}, @var{A}, @var{b}, @var{ctype}, @var{lb}, @var{ub}, @var{vartype}, @var{param}, @var{lpsolver}, @var{save_pb})
## This function is provided for compatibility with the old @sc{matlab}
## interface to the GNU @sc{glpk} library.  For Octave code, you should use
## the @code{glpk} function instead.
## @seealso{glpk}
## @end deftypefn

function [xopt, fopt, status, extra] = glpkmex (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "glpkmex is obsolete and will be removed from a future version of Octave; please use glpk instead");
  endif

  ## If there is no input output the version and syntax
  if (nargin < 4 || nargin > 11)
    print_usage ();
    return;
  endif

  ## reorder args:
  ##
  ##     glpkmex    glpk
  ##
  ##  1   sense      c
  ##  2   c          a
  ##  3   a          b
  ##  4   b          lb
  ##  5   ctype      ub
  ##  6   lb         ctype
  ##  7   ub         vartype
  ##  8   vartype    sense
  ##  9   param      param
  ## 10   lpsolver
  ## 11   savepb

  sense = varargin{1};
  c = varargin{2};
  a = varargin{3};
  b = varargin{4};

  nx = length  (c);

  if (nargin > 4)
    ctype = varargin{5};
  else
    ctype = repmat ("U", nx, 1);
  endif

  if (nargin > 5)
    lb = varargin{6};
  else
    lb = repmat (-Inf, nx, 1);
  endif

  if (nargin > 6)
    ub = varargin{7};
  else
    ub = repmat (Inf, nx, 1);
  endif

  if (nargin > 7)
    vartype = varargin{8};
  else
    vartype = repmat ("C", nx, 1);
  endif

  if (nargin > 8)
    param = varargin{9};
  else
    param = struct ();
  endif

  if (nargin > 9 && ! isfield (param, "lpsolver"))
    param.lpsolver = varargin{10};
  endif

  if (nargin > 10 && ! isfield (param, "save"))
    param.save = varargin{11};
  endif

  if (nargout == 0)
    glpk (c, a, b, lb, ub, ctype, vartype, sense, param);
  elseif (nargout == 1)
    xopt = glpk (c, a, b, lb, ub, ctype, vartype, sense, param);
  elseif (nargout == 2)
    [xopt, fopt] = glpk (c, a, b, lb, ub, ctype, vartype, sense, param);
  elseif (nargout == 3)
    [xopt, fopt, status] = ...
      glpk (c, a, b, lb, ub, ctype, vartype, sense, param);
  else
    [xopt, fopt, status, extra] = ...
      glpk (c, a, b, lb, ub, ctype, vartype, sense, param);
  endif

endfunction
