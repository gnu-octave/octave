## Copyright (C) 2016 Cristiano Dorigo, Octave Arena
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

## -*- texinfo -*-
## @deftypefn  {} {@var{[varargout]} =} __default__input__ (@var{def_val}, @var{varargin})
## Check if the arguments in input of a function are empty or missing
## and in such cases sets up them in default values.
##
## The input argoments are:
## @itemize @minus
## @item @var{def_val} is a cell array that contains the values to use
## as  default.
## @item @var{varargin} are the input argument
## @end itemize
##
## The output argoments:
## @itemize @minus
## @item @var{varargout} all the input argument with filled the empty
## or missing paramenters.
##
## @end itemize
##
## @end deftypefn


function [varargout] = __default__input__ (def_val, varargin)

  m = length (def_val);
  n = length (varargin);

  for i = 1:m
    if (n < i || isempty (varargin {i}))
      varargout {i} = def_val {i};
    else
      varargout {i} = varargin {i};
    endif
  endfor
