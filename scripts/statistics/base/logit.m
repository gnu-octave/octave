## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} logit (@var{p})
## For each component of @var{p}, return the logit @code{log (@var{p} /
## (1-@var{p}))} of @var{p}.
## @end deftypefn

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  Logit transformation

function y = logit (p)

  if (nargin == 1)
    y = logistic_inv (p);
  else
    usage ("logit (p)");
  endif

endfunction