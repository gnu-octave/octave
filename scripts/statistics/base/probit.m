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

## For each component of p, return the probit (the quantile of the
## standard normal distribution) of p.
  
## Written by KH <Kurt.Hornik@ci.tuwien.ac.at> on 1995/02/04
## Description:  Probit transformation

function y = probit (p)
  
  if (nargin == 1)
    y = stdnormal_inv (p);
  else
    usage ("probit (p)");
  endif

endfunction
  