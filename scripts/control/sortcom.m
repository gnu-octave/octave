## Copyright (C) 1996 Auburn University.  All rights reserved.
##
## This file is part of Octave. 
##
## Octave is free software; you can redistribute it and/or modify it 
## under the terms of the GNU General Public License as published by the 
## Free Software Foundation; either version 2, or (at your option) any 
## later version. 
## 
## Octave is distributed in the hope that it will be useful, but WITHOUT 
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
## for more details.
## 
## You should have received a copy of the GNU General Public License 
## along with Octave; see the file COPYING.  If not, write to the Free 
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 

## -*- texinfo -*-
## @deftypefn {Function File } { outputs =} sortcom ( inputs ) 
## @format
##  [yy,idx] = sortcom(xx[,opt]): sort a complex vector
##  xx: complex vector
##  opt: sorting option:
## 	"re": real part (default)
## 	"mag": by magnitude
## 	"im": by imaginary part
## 
##   if opt != "im" then complex conjugate pairs are grouped together,
##      a - jb followed by a + jb.
##  yy: sorted values
##  idx: permutation vector: yy = xx(idx)
## 
## 
## @end format
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: June 1995

function [yy, idx] = sortcom (xx, opt)

  if( nargin < 1 | nargin > 2 )
     usage("yy = sortcom(xx[,opt]");
  elseif( !(is_vector(xx) | isempty(xx) ))
    error("sortcom: first argument must be a vector");
  endif

  if(nargin == 1)         opt = "re";
  else
    if (!isstr(opt))
      error("sortcom: second argument must be a string");
    endif
  endif
 
  if(isempty(xx))
    yy = idx = [];
  else
    if(strcmp(opt,"re"))        datavec = real(xx);
    elseif(strcmp(opt,"im"))    datavec = imag(xx);
    elseif(strcmp(opt,"mag"))   datavec = abs(xx);
    else                        error(["sortcom: illegal option = ", opt])
    endif
  
    [datavec,idx] = sort(datavec);
    yy= xx(idx);
    
    if(strcmp(opt,"re") | strcmp(opt,"mag"))
      ## sort so that complex conjugate pairs appear together
      
      ddiff = diff(datavec);
      zidx = find(ddiff == 0);
  
      ## sort common datavec values
      if(!isempty(zidx))
        for iv=create_set(datavec(zidx))
          vidx = find(datavec == iv);
          [vals,imidx] = sort(imag(yy(vidx)));
          yy(vidx)  = yy(vidx(imidx));
          idx(vidx) = idx(vidx(imidx));
        endfor
      endif
    endif
  endif  
endfunction
  
