## Copyright (C) 1998-2004 Andy Adler
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} spy (@var{x})
## Plot the sparsity pattern of the sparse matrix @var{x}.
## @end deftypefn

function spy(S) 
  if issparse(S)
    [i,j,s,m,n]= spfind(S);
  else
    [i,j,s] = find(S);
    [m,n] = size(S);
  endif

  arp = automatic_replot;
  unwind_protect
    automatic_replot (0);

    eval(sprintf('__gnuplot_set__ nokey'))
    eval(sprintf('__gnuplot_set__ yrange [0:%d] reverse',m+1))
    eval(sprintf('__gnuplot_set__ xrange [0:%d] noreverse',n+1))

    if (length(i)<1000)
      plot(j,i,'*');
    else
      plot(j,i,'.');
    endif

    #TODO: we should store the reverse state so we don't undo it
    __gnuplot_set__ yrange [0:1] noreverse
    axis;
  unwind_protect_cleanup
    automatic_replot (arp);
  end_unwind_protect
endfunction
