## Copyright (C) 1996 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __plt3__ (@var{x}, @var{y}, @var{z}, @var{fmt})
## @end deftypefn

## Author: Paul Kienzle <kienzle.powernet.co.uk>
## 2001-04-06 Paul Kienzle <kienzle.powernet.co.uk>
##     * __gnuplot_set__ nohidden3d; vector X,Y, matrix Z => meshgrid(X,Y)

## Modified to use new gnuplot interface in octave > 2.9.0
## Dmitri A. Sergatskov <dasergatskov@gmail.com>
## April 18, 2005
## Modified to use NaN as seperator for gnuplot, so multiple calls
## aren't needed.
## David Bateman <dbateman@free.fr>
## May 25, 2006

function __plt3__ (x, usingstr, fmtstr, withstr)

  if (nargin < 2)
    have_usingstr = false;
    usingstr = "";
  else
    have_usingstr = true;
  endif
  if (nargin < 3)
    fmtstr = "";
  endif
  if (nargin < 4)
    withstr = "";
  endif

  __plot_globals__;

  __setup_plot__ ("__gnuplot_splot__");

  j = __plot_data_offset__{__current_figure__}(__multiplot_xi__,__multiplot_yi__);

  __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j} = x;

  if (iscell (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j}))
    for i = 1:length (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j})
    if (! have_usingstr)
	usingstr = __make_using_clause__ (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j}{i});
      endif
      __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__} \
	  = sprintf ("%s%s __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{%d}{%d} %s %s",
		     __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__},
		     __plot_command_sep__, j, i, usingstr, fmtstr{i});
      __plot_command_sep__ = ",\\\n";
    endfor
  else
    if (! have_usingstr)
      usingstr = __make_using_clause__ (__plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{j});
    endif
    __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__} \
	= sprintf ("%s%s __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}{%d} %s %s %s",
		   __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__},
		   __plot_command_sep__, j, usingstr, fmtstr, withstr);
    __plot_command_sep__ = ",\\\n";
  endif

  __plot_data_offset__{__current_figure__}(__multiplot_xi__,__multiplot_yi__) = ++j;

  if (__multiplot_mode__)
    __gnuplot_raw__ ("clear\n");
  endif

  if (! strcmp (__plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}, "__gnuplot_splot__"))
    eval (__plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__});
  endif
endfunction
