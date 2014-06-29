## Copyright (C) 2012 Ben Abbott  <bpabbott@mac.com>
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
## @deftypefn  {Function File} {} compare_plot_demos ()
##
## Uses @code{dump_demos} and @code{html_compare} to produce an html
## comparison of the plot demos for each of Octave's graphics
## toolkits. 
##
## A m-file names `dump_plots.m' will be created in the pwd. This function
## will be used to render and save the plot demo images.
##
## If they do not already exist, directories for each graphics toolkit
## are created. Each toolkit's directory will be populated with images
## of each plot demo in the png format.
##
## Finally, an htlm document named `compare_plot_demos.html' is produced.
## This page places each toolkits images side by side for a conventient
## comparison of the results.
##
## @deftypefnx  {Function File} {} compare_plot_demos (@var{toolkits})
## @var{toolkits} is used to specify a subset of the available graphics
## toolkits. This list may also include `matlab'.
##
## @end deftypefn

## Author: Ben Abbott  <bpabbott@mac.com>

function compare_plot_demos (varargin)

  in.toolkits = available_graphics_toolkits ();
  in.directory = "plot";
  in.fmt = "png";
  in.fcn_file = "";
  in.replace_images = false;

  for n = 1:2:numel(varargin)
    if (! ischar (varargin{n}))
      print_usage ();
    else
      in.(varargin{n}) = varargin{n+1};
    endif
  endfor

  if (ischar (in.toolkits))
    in.toolkits = {in.toolkits};
  elseif (! iscellstr (in.toolkits))
    error ("compare_plot_demos: Invalid value for ""toolkits""")
  endif

  if (! ischar (in.directory))
    error ("compare_plot_demos: Invalid value for ""directory""")
  endif

  if (! ischar (in.fmt))
    error ("compare_plot_demos: Invalid value for ""fmt""")
  endif

  if (isempty (in.fcn_file))
    in.fcn_file = sprintf ("dump_%s_demos.m", in.directory);
  endif

  ## Generate "dump_plots.m" for rendering/saving the plot demo images
  dump_demos ("plot", in.fcn_file, in.fmt)

  [~, fcn_name] = fileparts (in.fcn_file);

  ## Generate the plot demo images for each toolkit
  cwd = pwd ();
  unwind_protect
    addpath (pwd);
    for n = 1:numel(in.toolkits)
      dirs = dir ();
      dirs = dirs([dirs.isdir]);
      if (! any (strcmp ({dirs.name}, in.toolkits{n})))
        mkdir (in.toolkits{n})
      endif
      cd (in.toolkits{n})
      if (! isempty (dir (strcat ("*.", in.fmt))) && in.replace_images)
        delete (strcat ("*.", in.fmt))
      endif
      if (! strcmp (in.toolkits{n}, "matlab"))
        close all
        graphics_toolkit (in.toolkits{n});
        try
          eval (fcn_name);
        catch
          fprintf ("Error running plot demos for ""%s"" toolkit\n", in.toolkits{n})
          disp (lasterror)
        end_try_catch
      endif
      cd (cwd)
    endfor
  unwind_protect_cleanup
    rmpath (cwd);
  end_unwind_protect

  ## Generate the html comparison of the images
  ## TODO - pass the toolkits{} to allow num of columns to be formated
  html_compare_plot_demos ("output", "compare_plot_demos.html")

endfunction

