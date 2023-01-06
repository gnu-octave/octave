########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} compare_plot_demos ()
## @deftypefnx {} {} compare_plot_demos ("toolkits", @{@var{toolkit1}, @var{toolkit2}, @dots{}@})
##
## Use @code{dump_demos} and @code{html_compare_plot_demos} to produce an
## html comparison of the plot demos for each of Octave's graphics toolkits.
##
## An m-file named @file{dump_plots.m} will be created in the current working
## directory.  This function will be used to render and save the plot demo
## images.
##
## If they do not already exist, directories for each available graphics
## toolkit are created.  Each toolkit's directory will be populated with images
## of each plot demo in the png format.
##
## Finally, an html document named @file{compare_plot_demos.html} is produced.
## This page places each toolkit's images side by side for a convenient
## comparison of the results.
##
## If the property @qcode{"toolkits"} is given then compare only the listed
## toolkits in the cell string.  The list may also include the toolkit
## @qcode{"matlab"}.
##
## @end deftypefn

function compare_plot_demos (varargin)

  arg.toolkits = available_graphics_toolkits ();
  arg.directories = fullfile ("plot", {"appearance", "draw", "util"});
  arg.directories(end + 1) = "image";
  arg.fmt = "png";
  arg.fcn_file = "dump_plot_demos.m";
  arg.replace_images = false;

  for n = 1:2:numel (varargin)
    if (! ischar (varargin{n}))
      print_usage ();
    else
      arg.(varargin{n}) = varargin{n+1};
    endif
  endfor

  if (ischar (arg.toolkits))
    arg.toolkits = {arg.toolkits};
  elseif (! iscellstr (arg.toolkits))
    error ('compare_plot_demos: Invalid value for "toolkits"');
  endif

  if (ischar (arg.directories))
    arg.directories = {arg.directories};
  elseif (! iscellstr (arg.directories))
    error ('compare_plot_demos: Invalid value for "directory"');
  endif

  if (! ischar (arg.fmt))
    error ('compare_plot_demos: Invalid value for "fmt"');
  endif

  ## Generate arg.fcn_file for rendering/saving the plot demo images
  dump_demos (arg.directories, arg.fcn_file, arg.fmt);

  [~, fcn_name] = fileparts (arg.fcn_file);

  ## Generate the plot demo images for each toolkit
  cwd = pwd ();
  unwind_protect
    addpath (pwd);
    for n = 1:numel (arg.toolkits)
      if (! isfolder (fullfile (cwd, arg.toolkits{n})))
        mkdir (arg.toolkits{n});
      endif
      cd (arg.toolkits{n});
      if (arg.replace_images && ! isempty (dir (["*." arg.fmt])))
        delete (["*." arg.fmt]);
      endif
      if (! strcmp (arg.toolkits{n}, "matlab"))
        close all;
        graphics_toolkit (arg.toolkits{n});
        try
          eval (fcn_name);
        catch
          printf ("Error running plot demos for ""%s"" toolkit\n",
                  arg.toolkits{n});
          disp (lasterror);
        end_try_catch
      endif
      cd (cwd);
    endfor
  unwind_protect_cleanup
    rmpath (cwd);
  end_unwind_protect

  if (! strcmp (arg.toolkits, "matlab"))
    ## Generate the html comparison of the images
    html_compare_plot_demos (arg.toolkits);
  else
    ## We need to run matlab manually before the html page can be created
    printf ('\nNow run %s in Matlab.\nAfter this run html_compare_plot_demos,\n', arg.fcn_file);
    printf ('for example html_compare_plot_demos ({"fltk", "gnuplot", "matlab"}), to create the html page.\n');
  endif

endfunction
