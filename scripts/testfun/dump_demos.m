## Copyright (C) 2010 Søren Hauberg
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
## @deftypefn  {Function File} {} dump_demos ()
## @deftypefnx {Function File} {} dump_demos (@var{dirs})
## @deftypefnx {Function File} {} dump_demos (@var{dirs}, @var{mfile})
## @deftypefnx {Function File} {} dump_demos (@var{dirs}, @var{mfile}, @var{fmt})
## Produces a script, with the name specified by @var{mfile}, containing
## the demos in the directories, @var{dirs}. The demos are assumed to produce
## graphical output, whose renderings are saved with specified format,
## @var{fmt}.
##
## The defaults for each input are;
##
## @table @samp
##   @item @var{dirs}
##   @code{@{"plot/appearance", "plot/draw", "plot/util"@}}
##   @item @var{mfile}
##   @code{"dump.m"}
##   @item @var{fmt}
##   @code{"png"}
## @end table
##
## For example, to produce PNG output for all demos of the functions
## in the plot directory;
##
## @example
## @group
## dump_demos plot dump.m png
## @end group
## @end example
## @seealso{fntests, test, demo}
## @end deftypefn

## Author: Søren Hauberg  <soren@hauberg.org>

function dump_demos (dirs= {"plot/appearance", "plot/draw", "plot/util"}, output="dump.m", fmt="png")

  if (nargin > 3)
    print_usage ();
  endif

  if (strfind (output, ".m") != numel (output) - 1)
    output = strcat (output, ".m");
  endif

  ## Create script beginning (close figures, etc.)
  fid = fopen (output, "w");
  n = find (output == ".", 1, "last");
  if (isempty (n))
    n = numel (output);
  else
    n = n - 1;
  endif
  fprintf (fid, "%% DO NOT EDIT!  Generated automatically by dump_demos.m\n");
  fprintf (fid, "function %s ()\n", output(1:n));
  fprintf (fid, "close all\n");
  fprintf (fid, "more off\n");

  ## Run and print the demos in each directory
  for i = 1:numel (dirs)
    if (!is_absolute_filename (dirs{i}))
      fullname = dir_in_loadpath (dirs{i});
      if (! isempty (fullname))
        dirs{i} = fullname;
      else
        error ("dump_demos: expecting DIRS argument to be a cell arrays of strings with directory names");
      endif
    endif
    d = dirs{i};
    if (!exist (d, "dir"))
      error ("dump_demos: directory %s doesn't exist", d);
    endif
    dump_all_demos (d, fid, fmt);
  endfor

  ## Create script ending
  fprintf (fid, "end\n\n")

  ## TODO - Should dump_demos() attempt to convert the demos to traditional
  ##        syntax.
  ##        (1) oct2mat() to convert some Octave specific syntax.
  ##        (2) Embed sombrero(), vec(), cstrcat() and assert() in demos ?

  ## sombrero has now a default argument which isn't supported from matlab
  ## http://octave.1599824.n4.nabble.com/sombrero-default-argument-matlab-compatibility-td4665016.html
  ## TODO: we need to change it prior running
  ## -function [x, y, z] = sombrero (n = 41)
  ## +function [x, y, z] = sombrero (n)
  ## +
  ## +  if (nargin == 0)
  ## +    n = 41;
  ## +  endif
  
  for mfile = {"sombrero"}
    f = which (mfile{1});
    fid2 = fopen (f);
    code = char (fread (fid2));
    code = oct2mat (code);
    fprintf (fid, "%s", code);
    fclose (fid2);
  endfor
%~
  %~ fprintf (fid, "function x = vec (x)\n")
  %~ fprintf (fid, "  x = x(:);\n")
  %~ fprintf (fid, "end\n")
%~
  %~ fprintf (fid, "function str = cstrcat (varargin)\n")
  %~ fprintf (fid, "  str = [varargin{:}];\n")
  %~ fprintf (fid, "end\n")
%~
  %~ fprintf (fid, "function assert (varargin)\n")
  %~ fprintf (fid, "%% Do nothing.\n")
  %~ fprintf (fid, "end\n")

  ## Close script
  fclose (fid);
endfunction

function dump_all_demos (directory, fid, fmt)
  dirinfo = dir (fullfile (directory, "*.m"));
  flist = {dirinfo.name};
  ## Remove uigetdir, uigetfile, uiputfile, etc.
  flist = flist(! strncmp (flist, "ui", 2));
  for i = 1:numel (flist)
    fun = flist{i};
    fun (end-1:end) = []; # remove .m
    demos = get_demos (fun);
    for d = 1:numel (demos)
      if (d < 10)
        idx = sprintf ("0%d", d);
      else
        idx = sprintf ("%d", d);
      end
      fprintf (fid, "\ntry\n");
      fprintf (fid, "  %s\n\n", demos {d});
      fprintf (fid, "  drawnow\n");
      fprintf (fid, "  num_figures = get(0, 'currentfigure');\n");
      fprintf (fid, "  for fig = 1:num_figures\n");
      fprintf (fid, "    figure (fig);\n");
      fprintf (fid, "    name = sprintf ('%s_%s_%%d.%s', fig);\n", fun, idx, fmt);
      fprintf (fid, "    if (numel (dir (name)) == 0)\n");
      fprintf (fid, "      fprintf ('Printing ""%%s"" ... ', name);\n")
      fprintf (fid, "      print ('-d%s', name);\n", fmt);
#     fprintf (fid, "      pause (1);\n");
      fprintf (fid, "      fprintf ('done\\n');\n");
      fprintf (fid, "    else\n");
      fprintf (fid, "      fprintf ('File ""%%s"" exists.\\n', name);\n")
      fprintf (fid, "    end\n");
#     fprintf (fid, "    drawnow ();\n");
      fprintf (fid, "  end\n");
      fprintf (fid, "  close (2:num_figures)\n");
      fprintf (fid, "catch\n");
      fprintf (fid, "  fprintf ('ERROR in %s_%s: %%s\\n', lasterr ());\n", fun, idx);
      fprintf (fid, "end\n\n");
    endfor
  endfor
  fprintf (fid, "close all\n");
endfunction

function retval = get_demos (fun)
  [code, idx] = test (fun, "grabdemo");
  num_demos = length (idx) - 1;
  retval = cell (1, num_demos);
  ## Now split the demos into a cell array
  for k = 1:num_demos
    retval {k} = oct2mat (code (idx (k):idx (k+1)-1));
  endfor
endfunction

function code = oct2mat (code)
  ## Simple hacks to make things Matlab compatible
  code = strrep (code, "%!", "%%");
  code = strrep (code, "!", "~");
  ## Simple replacing double quotes with single quotes
  ## causes problems with strings like 'hello "world"'
  #code = strrep (code, "\"", "'");
  code = strrep (code, "#", "%");
  ## Fix the format specs for the errobar demos
  code = strrep (code, "%r", "#r");
  code = strrep (code, "%~", "#~");
  endkeywords = {"endfor", "endif", "endwhile", "end_try_catch", ...
                 "endfunction", "end_unwind_protect"};
  for k = 1:numel (endkeywords)
    code = strrep (code, endkeywords{k}, "end");
  endfor
  commentkeywords = {"unwind_proect"};
  for k = 1:numel (commentkeywords)
    code = strrep (code, commentkeywords{k}, strcat ("%", commentkeywords{k}));
  endfor
endfunction
