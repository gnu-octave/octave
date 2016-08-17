## Copyright (C) 2010-2016 Søren Hauberg
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
## @deftypefn  {} {} dump_demos ()
## @deftypefnx {} {} dump_demos (@var{dirs})
## @deftypefnx {} {} dump_demos (@var{dirs}, @var{mfile})
## @deftypefnx {} {} dump_demos (@var{dirs}, @var{mfile}, @var{fmt})
## Produce a script, with the name specified by @var{mfile}, containing
## the demos in the directories, @var{dirs}.  The demos are assumed to produce
## graphical output, whose renderings are saved with the specified format,
## @var{fmt}.
##
## The defaults for each input are;
##
## @table @var
##   @item @var{dirs}
##   @code{@{"plot/appearance", "plot/draw", "plot/util", "image"@}}
##
##   @item @var{mfile}
##   @qcode{"dump.m"}
##
##   @item @var{fmt}
##   @qcode{"png"}
## @end table
##
## For example, to produce PNG output for all demos of the functions
## in the plot directory;
##
## @example
## dump_demos plot dump.m png
## @end example
## @seealso{fntests, test, demo}
## @end deftypefn

## Author: Søren Hauberg  <soren@hauberg.org>

function dump_demos (dirs={"plot/appearance", "plot/draw", "plot/util", "image"}, output="dump_plot_demos.m", fmt="png")

  if (nargin > 3)
    print_usage ();
  endif

  if (ischar (dirs))
    dirs = {dirs};
  elseif (! iscellstr (dirs))
    error ("dump_demos: DIRS must be a cell array of strings with directory names");
  endif

  if (! isunix ())
    dirs = strrep (dirs, "/", filesep ());
  endif

  [~, funcname, ext] = fileparts (output);
  if (isempty (ext))
    output = [output ".m"];
  endif

  ## Create script beginning (close figures, etc.)
  fid = fopen (output, "w");
  fprintf (fid, "%% DO NOT EDIT!  Generated automatically by dump_demos.m\n");
  fprintf (fid, "function %s ()\n", funcname);
  fprintf (fid, "set (0, 'DefaultAxesColorOrder', ...\n");
  fprintf (fid, "  [ 0.00000   0.00000   1.00000 ;\n");
  fprintf (fid, "    0.00000   0.50000   0.00000 ;\n");
  fprintf (fid, "    1.00000   0.00000   0.00000 ;\n");
  fprintf (fid, "    0.00000   0.75000   0.75000 ;\n");
  fprintf (fid, "    0.75000   0.00000   0.75000 ;\n");
  fprintf (fid, "    0.75000   0.75000   0.00000 ;\n");
  fprintf (fid, "    0.25000   0.25000   0.25000 ]);\n");
  fprintf (fid, "close all\n");
  fprintf (fid, "more off\n");
  fprintf (fid, "diary diary.log\n");

  ## Run and print the demos in each directory
  for i = 1:numel (dirs)
    d = dirs{i};
    if (! is_absolute_filename (d))
      d = dir_in_loadpath (d);
    endif
    if (! exist (d, "dir"))
      error ("dump_demos: directory %s does not exist", d);
    endif
    dump_all_demos (d, fid, fmt);
  endfor

  ## Add helper functions like sombrero
  dump_helper_fcns (fid);

  ## Stop and flush diary
  fprintf (fid, "\ndiary off\n");

  ## Create script ending
  fprintf (fid, "end\n\n");

  ## Close script
  fclose (fid);

endfunction

function dump_all_demos (directory, fid, fmt)

  dirinfo = dir (fullfile (directory, "*.m"));
  flist = {dirinfo.name};
  ## Remove uigetdir, uigetfile, uiputfile, etc.
  flist = flist(! strncmp (flist, "ui", 2));
  ## Remove linkaxes, linkprops
  flist = flist(! strncmp (flist, "link", 4));
  ## Remove colormap
  flist = flist(! strncmp (flist, "colormap", 8));
  for i = 1:numel (flist)
    fcn = flist{i};
    fcn(end-1:end) = [];  # remove .m
    demos = get_demos (fcn);
    for d = 1:numel (demos)
      idx = sprintf ("%02d", d);
      base_fn = sprintf ("%s_%s", fcn, idx);
      fn = sprintf ('%s.%s', base_fn, fmt);
      ## Wrap each demo in a function which create a local scope
      ## to prevent that a previous demo overwrites i or pi, for example
      fprintf (fid, "\nfunction %s ()\n", base_fn);
      fprintf (fid, "    %s\n\n", demos{d});
      fprintf (fid, "end\n\n");

      fprintf (fid, "try\n");
      ## First check if the file already exists, skip demo if found
      fprintf (fid, "  if (~ exist ('%s', 'file'))\n", fn);
      ## Invoke the ancient, deprecated random seed generators, but there is an
      ## initialization mismatch with the more modern generators reported
      ## here (https://savannah.gnu.org/bugs/?42557).
      fprintf (fid, "    rand ('seed', 1);\n");
      fprintf (fid, "    tic ();\n");
      fprintf (fid, "    %s ();\n", base_fn);
      fprintf (fid, "    t_plot = toc ();\n");
      fprintf (fid, "    fig = (get (0, 'currentfigure'));\n");
      fprintf (fid, "    if (~ isempty (fig))\n");
      fprintf (fid, "      figure (fig);\n");
      fprintf (fid, "      fprintf ('Printing ""%s"" ... ');\n", fn);
      fprintf (fid, "      tic ();\n");
      fprintf (fid, "      print ('-d%s', '%s');\n", fmt, fn);
      fprintf (fid, "      t_print = toc ();\n");
      fprintf (fid, "      fprintf ('[%%f %%f] done\\n',t_plot, t_print);\n");
      fprintf (fid, "    end\n");
      ## Temporary fix for cruft accumulating in figure window.
      fprintf (fid, "    close ('all');\n");
      fprintf (fid, "  else\n");
      fprintf (fid, "    fprintf ('File ""%s"" already exists.\\n');\n", fn);
      fprintf (fid, "  end\n");
      fprintf (fid, "catch\n");
      fprintf (fid, "  fprintf ('ERROR in %s: %%s\\n', lasterr ());\n", base_fn);
      fprintf (fid, "  err_fid = fopen ('%s.err', 'w');\n", base_fn);
      fprintf (fid, "  fprintf (err_fid, '%%s', lasterr ());\n");
      fprintf (fid, "  fclose (err_fid);\n");
      fprintf (fid, "end\n");
    endfor
  endfor
  fprintf (fid, "\nclose all\n");

endfunction

function retval = get_demos (fcn)

  [code, idx] = test (fcn, "grabdemo");
  num_demos = length (idx) - 1;
  retval = cell (1, num_demos);
  ## Now split the demos into a cell array
  for k = 1:num_demos
    retval{k} = oct2mat (code(idx(k):idx(k+1)-1));
  endfor

endfunction

function code = oct2mat (code)

  ## Simple hacks to make things Matlab compatible
  code = strrep (code, "%!", "%%");    # system operator !
  code = strrep (code, "!", "~");      # logical not operator

  ## Simply replacing double quotes with single quotes
  ## causes problems with strings like 'hello "world"' or transpose.

  ## Test input for double quote replacement:
  ## title ("bar");
  ## a'
  ## foo 'bar' "baz"
  ## image (repmat ((1:64)', 1, 64));
  ## fprintf ('File "brighten_01.png" already exists.\n');
  ## title ({'x^2 + y^2'; 'plotted over circular disk with "circ"'});
  ## annotation ('textbox', [0.1 0 0.8 1], 'string', ...
  ##             '"headstyle" property:', ...

  code = regexprep (code, "[(,;\n][ ]*'[^']*'(*SKIP)(*F)|\"", "'",
                          "lineanchors", "dotexceptnewline");

  ## replace # not inside single quotes
  code = regexprep (code, "[(,;\n][ ]*'[^']*'(*SKIP)(*F)|#", "%");

  ## Shorten all long forms of endXXX to 'end'
  endkeywords = {"endfor", "endfunction", "endif", "endwhile", "end_try_catch"};
  for k = 1:numel (endkeywords)
    code = strrep (code, endkeywords{k}, "end");
  endfor
  ## Comment keywords unknown to Matlab
  commentkeywords = {"unwind_protect", "end_unwind_protect"};
  for k = 1:numel (commentkeywords)
    code = strrep (code, commentkeywords{k}, ["%" commentkeywords{k}]);
  endfor
  ## Fix in-place operators
  code = regexprep (code, '(\S+)(\s*)(.?[+-/*])=', '$1 = $1 $3');
  ## Fix x = y = z = XXX assignment
  code = regexprep (code, '^\s*([^=\s]+)\s*=\s*([^=\s]+)\s*=\s*([^=\s]+)\s*=\s*([^=\n]+)$', ...
                          "$1 = $4\n$2 = $4\n$3 = $4", "lineanchors");
  ## Fix x = y = XXX assignment
  code = regexprep (code, '^\s*([^=\s]+)\s*=\s*([^=\s]+)\s*=\s*([^=\n]+)$', ...
                          "$1 = $3\n$2 = $3", "lineanchors");

endfunction

function dump_helper_fcns (fid)

  fprintf (fid, "\n%s\n", repmat ("%", [1, 60]));
  fdisp (fid, "% Helper functions");
  fprintf (fid, "%s\n", repmat ("%", [1, 60]));

  ## Add sombrero function
  fdisp (fid, [
"function [x, y, z] = sombrero (n)                                            "
"                                                                             "
"  if (nargin == 0)                                                           "
"    n = 41;                                                                  "
"  end                                                                        "
"                                                                             "
"  [xx, yy] = meshgrid (linspace (-8, 8, n));                                 "
"  r = sqrt (xx.^2 + yy.^2) + eps;  % eps prevents div/0 errors               "
"  zz = sin (r) ./ r;                                                         "
"                                                                             "
"  if (nargout == 0)                                                          "
"    surf (xx, yy, zz);                                                       "
"  elseif (nargout == 1)                                                      "
"    x = zz;                                                                  "
"  else                                                                       "
"    x = xx;                                                                  "
"    y = yy;                                                                  "
"    z = zz;                                                                  "
"  end                                                                        "
"                                                                             "
"end                                                                          "
]);  # End of sombrero dump

  fprintf (fid, "\n");

  ## Add rgbplot function
  fdisp (fid, [
"function h = rgbplot (cmap, style)                                           "
"                                                                             "
"  if (nargin == 1)                                                           "
"    style = 'profile';                                                       "
"  end                                                                        "
"                                                                             "
"  switch (lower (style))                                                     "
"    case 'profile'                                                           "
"      htmp = plot (cmap(:,1),'r', cmap(:,2),'g', cmap(:,3),'b');             "
"      set (gca (), 'ytick', 0:0.1:1);                                        "
"      set (gca (), 'xlim', [0 size(cmap,1)]);                                "
"    case 'composite'                                                         "
"      htmp = image (1:size(cmap,1));                                         "
"      set (gca, 'ytick', []);                                                "
"      colormap (cmap);                                                       "
"  end                                                                        "
"  xlabel ('color index');                                                    "
"                                                                             "
"  if (nargout > 0)                                                           "
"    h = htmp;                                                                "
"  end                                                                        "
"                                                                             "
"end                                                                          "
]);  # End of rgbplot dump

  fprintf (fid, "\n");

  fprintf (fid, "\n%s\n", repmat ("%", [1, 60]));

endfunction

