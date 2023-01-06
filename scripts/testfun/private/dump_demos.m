########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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

function dump_demos (dirs={"plot/appearance", "plot/draw", "plot/util", "image"}, mfile="dump_plot_demos.m", fmt="png")

  if (ischar (dirs))
    dirs = {dirs};
  elseif (! iscellstr (dirs))
    error ("dump_demos: DIRS must be a cell array of strings with directory names");
  endif

  if (! isunix ())
    dirs = strrep (dirs, "/", filesep ());
  endif

  [~, funcname, ext] = fileparts (mfile);
  if (isempty (ext))
    mfile = [mfile ".m"];
  endif

  ## Create script beginning (close figures, etc.)
  fid = fopen (mfile, "w");
  fprintf (fid, "%% DO NOT EDIT!  Generated automatically by dump_demos.m\n");
  fprintf (fid, "function %s ()\n", funcname);
  fprintf (fid, "set (0, 'DefaultAxesColorOrder', ...\n");
  fprintf (fid, "  [ 0.00000   0.44700   0.74100 ;\n");
  fprintf (fid, "    0.85000   0.32500   0.09800 ;\n");
  fprintf (fid, "    0.92900   0.69400   0.12500 ;\n");
  fprintf (fid, "    0.49400   0.18400   0.55600 ;\n");
  fprintf (fid, "    0.46600   0.67400   0.18800 ;\n");
  fprintf (fid, "    0.30100   0.74500   0.93300 ;\n");
  fprintf (fid, "    0.63500   0.07800   0.18400 ]);\n");
  fprintf (fid, "set (0, 'DefaultFigureColorMap', ...\n");
  fprintf (fid, "  [ 0.2670040   0.0048743   0.3294152 ;\n");
  fprintf (fid, "    0.2726517   0.0258457   0.3533673 ;\n");
  fprintf (fid, "    0.2771063   0.0509139   0.3762361 ;\n");
  fprintf (fid, "    0.2803562   0.0742015   0.3979015 ;\n");
  fprintf (fid, "    0.2823900   0.0959536   0.4182508 ;\n");
  fprintf (fid, "    0.2832046   0.1168933   0.4371789 ;\n");
  fprintf (fid, "    0.2828093   0.1373502   0.4545959 ;\n");
  fprintf (fid, "    0.2812308   0.1574799   0.4704339 ;\n");
  fprintf (fid, "    0.2785162   0.1773480   0.4846539 ;\n");
  fprintf (fid, "    0.2747355   0.1969692   0.4972505 ;\n");
  fprintf (fid, "    0.2699818   0.2163303   0.5082545 ;\n");
  fprintf (fid, "    0.2643686   0.2354047   0.5177319 ;\n");
  fprintf (fid, "    0.2580262   0.2541617   0.5257802 ;\n");
  fprintf (fid, "    0.2510987   0.2725732   0.5325222 ;\n");
  fprintf (fid, "    0.2437329   0.2906195   0.5380971 ;\n");
  fprintf (fid, "    0.2360733   0.3082910   0.5426518 ;\n");
  fprintf (fid, "    0.2282632   0.3255865   0.5463354 ;\n");
  fprintf (fid, "    0.2204250   0.3425172   0.5492871 ;\n");
  fprintf (fid, "    0.2126666   0.3591022   0.5516350 ;\n");
  fprintf (fid, "    0.2050791   0.3753661   0.5534932 ;\n");
  fprintf (fid, "    0.1977219   0.3913409   0.5549535 ;\n");
  fprintf (fid, "    0.1906314   0.4070615   0.5560891 ;\n");
  fprintf (fid, "    0.1838194   0.4225638   0.5569522 ;\n");
  fprintf (fid, "    0.1772724   0.4378855   0.5575761 ;\n");
  fprintf (fid, "    0.1709575   0.4530630   0.5579740 ;\n");
  fprintf (fid, "    0.1648329   0.4681295   0.5581427 ;\n");
  fprintf (fid, "    0.1588454   0.4831171   0.5580587 ;\n");
  fprintf (fid, "    0.1529512   0.4980530   0.5576847 ;\n");
  fprintf (fid, "    0.1471316   0.5129595   0.5569733 ;\n");
  fprintf (fid, "    0.1414022   0.5278543   0.5558645 ;\n");
  fprintf (fid, "    0.1358330   0.5427501   0.5542887 ;\n");
  fprintf (fid, "    0.1305821   0.5576525   0.5521757 ;\n");
  fprintf (fid, "    0.1258984   0.5725631   0.5494454 ;\n");
  fprintf (fid, "    0.1221631   0.5874763   0.5460234 ;\n");
  fprintf (fid, "    0.1198724   0.6023824   0.5418306 ;\n");
  fprintf (fid, "    0.1196266   0.6172658   0.5367956 ;\n");
  fprintf (fid, "    0.1220459   0.6321070   0.5308480 ;\n");
  fprintf (fid, "    0.1276677   0.6468818   0.5239242 ;\n");
  fprintf (fid, "    0.1368349   0.6615629   0.5159668 ;\n");
  fprintf (fid, "    0.1496433   0.6761197   0.5069243 ;\n");
  fprintf (fid, "    0.1659673   0.6905190   0.4967519 ;\n");
  fprintf (fid, "    0.1855384   0.7047252   0.4854121 ;\n");
  fprintf (fid, "    0.2080305   0.7187010   0.4728733 ;\n");
  fprintf (fid, "    0.2331273   0.7324064   0.4591059 ;\n");
  fprintf (fid, "    0.2605315   0.7458020   0.4440959 ;\n");
  fprintf (fid, "    0.2900007   0.7588465   0.4278259 ;\n");
  fprintf (fid, "    0.3213300   0.7714979   0.4102927 ;\n");
  fprintf (fid, "    0.3543553   0.7837140   0.3914876 ;\n");
  fprintf (fid, "    0.3889303   0.7954531   0.3714207 ;\n");
  fprintf (fid, "    0.4249331   0.8066739   0.3500988 ;\n");
  fprintf (fid, "    0.4622468   0.8173376   0.3275447 ;\n");
  fprintf (fid, "    0.5007536   0.8274091   0.3037990 ;\n");
  fprintf (fid, "    0.5403370   0.8368582   0.2789167 ;\n");
  fprintf (fid, "    0.5808612   0.8456634   0.2530009 ;\n");
  fprintf (fid, "    0.6221708   0.8538156   0.2262237 ;\n");
  fprintf (fid, "    0.6640873   0.8613210   0.1988794 ;\n");
  fprintf (fid, "    0.7064038   0.8682063   0.1714949 ;\n");
  fprintf (fid, "    0.7488853   0.8745222   0.1450376 ;\n");
  fprintf (fid, "    0.7912731   0.8803462   0.1212910 ;\n");
  fprintf (fid, "    0.8333021   0.8857801   0.1033262 ;\n");
  fprintf (fid, "    0.8747175   0.8909453   0.0953508 ;\n");
  fprintf (fid, "    0.9152963   0.8959735   0.1004700 ;\n");
  fprintf (fid, "    0.9548396   0.9010058   0.1178764 ;\n");
  fprintf (fid, "    0.9932479   0.9061566   0.1439362 ]);\n");

  fprintf (fid, "close all\n");
  fprintf (fid, "more off\n");
  fprintf (fid, "diary diary.log\n");

  ## Run and print the demos in each directory
  for i = 1:numel (dirs)
    d = dirs{i};
    if (! is_absolute_filename (d))
      d = dir_in_loadpath (d);
    endif
    if (! isfolder (d))
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
      base_fcn = sprintf ("%s_%s", fcn, idx);
      fn = sprintf ('%s.%s', base_fcn, fmt);
      ## Wrap each demo in a function which create a local scope
      ## to prevent that a previous demo overwrites i or pi, for example
      fprintf (fid, "\nfunction %s ()\n", base_fcn);
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
      fprintf (fid, "    %s ();\n", base_fcn);
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
      fprintf (fid, "  fprintf ('ERROR in %s: %%s\\n', lasterr ());\n", base_fcn);
      fprintf (fid, "  err_fid = fopen ('%s.err', 'w');\n", base_fcn);
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
"  idx = 1:rows (cmap);                                                       "
"  switch (lower (style))                                                     "
"    case 'profile'                                                           "
"      htmp = plot (idx, cmap(:,1), 'r', ...                                  "
"                   idx, cmap(:,2), 'g', ...                                  "
"                   idx, cmap(:,3), 'b');                                     "
"      set (gca (), 'ytick', 0:0.1:1);                                        "
"      set (gca (), 'xlim', [0 rows(cmap)]);                                  "
"    case 'composite'                                                         "
"      htmp = image (idx);                                                    "
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
