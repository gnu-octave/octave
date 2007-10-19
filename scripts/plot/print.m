## Copyright (C) 1999, 2005, 2006, 2007 Daniel Heiserer
## Copyright (C) 2001 Laurent Mazet
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
## @deftypefn {Function File} {} print (@var{filename}, @var{options})
## Print a graph, or save it to a file
##
## @var{filename} defines the file name of the output file. If no
## filename is specified, output is sent to the printer.
##
## @var{options}:
## @table @code
## @item -P@var{printer}
##   Set the @var{printer} name to which the graph is sent if no
##   @var{filename} is specified.
## @item -color
## @itemx -mono
##   Monochrome or colour lines.
## @item -solid
## @itemx -dashed
##   Solid or dashed lines.
## @item -portrait
## @itemx -landscape
##   Plot orientation, as returned by "orient".
## @item -d@var{device}
##   Output device, where @var{device} is one of:
##   @table @code
##   @item ps
##   @itemx ps2
##   @itemx psc
##   @itemx psc2
##     Postscript (level 1 and 2, mono and color)
##   @item eps
##   @itemx eps2
##   @itemx epsc
##   @itemx epsc2
##     Encapsulated postscript (level 1 and 2, mono and color)
##   @item tex
##   @itemx epslatex
##   @itemx epslatexstandalone
##   @itemx pstex
##   @itemx pslatex
##     Generate a LaTeX (or TeX) file for labels, and eps/ps for
##     graphics.  The file produced by @code{epslatexstandalone} can be
##     processed directly by LaTeX.  The other formats are intended to
##     be included in a LaTeX (or TeX) document.  The @code{tex} device
##     is the same as the @code{epslatex} device.
##   @item ill
##   @itemx aifm
##     Adobe Illustrator
##   @item cdr
##   @itemx corel
##     CorelDraw
##   @item dxf
##     AutoCAD
##   @item emf
##     Microsoft Enhanced Metafile
##   @item fig
##     XFig.  If this format is selected the additional options
##     @code{-textspecial} or @code{-textnormal} can be used to control
##     whether the special flag should be set for the text in the figure
##     (default is @code{-textnormal}). 
##   @item hpgl
##     HP plotter language
##   @item mf
##     Metafont
##   @item png
##     Portable network graphics
##   @item pbm
##     PBMplus
##   @item svg
##     Scalable vector graphics
##   @end table
##
##   Other devices are supported by "convert" from ImageMagick.  Type
##   system("convert") to see what formats are available.
##
##   If the device is omitted, it is inferred from the file extension,
##   or if there is no filename it is sent to the printer as postscript.
##
## @itemx -S@var{xsize},@var{ysize}
##   Plot size in pixels for PNG and SVG.  If using the command form of
##   the print function, you must quote the @var{xsize},@var{ysize}
##   option.  For example, by writing @code{"-S640,480"}.
##
## @item -F@var{fontname}
## @itemx -F@var{fontname}:@var{size}
## @itemx -F:@var{size}
##   @var{fontname} set the postscript font (for use with postscript,
##   aifm, corel and fig). By default, 'Helvetica' is set for PS/Aifm,
##   and 'SwitzerlandLight' for Corel. It can also be 'Times-Roman'.
##   @var{size} is given in points. @var{fontname} is ignored for the
##   fig device.
## @end table
##
## The filename and options can be given in any order.
## @end deftypefn

## Author: Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>
## Adapted-By: jwe

## PKG_ADD: mark_as_command print

function print (varargin)

  orientation = orient ();
  use_color = 0; # 0=default, -1=mono, +1=color
  force_solid = 0; # 0=default, -1=dashed, +1=solid
  fontsize = "";
  font = "";
  size = "";
  name = "";
  devopt = "";
  printer = "";
  debug = false;
  debug_file = "octave-print-commands.log";
  special_flag = "textnormal";

  ## Ensure the last figure is on the screen for single line commands like
  ##   plot(...); print(...);
  drawnow ();

  for i = 1:nargin
    arg = varargin{i};
    if (ischar (arg))
      if (strcmp (arg, "-color"))
	use_color = 1;
      elseif (strcmp (arg, "-mono"))
	use_color = -1;
      elseif (strcmp (arg, "-solid"))
        force_solid = 1;
      elseif (strcmp (arg, "-dashed"))
        force_solid = -1;
      elseif (strcmp (arg, "-portrait"))
	orientation = "portrait";
      elseif (strcmp (arg, "-landscape"))
	orientation = "landscape";
      elseif (strcmp (arg, "-textspecial"))
	special_flag = "textspecial";
      elseif (strncmp (arg, "-debug", 6))
	debug = true;
	if (length (arg) > 7)
	  debug_file = arg(8:end);
	endif
      elseif (length (arg) > 2 && arg(1:2) == "-d")
	devopt = arg(3:end);
      elseif (length (arg) > 2 && arg(1:2) == "-P")
	printer = arg;
      elseif (length (arg) > 2 && arg(1:2) == "-F")
	idx = rindex(arg, ":");
	if (idx)
	  font = arg(3:idx-1);
	  fontsize = arg(idx+1:length(arg));
	else
	  font = arg(3:length(arg));
	endif
      elseif (length (arg) > 2 && arg(1:2) == "-S")
	size = arg(3:length(arg));
      elseif (length (arg) >= 1 && arg(1) == "-")
	error ("print: unknown option `%s'", arg);
      elseif (length (arg) > 0)
	name = arg;
      endif
    else
      error ("print: expects string options");
    endif
  endfor

  doprint = isempty (name);
  if (doprint)
    if (isempty (devopt))
      printname = strcat (tmpnam, ".ps");
    else
      printname = strcat (tmpnam, ".", devopt);
    endif
    name = printname;
  endif

  if (isempty (devopt))
    dot = rindex (name, ".");
    if (dot == 0)
      error ("print: no format specified");
    else
      dev = tolower (name(dot+1:end));
    endif
  else
    dev = devopt;
  endif

  if (strcmp (dev, "tex"))
    dev = "epslatex";
    ## gnuplot 4.0 wants ".eps" in the output name    
    if (compare_versions (__gnuplot_version__, "4.2", "<"))
      name = strcat (name(1:dot), "eps");
    endif
  elseif (strcmp (dev, "ill"))
    dev = "aifm";
  elseif (strcmp (dev, "cdr"))
    dev = "corel";
  endif

  ## check if we have to use convert
  dev_list = {"aifm", "corel", "fig", "png", "pbm", "dxf", "mf", "svg", ...
	      "hpgl", "ps", "ps2", "psc", "psc2", "eps", "eps2", ...
	      "epsc", "epsc2", "emf", "pstex", "pslatex", ...
	      "epslatex", "epslatexstandalone"};
  convertname = "";
  [idx, errmsg] = cellidx (dev_list, dev);
  if (! idx)
    if (! isempty (devopt))
      convertname = strcat (devopt, ":", name);
    else
      convertname = name;
    endif
    dev = "epsc";
    name = strcat (tmpnam, ".eps");
  endif

  if (strcmp (dev, "ps") || strcmp (dev, "ps2") ...
      || strcmp (dev, "psc")  || strcmp (dev, "psc2")
      || strcmp (dev, "epsc") || strcmp (dev, "epsc2")
      || strcmp (dev, "eps")  || strcmp (dev, "eps2")
      || strcmp (dev, "pstex")|| strcmp (dev, "pslatex")
      || strcmp (dev, "epslatex") || strcmp (dev, "epslatexstandalone"))

    ## Various postscript options
    if (strcmp (dev, "pstex")|| strcmp (dev, "pslatex")
	|| strcmp (dev, "epslatex"))
      termn = dev;
      options = "";
    elseif (strcmp (dev, "epslatexstandalone"))
      if (compare_versions (__gnuplot_version__, "4.2", ">="))
	termn = "epslatex";
	options = "standalone ";
      else
	error ("print: epslatexstandalone needs gnuplot 4.2 or higher");
      endif
    else
      if (dev(1) == "e")
	options = "eps ";
      else
	options = strcat (orientation, " ");
      endif
      options = strcat (options, "enhanced ");
      termn = "postscript";
    endif
    
    if (any (dev == "c") || use_color > 0)
      if (force_solid < 0)
	options = strcat (options, "color dashed ");
      else
	options = strcat (options, "color solid ");
      endif
    else
      if (force_solid > 0)
	options = strcat (options, "mono solid ");
      else
	options = strcat (options, "mono dashed ");
      endif
    endif

    if (! isempty (font))
      options = strcat (options, "\"", font, "\" ");
    endif
    if (! isempty (fontsize))
      options = strcat (options, " ", fontsize);
    endif
    
    new_terminal = strcat (termn, " ", options);
    
  elseif (strcmp (dev, "aifm") || strcmp (dev, "corel"))
    ## Adobe Illustrator, CorelDraw
    if (use_color >= 0)
      options = " color";
    else
      options = " mono";
    endif
    if (! isempty (font))
      options = strcat (options, " \"", font, "\"");
    endif
    if (! isempty (fontsize))
      options = strcat (options, " ", fontsize);
    endif

    new_terminal = strcat (dev, " ", options);

  elseif (strcmp (dev, "fig"))
    ## XFig
    options = orientation;
    if (use_color >= 0)
      options = " color";
    else
      options = " mono";
    endif
    options = strcat (options, " ", special_flag);
    if (! isempty (fontsize))
      options = strcat (options, " fontsize ", fontsize);
    endif

    new_terminal = strcat ("fig ", options);


  elseif (strcmp (dev, "emf"))
    ## Enhanced Metafile format
    options = " ";
    if (use_color >= 0)
      options = " color";
    else
      options = " mono";
    endif
    if (force_solid >= 0)
      options = strcat (options, " solid");
    endif
    if (! isempty (font))
      options = strcat (options, " \"", font, "\"");
    endif
    if (! isempty (fontsize))
      options = strcat (options, " ", fontsize);
    endif

    new_terminal = strcat ("emf ", options);

  elseif (strcmp (dev, "png") || strcmp (dev, "pbm"))
    ## Portable network graphics, PBMplus

    ## FIXME -- New PNG interface takes color as "xRRGGBB"
    ## where x is the literal character 'x' and 'RRGGBB' are the red,
    ## green and blue components in hex.  For now we just ignore it
    ## and use default.  The png terminal now is so rich with options,
    ## that one perhaps has to write a separate printpng.m function.
    ## DAS

    ## if (use_color >= 0)
    ##	eval (sprintf ("__gnuplot_set__ term %s color medium", dev));
    ##else
    ##eval (sprintf ("__gnuplot_set__ term %s mono medium", dev));
    ##endif

    if (isempty (size))
      options = " large";
    else
      options = strcat (" size ", size);
    endif
    new_terminal = strcat ("png", options);

  elseif (strcmp (dev, "dxf") || strcmp (dev, "mf") || strcmp (dev, "hpgl"))
    ## AutoCad DXF, METAFONT, HPGL
    new_terminal = dev;

  elseif (strcmp (dev, "svg"))
    ## SVG
    options = "";
    if (! isempty (size))
      options = strcat (" size ", size);
    endif
    new_terminal = strcat ("svg", options);

  endif

  if (debug)
    drawnow (new_terminal, name, debug_file);
  else
    drawnow (new_terminal, name);
  endif

  if (! isempty (convertname))
    command = sprintf ("convert '%s' '%s'", name, convertname);
    [errcode, output] = system (command);
    unlink (name);
    if (errcode)
      error ("print: could not convert");
    endif
  endif

  ## FIXME -- This looks like a dirty, Unix-specific hack.
  ## DAS
  if (doprint)
    system (sprintf ("lpr %s '%s'", printer, printname));
    unlink (printname);
  endif

endfunction
