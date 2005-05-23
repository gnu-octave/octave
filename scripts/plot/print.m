## Copyright (C) 1999 Daniel Heiserer
## Copyright (C) 2001 Laurent Mazet
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
## @deftypefn {Function File} {} print (@var{filename}, @var{options})
##
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
##   @item ill
##   @itemx aifm
##     Adobe Illustrator
##   @item cdr
##   @itemx corel
##     CorelDraw
##   @item hpgl
##     HP plotter language
##   @item fig
##     XFig
##   @item dxf
##     AutoCAD
##   @item mf
##     Metafont
##   @item png
##     Portable network graphics
##   @item pbm
##     PBMplus
##   @end table
##
##   Other devices are supported by "convert" from ImageMagick.  Type
##   system("convert") to see what formats are available.
##
##   If the device is omitted, it is inferred from the file extension,
##   or if there is no filename it is sent to the printer as postscript.
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

  orientation = "landscape"; ## orient;
  use_color = 0; # 0=default, -1=mono, +1=color
  force_solid = 0; # 0=default, -1=dashed, +1=solid
  fontsize = "";
  font = "";
  name = "";
  devopt = "";
  printer = "";

  for i = 1:nargin
    arg = varargin{i};
    if (isstr (arg))
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
      elseif (length (arg) > 2 && arg(1:2) == "-d")
	devopt = arg(3:length(arg));
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

  if (strcmp (dev, "ill"))
    dev = "aifm";
  elseif (strcmp (dev, "cdr"))
    dev = "corel";
  endif

  ## check if we have to use convert
  dev_list = {"aifm" "corel" "fig" "png" "pbm" "dxf" "mf" "hpgl", ...
	      "ps" "ps2" "psc" "psc2" "eps" "eps2" "epsc" "epsc2"};
  convertname = "";
  idx = cellidx (dev_list, dev);
  if (! idx)
    if (! isempty (devopt))
      convertname = strcat (devopt, ":", name);
    else
      convertname = name;
    endif
    dev = "epsc";
    name = strcat (tmpnam, ".eps");
  endif

  unwind_protect

    if (strcmp (dev, "ps") || strcmp (dev, "ps2") ...
	|| strcmp (dev, "psc")  || strcmp (dev, "psc2")
	|| strcmp (dev, "epsc") || strcmp (dev, "epsc2")
	|| strcmp (dev, "eps")  || strcmp (dev, "eps2"))
      ## Various postscript options
      if (dev(1) == "e")
	options = "eps ";
      else
	options = strcat (orientation, " ");
      endif
      options = strcat (options, "enhanced ");

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

      __gnuplot_raw__ (sprintf ("set terminal postscript %s;\n", options));


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

      __gnuplot_raw__ (sprintf ("set terminal %s %s;\n", dev, options));

    elseif (strcmp (dev, "fig"))
      ## XFig
      options = orientation;
      if (use_color >= 0)
	options = " color";
      else
	options = " mono";
      endif
      if (! isempty (fontsize))
	options = strcat (options, " fontsize ", fontsize);
      endif
      __gnuplot_raw__ (sprintf ("set terminal fig %s;\n", options));

    elseif (strcmp (dev, "png") || strcmp (dev, "pbm"))
      ## Portable network graphics, PBMplus

      ## XXX FIXME XXX -- New PNG interface takes color as "xRRGGBB"
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

      __gnuplot_raw__ ("set terminal png large;\n")

    elseif (strcmp (dev, "dxf") || strcmp (dev, "mf") || strcmp (dev, "hpgl"))
      ## AutoCad DXF, METAFONT, HPGL
      __gnuplot_raw__ (sprintf ("set terminal %s;\n", dev));
    endif

    ## Gnuplot expects " around output file name

    __gnuplot_raw__ (sprintf ("set output \"%s\";\n", name));
    __gnuplot_replot__

  unwind_protect_cleanup

    ## Restore init state
    __gnuplot_raw__ ("set terminal pop;\n");
    __gnuplot_raw__ ("set output;\n")
    __gnuplot_replot__

  end_unwind_protect

  if (! isempty (convertname))
    command = sprintf ("convert '%s' '%s'", name, convertname);
    [output, errcode] = system (command);
    unlink (name);
    if (errcode)
      error ("print: could not convert");
    endif
  endif

  ## XXX FIXME XXX -- This looks like a dirty, Unix-specific hack.
  ## DAS
  if (doprint)
    system (sprintf ("lpr %s '%s'", printer, printname));
    unlink (printname);
  endif

endfunction
