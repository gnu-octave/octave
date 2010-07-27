## Copyright (C) 2008, 2009 David Bateman
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
## @deftypefn  {Function File} {} print ()
## @deftypefnx {Function File} {} print (@var{options})
## @deftypefnx {Function File} {} print (@var{filename}, @var{options})
## @deftypefnx {Function File} {} print (@var{h}, @var{filename}, @var{options})
## Print a graph, or save it to a file
##
## @var{filename} defines the file name of the output file.  If the
## file name has no suffix, one is inferred from the specified
## device and appended to the file name.  If no
## filename is specified, the output is sent to the printer.
##
## @var{h} specifies the figure handle.  If no handle is specified
## the handle for the current figure is used.
##
## @var{options}:
## @table @code
## @item -P@var{printer}
##   Set the @var{printer} name to which the graph is sent if no
##   @var{filename} is specified.
##
## @item -G@var{ghostscript_command}
##   Specify the command for calling Ghostscript.  For Unix and Windows,
## the defaults are 'gs' and 'gswin32c', respectively.
##
## @item -color
## @itemx -mono
##   Monochrome or color lines.
##
## @item -solid
## @itemx -dashed
##   Solid or dashed lines.
##
## @item -portrait
## @itemx -landscape
##   Specify the orientation of the plot for printed output.
##
## @item -d@var{device}
##   Output device, where @var{device} is one of:
##   @table @code
##   @item ps
##   @itemx ps2
##   @itemx psc
##   @itemx psc2
##     Postscript (level 1 and 2, mono and color)
##
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
##     Generate a @LaTeX{} (or @TeX{}) file for labels, and eps/ps for
## graphics.  The file produced by @code{epslatexstandalone} can be
## processed directly by @LaTeX{}.  The other formats are intended to
## be included in a @LaTeX{} (or @TeX{}) document.  The @code{tex} device
## is the same as the @code{epslatex} device.
##
##   @item tikz
##     Generate a @LaTeX{} file using gnuplot's Lua/TikZ terminal.
##
##   @item ill
##   @itemx aifm
##     Adobe Illustrator
##
##   @item cdr
##   @itemx @nospell{corel}
##     CorelDraw
##
##   @item dxf
##     AutoCAD
##
##   @item emf
##   @itemx meta
##     Microsoft Enhanced Metafile
##
##   @item fig
##     XFig.  If this format is selected the additional options
##     @option{-textspecial} or @option{-textnormal} can be used to control
##     whether the special flag should be set for the text in
##     the figure (default is @option{-textnormal}). 
##
##   @item hpgl
##     HP plotter language
##
##   @item mf
##     Metafont
##
##   @item png
##     Portable network graphics
##
##   @item jpg
##   @itemx jpeg
##     JPEG image
##
##   @item gif
##     GIF image
##
##   @item pbm
##     PBMplus
##
##   @item svg
##     Scalable vector graphics
##
##   @item pdf
##     Portable document format
##   @end table
##
##   If the device is omitted, it is inferred from the file extension,
## or if there is no filename it is sent to the printer as postscript.
##
## @item -d@var{ghostscript_device}
##   Additional devices are supported by Ghostscript.
## Some examples are;
##
##   @table @code
##   @item ljet2p 
##     HP LaserJet IIP
##
##   @item ljet3 
##     HP LaserJet III
##
##   @item deskjet
##     HP DeskJet and DeskJet Plus
##
##   @item cdj550
##     HP DeskJet 550C
##
##   @item paintjet
##     HP PointJet
##
##   @item pcx24b
##     24-bit color PCX file format
##
##   @item ppm
##     Portable Pixel Map file format
##   @end table
##
##   For a complete list, type `system ("gs -h")' to see what formats
## and devices are available.
##
##   When Ghostscript output is sent to a printer the size is determined
## by the figure's "papersize" property.  When the output 
## is sent to a file the size is determined by the figure's
## "paperposition" property.
##
## @itemx -append
##   Appends the output to a pre-existing file.  Presently only PDF,
## and Postscript files are supported.
##
## @itemx -r@var{NUM}
##   Resolution of bitmaps in pixels per inch.  For both metafiles and 
## SVG the default is the screen resolution, for other it is 150 dpi.
## To specify screen resolution, use "-r0".
##
## @item -tight
##   Forces a tight bounding box for eps-files.  Since the Ghostscript
## devices are conversions of an eps-file, this option works for those
## devices as well.
##
## @itemx -S@var{xsize},@var{ysize}
##   Plot size in pixels for EMF, GIF, JPEG, PBM, PNG and SVG@.  If
## using the command form of the print function, you must quote the
## @var{xsize},@var{ysize} option.  For example, by writing
## @w{@code{"-S640,480"}}.  The size defaults to that specified by the
## figure's paperposition property.
##
## @item -F@var{fontname}
## @itemx -F@var{fontname}:@var{size}
## @itemx -F:@var{size}
##   @var{fontname} set the postscript font (for use with postscript,
## aifm, @nospell{corel} and fig).  By default, 'Helvetica' is set for PS/aifm,
## and 'SwitzerlandLight' for Corel.  It can also be 'Times-Roman'.
## @var{size} is given in points.  @var{fontname} is ignored for the
## fig device.
## @end table
##
## The filename and options can be given in any order.
## @end deftypefn

function varargout = print (varargin)

  f = gcf ();
  drawnow ();
  backend = (get (f, "__backend__"));

  varargout = cell (1, nargout);
  [varargout{:}] = feval (strcat ("__", backend, "_print__"), varargin{:});

endfunction
