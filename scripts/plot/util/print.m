########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {} print ()
## @deftypefnx {} {} print (@var{options})
## @deftypefnx {} {} print (@var{filename}, @var{options})
## @deftypefnx {} {} print (@var{hfig}, @dots{})
## @deftypefnx {} {@var{RGB} =} print (@qcode{"-RGBImage"}, @dots{})
## Format a figure for printing and either save it to a file, send it to a
## printer, or return an RGB image.
##
## @var{filename} defines the name of the output file.  If the filename has
## no suffix then one is inferred from the specified device and appended to the
## filename.  When neither a filename nor the @qcode{"-RGBImage"} option is
## present, the output is sent to the printer.  The various options and
## filename arguments may be given in any order, except for the figure handle
## argument @var{hfig} which must be first if it is present.
##
## Example: Print to a file using PDF and JPEG formats.
##
## @example
## @group
## figure (1);
## clf ();
## surf (peaks);
## print figure1.pdf    # The extension specifies the format
## print -djpg figure1  # Will produce "figure1.jpg" file
## @end group
## @end example
##
## If the first argument is a handle @var{hfig} to a figure object then it
## specifies the figure to print.  By default, the current figure returned
## by @code{gcf} is printed.
##
## For outputs to paged formats, for example, PostScript and PDF, the page size
## is specified by the figure's @code{papersize} property together with the
## @code{paperunits} property.  The location and size of the plot on the page
## are specified by the figure's @code{paperposition} property.  The
## orientation of the page is specified by the figure's @code{paperorientation}
## property.
##
## For non-page formats---for example, image formats like JPEG---the width and
## height of the output are specified by the figure's @code{paperposition(3:4)}
## property values.
##
## The @code{print} command supports many @var{options}:
##
## @table @code
## @item -f@var{h}
##   Specify the handle, @var{h}, of the figure to be printed.
##
## Example: Print figure 1.
##
## @example
## @group
## figure (1);
## clf ();
## surf (peaks);
## figure (2);
## print -f1 figure1.pdf
## ## Equivalent functional form:
## print (1, "figure1.pdf")
## @end group
## @end example
##
## @item -P@var{printer}
##   Set the @var{printer} name to which the plot is sent if no @var{filename}
## is specified.
##
## Example: Print to printer named PS_printer using PostScript format.
##
## @example
## @group
## clf ();
## surf (peaks);
## print -dpswrite -PPS_printer
## @end group
## @end example
##
## @item -RGBImage
##   Return an M-by-N-by-3 RGB image of the figure.  The size of the image
## depends on the formatting options.  This is similar to taking a screen
## capture of the plot, but formatting options may be changed such as the
## resolution or monochrome/color.
##
## Example: Get the pixels of a figure image.
##
## @example
## @group
## clf ();
## surf (peaks);
## @var{rgb} = print ("-RGBImage");
## @end group
## @end example
##
## @item  -image | -opengl
## @itemx -vector | -painters
##   Specifies whether the pixel-based renderer (@option{-image} or
## @option{-opengl}) or vector-based renderer (@option{-vector} or
## @option{-painters}) is used.  This is equivalent to changing the figure's
## @qcode{"Renderer"} property.  When the figure
## @nospell{@qcode{"RendererMode"}} property is @qcode{"auto"} (the default)
## Octave will use the @qcode{"opengl"} renderer for raster formats (e.g.,
## JPEG) and @qcode{"painters"} for vector formats (e.g., PDF)@.  These options
## are only supported for the "qt" graphics toolkit.
##
## @item  -svgconvert (default)
## @itemx -nosvgconvert
##   When using the @option{-painters} renderer, this enables or disables the
## SVG based backend toolchain with enhanced characteristics:
##
## @table @asis
## @item Font handling:
## For interpreters "none" and "tex", the actual font is embedded in the output
## file which allows for printing arbitrary characters and fonts in all vector
## formats.
##
## Strings using the @qcode{"latex"} interpreter, are rendered using path
## objects.  This looks good but note that textual info (font,
## characters@dots{}) are lost.
##
## @item Output Simplification:
## By default, the option @option{-painters} renders patch and surface objects
## using assemblies of triangles.  This may lead to anti-aliasing artifacts
## when viewing the file.  The @option{-svgconvert} option reconstructs
## polygons in order to avoid those artifacts (particularly for 2-D figures).
##
## @item Transparency:
## Allows for printing transparent graphics objects in PDF format.
## For PostScript formats the presence of any transparent object will cause the
## output to be rasterized.
## @end table
##
## Caution: If Octave was built against Qt version earlier than 5.13,
## @option{-svgconvert} may lead to inaccurate rendering of image objects.
##
## @item -polymerge
## @itemx -nopolymerge
## @itemx -polymerge-all
##   When using the SVG based backend @option{-svgconvert}, faces are rendered
## as triangles.  In some cases, some viewers might display fine lines where
## those triangles share an edge.  These options control whether all triangles
## that share edges are merged into polygons (@option{-polymerge-all} which
## might take some time for graphics consisting of many triangles -- including
## line markers), only consecutive polygons are merged (@option{-polymerge}),
## or no triangles are merged at all (@option{-no-polymerge}).  By default,
## only consecutive triangles sharing an edge are merged, unless the printed
## figure contains patch or surface graphics objects in which case all
## triangles that are sharing an edge are merged.
##
## @item  -portrait
## @itemx -landscape
##   Specify the orientation of the plot for printed output.
## For non-printed output the aspect ratio of the output corresponds to the
## plot area defined by the @qcode{"paperposition"} property in the
## orientation specified.  This option is equivalent to changing the figure's
## @qcode{"paperorientation"} property.
##
## @item  -fillpage
## @itemx -bestfit
##   When using a page-based format (PDF, PostScript, printer) ignore the
## @qcode{"paperposition"} property and have the plot occupy the entire page.
## The option @option{-fillpage} will stretch the plot to occupy the page with
## 0.25 inch margins all around.  The option @option{-bestfit} will expand the
## plot to take up as much room as possible on the page @strong{without}
## distorting the original aspect ratio of the plot.
##
## @item  -color
## @itemx -mono
##   Color or monochrome output.
##
## @item  -solid
## @itemx -dashed
##   Force all lines to be solid or dashed, respectively.
##
## @item -noui
##   Don't print uicontrol objects such as pushbuttons which may overlay the
## plot.  This is the default behavior and it is not possible to include
## uicontrol objects in the output without using an external screen capture
## tool.
##
## @item -r@var{NUM}
##   Resolution of bitmaps in dots per inch (DPI).  For both metafiles and SVG
## the default is the screen resolution; for other formats the default is 150
## DPI@.  To specify screen resolution, use @qcode{"-r0"}.
##
## Example: high resolution raster output.
##
## @example
## @group
## clf ();
## surf (peaks (), "facelighting", "gouraud");
## light ();
## print ("-r600", "lit_peaks.png");
## @end group
## @end example
##
## @item -S@var{xsize},@var{ysize}
##   Plot size in pixels for raster formats including PNG, JPEG, PNG, and
## @emph{unusually} SVG@.  For all vector formats, including PDF, PS, and EPS,
## the plot size is specified in points.  This option is equivalent to changing
## the width and height of the output by setting the figure property
## @code{paperposition(3:4)}.  When using the command form of the print
## function you must quote the @var{xsize},@var{ysize} option to prevent the
## Octave interpreter from recognizing the embedded comma (',').  For example,
## by writing @w{"-S640,480"}.
##
## @item  -tight
## @itemx -loose
##   Force a tight or loose bounding box for EPS files.  The default is tight.
##
## @item -@var{preview}
##   Add a preview to EPS files.  Supported formats are:
##
##   @table @code
##   @item -interchange
##     Provide an interchange preview.
##
##   @item -metafile
##     Provide a metafile preview.
##
##   @item -pict
##     Provide a pict preview.
##
##   @item -tiff
##     Provide a TIFF preview.
##   @end table
##
## @item -append
##   Append PostScript or PDF output to an existing file of the same type.
##
## @item  -F@var{fontname}
## @itemx -F@var{fontname}:@var{size}
## @itemx -F:@var{size}
##   Use @var{fontname} and/or @var{fontsize} for all text.
## @var{fontname} is ignored for some devices: fig, etc.
##
## @item -d@var{device}
##   The available output format is specified by the option @var{device}, and
## is one of the following (devices marked with a @qcode{'*'} are only
## available with the Gnuplot toolkit):
##
## Vector Formats
##
##   @table @code
##   @item svg
##     Scalable Vector Graphics.
##
##   @item  pdf
##   @itemx pdfcrop
##     Portable Document Format.  The @code{pdf} device formats the figure for
## printing on paper.  The size of the surrounding page and the position of the
## figure inside the page are defined by the
## @ref{XREFfigurepaperorientation,, paper* figure properties}.
##
## Use @code{pdfcrop} if you don't want the surrounding page.
##
## Caution: with @option{-nosvgconvert} option, PDF inherits the same
## limitations as PostScript (limited set of fonts and lack of transparency).
##
##   @item  eps(2)
##   @itemx epsc(2)
##     Encapsulated PostScript (level 1 and 2, mono and color).
##
## The OpenGL-based graphics toolkits always generate PostScript level 3.0.
## They have limited support for text unless using the @option{-svgconvert}
## option (the default).
## Limitations include using only ASCII characters (e.g., no Greek letters)
## and support for just three base PostScript fonts: Helvetica (the default),
## Times, or Courier.  Any other font will be replaced by Helvetica.
##
##   @item  ps(2)
##   @itemx psc(2)
##     Same as @code{eps} except that the figure is formatted for printing on
## paper.  The size of the surrounding page and position of the figure inside
## the page are defined by the
## @ref{XREFfigurepaperorientation,, paper* figure properties}.
##
##   @item  pslatex
##   @itemx epslatex
##   @itemx pdflatex
##   @itemx pslatexstandalone
##   @itemx epslatexstandalone
##   @itemx pdflatexstandalone
##     Generate a @LaTeX{} file @file{@var{filename}.tex} for the text portions
## of a plot and a file @file{@var{filename}.(ps|eps|pdf)} for the remaining
## graphics.  The graphics file suffix .ps|eps|pdf is determined by the
## specified device type.  The @LaTeX{} file produced by the @samp{standalone}
## option can be processed directly by @LaTeX{}.  The file generated without
## the @samp{standalone} option is intended to be included from another
## @LaTeX{} document.  In either case, the @LaTeX{} file contains an
## @code{\includegraphics} command so that the generated graphics file is
## automatically included when the @LaTeX{} file is processed.  The text that
## is written to the @LaTeX{} file contains the strings @strong{exactly} as
## they were specified in the plot.  If any special characters of the @TeX{}
## mode interpreter were used, the file must be edited before @LaTeX{}
## processing.  Specifically, the special characters must be enclosed with
## dollar signs @w{(@code{$ @dots{} $})}, and other characters that are
## recognized by @LaTeX{} may also need editing (e.g., braces).  The
## @samp{pdflatex} device, and any of the @samp{standalone} formats, are not
## available with the Gnuplot toolkit.
##
##   @item  epscairo*
##   @itemx pdfcairo*
##   @itemx epscairolatex*
##   @itemx pdfcairolatex*
##   @itemx epscairolatexstandalone*
##   @itemx pdfcairolatexstandalone*
##     Generate output with Cairo renderer.  The devices @code{epscairo} and
## @code{pdfcairo} are synonymous with the @code{epsc} device.  The @LaTeX{}
## variants generate a @LaTeX{} file, @file{@var{filename}.tex}, for the text
## portions of a plot, and an image file, @file{@var{filename}.(eps|pdf)}, for
## the graph portion of the plot.  The @samp{standalone} variants behave as
## described for @samp{epslatexstandalone} above.
##
##   @item canvas*
##     Javascript-based drawing on an HTML5 canvas viewable in a web browser.
##
##   @item  emf
##   @itemx meta
##     Microsoft Enhanced Metafile
##
##   @item fig
##     XFig.  For the Gnuplot graphics toolkit, the additional options
## @option{-textspecial} or @option{-textnormal} (default) can be used to
## control whether the special flag should be set for the text in the figure.
##
##   @item  latex*
##   @itemx eepic*
##     @LaTeX{} picture environment and extended picture environment.
##
##   @item  tikz
##   @itemx tikzstandalone*
##     Generate a @LaTeX{} file using PGF/TikZ format.  The OpenGL-based
## toolkits create a PGF file while Gnuplot creates a TikZ file.  The
## @samp{tikzstandalone} device produces a @LaTeX{} document which includes the
## TikZ file.
##
##   @end table
##
## Raster Formats
##
##   @table @code
##   @item png
##     Portable Network Graphics
##
##   @item  jpg
##   @itemx jpeg
##     JPEG image
##
##   @item  tif
##   @itemx tiff
##   @itemx tiffn
##     TIFF image with LZW compression (@nospell{tif}, tiff) or uncompressed
## (@nospell{tiffn}).
##
##   @item gif
##     GIF image
##
##   @item pbm
##     PBMplus
##
##   @item dumb*
##     ASCII art
##
##   @end table
##
##   If the device is omitted, it is inferred from the file extension,
## or if there is no filename then it is sent to the printer as PostScript.
##
## @item -d@var{ghostscript_device}
##   Additional devices are supported by Ghostscript.
## Some examples are:
##
##   @table @code
##   @item ljet2p
##     HP LaserJet @nospell{IIP}
##
##   @item pcx24b
##     24-bit color PCX file format
##
##   @item ppm
##     Portable Pixel Map file format
##   @end table
##
##   For a complete list of available formats and devices type
## @kbd{system ("gs -h")}.
##
##   When Ghostscript output is sent to a printer the size is determined by
## the figure's @qcode{"papersize"} property.  When the output is sent to a
## file the size is determined by the plot box defined by the figure's
## @qcode{"paperposition"} property.
##
## @item -G@var{ghostscript_command}
##   Specify the command for calling Ghostscript.  For Unix the default is
## @qcode{"gs"} and for Windows it is @qcode{"gswin32c"}.
##
## @item  -TextAlphaBits=@var{n}
## @itemx -GraphicsAlphaBits=@var{n}
##   Octave is able to produce output for various printers, bitmaps, and
## vector formats by using Ghostscript.  For bitmap and printer output
## anti-aliasing is applied using Ghostscript's TextAlphaBits and
## GraphicsAlphaBits options.  The default number of bits are 4 and 1
## respectively.  Allowed values for @var{N} are 1, 2, or 4.
## @end table
##
## @seealso{saveas, getframe, savefig, hgsave, orient, figure}
## @end deftypefn

function RGB = print (varargin)

  opts = __print_parse_opts__ (varargin{:});

  ## Check the requested file is writable
  if (! opts.rgb_output)
    folder = fileparts (opts.name);
    if (! isempty (folder) && ! isfolder (folder))
      error ("print: directory %s does not exist", folder);
    endif

    do_unlink = (exist (opts.name, "file") != 2);
    fid = fopen (opts.name, "a");
    if (fid == -1)
      error ("print: cannot open file %s for writing", opts.name);
    endif
    fclose (fid);
    if (do_unlink)
      unlink (opts.name);
    endif
  endif

  opts.pstoedit_cmd = @pstoedit;
  opts.fig2dev_cmd = @fig2dev;
  opts.latex_standalone = @latex_standalone;
  opts.lpr_cmd = @lpr;
  opts.epstool_cmd = @epstool;
  opts.svgconvert_cmd = @svgconvert;

  if (isempty (opts.figure) || ! isfigure (opts.figure))
    error ("print: no figure to print");
  endif

  if (isempty (findall (opts.figure, "-depth", 1, "type", "axes")))
    error ("print: no axes object in figure to print");
  endif

  orig_figure = get (0, "currentfigure");
  set (0, "currentfigure", opts.figure);

  if (opts.append_to_file)
    [~, ~, ext] = fileparts (opts.ghostscript.output);
    opts.ghostscript.prepend = [tempname() ext];
    copyfile (opts.ghostscript.output, opts.ghostscript.prepend);
  endif

  unwind_protect

    ## Modify properties as specified by options
    tk = get (opts.figure, "__graphics_toolkit__");
    props = [];
    nfig = 0;

    drawnow ();

    ## Set the __printing__ property first
    props(1).h = opts.figure;
    props(1).name = "__printing__";
    props(1).value = {"off"};
    set (opts.figure, "__printing__", "on");
    nfig += 1;

    ## print() requires children of axes to have units = "normalized" or "data"
    ## FIXME: Bug #59015.  The only graphics object type to which this
    ## requirement applies seems to be 'text' objects.  It is simpler, and
    ## clearer, to just select those objects.  The old code is left commented
    ## out until sufficient testing has been done.
    ## Change made: 2020/09/02.
    ##hobj = findall (opts.figure, "-not", "type", "figure", ...
    ##                "-not", "type", "axes", "-not", "type", "hggroup", ...
    ##                "-property", "units", ...
    ##                "-not", "units", "normalized", "-not", "units", "data");
    ##hobj(strncmp (get (hobj, "type"), "ui", 2)) = [];

    hobj = findall (opts.figure, "type", "text",
                    "-not", "units", "normalized", "-not", "units", "data");
    for n = 1:numel (hobj)
      props(end+1).h = hobj(n);
      props(end).name = "units";
      props(end).value = {get(hobj(n), "units")};
      set (hobj(n), "units", "data");
      nfig += 1;
    endfor

    if (strcmp (opts.renderer, "opengl"))
      ## Scale the figure to reach the required resolution
      scale = opts.ghostscript.resolution / 72;
      if (scale != 1)
        props(end+1).h = opts.figure;
        props(end).name = "__device_pixel_ratio__";
        props(end).value{1} = get (opts.figure, "__device_pixel_ratio__");
        set (opts.figure, "__device_pixel_ratio__", scale);
        nfig += 1;
      endif
    elseif (strcmp (tk, "qt"))
      ## Don't account for the actual pixel density
      props(end+1).h = opts.figure;
      props(end).name = "__device_pixel_ratio__";
      props(end).value = {get(opts.figure, "__device_pixel_ratio__")};
      set (opts.figure, "__device_pixel_ratio__", 1);
      nfig += 1;
    endif

    ## print() requires axes units = "normalized"
    hax = findall (opts.figure, "-depth", 1, "type", "axes", ...
      "-not", "units", "normalized");
    for n = 1:numel (hax)
      props(end+1).h = hax(n);
      props(end).name = "units";
      props(end).value = {get(hax(n), "units")};
      set (hax(n), "units", "normalized");
      nfig += 1;
    endfor

    ## With the -painters (gl2ps) renderer, line transparency is only
    ## handled for svg and pdf outputs using svgconvert.
    ## Otherwise, switch grid lines color to light gray so that the image
    ## output approximately matches on-screen experience.
    hax = findall (opts.figure, "type", "axes");
    if (! strcmp (tk, "gnuplot") && ! strcmp (opts.renderer, "opengl")
        && ! (opts.svgconvert && strcmp (opts.devopt, "pdfwrite"))
        && ! strcmp (opts.devopt, "svg"))
      for n = 1:numel (hax)
        if (strcmp (get (hax(n), "gridcolormode"), "auto"))
          props(end+1).h = hax(n);
          props(end).name = "gridcolormode";
          props(end).value = {"auto"};
          props(end+1).h = hax(n);
          props(end).name = "gridcolor";
          props(end).value = {get(hax(n), "gridcolor")};
          set (hax(n), "gridcolor", [0.85 0.85 0.85]);
          nfig += 2;
        endif
        if (strcmp (get (hax(n), "gridalphamode"), "auto"))
          props(end+1).h = hax(n);
          props(end).name = "gridalphamode";
          props(end).value = {"auto"};
          props(end+1).h = hax(n);
          props(end).name = "gridalpha";
          props(end).value = {get(hax(n), "gridalpha")};
          set (hax(n), "gridalpha", 1);
          nfig += 2;
        endif

        if (strcmp (get (hax(n), "minorgridcolormode"), "auto"))
          props(end+1).h = hax(n);
          props(end).name = "minorgridcolormode";
          props(end).value = {"auto"};
          props(end+1).h = hax(n);
          props(end).name = "minorgridcolor";
          props(end).value = {get(hax(n), "minorgridcolor")};
          set (hax(n), "minorgridcolor", [0.75 0.75 0.75]);
          nfig += 2;
        endif
        if (strcmp (get (hax(n), "minorgridalphamode"), "auto"))
          props(end+1).h = hax(n);
          props(end).name = "minorgridalphamode";
          props(end).value = {"auto"};
          props(end+1).h = hax(n);
          props(end).name = "minorgridalpha";
          props(end).value = {get(hax(n), "minorgridalpha")};
          set (hax(n), "minorgridalpha", 1);
          nfig += 2;
        endif
      endfor
    endif

    ## print() requires figure units to be "pixels"
    props(end+1).h = opts.figure;
    props(end).name = "units";
    props(end).value = {get(opts.figure, "units")};
    set (opts.figure, "units", "pixels");
    nfig += 1;

    ## graphics toolkit translates figure position to eps bbox (points)
    fpos = get (opts.figure, "position");
    props(end+1).h = opts.figure;
    props(end).name = "position";
    props(end).value = {fpos};
    fpos(3:4) = opts.canvas_size;
    set (opts.figure, "position", fpos);
    nfig += 1;

    ## Implement InvertHardCopy option
    do_hardcopy = strcmp (get (opts.figure, "inverthardcopy"), "on");

    if (do_hardcopy)
      ## Set figure background to white.
      props(end+1).h = opts.figure;
      props(end).name = "color";
      props(end).value{1} = get (opts.figure, "color");
      set (opts.figure, "color", "white");
      nfig += 1;
    endif

    if (do_hardcopy)
      ## Set background to white for all top-level axes objects
      hax = findall (opts.figure, "-depth", 1, "type", "axes",
                                  "-not", "tag", "legend",
                                  "-not", "color", "none");
      if (! isempty (hax))
        for n = 1:numel (hax)
          props(end+1).h = hax(n);
          props(end).name = "color";
          props(end).value{1} = get(hax(n), "color");
          set (hax(n), "color", "white");
          nfig += 1;
        endfor
      endif
    endif

    if (opts.force_solid != 0)
      h = findall (opts.figure, "-property", "linestyle");
      m = numel (props);
      for n = 1:numel (h)
        props(m+n).h = h(n);
        props(m+n).name = "linestyle";
        props(m+n).value = {get(h(n), "linestyle")};
      endfor
      if (opts.force_solid > 0)
        linestyle = "-";
      else
        linestyle = "--";
      endif
      set (h, "linestyle", linestyle);
    endif

    if (opts.use_color < 0)
      color_props = {"color", "facecolor", "edgecolor", "colormap"};
      for c = 1:numel (color_props)
        h = findall (opts.figure, "-property", color_props{c});
        hnone = findall (opts.figure, color_props{c}, "none");
        h = setdiff (h, hnone);
        m = numel (props);
        for n = 1:numel (h)
          if (ishghandle (h(n)))
            ## Need to verify objects exist since callbacks may delete objects
            ## as the colors for others are modified.
            rgb = get (h(n), color_props{c});
            props(end+1).h = h(n);
            props(end).name = color_props{c};
            props(end).value = {get(h(n), color_props{c})};
            if (isnumeric (rgb))
              ## convert RGB color to RGB gray scale
              xfer = repmat ([0.30, 0.59, 0.11], rows (rgb), 1);
              ggg = repmat (sum (xfer .* rgb, 2), 1, 3);
              set (h(n), color_props{c}, ggg);
            endif
          endif
        endfor
      endfor
    endif

    do_font = ! isempty (opts.font);
    do_scalefontsize =  ! isempty (opts.scalefontsize) && opts.scalefontsize != 1;
    do_fontsize = ! isempty (opts.fontsize) || do_scalefontsize;
    if (do_font || do_fontsize)
      h = findall (opts.figure, "-property", "fontname");
      m = numel (props);
      for n = 1:numel (h)
        if (ishghandle (h(n)))
          if (do_font)
            props(end+1).h = h(n);
            props(end).name = "fontname";
            props(end).value = {get(h(n), "fontname")};
          endif
          if (do_fontsize)
            props(end+1).h = h(n);
            props(end).name = "fontsize";
            props(end).value = {get(h(n), "fontsize")};
          endif
        endif
      endfor
      if (do_font)
        set (h(ishghandle (h)), "fontname", opts.font);
      endif
      if (do_fontsize)
        if (! isempty (opts.fontsize))
          ## Changing all fontsizes to a fixed value
          if (ischar (opts.fontsize))
            fontsize = str2double (opts.fontsize);
          else
            fontsize = opts.fontsize;
          endif
          if (do_scalefontsize)
            ## This is done to work around the bbox being whole numbers.
            fontsize *= opts.scalefontsize;
          endif

          ## FIXME: legend child objects need to be acted on first.
          ##        or legend fontsize callback will destroy them.
          hlist = h(ishghandle (h));
          haxes = strcmp (get (hlist, "type"), "axes");
          set (hlist(! haxes), "fontsize", fontsize);
          set (hlist(haxes), "fontsize", fontsize);

        else
          ## Scaling fonts
          ## FIXME: legend child objects need to be acted on first.
          ##        or legend fontsize callback will destroy them.
          hlist = h(ishghandle (h));
          haxes = strcmp (get (hlist, "type"), "axes");
          for h = hlist(! haxes).'
            fontsz = get (h, "fontsize");
            set (h, "fontsize", fontsz * opts.scalefontsize);
          endfor
          for h = hlist(haxes).'
            fontsz = get (h, "fontsize");
            set (h, "fontsize", fontsz * opts.scalefontsize);
          endfor

        endif
      endif
    endif

    ## When exporting latex files use "latex" for the ticklabelinterpreter.
    ## It will format tick labels in log axes correctly
    if (strfind (opts.devopt, "latex"))
      ## Disable warnings about Latex being unsupported since Octave will be
      ## passing Latex code directly to interpreter with no rendering.
      warning ("off", "Octave:text_interpreter", "local");
      h = findall (opts.figure, "type", "axes");
      for n = 1:numel (h)
        if (ishghandle (h(n)))
          props(end+1).h = h(n);
          props(end).name = "ticklabelinterpreter";
          props(end).value = {get(h(n), "ticklabelinterpreter")};
          set (h(n), "ticklabelinterpreter", "latex");
        endif
      endfor
    endif

    ## call the graphics toolkit print script
    switch (tk)
      case "gnuplot"
        opts = __gnuplot_print__ (opts);
      otherwise
        if (strcmp (opts.renderer, "opengl"))
          if (opts.rgb_output)
            RGB = __get_frame__ (opts.figure);
          else
            compression = "none";

            if (strcmp (opts.devopt, "tiff"))
              compression = "lzw";
            elseif (strcmp (opts.devopt, "tiffn"))
              opts.devopt = "tiff";
            endif

            imwrite (__get_frame__ (opts.figure), opts.name, ...
                     opts.devopt, "Compression", compression);
          endif
        else
          opts = __opengl_print__ (opts);
        endif
    endswitch

  unwind_protect_cleanup
    ## restore modified properties
    if (isstruct (props))
      ## Restore figure position and units first
      for n = nfig:-1:1
        if (ishghandle (props(n).h))
          set (props(n).h, props(n).name, props(n).value{1});
        endif
      endfor
      for n = numel (props):-1:(nfig + 1)
        if (ishghandle (props(n).h))
          set (props(n).h, props(n).name, props(n).value{1});
        endif
      endfor
    endif

    ## Avoid a redraw since the figure should not have changed
    ## FIXME: Bug #57552, marker sizes, requires that redraw be done.
    ## set (gcf, "__modified__", "off");

    ## Unlink temporary files
    for n = 1:numel (opts.unlink)
      [status, output] = unlink (opts.unlink{n});
      if (status != 0)
        warning ("Octave:print:unlinkerror", ...
                 "print: %s, '%s'", output, opts.unlink{n});
      endif
    endfor
  end_unwind_protect

  if (isfigure (orig_figure))
    set (0, "currentfigure", orig_figure);
  endif

endfunction


%!error <a graphics handle>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   x = 0:0.1:1;
%!   y1 = x;
%!   y2 = 2*x;
%!   ax = plotyy (x, y1, x, y2);
%!   saveas (ax, [tempname(), ".png"]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

function cmd = epstool (opts, filein, fileout)

  ## As epstool does not work with pipes, a subshell is used to
  ## permit piping.  Since this solution does not work with the DOS
  ## command shell, the -tight and -preview options are disabled if
  ## output must be piped.

  ## DOS Shell:
  ##   gs.exe [...] -sOutputFile=<filein> - & epstool -bbox -preview-tiff <filein> <fileout> & del <filein>
  ## Unix Shell:
  ##   cat > <filein> ; epstool -bbox -preview-tiff <filein> <fileout> ; rm <filein>

  dos_shell = (ispc () && ! isunix ());

  ## HACK: Keep track of whether ghostscript supports epswrite or eps2write.
  persistent epsdevice;
  if (dos_shell && isempty (epsdevice))
    if (isempty (opts.ghostscript.binary))
      error ("Octave:print:nogs",
             "print: 'gs' (Ghostscript) is required for specified output format, but binary is not available in PATH");
    endif

    [status, devlist] = system (sprintf ("%s -h", opts.ghostscript.binary));
    if (isempty (strfind (devlist, "eps2write")))
      epsdevice = "epswrite";
    else
      epsdevice = "eps2write";
    endif
  endif

  cleanup = "";
  if (nargin < 3)
    fileout = opts.name;
  elseif (isempty (fileout))
    fileout = "-";
  endif

  if (nargin < 2 || strcmp (filein, "-") || isempty (filein))
    pipein = true;
    filein = [tempname() ".eps"];
    if (dos_shell)
      cleanup = sprintf ('& del "%s" ', strrep (filein, '/', '\'));
    else
      cleanup = sprintf ('; rm "%s" ', filein);
    endif
  else
    pipein = false;
    filein = ["'" strtrim(filein) "'"];
  endif
  if (strcmp (fileout, "-"))
    pipeout = true;
    fileout = [tempname() ".eps"];
    if (dos_shell)
      cleanup = [cleanup, sprintf('& del "%s" ', strrep (fileout, '/', '\'))];
    else
      cleanup = [cleanup, sprintf('; rm "%s" ', fileout)];
    endif
  else
    pipeout = false;
    fileout = ["'" strtrim(fileout) "'"];
  endif

  if (! isempty (opts.preview) && opts.tight)
    warning ("Octave:print:previewandtight",
             "print: eps preview may not be combined with -tight");
  endif
  if (! isempty (opts.preview) || opts.tight)

    if (isempty (opts.epstool_binary))
      error ("Octave:print:noepstool", "print: 'epstool' is required for specified output format, but binary is not available in PATH");
    endif

    if (opts.tight)
      cmd = "--copy --bbox";
    elseif (! isempty (opts.preview))
      switch (opts.preview)
        case "tiff"
          cmd = sprintf ("--add-%s-preview --device tiffg3", opts.preview);
        case {"tiff6u", "tiff6p", "metafile"}
          cmd = sprintf ("--add-%s-preview --device bmpgray", opts.preview);
        case {"tiff4", "interchange"}
          cmd = sprintf ("--add-%s-preview", opts.preview);
        case "pict"
          cmd = sprintf ("--add-%s-preview --mac-single", opts.preview);
        otherwise
          error ("Octave:print:invalidpreview",
                 "print: epstool cannot include preview for format '%s'",
                 opts.preview);
      endswitch
      if (! isempty (opts.ghostscript.resolution))
        cmd = sprintf ("%s --dpi %d", cmd, fix (opts.ghostscript.resolution));
      endif
    else
      cmd = "";
    endif
    if (! isempty (cmd))
      if (dos_shell)
        ## ghostscript expects double, not single, quotes
        fileout(fileout == "'") = '"';
        ## epstool implicitly uses ghostscript and it needs the command name
        cmd = sprintf ("%s --gs %s --quiet %s %s %s ", opts.epstool_binary,
                       opts.ghostscript.binary, cmd, filein, fileout);
      else
        cmd = sprintf ("%s --quiet %s %s %s ", opts.epstool_binary,
                       cmd, filein, fileout);
      endif
    endif
    if (pipein)
      if (dos_shell)
        filein(filein=="'") = '"';
        gs_cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                                  "device", epsdevice,
                                  "source", "-",
                                  "output", filein);
        cmd = sprintf ("%s %s & %s", gs_cmd, filein, cmd);
      else
        cmd = sprintf ("cat > %s ; %s", filein, cmd);
      endif
    endif
    if (pipeout)
      if (dos_shell)
        cmd = sprintf ("%s & type %s", cmd, fileout);
      else
        cmd = sprintf ("%s ; cat %s", cmd, fileout);
      endif
    endif
    if (! isempty (cleanup))
      if (pipeout && dos_shell)
        error ("Octave:print:epstoolpipe",
               "print: cannot pipe output of 'epstool' for DOS shell");
      elseif (pipeout)
        cmd = sprintf ("( %s %s )", cmd, cleanup);
      else
        cmd = sprintf ("%s %s", cmd, cleanup);
      endif
    endif
  else
    if (pipein && pipeout)
      if (dos_shell)
        cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                               "device", epsdevice,
                               "source", "-",
                               "output", "-");
      else
        cmd = " cat ";
      endif
    elseif (pipein && ! pipeout)
      if (dos_shell)
        ## ghostscript expects double, not single, quotes
        fileout(fileout=="'") = '"';
        cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                               "device", epsdevice,
                               "source", "-",
                               "output", fileout);
      else
        cmd = sprintf (" cat > %s ", fileout);
      endif
    elseif (! pipein && pipeout)
      if (dos_shell)
        cmd = sprintf (" type %s ", filein);
      else
        cmd = sprintf (" cat %s ", filein);
      endif
    else
      if (dos_shell)
        cmd = sprintf (" copy %s %s ", filein, fileout);
      else
        cmd = sprintf (" cp %s %s ", filein, fileout);
      endif
    endif
  endif
  if (opts.debug)
    fprintf ("epstool command: '%s'\n", cmd);
  endif

endfunction

function cmd = fig2dev (opts, devopt)

  if (nargin < 2)
    devopt = opts.devopt;
  endif

  if (isempty (opts.fig2dev_binary))
    error ("Octave:print:nofig2dev", "print: 'fig2dev' is required for specified output format, but binary is not available in PATH");
  endif

  dos_shell = (ispc () && ! isunix ());
  if (dos_shell)
    ## FIXME: Is this the right thing to do for DOS?
    cmd = sprintf ("%s -L %s 2> NUL", opts.fig2dev_binary, devopt);
  else
    cmd = sprintf ("%s -L %s 2> /dev/null", opts.fig2dev_binary, devopt);
  endif

  if (opts.debug)
    fprintf ("fig2dev command: '%s'\n", cmd);
  endif

endfunction

function latex_standalone (opts)

  n = find (opts.name == ".", 1, "last");
  if (! isempty (n))
    opts.name = opts.name(1:n-1);
  endif
  latexfile = [opts.name ".tex"];

  switch (opts.devopt)
    case {"pdflatexstandalone"}
      packages = "\\usepackage{graphicx,color}";
    case {"pslatexstandalone"}
      packages = "\\usepackage{epsfig,color}";
    otherwise
      packages = "\\usepackage{epsfig,color}";
  endswitch

  packages = {packages "\\usepackage[utf8]{inputenc}"};

  papersize = sprintf ("\\usepackage[papersize={%.2fbp,%.2fbp},text={%.2fbp,%.2fbp}]{geometry}",
                       fix (opts.canvas_size), fix (opts.canvas_size));

  prepend = {"\\documentclass{minimal}", packages{:}, papersize, ...
             "\\begin{document}", "\\centering"};
  postpend = {"\\end{document}"};

  fid = fopen (latexfile, "r");
  if (fid < 0)
    error ("Octave:print:erroropeningfile",
           "print: error opening file '%s'", latexfile);
  endif
  latex = fscanf (fid, "%c", Inf);
  status = fclose (fid);
  if (status != 0)
    error ("Octave:print:errorclosingfile",
           "print: error closing file '%s'", latexfile);
  endif

  fid = fopen (latexfile, "w");
  if (fid >= 0)
    fprintf (fid, "%s\n", prepend{:});
    fprintf (fid, "%s", latex);
    fprintf (fid, "%s\n", postpend{:});
    status = fclose (fid);
    if (status != 0)
      error ("Octave:print:errorclosingfile",
             "print: error closing file '%s'", latexfile);
    endif
  else
    error ("Octave:print:erroropeningfile",
           "print: error opening file '%s'", latexfile);
  endif

endfunction

function cmd = lpr (opts)

  if (nargin < 2)
    devopt = opts.devopt;
  endif

  if (! isempty (opts.lpr_binary))
    cmd = opts.lpr_binary;
    if (! isempty (opts.lpr_options))
      cmd = sprintf ("%s %s", cmd, opts.lpr_options);
    endif
    if (! isempty (opts.printer))
      cmd = sprintf ("%s %s", cmd, opts.printer);
    endif
  elseif (isempty (opts.lpr_binary))
    error ("Octave:print:nolpr", "print: 'lpr' not found in PATH");
  endif
  if (opts.debug)
    fprintf ("lpr command: '%s'\n", cmd);
  endif

endfunction

function cmd = pstoedit (opts, devopt, do_svg = true)

  if (nargin < 2)
    devopt = opts.devopt;
  endif

  if (isempty (opts.pstoedit_binary))
    error ("Octave:print:nopstoedit", ...
           "print: 'pstoedit' is required for specified output format, but binary is not available in PATH");
  endif

  dos_shell = (ispc () && ! isunix ());

  if (! do_svg)
    if (dos_shell)
      cmd = sprintf ("%s -f %s 2> NUL", opts.pstoedit_binary, devopt);
    else
      cmd = sprintf ("%s -f %s 2> /dev/null", opts.pstoedit_binary, devopt);
    endif
  else
    cmd = svgconvert (opts, devopt);
    if (dos_shell)
      cmd = sprintf ('%s & %s -ssp -f %s "%%s" 2> NUL', cmd, ...
                     undo_string_escapes (opts.pstoedit_binary), ...
                     undo_string_escapes (devopt));
    else
      cmd = sprintf ('%s ; %s -ssp -f %s "%%s" 2> /dev/null', cmd,  ...
                     opts.pstoedit_binary, devopt);
    endif
  endif

  if (opts.debug)
    fprintf ("pstoedit command: '%s'\n", cmd);
  endif

endfunction

function cmd = svgconvert (opts, devopt)

  cmd = "";

  if (nargin < 2)
    devopt = opts.devopt;
  endif

  if (isempty (opts.svgconvert_binary))
    warning ("Octave:print:nosvgconvert", ...
             ["print: unable to find octave-svgconvert, ", ...
              "falling back to eps conversion"]);
  else
    fontdir = getenv ("OCTAVE_FONTS_DIR");

    if (isempty (fontdir))
      fontdir = __octave_config_info__ ("octfontsdir");
    endif

    cmd = sprintf ('%s - %%s %3.2f "%s" %d "%%s"', ...
                   undo_string_escapes (opts.svgconvert_binary), ...
                   get (0, "screenpixelsperinch"), ...
                   undo_string_escapes (fullfile (fontdir, "FreeSans.otf")),
                   opts.polymerge);

    if (opts.debug)
      fprintf ("svgconvert command: '%s'\n", cmd);
    endif
  endif

endfunction
