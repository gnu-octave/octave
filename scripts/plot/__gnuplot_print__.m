## Copyright (C) 1999, 2005, 2006, 2007, 2008, 2009 Daniel Heiserer
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
## @deftypefn {Function File} {} __gnuplot_ginput__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

## Author: Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>
## Adapted-By: jwe

function __gnuplot_print__ (varargin)

  persistent warn_on_inconsistent_orientation = true

  old_fig = get (0, "currentfigure");
  unwind_protect
    opts = __print_parse_opts__ (varargin{:});
    have_ghostscript = ! isempty (opts.ghostscript_binary);

    doprint = isempty (opts.name);
    if (doprint)
      if (isempty (opts.devopt))
        if (opts.use_color < 0)
          opts.devopt = "ps";
          printname = cstrcat (tmpnam, ".ps");
        else
          opts.devopt = "psc";
          printname = cstrcat (tmpnam, ".psc");
        endif
      else
        printname = cstrcat (tmpnam, ".", devopt);
      endif
      opts.name = printname;
    endif

    dot = rindex (opts.name, ".");
    if (isempty (opts.devopt))
      if (dot == 0)
        error ("print: no format specified");
      else
        dev = tolower (opts.name(dot+1:end));
      endif
    else
      dev = opts.devopt;
    endif

    dev_list = {"aifm", "corel", "fig", "png", "jpeg", ...
                "gif", "pbm", "dxf", "mf", "svg", "hpgl", ...
                "ps", "ps2", "psc", "psc2", "eps", "eps2", ...
                "epsc", "epsc2", "emf", "pdf", "pslatex", ...
                "epslatex", "epslatexstandalone", "pstex", "tikz"};
    suffixes = {"ai", "cdr", "fig", "png", "jpeg", ...
                "gif", "pbm", "dxf", "mf", "svg", "hpgl", ...
                "ps", "ps", "ps", "ps", "eps", "eps", ...
                "eps", "eps", "emf", "pdf", "tex", ...
                "tex", "tex", "tex", "tikz"};
    if (dot == 0)
      opts.name = strcat (opts.name, ".", suffixes {strcmpi (dev_list, dev)});
      dot = rindex (name, ".");
    endif

    if (opts.append_to_file)
      if (any (strcmpi (dev, {"ps", "ps2", "psc", "psc2", "pdf"})))
        if (have_ghostscript)
          file_exists = ((numel (dir (opts.name)) == 1) && (! isdir (opts.name)));
          if (! file_exists)
            opts.append_to_file = 0;
          end
        end
      else
        warning ("print.m: appended output is not supported for device '%s'", dev)
        opts.append_to_file = 0;
      endif
    endif

    if (strcmp (dev, "tex"))
      dev = "epslatex";
    elseif (strcmp (dev, "ill"))
      dev = "aifm";
    elseif (strcmp (dev, "cdr"))
      dev = "corel";
    elseif (strcmp (dev, "meta"))
      dev = "emf";
    elseif (strcmp (dev, "jpg"))
      dev = "jpeg";
    endif

    if (strcmp (dev, "epslatex"))
      ## gnuplot 4.0 wants ".eps" in the output name    
      if (! __gnuplot_has_feature__ ("epslatex_implies_eps_filesuffix"))
        opts.name = cstrcat (name(1:dot), "eps");
      endif
    endif

    ## Check if the specified device is one that is supported by gnuplot.
    ## If not, assume it is a device/format supported by Ghostscript.
    if (! any (strcmp (dev, dev_list)) && have_ghostscript)
      ghostscript_output = opts.name;
      ghostscript_device = dev;
      if (doprint)
        ## If printing, use color postscript.
        dev = "psc";
        opts.name = cstrcat (tmpnam, ".ps");
      else
        ## If saving to a file, use color encapsulated postscript.
        dev = "epsc";
        opts.name = cstrcat (tmpnam, ".eps");
      endif
    elseif (doprint && all (! strcmpi (suffixes {strcmpi (dev_list, dev)}, 
                                       {"ps", "eps", "hpgl"})))
      ## When not using Ghostscript, verify the format is compatible with
      ## hard copy output.
      error ("print: missing file name, or invalid print format.");
    else
      ghostscript_output = "";
    endif

    termn = dev;

    ## SVG isn't actually a bitmap, but gnuplot treats its size option as it
    ## does the bitmap terminals.
    bitmap_devices = {"emf", "gif", "jpeg", "pbm", "png", "svg"};

    if (any (strcmp (dev, {"ps", "ps2", "psc", "psc2", "epsc", "epsc2", ...
                           "eps", "eps2", "pstex", "pslatex", "epslatex", ...
                           "epslatexstandalone"})))

      ## Various postscript options
      if (any (strcmp (dev, {"pstex", "pslatex", "epslatex"})))
        options = "";
      elseif (strcmp (dev, "epslatexstandalone"))
        if (__gnuplot_has_feature__ ("epslatexstandalone_terminal"))
          termn = "epslatex";
          options = "standalone ";
        else
          error ("print: epslatexstandalone needs gnuplot 4.2 or higher");
        endif
      else
        if (dev(1) == "e")
          options = "eps ";
        else
          options = "";
        endif
        termn = "postscript";
      endif

      if (any (dev == "c") || opts.use_color > 0
          || (! isempty (strfind (dev, "tex")) && opts.use_color == 0))
        opts.use_color = 1;
      else
        opts.use_color = -1;
      endif
      
      if (opts.use_color > 0)
        if (opts.force_solid < 0)
          options = cstrcat (options, "color dashed ");
        else
          options = cstrcat (options, "color solid ");
        endif
      else
        if (opts.force_solid > 0)
          options = cstrcat (options, "mono solid ");
        else
          options = cstrcat (options, "mono dashed ");
        endif
      endif

      if (! isempty (opts.font))
        options = cstrcat (options, "\"", opts.font, "\" ");
      endif
      if (! isempty (opts.fontsize))
        options = cstrcat (options, " ", opts.fontsize);
      endif

    elseif (strcmp (dev, "tikz"))
      if (! isempty (font) && ! isempty (opts.fontsize))
        options = cstrcat (options, "\"", opts.font, ",", opts.fontsize, "\" ");
      elseif (! isempty (font))
        options = cstrcat (options, "\"", opts.font, "\" ");
      else
        options = "";
      endif

    elseif (strcmp (dev, "aifm") || strcmp (dev, "corel"))
      ## Adobe Illustrator, CorelDraw
      if (opts.use_color >= 0)
        options = " color";
      else
        options = " mono";
      endif
      if (! isempty (opts.font))
        options = cstrcat (options, " \"", opts.font, "\"");
      endif
      if (! isempty (opts.fontsize))
        options = cstrcat (options, " ", opts.fontsize);
      endif

    elseif (strcmp (dev, "fig"))
      ## XFig
      options = opts.orientation;
      if (opts.use_color >= 0)
        options = " color";
      else
        options = " mono";
      endif
      options = cstrcat (options, " ", opts.special_flag);
      if (! isempty (opts.fontsize))
        options = cstrcat (options, " fontsize ", opts.fontsize);
      endif

    elseif (strcmp (dev, "emf"))
      ## Enhanced Metafile format
      options = " ";
      if (opts.use_color >= 0)
        options = " color";
      else
        options = " mono";
      endif
      if (opts.force_solid >= 0)
        options = cstrcat (options, " solid");
      endif
      if (! isempty (opts.font))
        options = cstrcat (options, " \"", opts.font, "\"");
      endif
      if (! isempty (opts.fontsize))
        options = cstrcat (options, " ", opts.fontsize);
      endif

    elseif (any (strcmp (dev, bitmap_devices)))

      if (isempty (opts.canvas_size) && isempty (opts.resolution) 
          && any (strcmp (dev, {"pbm", "gif", "jpeg", "png"})))
        options = "";
      elseif (strcmp (dev, "svg"))
        ## Referring to size, either "dynamic" or "fixed"
        options = "fixed";
      else
        options = "";
      end
      if (! isempty (opts.canvas_size))
        options = cstrcat (options, " size ", opts.canvas_size);
      endif

    elseif (any (strcmp (dev, {"dxf", "mf", "hpgl"})))
      ## AutoCad DXF, METAFONT, HPGL
      options = "";

    elseif (strcmp (dev, "pdf"))
      ## Portable Document format
      options = " ";
      if (opts.use_color >= 0)
        options = "color";
      else
        options = "mono";
      endif
      if (opts.force_solid >= 0)
        options = cstrcat (options, " solid");
      elseif (opts.force_solid < 0)
        options = cstrcat (options, " dashed");
      endif
      if (! isempty (opts.font))
        options = cstrcat (options, "\"", opts.font, "\" ");
      endif
      if (! isempty (opts.fontsize))
        options = cstrcat (options, " ", opts.fontsize);
      endif

    endif
 
    if (__gnuplot_has_feature__ ("variable_GPVAL_TERMINALS"))
      available_terminals = __gnuplot_get_var__ (gcf, "GPVAL_TERMINALS");
      available_terminals = regexp (available_terminals, "\\b\\w+\\b", "match");
      ## Favor the cairo terminals.
      if (strcmp (termn, "pdf") 
          && any (strcmp (available_terminals, "pdfcairo")))
        termn = "pdfcairo";
        gnuplot_supports_term = true;
      elseif (strcmp (termn, "png")
              && any (strcmp (available_terminals, "pngcairo")))
        termn = "pngcairo";
        gnuplot_supports_term = true;
      else
        gnuplot_supports_term = any (strcmp (available_terminals, termn));
      endif
    elseif (strcmp (termn, "pdf"))
      ## Some Linux variants do not include a "pdf" capable gnuplot.
      ## To be safe, use Ghostscript.
      if (have_ghostscript)
        gnuplot_supports_term = false;
        ghostscript_device = "pdfwrite";
      else
        gnuplot_supports_term = true;
      endif
    else
      gnuplot_supports_term = true;
    endif

    if (! gnuplot_supports_term)
      if (strcmp (termn, "pdf"))
        ## If there the installed gnuplot does not support pdf, use Ghostscript.
        ghostscript_device = "pdfwrite";
        if (strfind (opts.name, ".pdf") == numel (opts.name) - 3)
          ghostscript_output = opts.name;
        else
          ghostscript_output = strcat (opts.name, ".pdf");
        endif
        name = cstrcat (tmpnam, ".ps");
        termn = "postscript";
        ## All "options" for pdf work for postscript as well.
      else
        error ("print: the device, \"%s\", is not available.", dev)
      endif
    endif

    is_eps_file = strncmp (dev, "eps", 3);
    p.units = get (gcf, "units");
    p.paperunits = get (gcf, "paperunits");
    p.papersize = get (gcf, "papersize");
    p.paperposition = get (gcf, "paperposition");
    p.paperpositionmode = get (gcf, "paperpositionmode");
    p.paperorientation = get (gcf, "paperorientation");
    p.color = get (gcf, "color");
    if (p.papersize(1) > p.papersize(2))
      paperorientation = "landscape";
    else
      paperorientation = "portrait";
    endif
    if (! strcmp (paperorientation, get (gcf, "paperorientation"))
        && warn_on_inconsistent_orientation)
       msg = {"print.m - inconsistent papersize and paperorientation properties.\n",
               sprintf("         papersize = %.2f, %.2f\n", p.papersize),
               sprintf("         paperorientation = \"%s\"\n", p.paperorientation),
                       "         the paperorientation property has been ignored"};
      warning ("%s",msg{:})
      warn_on_inconsistent_orientation = false;
    endif

    if (strcmp (termn, "postscript") && ! strncmp (dev, "eps", 3))
      if (isempty (opts.orientation))
        opts.orientation = paperorientation;
      endif
      ## This is done here to accommodate ghostscript conversion.
      options = cstrcat (opts.orientation, " ", options);
    end

    new_terminal = cstrcat (termn, " ", options);

    mono = (opts.use_color < 0);

    terminals_for_prn = {"postscript", "pdf", "pdfcairo"};
    output_for_printer = any (strncmp (termn, terminals_for_prn, numel(termn)));

    if (isempty (opts.resolution))
      if (any (strcmp (dev, {"emf", "svg"})) || output_for_printer)
        opts.resolution = get (0, "screenpixelsperinch");
      else
        opts.resolution = 150;
      endif
    else
      opts.resolution = str2num (opts.resolution);
      if (opts.resolution == 0)
        opts.resolution = get (0, "screenpixelsperinch");
      endif
    endif
    figure_properties = get (gcf);
    if (! isfield (figure_properties, "__pixels_per_inch__"))
      addproperty ("__pixels_per_inch__", gcf, "double", opts.resolution);
    endif
    set (gcf, "__pixels_per_inch__", opts.resolution)

    unwind_protect
      set (gcf, "paperunits", "inches");
      set (gcf, "units", "pixels");
      set (gcf, "color", "none");
      restore_properties = true;
      if ((! output_for_printer || is_eps_file) && ! doprint)
        ## If not PDF or PostScript, and the result is not being sent to a printer,
        ## render an image the size of the paperposition box.
        ## Trigger the listener to convert all paper props to inches.
        if (! isempty (opts.canvas_size))
          size_in_pixels = sscanf (opts.canvas_size ,"%d, %d");
          size_in_pixels = reshape (size_in_pixels, [1, numel(size_in_pixels)]);
          papersize_in_inches = size_in_pixels ./ opts.resolution;
          paperposition_in_inches = [0, 0, papersize_in_inches];
        else
          paperposition_in_inches = get (gcf, "paperposition");
          paperposition_in_inches(1:2) = 0;
          papersize_in_inches = paperposition_in_inches(3:4);
        endif
        set (gcf, "papersize", papersize_in_inches);
        set (gcf, "paperposition", paperposition_in_inches);
        set (gcf, "paperpositionmode", "manual");
      else
        if (strcmp (p.paperpositionmode, "auto"))
          size_in_pixels = get (gcf, "position")(3:4);
          paperposition_in_inches(3:4) = size_in_pixels ./ opts.resolution;
          paperposition_in_inches(1:2) = (p.papersize - paperposition_in_inches(3:4))/2;
        else
          paperposition_in_inches = p.paperposition;
        endif
        if (! isempty (opts.orientation) && ! strcmp (opts.orientation, paperorientation))
          ## When -landscape/portrait changes the orientation, flip both the
          ## papersize and paperposition.
          restore_properties = true;
          set (gcf, "papersize", p.papersize([2, 1]));
          set (gcf, "paperposition", paperposition_in_inches([2, 1, 4, 3]));
        else
          set (gcf, "paperposition", paperposition_in_inches);
        endif
      endif
      if (opts.use_color < 0)
        [objs_with_color, color_of_objs] = convert_color2mono (gcf);
      endif
    if (opts.append_to_file)
         appended_file_name = opts.name;
         if (index(termn, "pdf"))
           opts.name = cstrcat (tmpnam, ".pdf");
           temp_name = cstrcat (tmpnam, ".pdf");
           ghostscript_device = "pdfwrite";
         else
           opts.name = cstrcat (tmpnam, ".ps");
           temp_name = cstrcat (tmpnam, ".ps");
           ghostscript_device = "pswrite";
         endif
    endif
      if (opts.debug)
        drawnow (new_terminal, opts.name, mono, opts.debug_file);
      else
        drawnow (new_terminal, opts.name, mono);
      endif
      if (opts.append_to_file)
        ghostscript_options = "-q -dBATCH -dSAFER -dNOPAUSE";
        command = sprintf ("%s %s -sDEVICE=%s -sOutputFile=%s %s %s -q", ...
                    opts.ghostscript_binary, ghostscript_options, ghostscript_device,  ...
                    temp_name, appended_file_name, opts.name);
        status1 = system (command);
        status2 = system (sprintf ("mv %s %s", temp_name, appended_file_name));
        if (status1 != 0 || status2 != 0)
          error ("print: output failed to append to '%s'.", appended_file_name);
        endif
      endif
    unwind_protect_cleanup
      ## FIXME - it would be nice to delete "__pixels_per_inch__" property here.
      if (restore_properties)
        props = fieldnames (p);
        for n = 1:numel(props)
          set (gcf, props{n}, p.(props{n}))
        endfor
      endif
      if (opts.use_color < 0)
        convert_mono_to_or_from_color (objs_with_color, color_of_objs, false);
      endif
    end_unwind_protect

    if (! isempty (ghostscript_output))
      if (is_eps_file && opts.tight_flag)
        ## If gnuplot's output is an eps-file then crop at the bounding box.
        __fix_eps_bbox__ (name);
      endif
      ghostscript_options = "-q -dBATCH -dSAFER -dNOPAUSE -dTextAlphaBits=4";
      if (is_eps_file)
        ghostscript_options = sprintf ("%s -dEPSCrop", ghostscript_options);
      endif
      if (isempty (strfind (lower (ghostscript_device), "write")))
        ## If output is a bitmap then include the resolution
        ghostscript_options = sprintf ("%s -r%d", ghostscript_options, opts.resolution);
      endif
      ghostscript_options = sprintf ("%s -sDEVICE=%s", ghostscript_options,
                                     ghostscript_device);
      command = sprintf ("\"%s\" %s -sOutputFile=\"%s\" \"%s\" 2>&1", 
                         opts.ghostscript_binary,
                         ghostscript_options, ghostscript_output, opts.name);
      [errcode, output] = system (command);
      unlink (name);
      if (errcode)
        error ("print: Conversion failed, %s -> %s.\nError was:\n%s\n",
               name, ghostscript_output, output);
      endif
    elseif (is_eps_file && opts.tight_flag && ! doprint)
      ## If the saved output file is an eps file, use ghostscript to set a tight bbox.
      ## This may result in a smaller or larger bbox geometry.
      if (have_ghostscript)
        __fix_eps_bbox__ (name);
      endif
    endif

    if (doprint)
      if (isunix ())
        prn_opt = "-l";
      elseif (ispc ())
        prn_opt = "-o l";
      else
        ## FIXME - besides Unix and Windows, what other OS's might be considered.
        prn_opt = "";
      endif
      if (isempty (opts.printer))
        prn_cmd = sprintf ("lpr %s '%s' 2>&1", prn_opt, printname);
      else
        prn_cmd = sprintf ("lpr %s -P %s '%s' 2>&1", prn_opt, opts.printer, printname);
      endif
      [status, output] = system (prn_cmd);
      if (status != 0)
        disp (output)
        warning ("print.m: printing failed.")
      endif
      [status, output] = unlink (printname);
      if (status != 0)
        disp (output)
        warning ("print.m: failed to delete temporay file, '%s'.", printname)
      endif
    endif

  unwind_protect_cleanup
    if (isfigure (old_fig))
      figure (old_fig)
    endif
  end_unwind_protect

endfunction

function [h, c] = convert_color2mono (hfig)
  unwind_protect
    showhiddenhandles = get (0, "showhiddenhandles");
    set (0, "showhiddenhandles", "on");
    h.color = findobj (hfig, "-property", "color");
    h.facecolor = findobj (hfig, "-property", "facecolor");
    h.edgecolor = findobj (hfig, "-property", "edgecolor");
    h.backgroundcolor = findobj (hfig, "-property", "backgroundcolor");
    h.colormap = findobj (hfig, "-property", "colormap");
  unwind_protect_cleanup
    set (0, "showhiddenhandles", showhiddenhandles);
  end_unwind_protect
  f = fieldnames (h);
  for nf = 1:numel(f)
    if (! isempty (h.(f{nf})))
      v = get (h.(f{nf}), f{nf});
      if (! iscell (v))
        v = {v};
      endif
      c.(f{nf}) = v;
    endif
  endfor
  convert_mono_to_or_from_color (h, c, true)
endfunction

function convert_mono_to_or_from_color (h, c, mono)
  f = fieldnames (h);
  for nf = 1:numel(f)
    for nh = 1:numel (h.(f{nf}))
      color = c.(f{nf}){nh};
      ## Ignore color == {"none", "flat", ...}
      if (isfloat (color))
        if (mono)
          ## Same method as used by rgb2gray in the image pkg.
          color = rgb2ntsc (color)(:,1) * ones (1, 3);
        endif
        set (h.(f{nf})(nh), f{nf}, color);
      endif
    endfor
  endfor
endfunction

