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
## @deftypefn  {} {} whitebg ()
## @deftypefnx {} {} whitebg (@var{color})
## @deftypefnx {} {} whitebg ("none")
## @deftypefnx {} {} whitebg (@var{hfig})
## @deftypefnx {} {} whitebg (@var{hfig}, @var{color})
## @deftypefnx {} {} whitebg (@var{hfig}, "none")
## Invert the colors in the current color scheme.
##
## The root properties are also inverted such that all subsequent plots will
## use the new color scheme.
##
## If the optional argument @var{color} is present then the background color
## is set to @var{color} rather than inverted.  @var{color} may be a string
## representing one of the eight known colors or an RGB triplet.  The special
## string argument @qcode{"none"} restores the plot to the factory default
## colors.
##
## If the first argument @var{hfig} is a figure handle or list of figure
## handles, then operate on these figures rather than the current figure
## returned by @code{gcf}.  The root properties will not be changed unless 0
## is in the list of figures.
##
## Programming Note: @code{whitebg} operates by changing the color properties
## of the children of the specified figures.  Only objects with a single color
## are affected.  For example, a patch with a single @qcode{"FaceColor"} will
## be changed, but a patch with shading (@qcode{"interp"}) will not be
## modified.  For inversion, the new color is simply the inversion in RGB
## space: @code{@var{cnew} = [1-@var{R} 1-@var{G} 1-@var{B}]}.  When a color
## is specified, the axes and figure are set to the new color, and the color
## of child objects are then adjusted to have some contrast (visibility)
## against the new background.
## @seealso{reset, get, set}
## @end deftypefn

## FIXME: It's not clear whether Matlab also changes color properties
## of the figure object itself, or only the children.  However, visually,
## it looks better to change the figure along with the axes background.

function whitebg (varargin)

  if (nargin > 2)
    print_usage ();
  endif

  h = 0;
  color = NaN;
  have_fig = false;

  if (nargin > 0)
    if (all (ishghandle (varargin{1})))
      h = varargin{1};
      have_fig = true;
      if (nargin == 2)
        color = varargin{2};
      endif
    elseif (nargin == 1)
      color = varargin{1};
    else
      print_usage ();
    endif
  endif

  if (! have_fig)
    fig = gcf ();
    do_root = true;
  elseif (all (isfigure (h) | h == 0))
    fig = h(h != 0);
    do_root = any (h == 0);
  else
    error ("whitebg: HFIG must be a valid figure handle");
  endif

  if (isnan (color))
    if (do_root)
      ## Set the default axes and figure properties on root
      ## so that subsequent plots have the new color scheme
      fac = get (0, "factory");
      fields = fieldnames (fac);
      idx = ! cellfun ("isempty", regexp (fields,
                                          '^factory(axes|figure).*color$'));

      ## Use default value in place of factory value if specified.
      for field = fields(idx)'
        defaultfield = strrep (field{1}, "factory", "default");
        try
          defaultvalue = 1 - get (0, defaultfield);
        catch
          defaultvalue = 1 - fac.(field{1});
        end_try_catch
        set (0, defaultfield, defaultvalue);
      endfor
    endif

    ## The sort is necessary so that child legend objects are acted on
    ## before the legend axes object.
    hlist = sort (findobj (fig));

    for h = hlist'
      props = get (h);
      fields = fieldnames (props);
      ## Find all fields with the word color in them and invert.
      idx = find (! cellfun ("isempty", regexp (fields, 'color$')));
      for field = fields(idx)'
        c = props.(field{1});
        if (! ischar (c) && columns (c) == 3)
          set (h, field{1}, 1 - c);
        endif
      endfor
    endfor

  else  # 2nd argument such as a color or "none"

    if (! strcmp (color, "none"))
      ## Set the specified color on the figure and all axes objects
      hlist = [fig; findobj(fig, "type", "axes")];
      set (hlist, "color", color);
      if (do_root)
        defs = get (0, "default");
        if (isfield (defs, "defaultaxescolor")
            && strcmp (defs.defaultaxescolor, "none"))
          set (0, "defaultaxescolor", color);
        endif
      endif

      ## Adjust colors of remaining objects to have some contrast
      bg = rgb2hsv (get (fig, "color"));
      ## List of children without the figure and axes objects already changed
      hlist = setdiff (findobj (fig), hlist);
      for h = hlist'
        props = get (h);
        fields = fieldnames (props);
        ## Find all fields with the word color in them and adjust HSV.
        idx = find (! cellfun ("isempty", regexp (fields, 'color$')));
        for field = fields(idx)'
          c = props.(field{1});
          if (! ischar (c) && columns (c) == 3)
            set (h, field{1}, calc_contrast_color (bg, c));
          endif
        endfor
      endfor

    else
      ## Reset colors to factory defaults
      if (do_root)
        fac = get (0, "factory");
        fields = fieldnames (fac);
        idx = ! cellfun ("isempty", regexp (fields,
                                            '^factory(axes|figure).*color$'));
        for field = fields(idx)'
          factoryfield = field{1};
          factoryvalue = fac.(factoryfield);
          if (strncmp (factoryfield, "factoryfigure", 13))
            ## Strip off "factoryfigure" part of fieldname before applying
            set (fig, factoryfield(14:end), factoryvalue);
          endif
          ## Remove applied default from root
          defaultfield = strrep (factoryfield, "factory", "default");
          set (0, defaultfield, "remove");
        endfor
      endif

      hlist = sort (findobj (fig));
      for h = hlist'
        props = get (h);
        fields = fieldnames (props);
        ## Find all fields with the word color in them and restore to factory.
        idx = find (! cellfun ("isempty", regexp (fields, 'color$')));
        for field = fields(idx)'
          set (h, field{1}, "factory");
        endfor
      endfor
    endif
  endif

endfunction

## Calculate a new color which contrasts with the supplied bg color and is
## perceptually related to the original color.
##
## Input color bg must be in HSV space.  Input color corig is in RGB space.
##
## FIXME: Calculation is segregated into its own function in case anyone wants
## to try and improve the selection of a "contrasting" color.
##
## This algorithm maintains at least 90 degrees separation between corig and bg
## in Hue rotation space.  No modifications are done for saturation or value.
function cnew = calc_contrast_color (bg, corig)

  hsv = rgb2hsv (corig);
  contrast_hue = mod (bg(1) + 0.5, 1);  # Generate a contrasting bg color

  ## If close to existing contrast color, leave alone
  delta = abs (hsv(1) - contrast_hue);
  if (delta < 0.25 || delta > 0.75)
    cnew(1) = hsv(1);
  else
    cnew(1) = mod (hsv(1) + 0.5, 1);
  endif

  ## No modifications to saturation or value.
  cnew(2:3) = hsv(2:3);

  cnew = hsv2rgb (cnew);

endfunction


%!test
%! dac = get (0, "defaultaxescolor");
%! dfc = get (0, "defaultfigurecolor");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hl = line ("Color", "r");
%!   assert (get (hf, "color"), dfc);
%!   assert (get (gca, "color"), dac);
%!   assert (get (hl, "color"), [1 0 0]);
%!   whitebg (hf);
%!   assert (get (hf, "color"), 1 - dfc);
%!   assert (get (gca, "color"), 1 - dac);
%!   assert (get (hl, "color"), [0 1 1]);
%!   c = [0.2 0.2 0.2];
%!   whitebg (hf, c);
%!   assert (get (hf, "color"), c);
%!   assert (get (gca, "color"), c);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error whitebg (1,2,3)
%!error whitebg ({1}, 2)
