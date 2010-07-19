## Copyright (C) 2010 David Bateman
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
## @deftypefn  {Function File} {} whitebg ()
## @deftypefnx {Function File} {} whitebg (@var{color})
## @deftypefnx {Function File} {} whitebg (@var{fig})
## @deftypefnx {Function File} {} whitebg (@var{fig}, @var{color})
## Invert the colors in the current color scheme.  The root properties are
## also inverted such that all subsequent plot use the new color scheme.
##
## If defined, @var{fig} is the handle to the figure to be inverted.  In
## this case only this figure has its color properties changed
##
## The background colors of the figure and its children can be set 
## specifically if @var{color} is defined.  @var{color} must be a valid
## color set as a string or an RGB triplet, or the value "none".
##
## @seealso{reset, colordef}
## @end deftypefn

function whitebg (varargin)
  h = 0;
  color = NaN;

  if (nargin > 0 && nargin < 2)
    if (ishandle (varargin{1}))
      h = varargin{1};
      if (nargin == 2)
        color = varargin{2};
      endif
    elseif (nargin == 1)
      color = varargin{1};
    else
      print_usage ();
    endif
  elseif (nargin != 0)
    print_usage ();
  endif

  typ = get (h, "type");

  if (strcmp (typ, "root"))
    isroot = true;
    fig = gcf ();
  elseif (strcmp (typ, "figure"))
    isroot = false;
    fig = h;
  else
    error ("expecting a figure handle");
  endif

  axes = findall (fig, "type", "axes");
  if (isnan (color))
    ## Root figure. Set the default axes and figure properties so that
    ## subsequent plots have the new color scheme
    if (isroot)
      fac = get (0, "factory");
      fields = fieldnames (fac);
      fieldindex = intersect (find (!cellfun (@isempty, regexp(fields, ".*color.*"))), union (find (!cellfun (@isempty, regexp(fields, "factoryaxes.*"))), find (!cellfun (@isempty, regexp(fields, "factoryfigure.*")))));

      ## Check whether the factory value has been replaced
      for nf = 1 : numel (fieldindex);
        defaultfield = strrep (fields {fieldindex (nf)}, "factory", "default");
        try
          defaultvalue = 1 - get (0, defaultfield {n});
        catch
          field = fields {fieldindex (nf)};
          defaultvalue = 1 - subsref (fac, struct ("type", ".", "subs", field));
        end_try_catch
        set (0, defaultfield, defaultvalue);
      endfor
    endif

    ## Load all objects which qualify for being searched.
    handles = fig;
    h = fig;
    while (numel (handles))
      children = [];
      for n = 1 : numel (handles)
        children = union (children, get(handles(n), "children"));
      endfor 
      handles = children;
      h = union (h, children);
    endwhile

    for nh = 1 : numel(h)
      p = get (h (nh));
      fields = fieldnames (p);
      fieldindex = find (!cellfun (@isempty, regexp(fields, ".*color.*")));
      if (numel (fieldindex))
        for nf = 1 : numel (fieldindex);
          field = fields {fieldindex (nf)};
          c = subsref (p, struct ("type", ".", "subs", field));
          if (! ischar(c) && columns(c) == 3)
            set (h (nh), field, 1 - c);
          endif
        endfor
      endif

      ## If h(nh) is a figure or axes invert default color properties
      typ = subsref (p, struct ("type", ".", "subs", "type"));
      if (strcmp (typ, "axes") || strcmp (typ, "figure"))
        def = get (h (nh), "default");
        fields = fieldnames (def);
        if (! isempty (fields))
          fieldindex = find (!cellfun (@isempty, regexp(fields, ".*color.*")));
          for nf = 1 : numel (fieldindex)
            defaultfield = fields {fieldindex (nf)};
            defaultvalue = 1 - subsref (def, struct ("type", ".", "subs", defaultfield));
            set (h (nh), defaultfield, defaultvalue);
          endfor
        endif
      endif
    endfor
  else
    ## FIXME
    ## Is this the right thing to do in this case?
    set (findall (fig, "type", "axes"), "color", color);
    if (isroot)
      defs = get (0, "defaults");
      if (isfield (defs, "defaultaxescolor") 
          && strcmp (defs.defaultaxescolor, "none"))
        set (0, "defaultaxescolor", color);
      endif
    endif
  endif
endfunction
