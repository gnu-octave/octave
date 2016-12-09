## Copyright (C) 2012-2016 pdiribarne
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {} {@var{s} =} hdl2struct (@var{h})
## Return a structure, @var{s}, whose fields describe the properties
## of the object, and its children, associated with the handle, @var{h}.
##
## The fields of the structure @var{s} are @qcode{"type"}, @qcode{"handle"},
## @qcode{"properties"}, @qcode{"children"}, and @qcode{"special"}.
## @seealso{struct2hdl, hgsave, findobj}
## @end deftypefn

## Author: pdiribarne <pdiribarne@new-host.home>
## Created: 2012-03-04

function s = hdl2struct (h)

  if (nargin != 1 || ! ishandle (h))
    print_usage ();
  endif

  hiddenh = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "off");

    ## main object
    s.handle = h;
    s.type = get (h, "type");
    s.properties = getprops (h);
    s.children = [];
    s.special = [];

    ## Process, in reverse order, all children except for
    ## legends, colorbars, uimenu, and hggroup children
    ii = 0;
    allkids = get (h, "children");
    if (! strcmp (s.type, "hggroup"))
      hnot = findobj (h, "-depth", 1, "tag", "legend", "-or", "tag", "colorbar",
                                      "-or", "type", "uimenu");
      kids = allkids(! ismember (allkids, hnot));
      nkids = length (kids);
      for i = nkids:-1:1
        s.children(++ii) = hdl2struct (kids(i));
      endfor
    endif

    ## add non "children" children objects (title, xlabel, ...) and
    ## hggroup children and tag them in "special"
    if (strcmp (s.type, "hggroup"))
      special = allkids;
    else
      special = [];
    endif
    special = [special getspecial(h)];
    nsp = length (special);
    while (nsp)
      ii += 1;
      s.children(ii) = hdl2struct (special(nsp));
      s.special(nsp) = ii;
      nsp -= 1;
    endwhile

    if (strcmp (s.type, "axes") && isempty (get (h, "tag")))
      ## look for legends and colorbars among axes brothers and add them
      ## to the children list

      par = get (h, "parent");
      lg = findobj (par, "-depth", 1, "tag", "legend");
      if (! isempty (lg))
        ## identify legends which are attached to this axes.
        idx = ([[get(lg).userdata].handle] == h);
        lg = lg(idx);
      endif
      nlg = length (lg);
      if (nlg == 1)
        ii += 1;
        s.children(ii) = hdl2struct (lg);
      elseif (nlg > 1)
        error ("hdl2struct: more than one legend found");
      endif

      cb = findobj (par, "-depth", 1, "tag", "colorbar");
      if (! isempty (cb))
        ## identify colorbars which are attached to this axes.
        idx2 = ([get(cb).axes] == h);
        cb = cb(idx);
      endif
      ncb = length (cb);
      if (ncb == 1)
        ii += 1;
        s.children(ii) = hdl2struct (cb);
      elseif (ncb > 1)
        error ("hdl2struct: more than one colorbar found");
      endif
    endif

  unwind_protect_cleanup
    set (0, "showhiddenhandles", hiddenh);
  end_unwind_protect

endfunction

function hlist = getspecial (h)

  ## return handles to special children
  hlist = [];

  regkids = get (h, "children");
  ## inline version of allchild() for performance
  set (0, "showhiddenhandles", "on");
  allkids = get (h, "children");
  set (0, "showhiddenhandles", "off");
  speckids = ! ismember (allkids, regkids);
  hlist = allkids(speckids);
  hlist = hlist(:).';  # return row vector

endfunction

function propstruct = getprops (h)

  persistent excluded;

  if (isempty (excluded))
    excluded = cell2struct (repmat ({[]}, 1, 21),
                            {"beingdeleted", "busyaction", "buttondownfcn", ...
                             "children", "clipping", "createfcn", ...
                             "deletefcn", "handlevisibility", "hittest", ...
                             "interruptible", "parent", "selected" , ...
                             "selectionhighlight", "type", "uicontextmenu", ...
                             "currentaxes", "currentcharacter", ...
                             "currentobject", "tightinset", "currentpoint", ...
                             "extent"}, 2);
  endif

  obj = get (h);
  ## get useful properties rejecting readonly, children, handles ...
  fields = fieldnames (obj);
  tf = isfield (excluded, fields);
  propstruct = rmfield (obj, fields(tf));

  ## hidden properties
  hidden_props = {"__autopos_tag__", "looseinset"};
  for prop = hidden_props
    try
      val = get (h, prop{1});
      propstruct.(prop{1}) = val;
    end_try_catch
  endfor

endfunction


## FIXME: need validation tests
## FIXME: Need to test code for legends, colorbars.

