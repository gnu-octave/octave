## Copyright (C) 2012 pdiribarne
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{h} =} struct2hdl (@var{s})
## @deftypefnx {Function File} {@var{h} =} struct2hdl (@var{s}, @var{p})
## @deftypefnx {Function File} {@var{h} =} struct2hdl (@var{s}, @var{p}, @var{hilev})
## Constructs an object from the structure @var{s}. The structure must
## contain the fields "handle", "type", "children", "properties", and
## "special".  If the handle of an existing figure or axes is specified,
## @var{p}, the new object will be created as a child to that object.
## If no object handle is provided, then a new figure and the necessary
## children will be constructed using the default object values from
## the root figure.
##
## A third boolean argument @var{hilev} can be passed to specify wether
## the function should try to preserve listeners/calbacks e.g for
## legends or hggroups. Default is false.
## @seealso{findobj, get, hdl2struct, set}
## @end deftypefn

## Author: pdiribarne <pdiribarne@new-host.home>
## Created: 2012-03-04

function [ h, matchout ] = struct2hdl (hgS, matchin=[], hilev = false)

  fields = { "handle", "type", "children", "properties", "special"};
  partypes = {"root", "figure", "axes", "hggroup"};
  othertypes = {"line", "patch", "surface", "image", "text"};
  alltypes = [partypes othertypes];

  if (nargin > 3 || ! isstruct (hgS))
    print_usage ();
  elseif (! all (isfield (hgS, fields)))
    print_usage ();
  elseif (isscalar (matchin))
    if (! ishandle (matchin))
      error ("struct2hdl: argument #2 is not a handle to graphic object")
    endif
    if (any (strcmp (get (matchin).type, partypes)))
      paridx = find (strcmp (get (matchin).type, alltypes));
      kididx = find (strcmp (hgS.type, alltypes));
      if (kididx <= paridx)
        error ("struct2hdl: incompatible input handles")
      endif
    else
      error ("struct2hdl: %s object can't be parent object", get (matchin).type)
    endif
    hpar = matchin;
    matchin = [NaN; hpar];
    ## create appropriate parent if needed
    if (any (strcmp (hgS.type, othertypes)))
      for ii = (paridx+1) : (numel (partypes)-1)
        eval (["hpar = " partypes{ii} "(\"parent\", hpar);"]);
        matchin = [matchin [NaN; hpar]];
      endfor
    elseif (any (strcmp (hgS.type, {"hggroup", "axes"})))
      for ii = (paridx+1) : (kididx-1)
        eval (["hpar = " partypes{ii} "(\"parent\", hpar);"]);
        matchin = [matchin [NaN; hpar]];
      endfor
    else
      par = NaN;
    endif
  elseif (isempty (matchin))
    if (any (strcmp (hgS.type, othertypes)))
      par = axes ();
    elseif (any (strcmp (hgS.type, {"hggroup", "axes"})))
      par = figure ();
    else
      par = NaN;
    endif
    matchin = [NaN; par];
  endif
  ## read parent (last column) in matchin and remove it if duplicate
  par = matchin (2,end);
  tst = find (matchin (2,:) == par);
  if (numel (tst) > 1)
    matchin = matchin (1:2, 1:(tst(end)-1));
  endif

  ## create object
  if (strcmpi (hgS.type, "root"))
    h = 0;
    hgS.properties = rmfield (hgS.properties, ...
                              {"callbackobject", "commandwindowsize", ...
                               "screendepth", "screenpixelsperinch", ...
                               "screensize"});
  elseif (strcmpi (hgS.type, "figure"))
    h = figure ();
  elseif (strcmpi (hgS.type, "axes"))
    ## legends and colorbars are "transformed" in normal axes
    ## if hilev is not requested
    if (! hilev)
      if (strcmp (hgS.properties.tag, "legend"))
        hgS.properties.tag = "";
        hgS.properties.userdata = [];
        par = gcf;
      elseif (strcmp (hgS.properties.tag, "colorbar"))
        hgS.properties.tag = "";
        hgS.properties.userdata = [];
        par = gcf;
      endif
    endif

    [h, hgS] = createaxes (hgS, matchin, par);
  elseif (strcmpi (hgS.type, "line"))
    h = createline (hgS, par);
  elseif (strcmpi (hgS.type, "patch"))
    [h, hgS] = createpatch (hgS, par);
  elseif (strcmpi (hgS.type, "text"))
    h = createtext (hgS, par);
  elseif (strcmpi (hgS.type, "image"))
    h = createimage (hgS, par);
  elseif (strcmpi (hgS.type, "surface"))
    h = createsurface (hgS, par);
  elseif (strcmpi (hgS.type, "hggroup"))
    [h, hgS, matchin] = createhg (hgS, matchin, par, hilev);
  endif

  ## children
  matchin = [matchin [hgS.handle; h]]; # [original; new]
  kids = hgS.children;
  nkids = length (kids);
  ii = 0;
  while nkids
    ii++;
    if (! any (ii == hgS.special))
      [h2, matchin] = struct2hdl (hgS.children(ii),
                                  [matchin [hgS.handle; h]], hilev);
    endif
    nkids--;
  endwhile

  ## paste properties
  setprops (hgS, h, matchin, hilev);

  matchout = matchin;

endfunction

function [h, hgSout] = createaxes (hgS, matchin, par);
  ## regular axes
  if (strcmpi (hgS.properties.tag, ""))
    propval = {"position", hgS.properties.position};
    hid = {"autopos_tag", "looseinset"};
    for ii = 1:numel (hid)
      prop = hid{ii};
      if (isfield (hgS.properties, prop))
        val = hgS.properties.(prop);
        propval = [propval, prop, val];
      endif
    endfor
    h = axes (propval{:}, "parent", par);

    if isfield (hgS.properties, "__plotyy_axes__")
      plty = hgS.properties.__plotyy_axes__;
      addproperty ("__plotyy_axes__", h, "any")
      tmp = [matchin [hgS.handle; h]];
      tst = arrayfun (@(x) any (plty == x), tmp (1:2:end));
      if sum (tst) == numel (plty)
        for ii = 1:numel (plty)
          plty(ii) = tmp (find (tmp == plty(ii)) + 1);
        endfor
        for ii = 1:numel (plty)
          set (plty(ii), "__plotyy_axes__", plty);
        endfor
      endif
      hgS.properties = rmfield (hgS.properties, "__plotyy_axes__");
    endif

    ## delete non-default and already set properties
    fields = fieldnames (hgS.properties);
    tst = cellfun (@(x) isprop (h, x), fields);
    hgS.properties = rmfield (hgS.properties,  fields(find (tst == 0)));

  elseif (strcmpi (hgS.properties.tag, "legend"))
    ## legends
    oldax = hgS.properties.userdata.handle;
    idx = find (matchin == oldax);
    newax = matchin(idx+1);
    strings = {};
    kids = hgS.children;
    kids(hgS.special) = [];
    oldh = unique (arrayfun (@(x) x.properties.userdata(end), kids));
    for ii = 1:length (oldh)
      idx = find (matchin(1:2:end) == oldh(ii)) * 2;
      if (! isempty (idx))
        newh(ii) = matchin (idx);
        if (! strcmp (get (newh(ii), "type"), "hggroup"))
          str = get (newh(ii), "displayname");
          strings = [strings str];
        else
          str = get (get (newh(ii), "children")(1), "displayname");
          strings = [strings str];
        endif
      else
        error ("struct2hdl: didn't find a legend item")
      endif
    endfor
    location = hgS.properties.location;
    orientation = hgS.properties.orientation;
    textpos = hgS.properties.textposition;
    box = hgS.properties.box;

    h = legend (newax, newh, strings, "location", location, ...
                "orientation", orientation);
    set (h, "textposition", textpos); # bug makes "textposition"
                                # redefine the legend
    h = legend (newax, newh, strings, "location", location, ...
                "orientation", orientation);
    ## box
    if (strcmp (box, "on"))
      legend boxon
    endif

    ## visibility
    tst = arrayfun (@(x) strcmp (x.properties.visible, "on"), kids);
    if !any (tst)
      legend ("hide");
    endif

    ## remove all properties such as "textposition" that redefines
    ## the entire legend. Also remove chidren
    hgS.properties = rmfield (hgS.properties, ...
                                {"userdata", "xlabel",...
                                 "ylabel", "zlabel", "location", ...
                                 "title", "string","orientation", ...
                                 "visible", "textposition"});

    hgS.children = [];

  elseif (strcmpi (hgS.properties.tag, "colorbar"))
    ## colorbar
    oldax = hgS.properties.axes;
    if (! isempty (idx = find (oldax == matchin)))
      ax = matchin(idx+1);
      location = hgS.properties.location;
      h = colorbar ("peer", ax, location);
      hgS.properties = rmfield (hgS.properties, ...
                              {"userdata", "xlabel" ...
                               "ylabel", "zlabel", ...
                               "title", "axes"});
      hgS.children= [];
    else
      error ("hdl2struct: didn't find an object")
    endif
  endif
  hgSout = hgS;
endfunction

function [h] = createline (hgS, par);
  h = line ("parent", par);
  addmissingprops (h, hgS.properties);
endfunction

function [h, hgSout] = createpatch (hgS, par);
  prp.faces = hgS.properties.faces;
  prp.vertices = hgS.properties.vertices;
  prp.facevertexcdata = hgS.properties.facevertexcdata;
  h = patch (prp);
  set (h, "parent", par);
  hgS.properties = rmfield (hgS.properties,
                            {"faces", "vertices", "facevertexcdata"});
  addmissingprops (h, hgS.properties);
  hgSout = hgS;
endfunction

function [h] = createtext (hgS, par);
  h = text ("parent", par);
  addmissingprops (h, hgS.properties)
endfunction

function [h] = createimage (hgS, par);
  h = image ("parent", par);
  addmissingprops (h, hgS.properties)
endfunction

function [h] = createsurface (hgS, par);
  h = surface ("parent", par);
  addmissingprops (h, hgS.properties)
endfunction

function [h, hgSout, matchout] = createhg (hgS, matchin, par, hilev)
  ## Here we infer from properties the type of hggroup we should build
  ## an call corresponding high level functions
  ## We manually set "hold on" to avoid next hggroup be deleted
  ## the proper value of axes "nextplot" will finally be recovered

  hold on;
  if (hilev)
    [h, hgS, matchin] = createhg_hilev (hgS, matchin, par);
    if (numel (hgS.children) != numel (get (h).children))
      warning (["struct2hdl: couldn't infer the hggroup type. ", ...
                "Will build objects but listener/callback functions ", ...
                "will be lost"]);
      if isfield (h, "bargroup")
        delete (get (h).bargroup);
      else
        delete (h);
      endif
      h = hggroup ("parent", par);
      addmissingprops (h, hgS.properties);
      hgS.special = [];
    else
      oldkids = hgS.children;
      newkids = get (h).children;
      nkids = numel (oldkids);
      ii = 1;
      while nkids
        matchin = [matchin [oldkids(ii++).handle; newkids(nkids--)]];
      endwhile
    endif
  else
    h = hggroup ("parent", par);
    addmissingprops (h, hgS.properties);
    hgS.special = [];
  endif
  hgSout = hgS;
  matchout = matchin;
endfunction

function [h, hgSout, matchout] = createhg_hilev (hgS, matchin, par)
  fields = hgS.properties;
  if (isfield (fields, "contourmatrix"))
    ## contours
    xdata = hgS.properties.xdata;
    ydata = hgS.properties.ydata;
    zdata = hgS.properties.zdata;
    levellist = hgS.properties.levellist;
    textlist = hgS.properties.textlist;

    ## contour creation
    if (isempty (hgS.children(1).properties.zdata))
      if (strcmpi (hgS.properties.fill, "on"))
        [cm2, h] = contourf (xdata, ydata, zdata, levellist);
      else
        [cm2, h] = contour (xdata, ydata, zdata, levellist);
      endif

      ## labels
      if (strcmpi (hgS.properties.showtext, "on"))
        clabel (cm2, h, textlist);
      endif
    else
      [cm2, h] = contour3 (xdata, ydata, zdata, levellist);
    endif

    ## delete already set properties and children
    hgS.properties = rmfield (hgS.properties, ...
                              {"xdata", "ydata", "zdata", ...
                               "contourmatrix", "levellist", ...
                               "fill", "labelspacing", ...
                               "levellistmode", "levelstep", ...
                               "levelstepmode", "textlist"...
                               "textlistmode" , "textstep", ...
                               "textstepmode", "zlevel", ...
                               "zlevelmode"});

  elseif (isfield (fields, "udata") && isfield (fields, "vdata"))
    ## quiver
    xdata = hgS.properties.xdata;
    ydata = hgS.properties.ydata;

    udata = hgS.properties.udata;
    vdata = hgS.properties.vdata;

    h = quiver (xdata, ydata, udata, vdata);

    ## delete already set properties and children
    hgS.properties = rmfield (hgS.properties, ...
                              {"xdata", "ydata", "zdata", ...
                               "xdatasource", "ydatasource", "zdatasource", ...
                               "udata", "vdata", "wdata", ...
                               "udatasource", "vdatasource", "wdatasource"});

  elseif (isfield (fields, "format"))
    ##errorbar
    form = hgS.properties.format;
    xdata = hgS.properties.xdata;
    ydata = hgS.properties.ydata;
    xldata = hgS.properties.xldata;
    ldata = hgS.properties.ldata;
    xudata = hgS.properties.xudata;
    udata = hgS.properties.udata;

    switch form
      case "xerr"
        h = errorbar (xdata, ydata, xldata, xudata, ">");
      case "yerr"
        h = errorbar (xdata, ydata, ldata, udata, "~");
      case "xyerr"
        h = errorbar (xdata, ydata, xldata, xudata, ldata, udata, "~>");
      case "box"
        h = errorbar (xdata, ydata, xldata, xudata, "#");
      case "boxy"
        h = errorbar (xdata, ydata, ldata, udata, "#~");
      case "boxxy"
        h = errorbar (xdata, ydata, xldata, xudata, ldata, udata, "#~>");
      otherwise
        error ("struct2hdl: couldn't guess the errorbar format")
    endswitch
    ## delete already set properties
    hgS.properties = rmfield (hgS.properties, ...
                              {"xdata", "ydata", ...
                               "xldata", "ldata", ...
                               "xudata", "udata", ...
                               "xldatasource", "ldatasource", ...
                               "xudatasource", "udatasource", ...
                               "format"});

  elseif (isfield (fields, "bargroup"))
    ## bar plot
    ## FIXME - here we don't have access to brothers so we first create all
    ## the barseries of the bargroup (but the last), then retrieve information,
    ## and rebuild the whole bargroup.
    ## The duplicate are deleted after calling "setprops"

    bargroup = hgS.properties.bargroup;
    oldh = hgS.handle;

    temp = arrayfun (@(x) any(x == bargroup), [matchin(1:2:end) oldh]);
    tst = sum (temp) == length (bargroup);

    if (isscalar (bargroup) || !tst)
      xdata = hgS.properties.xdata;
      ydata = hgS.properties.ydata;

      h = bar (xdata, ydata);

      ## delete already set properties,
      hgS.properties = rmfield (hgS.properties, ...
                                {"xdata", "ydata", ...
                                 "xdatasource", "ydatasource", ...
                                 "bargroup", ...
                                 "barwidth", "baseline"});
    else
      xdata = [];
      ydata = [];

      ##build x/y matrix
      nbar = length (bargroup);
      tmp = struct ("handle", NaN,"type", "", "children", [], "special", []);
      for ii = 1:(nbar - 1)
        idx = find (matchin(1:2:end) == bargroup(ii)) * 2;
        hdl = matchin (idx);
        xdata = [xdata get(hdl).xdata];
        ydata = [ydata get(hdl).ydata];
        tmp.children(ii) = hdl2struct (hdl);
      endfor

      xdata = [xdata hgS.properties.xdata];
      ydata = [ydata hgS.properties.ydata];
      width = hgS.properties.barwidth;
      h = bar (ydata, width);

      ## replace previous handles in "match", copy props and delete redundant
      for ii = 1:(nbar - 1)
        props = tmp.children(ii).properties;
        bl = props.baseline;
        tmp.children(ii).properties = rmfield (props, {"baseline", "bargroup"});
        setprops (tmp.children(ii), h(ii), matchin, 1);
        delete (tmp.children(ii).handle);
        delete (bl);
        idxpar = find (matchin == tmp.children(ii).handle);
        matchin (idxpar) = h(ii);
        idxkid = idxpar - 2;
        matchin (idxkid) = get (h(ii), "children");
      endfor
      matchin (2,((end-nbar+2):end)) = h (1:(end-1));
      h = h (end);

      ## delete already set properties ,
      hgS.properties = rmfield (hgS.properties, ...
                                {"xdata", "ydata", "bargroup"...
                                 "barwidth", "baseline"});
    endif
  elseif (isfield (fields, "baseline"))
    ## stem plot
    xdata = hgS.properties.xdata;
    ydata = hgS.properties.ydata;

    h = stem (xdata, ydata);

    ## delete already set properties,
    hgS.properties = rmfield (hgS.properties, ...
                              {"xdata", "ydata", ...
                               "xdatasource", "ydatasource", ...
                               "baseline"});
  elseif (isfield (fields, "basevalue"))
    ## area plot
    xdata = hgS.properties.xdata;
    ydata = hgS.properties.ydata;
    level = hgS.properties.basevalue;

    h = area (xdata, ydata, level);

    ## delete already set properties,
    hgS.properties = rmfield (hgS.properties, ...
                              {"xdata", "ydata", ...
                               "xdatasource", "ydatasource"});
  else
    warning ("struct2hdl: couldn't infer the hggroup type. Will build objects but listener/callback functions will be lost");
    h = hggroup ("parent", par);
    addmissingprops (h, hgS.properties);
    hgS.special = [];           # children will be treated as normal children
  endif
  hgSout = hgS;
  matchout = matchin;
endfunction

function setprops (hgS, h, matchin, hilev)
  more off
  if (strcmpi (hgS.properties.tag, ""))
    specs = hgS.children(hgS.special);
    hdls = arrayfun (@(x) x.handle, specs);
    nh = length(hdls);
    msg = "";
    if (! nh)
      set (h, hgS.properties);
    else
      ## Specials are objects that where automatically constructed with
      ## current object. Among them are "x(yz)labels", "title", high
      ## level hggroup children
      fields = fieldnames (hgS.properties);
      vals = struct2cell (hgS.properties);
      idx = find (cellfun (@(x) valcomp(x, hdls) , vals));
      hgS.properties = rmfield (hgS.properties, fields(idx));

      ## set all properties but special handles
      set (h, hgS.properties);

      ## find  props with val == (one of special handles)
      nf = length (idx);
      fields = fields(idx);
      vals = vals(idx);
      while nf
        field = fields{nf};
        idx = find (hdls == vals{nf});
        spec = specs(idx);
        if (isprop (h, field))
           h2 = get (h , field);
           set (h2, spec.properties);
        endif
        nf--;
      endwhile

      ## If hggroup children  were created by high level functions,
      ## copy only usefull properties.
      if (hilev)
        if (strcmpi (hgS.type, "hggroup"))
          nold = numel (hgS.children);
          nnew = numel (get(h).children);

          if (nold == nnew)
            hnew = get(h).children;
            ii = 1;
            while ii <= nnew
              try
                set (hnew (ii), "displayname", ...
                     hgS.children(ii).properties.displayname);
              catch
                sprintf ("struct2hdl: couldn't set hggroup children #%d props.", ii)
              end_try_catch
              ii ++;
            endwhile

          else
            error ("struct2hdl: non-conformant number of children in hgggroup")
          endif
        endif
      endif
    endif

  elseif (strcmpi (hgS.properties.tag, "legend")
          || strcmpi (hgS.properties.tag, "colorbar"))
    set (h, hgS.properties);
  endif

endfunction

function out = valcomp (x, hdls)
  if (isfloat(x) && isscalar(x))
    out = any (x == hdls);
  else
    out = 0;
  endif
endfunction

function addmissingprops (h, props)
  hid = {"autopos_tag", "looseinset"};
  oldfields = fieldnames (props);
  curfields = fieldnames (get (h));
  missing = cellfun (@(x) !any (strcmp (x, curfields)), oldfields);
  idx = find (missing);
  for ii = 1:length(idx)
    prop = oldfields{idx(ii)};
    if (! any (strcmp (prop, hid)))
      addproperty (prop, h, "any");
    endif
  endfor
endfunction
