########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{h} =} waitbar (@var{frac})
## @deftypefnx {} {@var{h} =} waitbar (@var{frac}, @var{msg})
## @deftypefnx {} {@var{h} =} waitbar (@dots{}, "createcancelbtn", @var{fcn}, @dots{})
## @deftypefnx {} {@var{h} =} waitbar (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} waitbar (@var{frac})
## @deftypefnx {} {} waitbar (@var{frac}, @var{h})
## @deftypefnx {} {} waitbar (@var{frac}, @var{h}, @var{msg})
## Return a handle @var{h} to a new progress indicator ("waitbar") object.
##
## The waitbar is filled to fraction @var{frac} which must be in the range
## [0, 1].
##
## The optional message @var{msg} is centered and displayed above the waitbar.
##
## A cancel button can be added to the bottom of the waitbar using the
## @qcode{"createcancelbtn"} property of waitbar figures.  The action to be
## executed when the user presses the button is specified using a string or
## function handle @var{fcn}.
##
## The appearance of the waitbar figure window can be configured by passing
## @var{prop}/@var{val} pairs to the function.  The full list of properties is
## documented at @ref{Figure Properties}.
##
## When called with a single input the current waitbar, if it exists, is
## updated to the new value @var{frac}.  If there are multiple outstanding
## waitbars they can be updated individually by passing the handle @var{h}
## of the specific waitbar to modify.
##
## @seealso{delete}
## @end deftypefn

function h = waitbar (varargin)

  persistent curr_waitbar;

  if (nargin < 1)
    print_usage ();
  endif

  frac = varargin{1};
  varargin(1) = [];

  if (! (isnumeric (frac) && isscalar (frac) && frac >= 0 && frac <= 1))
    error ("waitbar: FRAC must be between 0 and 1");
  endif

  ## Use existing waitbar if it still points to a valid graphics handle.
  if (nargin == 1 && ishghandle (curr_waitbar))
    hf = curr_waitbar;
  else
    hf = false;
  endif

  if (! isempty (varargin) && isnumeric (varargin{1}))
    hf = varargin{1};
    varargin(1) = [];
    if (! isfigure (hf) || ! strcmp (get (hf, "tag"), "waitbar"))
      error ("waitbar: H must be a handle to a waitbar object");
    endif
  endif

  msg = false;

  if (! isempty (varargin))
    msg = varargin{1};
    varargin(1) = [];
    if (! (ischar (msg) || iscellstr (msg)))
      error ("waitbar: MSG must be a character string or cell array of strings");
    endif
  endif

  if (rem (numel (varargin), 2) != 0)
    error ("waitbar: invalid number of property/value pairs");
  endif

  if (hf)
    gd = get (hf, "__guidata__");
    ## Get the cached handles.
    ax = gd(1);
    hp = gd(2);

    set (hp, "xdata", [0; frac; frac; 0]);

    if (ischar (msg) || iscellstr (msg))
      th = get (ax, "title");
      curr_msg = get (th, "string");
      ## graphics handles always store data as column vectors
      if (iscellstr (msg))
        msg = msg(:);
      endif
      cmp = strcmp (msg, curr_msg);
      if (! all (cmp(:)))
        set (th, "string", msg);
      endif
    endif
  else
    ## Save and restore current figure
    cf = get (0, "currentfigure");

    hf = figure ("units", "pixels",
                 "position", [250, 500, 400, 100],
                 "numbertitle", "off",
                 "menubar", "none", "toolbar", "none",
                 "integerhandle", "off",
                 "handlevisibility", "callback",
                 "tag", "waitbar");

    ax = axes ("parent", hf,
               "xtick", [], "ytick", [],
               "xlim", [0, 1], "ylim", [0, 1],
               "position", [0.1, 0.3, 0.8, 0.2]);

    ## Add createcancelbtn property
    addproperty ("createcancelbtn", hf, "figurebuttondownfcn");
    ## FIXME: Can't add listener because of bug #55963.
    ## addlistener (hf, "createcancelbtn", {@updatecancelbutton, ax});

    if (! isempty (varargin))
      set (hf, varargin{:});
    endif

    ## Add listener and create cancel button only after setting properties
    ## which could change "createcancelbtn" property (bug #55963).
    addlistener (hf, "createcancelbtn", {@updatecancelbutton, ax});
    if (any (strcmp ("createcancelbtn", varargin)))
      updatecancelbutton (hf, [], ax);
    endif

    hp = patch (ax, [0; frac; frac; 0], [0; 0; 1; 1], [0, 0.35, 0.75]);

    ## Cache the axes and patch handles.
    set (hf, "__guidata__", [ax, hp]);

    if (! (ischar (msg) || iscellstr (msg)))
      msg = "Please wait...";
    endif
    title (ax, msg);

    set (0, "currentfigure", cf);
  endif

  drawnow ();

  if (nargout > 0)
    h = hf;
  endif

  ## If there were no errors, update current waitbar.
  curr_waitbar = hf;

endfunction

function updatecancelbutton (hf, ~, hax)

  if (! strcmp (get (hf, "__graphics_toolkit__"), "qt"))
    return;
  endif

  hbtn = findobj (hf, "type", "uicontrol", "-and", "style", "pushbutton");
  cb = get (hf, "createcancelbtn");
  if (! isempty (cb))
    if (isempty (hbtn))
      units = get (hax, "units");
      fpos = get (hf, "position");
      set (hax, "units", "pixels");
      apos = get (hax, "position");

      fpos(2) -= 40;
      fpos(4) += 40;
      apos(2) += 40;
      set (hf, "position", fpos);
      set (hax, "position", apos, "units", units);

      hbtn = uicontrol (hf, "style", "pushbutton", "string", "Cancel", ...
                            "position", [fpos(3)-100, 10, 60, 25], ...
                            "callback", cb);
    else
      set (hbtn, "callback", cb);
    endif
  elseif (! isempty (hbtn))
    delete (hbtn);
    units = get (hax, "units");
    fpos = get (hf, "position");
    set (hax, "units", "pixels");
    apos = get (hax, "position");

    fpos(2) += 40;
    fpos(4) -= 40;
    apos(2) -= 40;
    set (hf, "position", fpos);
    set (hax, "position", apos, "units", units);
  endif

endfunction


%!demo
%! h = waitbar (0, '0.00%');
%! for i = 0:0.01:1
%!   waitbar (i, h, sprintf ('%.2f%%', 100*i));
%!   if (strcmp (graphics_toolkit (), "qt"))
%!     pause (0.01);
%!   endif
%! endfor
%! close (h);

%!demo
%! h = waitbar (0, 'please wait...');
%! for i = 0:0.01:0.6
%!   waitbar (i);
%!   if (strcmp (graphics_toolkit (), "qt"))
%!     pause (0.01);
%!   endif
%! endfor
%! i = 0.3;
%! waitbar (i, h, 'don''t you hate taking a step backward?');
%! pause (0.5);
%! for i = i:0.005:0.7
%!   waitbar (i, h);
%!   if (strcmp (graphics_toolkit (), "qt"))
%!     pause (0.01);
%!   endif
%! endfor
%! waitbar (i, h, 'or stalling?');
%! pause (1);
%! for i = i:0.003:0.85
%!   waitbar (i, h, 'just a little longer now');
%!   if (strcmp (graphics_toolkit (), "qt"))
%!     pause (0.01);
%!   endif
%! endfor
%! for i = i:0.001:1
%!   waitbar (i, h, 'please don''t be impatient');
%!   if (strcmp (graphics_toolkit (), "qt"))
%!     pause (0.01);
%!   endif
%! endfor
%! close (h);

%!demo
%! h1 = waitbar (0, 'Waitbar #1');
%! h2 = waitbar (0, 'Waitbar #2');
%! h2pos = get (h2, 'position');
%! h2pos(1) = h2pos(1) + (h2pos(3) + 50);
%! set (h2, 'position', h2pos);
%! pause (0.5);
%! for i = 1:4
%!   waitbar (i/4, h1);
%!   pause (0.5);
%!   waitbar (i/4, h2);
%!   pause (0.5);
%! endfor
%! pause (0.5);
%! close (h1);
%! close (h2);

%!demo
%! clf ();
%! niter = 7;
%! l = 1;
%! xx = [0 l];
%! yy = [0 0];
%! hli = plot (xx, yy);
%! pos1 = get (gcf, "position");
%!
%! disp ("Push the <cancel> button to stop the process.");
%! hf = waitbar (0,"0","Name","Building Koch curve ...",...
%!               "createcancelbtn", "setappdata (gcbf,'interrupt', true)");
%!
%! pos2 = get (hf, "position");
%! set (hf, "position", [pos1(1)+(pos1(3)-pos2(3))/2, pos1(2)+pos1(4), pos2(3:4)]);
%!
%! for ii = 1:niter
%!   ## Check cancel request
%!   if (! ishghandle (hf))
%!     break;
%!   elseif (getappdata (hf, "interrupt"))
%!     delete (hf);
%!     break;
%!   else
%!     waitbar (ii/niter, hf, sprintf ("Step %d/%d", ii, niter));
%!   endif
%!
%!   ## Increasingly lengthy computation
%!   l /= 3;
%!   theta = angle (complex (diff (xx), diff (yy)));
%!
%!   xy = @(th, x0, y0) [cos(th) -sin(th) x0
%!                       sin(th) cos(th) y0] * [0 l l*3/2      2*l;
%!                                              0 0 l*(3)^.5/2 0;
%!                                              1 1 1          1];
%!   tmp = arrayfun (xy, theta, xx(1:end-1), yy(1:end-1),
%!                   "uniformoutput", false);
%!
%!   tmp = cell2mat (tmp);
%!   xx = [tmp(1,:) xx(end)];
%!   yy = [tmp(2,:) yy(end)];
%!   set (hli, "xdata", xx, "ydata", yy);
%!   drawnow ();
%!   pause (0.75);
%! endfor
%!
%! if (ishghandle (hf))
%!   delete (hf);
%! endif

## Test input validation
%!error <FRAC must be between 0 and 1> waitbar (-0.5)
%!error <FRAC must be between 0 and 1> waitbar (1.5)
%!error <MSG must be a character string> waitbar (0.5, struct ())
%!error <invalid number of property/value pairs> waitbar (0.5, "msg", "Name")
