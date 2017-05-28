## Copyright (C) 2017 Pantxo Diribarne
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

## -*- texinfo -*-
## @deftypefn  {} {} movie (@var{mov})
## @deftypefnx {} {} movie (@var{mov}, @var{n})
## @deftypefnx {} {} movie (@var{mov}, @var{n}, @var{fps})
## @deftypefnx {} {} movie (@var{h}, @dots{})
## Play a movie defined by an array of frame structures.
##
## The movie @var{mov} must be a struct array of frames with fields
## @qcode{"cdata"} and @qcode{"colormap"}, as returned by the @code{getframe}
## function.  By default all images are displayed once, at 12 fps, in the
## current axes.
##
## The optional argument @var{n} is a scalar or vector of integers that
## controls the number of times the movie is displayed and which particular
## frames are shown:
##
## @table @asis
## @item First element:
##
## @table @asis
## @item @var{n}(1) > 0
## Play the movie @var{n}(1) times.
##
## @item @var{n}(1) < 0
## Play the movie @code{abs (@var{n}(1)} times alternatively in forward and
## backward order.
## @end table
##
## @item Other elements (if any):
## Indices of the frames in @var{mov} that will be displayed.
## @end table
##
## If the first argument is a handle to a figure or axes @var{h}, the movie is
## played in that figure or axes instead of the current axes.
##
## @seealso{getframe, im2frame, frame2im}
## @end deftypefn

function movie (varargin)

  if (nargin == 0)
    print_usage ();
  endif

  ## Extract possible handle argument
  hax = [];
  if (isaxes (varargin{1}))
    hax = varargin{1};
    varargin(1) = [];
  elseif (isfigure (varargin{1}))
    hax = get (varargin{1}, "currentaxes");
    if (isempty (hax))
      hax = axes ("parent", varargin{1});
    endif
    varargin(1) = [];
  endif

  ## Extract other arguments
  n = 1;
  fps = 12;
  idx = [];
  nargs = numel (varargin);
  if (nargs == 0)
    print_usage ();
  elseif (nargs >= 1)
    mov = varargin{1};
    if (! isfield (mov, "cdata") || ! isfield (mov, "colormap"))
      error ("movie: MOV must be a frame struct array");
    endif

    if (nargs >= 2)
      n = varargin{2};
      if (! isindex (abs (n(1))))
        error ("movie: N must be a non-zero integer");
      elseif (! isscalar (n))
        idx = n(2:end)(:)';
        n = n(1);
        if (! isindex (idx, numel (mov)))
          error (["movie: All elements N(2:end) must be valid indices ", ...
                  "into the MOV struct array"]);

        endif
      endif
        
      if (nargs >= 3)
        fps = varargin{3};
        if (! (isnumeric (fps) && isscalar (fps) && fps > 0))
          error ("movie: FPS must be a numeric scalar > 0");
        endif
      endif
    endif
  endif

  if (isempty (hax))
    hax = gca ();
  endif

  ## Build the list of frames to be displayed
  if (isempty (idx))
    idx = (1:numel (mov));
  endif

  if (n > 0)
    idx = repmat (idx, 1, n);
  else
    n = -n;
    tmp = repmat ([idx fliplr(idx)], 1, fix (n/2));
    if (fix (n/2) != n/2)
      idx = [tmp, idx];
    else
      idx = tmp;
    endif
  endif

  tau = 1/fps;
  set (hax, "ydir", "reverse", "visible", "off");
  tic ();
  him = image ("parent", hax, "cdata", mov(1).cdata);
  for ii = idx
    pause (tau - toc ());
    tic ();
    if (! isempty (mov(ii).colormap))
      set (hax, "colormap", mov(ii).colormap)
    endif
    set (him, "cdata", mov(ii).cdata);
  endfor
  
endfunction


%!demo
%! nframes = 20;
%! colors = jet (nframes);
%! baseim = ones (20, 20, 3, "uint8");
%! mov(nframes) = struct ("cdata", [], "colormap", []);
%! for ii = 1:nframes
%!   im = baseim * 255;
%!   im(:,ii,1) = colors(ii,1) * 255;
%!   im(:,ii,2) = colors(ii,2) * 255;
%!   im(:,ii,3) = colors(ii,3) * 255;
%!   mov(ii).cdata = im;
%! endfor
%! clf ();
%! title "Play movie forward 2 times"
%! movie (mov, 2);

%!demo
%! nframes = 20;
%! colors = jet (nframes);
%! baseim = ones (20, 20, 3, "uint8");
%! mov(nframes) = struct ("cdata", [], "colormap", []);
%! for ii = 1:nframes
%!   im = baseim * 255;
%!   im(:,ii,1) = colors(ii,1) * 255;
%!   im(:,ii,2) = colors(ii,2) * 255;
%!   im(:,ii,3) = colors(ii,3) * 255;
%!   mov(ii).cdata = im;
%! endfor
%! clf ();
%! title "Play movie forward and backward 5 times at 25 fps"
%! movie (mov, -5, 25);

%!demo
%! nframes = 20;
%! colors = jet (nframes);
%! baseim = ones (20, 20, 3, "uint8");
%! mov(nframes) = struct ("cdata", [], "colormap", []);
%! for ii = 1:nframes
%!   im = baseim * 255;
%!   im(:,ii,1) = colors(ii,1) * 255;
%!   im(:,ii,2) = colors(ii,2) * 255;
%!   im(:,ii,3) = colors(ii,3) * 255;
%!   mov(ii).cdata = im;
%! endfor
%! clf ();
%! title "Play downsampled movie 5 times" 
%! movie (mov, [5 1:3:nframes]);

%!demo
%! clf ();
%! z = sombrero ();
%! hs = surf (z);
%! axis manual
%! nframes = 50;
%! mov(nframes) = struct ("cdata", [], "colormap", []);
%! for ii = 1:nframes
%!   set (hs, "zdata", z * sin (2*pi*ii/nframes));
%!   mov(ii) = getframe ();
%! endfor
%! clf ();
%! movie (mov, 5, 25);

## Test input validation
%!error movie ()
%!error <MOV must be a frame struct array> movie ({2})
%!error <N must be a non-zero integer>
%! movie (struct ("cdata", [], "colormap", []), 2.3);
%!error <N must be a non-zero integer>
%! movie (struct ("cdata", [], "colormap", []), [2.3 -6]);
