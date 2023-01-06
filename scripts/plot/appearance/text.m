########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} text (@var{x}, @var{y}, @var{string})
## @deftypefnx {} {} text (@var{x}, @var{y}, @var{z}, @var{string})
## @deftypefnx {} {} text (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} text (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} text (@dots{})
## Create a text object with text @var{string} at position @var{x}, @var{y},
## (@var{z}) on the current axes.
##
## Multiple locations can be specified if @var{x}, @var{y}, (@var{z}) are
## vectors.  Multiple strings can be specified with a character matrix or
## a cell array of strings.
##
## Optional property/value pairs may be used to control the appearance of the
## text.
##
## If the first argument @var{hax} is an axes handle, then add text to this
## axes, rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to the
## created text objects.
##
## Example 1 : multi-line text via 3 different methods
##
## @example
## @group
## text (0.5, 0.8, @{"Line 1", "Line 2"@})
## text (0.5, 0.6, ["Line 1"; "Line 2"])
## text (0.5, 0.4, "Line 1\nLine 2")
## @end group
## @end example
##
## Example 2 : text at multiple locations
##
## @example
## @group
## text ([0.2, 0.2], [0.8, 0.6], "Same text at two locations")
## text ([0.4, 0.4], [0.8, 0.6], @{"Point 1 Text", "Point 2 text"@})
## text ([0.6, 0.6], [0.8, 0.6], @{@{"Point 1 Line 1", "Point 1 Line 2@},
##                                "Point 2 text"@})
## @end group
## @end example
##
## Example 2 : adjust appearance using text properties
##
## @example
## @group
## ht = text (0.5, 0.5, "Hello World", "fontsize", 20);
## set (ht, "color", "red");
## @end group
## @end example
##
## Programming Notes: The full list of properties is documented at
## @ref{Text Properties}.
##
## Any numeric entries in a cell array will be converted to text using
## @code{sprintf ("%g")}.  For more precise control of the appearance convert
## any numeric entries to strings using @code{num2str}, @code{sprintf}, etc.,
## before calling @code{text}.
## @seealso{gtext, title, xlabel, ylabel, zlabel}
## @end deftypefn

## Caution: The following code is rigged for Matlab compatibility and is
##          full of hidden assumptions.  Be very wary when modifying.

function h = text (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("text", varargin{:});

  nargs = nargin;
  offset = 0;

  if (nargs > 2 && isnumeric (varargin{1}) && isnumeric (varargin{2}))
    x = varargin{1};
    y = varargin{2};

    if (nargin > 3 && isnumeric (varargin{3}))
      z = varargin{3};
      offset = 4;
    else
      z = zeros (size (x));
      offset = 3;
    endif

    string = varargin{offset};
    varargin(1:offset) = [];

    nx = numel (x);
    ny = numel (y);
    nz = numel (z);
    if (nx != ny || nx != nz)
      error ("text: number of X, Y, and Z coordinates must match");
    endif

  else  # Only PROP/VALUE pairs
    x = y = z = 0;
    nx = ny = nz = 1;
    string = "";
  endif

  ## Any remaining inputs must occur as PROPERTY/VALUE pairs
  if (rem (numel (varargin), 2) != 0)
    print_usage ();
  endif

  ## String argument may be in PROP/VAL pair
  idx = find (strcmpi (varargin, "string"), 1);
  if (idx)
    string = varargin{idx+1};
    varargin(idx:idx+1) = [];
  endif

  ## Position argument may be in PROP/VAL pair
  idx = find (strcmpi (varargin, "position"), 1);
  if (idx)
    pos = varargin{idx+1};
    varargin(idx:idx+1) = [];
  else
    pos = [x(:), y(:), z(:)];
  endif

  ## Validate string argument
  if (ischar (string))

    do_keyword_repl = true;
    nt = rows (string);
    if (nx == 1 && (nt == 1 || nt == 0))
      ## Single text object with one line or empty line
      string = {string};
      nt = 1;
    elseif (nx == 1 && nt > 1)
      ## Single text object with multiple lines
      ## FIXME: "default" or "factory" as first row
      ##        should be escaped to "\default" or "\factory"
      ##        Other rows do not require escaping.
      do_keyword_repl = false;
      string = {string};
    elseif (nx > 1 && nt == nx)
      ## Multiple text objects with different strings
      string = cellstr (string);
    elseif (nx > 1 && nt == 1)
      ## Multiple text objects with same string
      string = repmat ({string}, [nx, 1]);
      nt = nx;
    else
      error ("text: invalid combination of points and text strings");
    endif

    ## Escape special keywords
    if (do_keyword_repl)
      string = regexprep (string, '^(default|factory)$', '\\$1');
    endif

  elseif (iscell (string))

    if (! iscellstr (string))
      ## Matlab compatibility: convert any numeric cells to strings
      string = cell2cellstr (string);
    endif

    nt = numel (string);
    if (nx == 1)
      ## Single text object with one or more lines
      string = {string};
      nt = 1;
    elseif (nx > 1 && nt == nx)
      ## Multiple text objects with different strings
    elseif (nx > 1 && nt == 1)
      ## Multiple text objects with same string
      string = repmat ({cellstr(string)}, [nx, 1]);
      nt = nx;
    else
      error ("text: invalid combination of points and text strings");
    endif

  else

    error ("text: STRING must be a character string or cell array of character strings");

  endif

  ## Select the correct axes
  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  ## Call __go_text__ to do the work
  htmp = zeros (nt, 1);
  if (nx == 1)
    ## Set varargin first, in case it changes units or interpreter properties.
    htmp = __go_text__ (hax, varargin{:}, "position", pos,
                             "string", string{1});
  else
    for n = 1:nt
      htmp(n) = __go_text__ (hax, varargin{:}, "position", pos(n,:),
                                  "string", string{n});
    endfor
    __request_drawnow__ ();
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction

## Helper function converts any numeric entries to strings
function cstr = cell2cellstr (c)
  cstr = c;
  idx = cellfun (@isnumeric, c);
  cstr(idx) = cellfun (@(x) sprintf ("%g", x), c(idx), "uniformoutput", false);
endfunction


%!demo
%! clf;
%! ha = {"left", "center", "right"};
%! va = {"bottom", "middle", "top"};
%! x = [0.25 0.5 0.75];
%! y = x;
%! for t = 0:30:359;
%!   for nh = 1:numel (ha)
%!     for nv = 1:numel (va)
%!       text (x(nh), y(nv), "Hello World", ...
%!             "rotation", t, ...
%!             "horizontalalignment", ha{nh}, ...
%!             "verticalalignment", va{nv});
%!     endfor
%!   endfor
%! endfor
%! set (gca, "xtick", [0.25, 0.5, 0.75], ...
%!           "xticklabel", ha, ...
%!           "ytick", [0.25, 0.5, 0.75], ...
%!           "yticklabel", va);
%! axis ([0 1 0 1]);
%! xlabel ("horizontal alignment");
%! ylabel ("vertical alignment");
%! title ("text alignment and rotation (0:30:360 degrees)");

%!demo
%! clf;
%! h = mesh (peaks, "edgecolor", 0.7 * [1 1 1], ...
%!                  "facecolor", "none", ...
%!                  "facealpha", 0);
%! colors = jet (9);
%! ii = 1;
%! for t = 0:45:359;
%!   ii = ii +1;
%!   text (25, 25, 0, "Vertical Alignment = Bottom", ...
%!                    "rotation", t, ...
%!                    "horizontalalignment", "left", ...
%!                    "backgroundcolor", colors(ii,:), ...
%!                    "edgecolor", "k", ...
%!                    "verticalalignment", "bottom");
%! endfor
%! caxis ([-100 100]);
%! title ("Vertically Aligned at Bottom");

%!demo
%! clf;
%! axis ([0 8 0 8]);
%! title (["1st title";"2nd title"]);
%! xlabel (["1st xlabel";"2nd xlabel"]);
%! ylabel (["1st ylabel";"2nd ylabel"]);
%! text (4, 4, {"Hello", "World"}, ...
%!       "horizontalalignment", "center", ...
%!       "verticalalignment", "middle");
%! grid on;

%!demo
%! clf;
%! h = mesh (peaks (), "edgecolor", 0.7 * [1 1 1], ...
%!                     "facecolor", "none", ...
%!                     "facealpha", 0);
%! title (["1st title";"2nd title"]);
%! xlabel (["1st xlabel";"2nd xlabel"]);
%! ylabel (["1st ylabel";"2nd ylabel"]);
%! zlabel (["1st zlabel";"2nd zlabel"]);
%! text (0, 0, 5, {"Hello", "World"}, ...
%!       "horizontalalignment", "center", ...
%!       "verticalalignment", "middle");
%! hold on;
%! plot3 (0, 0, 5, "+k");

%!demo
%! clf;
%! h = text (0.5, 0.3, "char");
%! h = text (0.5, 0.4, ["char row 1"; "char row 2"]);
%! h = text (0.5, 0.6, {"cell2str (1,1)", "cell2str (1,2)"; "cell2str (2,1)", "cell2str (2,2)"});
%! h = text (0.5, 0.8, "foobar");
%! set (h, "string", 1:3);
%! h = text ([0.1, 0.1], [0.3, 0.4], "one string & two objects");
%! h = text ([0.1, 0.1], [0.5, 0.6], {"one cellstr & two objects"});
%! h = text ([0.1, 0.1], [0.7, 0.8], {"cellstr 1 object 1", "cellstr 2 object 2"});
%! h = text ([0.1, 0.1], [0.1, 0.2], ["1st string & 1st object"; "2nd string & 2nd object"]);
%! h = text (0.7, 0.6, "single string");
%! h = text (0.7, 0.5, {"single cell-string"});
%! xlabel (1:2);
%! ylabel (1:2);
%! title (1:2);

%!demo
%! clf;
%! title ("Use of the \"interpreter\" property")
%! xlim ([0 1])
%! ylim ([-1 1])
%! text (0.1, 0.5, "\"none\": erf(x) = (2/\\pi^{1/2})\\int_0^x e^{y^2}dy", ...
%!       "interpreter", "none", ...
%!       "fontsize", 20, ...
%!       "backgroundcolor", "r");
%! text (0.1, 0, "\"tex\"(def.): erf(x) = (2/\\pi^{1/2})\\int_0^x e^{y^2}dy", ...
%!       "fontsize", 20, ...
%!       "backgroundcolor", "r");
%! text (0.1, -0.5, "\"latex\": $erf(x) = (2/\\pi^{1/2})\\int_0^x e^{y^2}dy$", ...
%!       "interpreter", "latex", ...
%!       "fontsize", 20, ...
%!       "backgroundcolor", "r");
%! grid on;

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ## Single object with one line
%!   h = text (0.5, 0.5, "single object with one line");
%!   obs = get (h, "string");
%!   assert (class (obs), "char");
%!   assert (obs, "single object with one line");
%!
%!   ## Single object with multiple lines
%!   h = text (0.5, 0.3, ["char row 1"; "char row 2"]);
%!   obs = get (h, "string");
%!   assert (class (obs), "char");
%!   assert (obs, ["char row 1"; "char row 2"]);
%!
%!   ## Multiple objects with single line
%!   h = text ([0.1, 0.1], [0.3, 0.4], "two objects with same string");
%!   assert (class (get (h(1), "string")), "char");
%!   assert (class (get (h(2), "string")), "char");
%!   assert (get (h(1), "string"), "two objects with same string");
%!   assert (get (h(2), "string"), "two objects with same string");
%!
%!   ## Multiple objects with multiple lines
%!   h = text ([0.7, 0.7], [0.3, 0.4], ["string1"; "string2"]);
%!   assert (class (get (h(1), "string")), "char");
%!   assert (class (get (h(2), "string")), "char");
%!   assert (get (h(1), "string"), "string1");
%!   assert (get (h(2), "string"), "string2");
%!
%!   ## Test special keyword processing
%!   h = text (0.5, 0.5, "default");
%!   assert (get (h, "string"), "default");
%!   h = text (0.5, 0.5, "factory");
%!   assert (get (h, "string"), "factory");
%!
%!   ## Test special null ("") string
%!   h = text (0.5, 0.5, "");
%!   assert (get (h, "string"), "");
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Tests repeated with cell input ##
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ## Single object with one line
%!   h = text (0.5, 0.5, {"single object with one line"});
%!   obs = get (h, "string");
%!   assert (class (obs), "cell");
%!   assert (obs, {"single object with one line"});
%!
%!   # Single object with multiple lines
%!   h = text (0.5, 0.3, {"cell2str (1,1)", "cell2str (1,2)";
%!                        "cell2str (2,1)", "cell2str (2,2)"});
%!   obs = get (h, "string");
%!   assert (class (obs), "cell");
%!   assert (obs, {"cell2str (1,1)"; "cell2str (2,1)";
%!                 "cell2str (1,2)"; "cell2str (2,2)"});
%!
%!   ## Single object with multiple lines including empty cell
%!   h = text (0.5, 0.9, {"Line1"; []; "Line3"});
%!   obs = get (h, "string");
%!   assert (class (obs), "cell");
%!   assert (obs, {"Line1"; ""; "Line3"});
%!
%!   ## Multiple objects with single line
%!   h = text ([0.1, 0.1], [0.5, 0.6], {"two objects with same cellstr"});
%!   assert (class (get (h(1), "string")), "cell");
%!   assert (class (get (h(2), "string")), "cell");
%!   assert (get (h(1), "string"), {"two objects with same cellstr"});
%!   assert (get (h(2), "string"), {"two objects with same cellstr"});
%!
%!   ## Multiple objects with multiple lines
%!   h = text ([0.1, 0.1], [0.7, 0.8], {"cellstr1", "cellstr2"});
%!   assert (class (get (h(1), "string")), "char");
%!   assert (class (get (h(2), "string")), "char");
%!   assert (get (h(1), "string"), "cellstr1");
%!   assert (get (h(2), "string"), "cellstr2");
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test <*54067>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ht = text ("String", "Hello", "Position", [0.5, 0.5], "FontSize", 16);
%!   assert (get (ht, "String"), "Hello");
%!   assert (get (ht, "FontSize"), 16);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test <*54109>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ht = text ();
%!   assert (get (ht, "string"), "");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test <*56395>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ht = text (.3, .8, {"Hello", 'world', [], 1e7});
%!   assert (get (ht, "string"), {"Hello"; "world"; ""; "1e+07"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <X, Y, and Z coordinates must match> text (1, [2 3], "foobar")
%!error <X, Y, and Z coordinates must match> text (1, 2, [3 4], "foobar")
%!error <Invalid call to text> text (1,2, "text", "opt1")
%!error <invalid combination> text ([1 2], [3, 4], ['a'; 'b'; 'c'])
%!error <invalid combination> text ([1 2], [3, 4], {'a', 'b', 'c'})
%!error <STRING must be a character string> text (1, 2, 3)
