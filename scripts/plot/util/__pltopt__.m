########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{options}, @var{valid}] =} __pltopt__ (@var{caller}, @var{opt}, @var{err_on_invalid})
##
## Decode plot option strings.
##
## @var{opt} can currently be some combination of the following:
##
## @table @asis
## @item @qcode{"-"}
## For solid linestyle (default).
##
## @item @qcode{"--"}
## For dashed line style.
##
## @item @qcode{"-."}
## For linespoints plot style.
##
## @item @qcode{":"}
## For dots plot style.
##
## @item  @qcode{"r"}
## @itemx @qcode{"red"}
## Red line color.
##
## @item  @qcode{"g"}
## @itemx @qcode{"green"}
## Green line color.
##
## @item  @qcode{"b"}
## @itemx @qcode{"blue"}
## Blue line color.
##
## @item  @qcode{"c"}
## @itemx @qcode{"cyan"}
## Cyan line color.
##
## @item  @qcode{"m"}
## @itemx @qcode{"magenta"}
## Magenta line color.
##
## @item  @qcode{"y"}
## @itemx @qcode{"yellow"}
## Yellow line color.
##
## @item  @qcode{"k"}
## @itemx @qcode{"black"}
## Black line color.
##
## @item  @qcode{"w"}
## @itemx @qcode{"white"}
## White line color.
##
## @item @qcode{";displayname;"}
## The text between semicolons is used to set the @qcode{"displayname"}
## property which determines the label used for the plot legend.
##
## @item  @qcode{"+"}
## @itemx @qcode{"o"}
## @itemx @qcode{"*"}
## @itemx @qcode{"."}
## @itemx @qcode{"x"}
## @itemx @qcode{"|"}
## @itemx @qcode{"_"}
## @itemx @qcode{"s"}
## @itemx @qcode{"d"}
## @itemx @qcode{"^"}
## @itemx @qcode{"v"}
## @itemx @qcode{">"}
## @itemx @qcode{"<"}
## @itemx @qcode{"p"}
## @itemx @qcode{"h"}
## Used in combination with the points or linespoints styles, set the point
## style.
## @end table
##
## @end deftypefn

function [options, valid] = __pltopt__ (caller, opt, err_on_invalid = true)

  if (ischar (opt))
    opt = cellstr (opt);
  elseif (! iscellstr (opt))
    ## FIXME: This is an internal function.  Can't we rely on valid input?
    error ("__pltopt__: argument must be a character string or cell array of character strings");
  endif

  nel = numel (opt);

  if (nel)
    for i = nel:-1:1
      [options(i), valid] = decode_linespec (caller, opt{i}, err_on_invalid);
      if (! err_on_invalid && ! valid)
        return;
      endif
    endfor
  else
    options = __default_plot_options__ ();
    valid = true;
  endif

endfunction

## Really decode plot option strings.
function [options, valid] = decode_linespec (caller, opt, err_on_invalid)

  persistent default_options = __default_plot_options__ ();

  options = default_options;
  valid = true;

  have_linestyle = false;
  have_marker = false;

  ## If called by __errplot__, extract the linestyle before proceeding.
  if (strcmp (caller, "__do_errplot__"))
    if (strncmp (opt, "#~>", 3))
      n = 3;
    elseif (strncmp (opt, "#~", 2) || strncmp (opt, "~>", 2))
      n = 2;
    elseif (strncmp (opt, "~", 1) || strncmp (opt, ">", 1)
            || strncmp (opt, "#", 1))
      n = 1;
    else
      n = 0;
    endif
    options.errorstyle = opt(1:n);
    opt(1:n) = [];
  else
    options.errorstyle = "~";
  endif

  while (! isempty (opt))
    topt = opt(1);
    n = 1;

    if (any (topt == "0":"6"))
      warning ("Octave:deprecated-option", ...
               ["%s: using numbers to select line colors is deprecated.  ", ...
                "Use the corresponding color identifier instead."], caller);
    endif

    ## LineStyles
    if (strncmp (opt, "--", 2) || strncmp (opt, "-.", 2))
      options.linestyle = opt(1:2);
      have_linestyle = true;
      n = 2;
    elseif (topt == "-" || topt == ":")
      have_linestyle = true;
      options.linestyle = topt;
    ## Markers
    elseif (any (topt == "+o*.x|_sd^v><ph"))
      have_marker = true;
      ## Check for long form marker styles
      if (any (topt == "sdhp"))
        if (strncmp (opt, "square", 6))
          n = 6;
        elseif (strncmp (opt, "diamond", 7))
          n = 7;
        elseif (strncmp (opt, "hexagram", 8))
          n = 8;
        elseif (strncmp (opt, "pentagram", 9))
          n = 9;
        endif
      endif
      ## Backward compatibility.  Leave undocumented.
      if (topt == "@")
        warning ("Octave:deprecated-option", ...
                 "%s: marker type '@' is deprecated.  Use '+' instead.", ...
                 caller);
        topt = "+";
      endif
      options.marker = topt;
    ## Numeric color specs are for backward compatibility.  Don't document.
    elseif (topt == "k" || topt == "0")
      options.color = [0, 0, 0];
    elseif (topt == "r" || topt == "1")
      if (strncmp (opt, "red", 3))
        n = 3;
      endif
      options.color = [1, 0, 0];
    elseif (topt == "g" || topt == "2")
      if (strncmp (opt, "green", 5))
        n = 5;
      endif
      options.color = [0, 1, 0];
    elseif (topt == "b" || topt == "3")
      if (strncmp (opt, "black", 5))
        options.color = [0, 0, 0];
        n = 5;
      elseif (strncmp (opt, "blue", 4))
        options.color = [0, 0, 1];
        n = 4;
      else
        options.color = [0, 0, 1];
      endif
    elseif (topt == "y")
      if (strncmp (opt, "yellow", 6))
        n = 6;
      endif
      options.color = [1, 1, 0];
    elseif (topt == "m" || topt == "4")
      if (strncmp (opt, "magenta", 7))
        n = 7;
      endif
      options.color = [1, 0, 1];
    elseif (topt == "c" || topt == "5")
      if (strncmp (opt, "cyan", 4))
        n = 4;
      endif
      options.color = [0, 1, 1];
    elseif (topt == "w" || topt == "6")
      if (strncmp (opt, "white", 5))
        n = 5;
      endif
      options.color = [1, 1, 1];
    elseif (isspace (topt))
      ## Do nothing.
    elseif (topt == ";")
      t = index (opt(2:end), ";");
      if (t)
        options.key = opt(2:t);
        n = t+1;
      else
        if (err_on_invalid)
          error ("%s: unfinished key label", caller);
        else
          valid = false;
          options = default_options;
          return;
        endif
      endif
    else
      if (err_on_invalid)
        error ("%s: unrecognized format character: '%s'", caller, topt);
      else
        valid = false;
        options = default_options;
        return;
      endif
    endif

    opt(1:n) = [];  # Delete decoded portion
  endwhile

  if (! have_linestyle && have_marker)
    options.linestyle = "none";
  endif

  if (have_linestyle && ! have_marker)
    options.marker = "none";
  endif

endfunction


## Only cursory testing.  Real testing done by appearance of plots.
%!test
%! opts = __pltopt__ ("abc", "");
%! assert (opts.color, []);
%! assert (opts.linestyle, []);
%! assert (opts.marker, []);
%! assert (opts.key, "");
%!test
%! opts = __pltopt__ ("abc", "r:x");
%! assert (opts.color, [1 0 0]);
%! assert (opts.linestyle, ":");
%! assert (opts.marker, "x");
%!test
%! opts = __pltopt__ ("abc", "-.blackx");
%! assert (opts.color, [0 0 0]);
%! assert (opts.linestyle, "-.");
%! assert (opts.marker, "x");
%!test
%! opts = __pltopt__ ("abc", "gsquare");
%! assert (opts.color, [0 1 0]);
%! assert (opts.linestyle, "none");
%! assert (opts.marker, "s");
%!test
%! opts = __pltopt__ ("abc", ";Title;");
%! assert (opts.key, "Title");
%! assert (opts.color, []);
%! assert (opts.linestyle, []);
%! assert (opts.marker, []);
%!test
%! opts = __pltopt__ ("__do_errplot__", "~>r");
%! assert (opts.errorstyle, "~>");
%! assert (opts.color, [1 0 0 ]);
%! assert (opts.linestyle, []);
%! assert (opts.marker, []);

## Test input validation
%!error <argument must be a character string or cell array> __pltopt__ ("abc", 1)
%!error <unfinished key label> __pltopt__ ("abc", "rx;my_title", true)
%!error <unrecognized format character: 'u'> __pltopt__ ("abc", "u", true)
