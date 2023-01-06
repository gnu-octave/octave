########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{fobj} =} inline (@var{str})
## @deftypefnx {} {@var{fobj} =} inline (@var{str}, @var{arg1}, @dots{})
## @deftypefnx {} {@var{fobj} =} inline (@var{str}, @var{n})
##
## This function is obsolete.  Use anonymous functions
## (@pxref{Anonymous Functions}) instead.
##
## Create an inline function object from the character string @var{str}.
##
## If called with a single argument, the arguments of the generated
## function are extracted from the function itself.  The generated
## function arguments will then be in alphabetical order.  It should be
## noted that i and j are ignored as arguments due to the ambiguity
## between their use as a variable and their use as an built-in constant.
## All arguments followed by a parenthesis are considered to be
## functions.  If no arguments are found, a function taking a single
## argument named @code{x} will be created.
##
## If the second and subsequent arguments are character strings, they
## are the names of the arguments of the function.
##
## If the second argument is an integer @var{n}, the arguments are
## @qcode{"x"}, @qcode{"P1"}, @dots{}, @qcode{"P@var{N}"}.
##
## @strong{Caution:} the use of @code{inline} is discouraged and it may
## be removed from a future version of Octave.  The preferred way to
## create functions from strings is through the use of anonymous
## functions (@pxref{Anonymous Functions}) or @code{str2func}.
## @seealso{argnames, formula, vectorize, str2func}
## @end deftypefn

function fobj = inline (expr, varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:legacy-function",
             "inline is obsolete; use anonymous functions instead\n");
  endif

  if (nargin == 0)
    print_usage ();
  endif

  if (! ischar (expr))
    error ("inline: EXPR must be a string");
  endif

  if (nargin == 1)
    args = parse_expr_for_args (expr);
  elseif (nargin == 2)
    n = varargin{1};
    if (isnumeric (n))
      if (isscalar (n) && fix (n) == n)
        if (n > 0)
          args = strsplit (["x", sprintf(":P%d", 1:n)], ":");
        else
          error ("inline: N must be a positive integer");
        endif
      else
        error ("inline: N must be an integer");
      endif
    else
      args = {"x"};
    endif
  elseif (iscellstr (varargin))
    args = varargin;
  else
    error ("inline: additional arguments must be strings");
  endif

  p.expr = expr;
  p.args = args(:);
  p.nargs = numel (args);
  p.fh = eval (sprintf ("@(%s) %s", strjoin (args(:), ","), expr));

  ## FIXME: Do we need these parts of inline struct anymore (4/6/22)?
  tmp = [args; num2cell(1:numel(args))];
  p.inputExpr = sprintf ("%s = INLINE_INPUTS_{%d}; ", tmp{:});
  p.isEmpty = false;
  p.version = 1;

  fobj = __inline_ctor__ (p);

endfunction

## The following function was translated directly from the original C++
## version.  Yes, it will be slow, but the use of inline functions is
## strongly discouraged anyway, and most expressions will probably be
## short.  It may also be buggy.  Well, don't use this object!  Use
## function handles instead!

function args = parse_expr_for_args (expr)

  persistent symbols_to_skip = {"i", "j", "NaN", "nan", "Inf", "inf", ...
                                "NA", "pi", "e", "eps"};

  is_arg = false;
  in_string = false;
  tmp_arg = "";
  i = 1;
  expr_length = length (expr);
  args = {};

  while (i <= expr_length)

    terminate_arg = false;
    c = expr(i++);

    if (in_string)
      if (c == "'" || c == '"')
        in_string = false;
      endif
    elseif (c == "'" || c == '"')
      in_string = true;
      if (is_arg)
        terminate_arg = true;
      endif
    elseif (! isalpha (c) && c != "_")
      if (! is_arg)
        continue;
      elseif (isdigit (c))
        tmp_arg(end+1) = c;
      else
        ## Before we do anything remove trailing whitespaces.
        while (i <= expr_length && isspace (c))
          c = expr(i++);
        endwhile

        ## Do we have a variable or a function?
        if (c != "(")
          terminate_arg = true;
        else
          tmp_arg = "";
          is_arg = false;
        endif
      endif
    elseif (! is_arg)
      if (c == "e" || c == "E")
        ## Possible number in exponent form, not arg.
        if (isdigit (expr(i)) || expr(i) == "-" || expr(i) == "+")
          continue;
        endif
      endif
      is_arg = true;
      tmp_arg(end+1) = c;
    else
      tmp_arg(end+1) = c;
    endif

    if (terminate_arg || (i == expr_length+1 && is_arg))
      have_arg = false;
      if (any (strcmp (tmp_arg, args)))
        have_arg = true;
      endif

      if (! (have_arg || any (strcmp (tmp_arg, symbols_to_skip))))
        args{end+1} = tmp_arg;
      endif

      tmp_arg = "";
      is_arg = false;
    endif

  endwhile

  ## Sort the arguments into ASCII order.
  args = sort (args);

  if (isempty (args))
    args = {"x"};
  endif

endfunction
