########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn  {} {} clearvars
## @deftypefnx {} {} clearvars @var{pattern} @dots{}
## @deftypefnx {} {} clearvars -regexp @var{pattern} @dots{}
## @deftypefnx {} {} clearvars @dots{} -except @var{pattern} @dots{}
## @deftypefnx {} {} clearvars @dots{} -except -regexp @var{pattern} @dots{}
## @deftypefnx {} {} clearvars -global @dots{}
## Delete the variables matching the given @var{pattern}s from memory.
##
## The @var{pattern} may contain the following special characters:
##
## @table @code
## @item ?
## Match any single character.
##
## @item *
## Match zero or more characters.
##
## @item [ @var{list} ]
## Match the list of characters specified by @var{list}.  If the first
## character is @code{!} or @code{^}, match all characters except those
## specified by @var{list}.  For example, the pattern @code{[a-zA-Z]} will
## match all lowercase and uppercase alphabetic characters.
## @end table
##
## If the @option{-regexp} option is given then subsequent patterns are treated
## as regular expressions and any matches will be cleared.
##
## If the @option{-except} option is given then subsequent patterns select
## variables that will @strong{not} be cleared.
##
## If the @option{-global} option is given then all patterns will be applied
## to global variables rather than local variables.
##
## When called with no arguments, @code{clearvars} deletes all local variables.
##
## Example Code:
##
## Clear all variables starting with @qcode{'x'} and the specific variable
## @qcode{"foobar"}
##
## @example
## clearvars x* foobar
## @end example
##
## Clear the specific variable @qcode{"foobar"} and use regular expressions to
## clear all variables starting with @qcode{'x'} or @qcode{'y'}.
##
## @example
## clearvars foobar -regexp ^x ^y
## @end example
##
## Clear all variables except for @qcode{"foobar"}
##
## @example
## clearvars -except foobar
## @end example
##
## Clear all variables beginning with @qcode{"foo"}, except for those ending
## in @qcode{"bar"}
##
## @example
## clearvars foo* -except -regexp bar$
## @end example
##
## @seealso{clear, who, whos, exist}
## @end deftypefn

function clearvars (varargin)

  numvar = 0;
  global_mode = false;
  except_mode = false;
  regexp_mode = false;

  ## Parse arguments
  for cellarg = varargin
    arg = cellarg{1};

    ## Parse options
    if (strcmp (arg, "-global"))
      if (numvar > 0)
        error ("clearvars: '-global' must be the first option when present");
      endif
      global_mode = true;
      continue;
    elseif (strcmp (arg, "-except"))
      if (except_mode)
        error ("clearvars: '-except' may only be specified once");
      endif
      except_mode = true;
      regexp_mode = false;
      continue;
    elseif (strcmp (arg, "-regexp"))
      regexp_mode = true;
      continue;
    endif

    ## Parse patterns
    numvar += 1;
    vars(numvar).except = except_mode;
    if (! regexp_mode)
      vars(numvar).var_name = [ '\<' regexptranslate("wildcard", arg) '\>' ];
    else
      vars(numvar).var_name = arg;
    endif

  endfor

  if (global_mode)
    varlist = evalin ("caller", "who ('global')");
  else
    varlist = evalin ("caller", "who ()");
  endif

  ## evalin will cause the automatic creation of 'ans' variable (bug #53339).
  ## Determine if it needs to be removed at the end of the function.
  clear_ans = ! any (strcmp (varlist, "ans"));

  if (numvar == 0 || all ([vars.except]))
    ## For wildcard, select all variables in list
    idx_clear = true (numel (varlist), 1);
  else
    ptn = strjoin ({ vars(! [vars.except]).var_name }, '|');
    idx_clear = ! cellfun (@isempty, regexp (varlist, ptn));
  endif

  if (numvar > 0 && any ([vars.except]))
    ptn = strjoin ({ vars([vars.except]).var_name }, '|');
    idx_except = ! cellfun (@isempty, regexp (varlist, ptn));
    idx_clear(idx_except) = false;
  endif

  varlist = varlist(idx_clear);
  names = strjoin (varlist, "', '");

  if (! isempty (names))
    if (global_mode)
      evalin ("caller", ["clear ('-global', '", names, "')"]);
    else
      evalin ("caller", ["clear ('", names, "')"]);
    endif
  endif

  ## Clean up automatic variable "ans" if necessary
  if (clear_ans)
    evalin ("caller", "clear ('ans')");
  endif

endfunction


## Tests must be done in a function namespace;
## Otherwise, they interfere with the BIST environment itself.
%!function __test_local_vars__ ()
%!  global x y z
%!  a = 1; b = 2; c = 3;
%!  assert (all (ismember ({"a"; "b"; "c"}, who ())));
%!  ## Test 0-argument form
%!  clearvars
%!  assert (isempty (who ()));
%!
%!  a = 1; a2 = 2; a33 = 3;
%!  ## Test special wildcard pattern
%!  clearvars a?3
%!  assert (isempty (who ("a33")));
%!
%!  a33 = 3;
%!  ## Test -regexp option
%!  clearvars -regexp 2 3$
%!  assert (who ("a*"), {"a"});
%!
%!  a = 1; a2 = 2; a33 = 3;
%!  ## Test -except option
%!  clearvars a* -except a33
%!  assert (who ("a", "a2", "a33"), {"a33"});
%!
%!  ## Test that non-regexp patterns only select full words
%!  clearvars a3
%!  assert (who ("a33"), {"a33"});
%!endfunction

%!function __test_global_vars__ ()
%!  global x y z
%!  a = 1; b = 2; c = 3;
%!  assert (all (ismember ({"x"; "y"; "z"}, who ("global"))));
%!  clearvars -global
%!  assert (isempty (who ("global")));
%!
%!  global x y z
%!  clearvars -global -regexp ^y
%!  assert (! any (strcmp ("y", who ("global"))));
%!endfunction

## Run BIST test functions
%!test __test_local_vars__ ();
%!test __test_global_vars__ ();

## Test input validation
%!error <'-global' must be the first option> clearvars ("ans", "-global")
%!error <'-except' may only be specified once> clearvars ("-except", "-except")

%!test
%!  clear -global x y z;  # cleanup after test
