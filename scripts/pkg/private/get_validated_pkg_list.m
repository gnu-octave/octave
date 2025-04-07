########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
## @deftypefn {} @var{__pkg__} = get_validated_pkg_list ()
## Download list of current packages and validate that it fits expected
## patterns.
##
## Return @code{__pkg__} struct used by other functions.
## @end deftypefn

function retval = get_validated_pkg_list ()

  ## The __pkg__ struct is what we return with all the package information.
  ## We make it persistent to avoid querying the server each time.
  persistent __pkg__;

  if (! isempty (__pkg__))
    ## This function has been called already and __pkg__ exists.
    ## No need to query the server again.
    retval = __pkg__;
    return;
  endif

  [list, succ] = urlread ("https://packages.octave.org/packages/");
  if (! succ)
    error ("pkg: could not read URL, please verify internet connection");
  endif

  ## `list` begins with the known HTML prefix "<pre>".
  ## If this fails, the rest of the code is likely not valid, so fail early.
  if (! strncmp (list, "<pre>", 5))
    error ("pkg: server returned data of unknown format");
  endif
  list(1:5) = [];

  ## Convert known HTML markup to text.
  list = strrep (list, "&gt;",  ">");
  list = strrep (list, "&lt;",  "<");
  list = strrep (list, "&amp;", "&");
  list = strrep (list, "&#39;", "'");

  ## The rest of `list` is a sequence of Octave assignment commands,
  ## meant for execution with `eval`.
  ##
  ## We *could* pass it straight to `eval`, but if packages.octave.org were
  ## to be compromised by a third party, then `list` might have malicious code
  ## like `system ("do_something_bad")`.
  ##
  ## As a basic precaution against mindlessly executing that with `eval`,
  ## we ensure that all the Octave code in `list` is only a set of assignment
  ## commands of a known format, and nothing else.
  ##
  ## This check also helps if the server is safe but the internet connection
  ## is unstable and causes `list` to be incomplete or malformed, so that
  ## we don't end up passing line noise to `eval`.
  ##
  ## FIXME Improve the following security checks.

  ## `list` should end with `%</pre>` with zero or more line breaks.
  ## First remove any trailing line breaks.
  while (list(end) == "\n")
    list(end) = [];
  endwhile

  ## Ensure known closing string.
  if (! strcmp (list(end-6:end), "%</pre>"))
    error ("pkg: server returned data of unknown format");
  endif
  list(end-6:end) = [];

  ## Remove any trailing line breaks again.
  while (list(end) == "\n")
    list(end) = [];
  endwhile

  ## Remove consecutive line breaks to help with further checks.
  do
    len = numel (list);
    list = strrep (list, "\n\n", "\n");
  until (numel (list) == len)

  ## Every statement must now be of the general format
  ##     __pkg__.FOO = BAR;
  ## so we check for `__pkg__.` at the start and after all line breaks.
  for f = [0, strfind(list, "\n")]  # all line breaks; 0 for the start
    if (! all (list((f+1) : (f+8)) == "__pkg__."))
      error ("pkg: server returned data of unknown format");
    endif
  endfor

  ## At this point, all lines start with the known string `__pkg__.`
  ## so we deem it safe from `system` and other funny business.
  eval (list);  # this creates a struct called `__pkg__`

  ## Verify that it exists and is a struct.
  ## FIXME: Do we really need these checks?  Won't eval above fail first?
  assert (exist ("__pkg__"));
  assert (class (__pkg__), "struct");

  retval = __pkg__;

endfunction
