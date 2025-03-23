########################################################################
##
## Copyright (C) 2005-2025 The Octave Project Developers
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
## @deftypefn {} {@var{list} =} list_forge_packages ()
## Undocumented internal function.
## @end deftypefn

function retval = list_forge_packages ()

  [list, succ] = urlread ("https://packages.octave.org/packages/");
  if (! succ)
    error ("pkg: could not read URL, please verify internet connection");
  endif

  ## `list` begins with the known HTML prefix "<pre>".
  ## If this fails, the rest of the code is likely not valid, so fail early.
  if (! (numel (list) >= 5 && all (list(1:5) == "<pre>")))
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
  ## We *could* pass it straight to `eval`,
  ## but if packages.octave.org were to be compromised by a third party,
  ## then `list` might have malicious code like `system ("do_something_bad")`.
  ## For some basic precautions against blindly executing that with `eval`,
  ## we ensure that all the Octave code in `list` is only a set of
  ## assignment commands of a known format.
  ##
  ## This check also helps if the server is safe but the internet connection
  ## is unstable and causes `list` to be incomplete or malformed.
  ##
  ## FIXME Improve the following security checks.

  ## `list` should end with `%</pre>` with zero or more line breaks.
  ## First remove any trailing line breaks.
  while (list(end) == "\n")
    list(end) = [];
  endwhile

  ## Ensure known closing string.
  if (! all (list(end-6:end) == "%</pre>"))
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
  until (numel (list) == len);

  ## Every statement must now be of the format
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
  assert (exist ("__pkg__"));
  assert (class (__pkg__), "struct");

  pkgnames = fieldnames (__pkg__);

  formatmore = (nargout == 0);  # do further string formatting for display

  ## Determine whether each package can be installed by `pkg install -forge`.
  ## This is possible if `pkg` is listed as a prerequisite for that package.
  lgl = false (1, numel (pkgnames));
  for i = 1:numel (pkgnames)
    this = char (pkgnames(i));

    prereq = char (__pkg__.(this).versions(1).depends.name);
    lgl(i) = any (cell2mat (strfind (cellstr (prereq), "pkg")));

    if (formatmore)  # format output as a string

      ## Get version.
      ## In the case of multiple versions, versions(1) is the most recent.
      v = __pkg__.(this).versions(1).id;

      tmp = sprintf ("%s %s", this, v);

      if (lgl(i))
        ## FIXME Currently we do nothing for this case.
        ## If we want to give a helpful command on how to install the package,
        ## we could uncomment the following line:
        # tmp = [tmp, sprintf(' (command: `pkg install -forge %s`)', this)];
      else
        tmp = [tmp, " (download and install manually)"];
      endif

      retval(i, 1:numel (tmp)) = tmp;

    endif

  endfor

  if (! formatmore)  # we want only the package names not the versions.

    ## Return only those packages that can be installed with `pkg`.
    retval = char (pkgnames(lgl));

  else

    ## `retval` has already been built above for display.
    page_screen_output (false, "local");
    puts ("These are the current packages for Octave:\n");
    disp (retval);

  endif

endfunction
