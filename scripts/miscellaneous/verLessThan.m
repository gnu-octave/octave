########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn {} {@var{tf} =} verLessThan (@var{package}, @var{version})
## Return true if the installed version of the package is less than
## @var{version}.
##
## @var{package} is the name of the package to check.  Use @qcode{"Octave"} as
## the @var{package} to check the version of Octave itself.
##
## @var{version} is the version to compare it to.  A version is a string in the
## format accepted by @code{compare_versions}: an arbitrarily long string
## composed of numeric and period characters, possibly followed by an arbitrary
## string (e.g., @qcode{"1.2.3"}, @qcode{"0.3"}, @qcode{"0.1.2+"}, or
## @qcode{"1.2.3.4-test1"}).
##
## Examples:
##
## @example
## @group
## tf = verLessThan ("Octave", "5")
## @result{} tf = 0
##
## tf = verLessThan ("io", "2.4.12")
## @result{} ...
##
## if (! verLessThan ("Octave", "5"))
##   ## ... use new Octave 5 features ...
## endif
## @end group
## @end example
##
## @seealso{compare_versions, version, ver, pkg}
## @end deftypefn

function tf = verLessThan (package, version)

  if (nargin != 2)
    print_usage ();
  endif

  if (! ischar (package) || rows (package) != 1)
    error ("verLessThan: PACKAGE must be a string");
  endif

  v = ver ();
  idx = find (strcmpi (package, {v.Name}));
  if (isempty (idx))
    error ('verLessThan: package "%s" is not installed', package);
  endif

  tf = compare_versions (v(idx).Version, version, "<");

endfunction


%!assert (! verLessThan ("Octave", "0"))
%!assert (! verLessThan ("Octave", "6"))
%!assert (! verLessThan ("Octave", "3.0.0"))
%!assert (verLessThan ("Octave", "99.9.9"))

## Test input validation
%!error <Invalid call> verLessThan ()
%!error verLessThan ("a")
%!error verLessThan ("a", "1", "b")
%!error <package "no-such-package" is not installed>
%! verLessThan ("no-such-package", "1.1.1")
%!error <compare_versions: version numbers V1 and V2 must be strings>
%! verLessThan ("Octave", 4.1)
