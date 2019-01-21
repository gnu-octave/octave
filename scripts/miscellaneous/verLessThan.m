## Copyright (C) 2019 Andrew Janke
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
## @deftypefn  {} {@var{out} =} verLessThan (@var{package}, @var{version})
## True if the installed version of the package is less than @var{version}.
##
## @var{package} is the name of the package to check the version of. Use
## @code{"Octave"} as the @var{package} to check the version of Octave itself.
##
## @var{version} is the version to compare it to. A version is a string in the
## format accepted by @ref{XREFcompare_versions, ,compare_versions function}:
## an arbitrarily long string made of numeric and period characters possibly
## followed by an arbitrary string (e.g., @code{"1.2.3"}, @code{"0.3"},
## @code{"0.1.2+"}, or @code{"1.2.3.4-test1"}).
##
## @example
## @group
## tf = verLessThan ("Octave", "5.0.0")
## tf = verLessThan ("io", "2.4.12")
##
## if (! verLessThan ("Octave", "5.0.0"))
##   ## ... use new Octave 5.0 features ...
## endif
## @end group
## @end example
##
## @seealso{compare_versions, version, ver, pkg}
## @end deftypefn

function out = verLessThan(package, version)

  if (! ischar (package) || size (package, 1) > 1)
    error ("verLessThan: package must be a char vector");
  endif

  v = ver ();
  idx = find (strcmpi (package, {v.Name}));
  if (isempty (idx))
    error ("verLessThan: Package ""%s"" is not installed.", package);
  endif

  out = compare_versions (v(idx).Version, version, "<");

endfunction

%!assert (! verLessThan ("Octave", "3.0.0"))
%!assert (verLessThan ("Octave", "99.9.9"))
%!error <Package "no-such-package" is not installed.>
%! verLessThan ("no-such-package", "1.1.1")
%!error <compare_versions: version numbers V1 and V2 must be strings>
%! verLessThan ("Octave", 4.1)
