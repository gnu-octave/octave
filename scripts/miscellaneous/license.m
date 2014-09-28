## Copyright (C) 2005-2013 William Poetra Yoga Hadisoeseno
## Copyright (C) 2014 Carnë Draug
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Command} {} license
## @deftypefnx {Command} {} license inuse
## @deftypefnx {Command} {} license inuse @var{feature}
## @deftypefnx {Function File} {} license ("inuse")
## @deftypefnx {Function File} {@var{retval} =} license ("inuse")
## @deftypefnx {Function File} {@var{retval} =} license ("test", @var{feature})
## @deftypefnx {Function File} {@var{retval} =} license ("checkout", @var{feature})
## @deftypefnx {Function File} {[@var{retval}, @var{errmsg}] =} license ("checkout", @var{feature})
## Get license information for Octave and Octave packages.
##
## GNU Octave is free software distributed under the GNU General Public
## License (GPL), and a license manager makes no sense.  This function is
## provided only for @sc{Matlab} compatibility.
##
## When called with no extra input arguments, it returns the Octave license,
## otherwise the first input defines the operation mode and must be one of
## the following strings: @code{inuse}, @code{test}, and @code{checkout}.
## The optional @var{feature} argument can either be @qcode{"octave"} (core),
## or an Octave package.
##
## @table @asis
## @item @qcode{"inuse"}
## Returns a list of loaded features, i.e., octave and the list of loaded
## packages.  If an output is requested, it returns a struct array with
## the fields @qcode{"feature"}, and @qcode{"user"}.
##
## @item @qcode{"test"}
## Return true if the specified @var{feature} is installed, false otherwise.
##
## An optional third argument @qcode{"enable"} or @qcode{"disable"} is
## accepted but ignored.
##
## @item @qcode{"checkout"}
## Return true if the specified @var{feature} is installed, false otherwise.
## An optional second output will have an error message if a package is not
## installed.
##
## @end table
##
## @seealso{pkg, ver, version}
## @end deftypefn

## Author: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function [retval, errmsg] = license (cmd, feature, toogle)

  if (nargin > 3)
    print_usage ();
  endif

  ## Then only give information about Octave core
  if (nargin == 0)
    retval = "GNU General Public License";
    return
  endif

  [features, desc, flags] = get_all_features ();

  switch tolower (cmd)
    case "inuse"
      if (nargin > 2)
        print_usage ();
      endif

      unloaded = cellfun (@(x) x.name, desc(strcmpi (flags, "Not loaded")),
                          "UniformOutput", false);
      features(ismember (features, unloaded)) = [];

      if (nargin > 1)
        features = features(strcmp (features, feature));
      endif
      if (nargout == 0)
        printf ("%s\n", features{:});
      else
        retval = struct ("feature", features, "user", get_username ());
      endif

    case "test"
      if (nargin < 2)
        print_usage ();
      endif

      if (nargin > 2)
        ## We ignore the toogle argument because... what's the point?  We
        ## don't need a license management system on Octave.  This function
        ## will return true, even if anyone tries to disabled a license.
        switch tolower (toogle)
          case "enable",  # do nothing
          case "disable", # do nothing
          otherwise,      error ("license: TOOGLE must be enable or disable");
        endswitch
      endif

      retval = any (strcmp (features, feature));

    case "checkout"
      ## I guess we could have the checkout command load packages but it's not
      ## really the same thing.  The closest we have is simply to check if
      ## there is a package with the feature name, and give an error if not.

      if (nargin != 2)
        print_usage ();
      endif

      retval = any (strcmp (features, feature));
      errmsg = "";

      if (! retval)
        errmsg = ["No package named \"" feature "\" installed"];
      endif

    otherwise
      print_usage ();
  endswitch

endfunction

function username = get_username ()
  pw = getpwuid (getuid ());
  if (isstruct (pw))
    username = pw.name;
  else
    username = "octave_user";
  endif
endfunction

function [features, desc, loaded] = get_all_features ()
  [desc, loaded] = pkg ("describe", "all");
  pkg_names = cellfun (@(x) x.name, desc, "UniformOutput", false);
  features = {"octave", pkg_names{:}};
endfunction

%!assert (license (), "GNU General Public License")
%!assert ((license ("inuse", "octave")).feature, "octave")

%!test
%! [desc, flags] = pkg ("describe", "all");
%! for idx = 1: numel (desc)
%!   name = desc{idx}.name;
%!   switch (flags{idx})
%!     case "Loaded"
%!       assert ((license ("inuse", name)).feature, name);
%!     case "Not loaded"
%!       rv = license ("inuse", name);
%!       assert (isstruct (rv));
%!       assert (all (ismember ({"feature", "user"}, fieldnames (rv))));
%!     otherwise
%!       error ("code in license out of date");
%!   endswitch
%! endfor

%!assert (license ("test", "octave"), true)
%!assert (license ("test", "not_a_valid package name"), false)

%!test
%! desc = pkg ("describe", "all");
%! for idx = 1: numel (desc)
%!   assert (license ("test", desc{idx}.name), true)
%! endfor

%!assert (license ("checkout", "octave"), true)

%!test
%! [s, e] = license ("checkout", "NOT_A_PACKAGE");
%! assert (e, "No package named \"NOT_A_PACKAGE\" installed");

%% Test input validation
%!error license ("not_inuse")
%!error license ("not_test", "octave", "enable")
%!error <TOOGLE must be enable or disable> license ("test", "octave", "invalid_toogle")

