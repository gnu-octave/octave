## Copyright (C) 2005-2012 William Poetra Yoga Hadisoeseno
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
## @deftypefn {Function File} {} license
## Display the license of Octave.
##
## @deftypefnx {Function File} {} license ("inuse")
## Display a list of packages currently being used.
##
## @deftypefnx {Function File} {@var{retval} =} license ("inuse")
## Return a structure containing the fields @code{feature} and @code{user}.
##
## @deftypefnx {Function File} {@var{retval} =} license ("test", @var{feature})
## Return 1 if a license exists for the product identified by the string
## @var{feature} and 0 otherwise.  The argument @var{feature} is case
## insensitive and only the first 27 characters are checked.
##
## @deftypefnx {Function File} {} license ("test", @var{feature}, @var{toggle})
## Enable or disable license testing for @var{feature}, depending on
## @var{toggle}, which may be one of:
##
## @table @asis
## @item "enable"
## Future tests for the specified license of @var{feature} are conducted
## as usual.
##
## @item "disable"
## Future tests for the specified license of @var{feature} return 0.
## @end table
##
## @deftypefnx {Function File} {@var{retval} =} license ("checkout", @var{feature})
## Check out a license for @var{feature}, returning 1 on success and 0
## on failure.
##
## This function is provided for compatibility with @sc{matlab}.
## @seealso{ver, version}
## @end deftypefn

## Author: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function retval = license (varargin)

  persistent __octave_licenses__;

  if (isempty (__octave_licenses__))
    __octave_licenses__ = cell ();
    __octave_licenses__{1,1} = "Octave";
    __octave_licenses__{1,2} = "GNU General Public License";
    __octave_licenses__{1,3} = true;
    if (exist ("OCTAVE_FORGE_VERSION"))
      __octave_licenses__{2,1} = "octave-forge";
      __octave_licenses__{2,2} = "<various licenses>";
      __octave_licenses__{2,3} = true;
    endif
  endif

  nout = nargout;
  nin = nargin;
  nr_licenses = rows (__octave_licenses__);

  if (nout > 1 || nin > 3)
    print_usage ();
  endif

  if (nin == 0)  

    found = find (strcmp (__octave_licenses__(:,1), "Octave"), 1);

    if (! isempty (found))
      result = __octave_licenses__{found,2};
    else
      result = "unknown";
    endif

    if (nout == 0)
      printf ("%s\n", result);
    else
      retval = result;
    endif

  elseif (nin == 1)

    if (nout == 0)

      if (! strcmp (varargin{1}, "inuse"))
        usage ('license ("inuse")');
      endif

      printf ("%s\n", __octave_licenses__{:,1});

    else

      if (! strcmp (varargin{1}, "inuse"))
        usage ('retval = license ("inuse")');
      endif

      pw = getpwuid (getuid ());
      if (isstruct (pw))
        username = pw.name;
      else
        username = "octave_user";
      endif

      retval = struct ("feature", __octave_licenses__(:,1), "user", username);

    endif

  else

    feature = varargin{2}(1:(min ([(length (varargin{2})), 27])));

    if (strcmp (varargin{1}, "test"))

      found = find (strcmpi (__octave_licenses__(:,1), feature), 1);

      if (nin == 2)
        retval = ! isempty (found) && __octave_licenses__{found,3};
      else
        if (! isempty (found))
          if (strcmp (varargin{3}, "enable"))
            __octave_licenses__{found,3} = true;
          elseif (strcmp (varargin{3}, "disable"))
            __octave_licenses__{found,3} = false;
          else
            error ("license: TOGGLE must be either 'enable' or 'disable'");
          endif
        else
          error ("license: FEATURE '%s' not found", feature);
        endif
      endif

    elseif (strcmp (varargin{1}, "checkout"))

      if (nin != 2)
        usage ('retval = license ("checkout", feature)');
      endif

      found = find (strcmpi (__octave_licenses__(:,1), feature), 1);

      retval = ! isempty (found) && __octave_licenses__{found,3};

    else
      print_usage ();
    endif

  endif

endfunction


%!assert (license(), "GNU General Public License")
%!assert ((license ("inuse")).feature, "Octave")

%!test
%! lstate = license ("test", "Octave");
%! license ("test", "Octave", "disable");
%! assert (license ("test", "Octave"), false);
%! license ("test", "Octave", "enable");
%! assert (license ("test", "Octave"), true);
%! if (lstate == false)
%!   license ("test", "Octave", "disable");
%! endif

%!assert (license ("checkout", "Octave"), true)

%% Test input validation
%!error license ("not_inuse")
%!error <TOGGLE must be either> license ("test", "Octave", "not_enable")
%!error <FEATURE 'INVALID' not found> license ("test", "INVALID", "enable")
%!error license ("not_test", "Octave", "enable")

