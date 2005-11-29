## Copyright (C) 2005 William Poetra Yoga Hadisoeseno
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} license
## @deftypefnx {Function File} {} license ("inuse")
## @deftypefnx {Function File} {@var{retval}} = license ("inuse")
## @deftypefnx {Function File} {@var{retval}} = license ("test", @var{feature})
## @deftypefnx {Function File} {} license ("test", @var{feature}, @var{toggle})
## @deftypefnx {Function File} {@var{retval}} = license ("checkout", @var{feature})
## Display the license of Octave.
##
## @itemize
##
## @item
## @code{license} displays the license of Octave (GNU GPL v2).
##
## @item
## @code{license ("inuse")} also displays a list of products currently
## being used.
##
## @item
## @code{@var{retval} = license ("inuse")} returns a structure which has
## two fields: @code{feature} is the product name ("Octave") and
## @code{user} is the current username (only works on Unix systems; on
## Windows systems it contains "octave_user").
##
## @item
## @code{@var{retval} = license ("test", @var{feature})} returns 1 if a
## license exists for the product identified by the string @var{feature}
## and 0 otherwise. @var{feature} is case insensitive and only the first
## 27 characters are checked.
##
## @item
## @code{license ("test", @var{feature}, @var{toggle})} enables or disables
## license testing for @var{feature}, depending on @var{toggle}, which
## can be one of:
##
## @table @samp
## @item "enable"
## Future tests for the specified license of @var{feature} are conducted
## as usual.
## @item "disable"
## Future tests for the specified license of @var{feature} return 0.
## @end table
##
## @item
## @code{@var{retval} = license ("checkout", @var{feature})} checks out
## a license for @var{feature}, returning 1 on success and 0 on failure.
##
## @end itemize
##
## @end deftypefn
## @seealso{ver, version}

## Author: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function retval = license (varargin)

  global __octave_licenses__

  if (isempty (__octave_licenses__))
    __octave_licenses__ = cell ();
    __octave_licenses__{1,1} = "Octave";
    __octave_licenses__{1,2} = "GNU General Public License, Version 2";
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
    error ("type `help license' for usage info");
  endif

  if (nin == 0)

    if (nout == 0)

      found = false;
      for p = 1:nr_licenses
        if (strcmp (__octave_licenses__{p,1}, "Octave"))
          found = true;
          break;
        endif
      endfor

      if (found)
        disp (__octave_licenses__{p,2});
      else
        disp ("unknown");
      endif

    else

      usage ("license");

    endif

  elseif (nin == 1)

    if (nout == 0)

      if (! strcmp (varargin{1}, "inuse"))
        usage ("license (\"inuse\")");
      endif

      for p = 1:nr_licenses
        disp (__octave_licenses__{p,1});
      endfor

    else

      if (! strcmp (varargin{1}, "inuse"))
        usage ("retval = license (\"inuse\")");
      endif

      if (isunix)
        [t, username] = unix ("id -un");
        if (t == 0)
          username = username(1:end-1);
        else
          username = "octave_user";
        endif
      else
        username = "octave_user";
      endif

      retval(1:nr_licenses) = struct ("feature", "", "user", "");
      for p = 1:nr_licenses
        retval(p).feature = __octave_licenses__{p,1};
        retval(p).user = username;
      endfor

    endif

  else

    feature = varargin{2}(1:(min ([(length (varargin{2})), 27])));

    if (strcmp (varargin{1}, "test"))

      found = false;
      for p = 1:nr_licenses
        if (strcmpi (feature, __octave_licenses__{p,1}))
          found = true;
          break;
        endif
      endfor

      if (nin == 2)
        retval = found && __octave_licenses__{p,3};
      else
        if (found)
          if (strcmp (varargin{3}, "enable"))
            __octave_licenses__{p,3} = true;
          elseif (strcmp (varargin{3}, "disable"))
            __octave_licenses__{p,3} = false;
          else
            error ("toggle must be either `enable' of `disable'");
          endif
        else
          error ("feature `%s' not found", feature);
        endif
      endif

    elseif (strcmp (varargin{1}, "checkout"))

      if (nin != 2)
        usage ("retval = license (\"checkout\", feature)");
      endif

      found = false;
      for p = 1:nr_licenses
        if (strcmpi (feature, __octave_licenses__{p,1}))
          found = true;
          break;
        endif
      endfor

      retval = found && __octave_licenses__{p,3};

    else

      error ("type `help license' for usage info");

    endif

  endif

endfunction
