########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{msg} =} javachk (@var{feature})
## @deftypefnx {} {@var{msg} =} javachk (@var{feature}, @var{caller})
## Check for the presence of the Java @var{feature} in the current session.
## Return an error structure if @var{feature} is not available, not enabled,
## or not recognized.
##
## Possible recognized features are:
##
## @table @asis
## @item @nospell{@qcode{"awt"}}
## Abstract Window Toolkit for GUIs.
##
## @item @qcode{"desktop"}
## Interactive desktop is running.
##
## @item @qcode{"jvm"}
## Java Virtual Machine.
##
## @item @qcode{"swing"}
## Swing components for lightweight GUIs.
## @end table
##
## If @var{feature} is not supported, a scalar struct with field
## @qcode{"message"} and @qcode{"identifier"} is returned.  The field
## @qcode{"message"} contains an error message mentioning @var{feature} as
## well as the optional user-specified @var{caller}.  This structure is
## suitable for passing in to the @code{error} function.
##
## If @var{feature} is supported and available, an empty struct array is
## returned with fields @qcode{"message"} and @qcode{"identifier"}.
##
## @code{javachk} determines if specific Java features are available in an
## Octave session.  This function is provided for scripts which may alter
## their behavior based on the availability of Java or specific Java runtime
## features.
##
## Compatibility Note: The feature @qcode{"desktop"} is never available since
## Octave has no Java-based desktop.
##
## @seealso{usejava, error}
## @end deftypefn

function msg = javachk (feature, caller = "")

  if (nargin < 1)
    print_usage ();
  elseif (! ischar (feature))
    error ("javachk: FEATURE must be a string");
  elseif (! ischar (caller))
    error ("javachk: CALLER must be a string");
  endif

  if (isempty (caller))
    caller = "this function";
  endif

  msg = struct ("message", "", "identifier", "");

  ## Check that Octave was compiled with Java support
  chk = logical (__octave_config_info__ ("build_features").JAVA);
  if (! chk)
    msg.message = sprintf (["javachk: %s is not supported, " ...
                            "Octave was not compiled with Java support"],
                            caller);
    msg.identifier = "Octave:javachk:java-not-supported";
  endif

  if (chk)
    chk = false;
    ## For each feature, try methods() on a Java class of a feature
    switch (feature)
      case "awt"
        try
          dum = methods ("java.awt.Frame");
          chk = true;
        end_try_catch
      case "desktop"
        ## Octave has no Java based GUI/desktop, leave chk = false
      case "jvm"
        try
          dum = methods ("java.lang.Runtime");
          chk = true;
        end_try_catch
      case "swing"
        try
          dum = methods ("javax.swing.Popup");
          chk = true;
        end_try_catch
      otherwise
        ## Unrecognized feature is the same as disabled feature
    endswitch

    if (! chk)
      msg.message = sprintf (['javachk: %s is not supported, Java feature '...
                              '"%s" is not available'], caller, feature);
      msg.identifier = "Octave:javachk:feature-not-available";
    endif

  endif

  if (isempty (msg.message))
    ## Compatibility: Matlab returns a 0x1 empty struct when javachk passes
    msg = resize (msg, 0, 1);
  endif

endfunction


%!testif ; ! __octave_config_info__ ().build_features.JAVA
%! msg = javachk ("desktop");
%! assert (msg.message, "javachk: this function is not supported, Octave was not compiled with Java support");
%! assert (msg.identifier, "Octave:javachk:java-not-supported");

%!testif HAVE_JAVA
%! msg = javachk ("desktop");
%! assert (msg.message, 'javachk: this function is not supported, Java feature "desktop" is not available');
%! assert (msg.identifier, "Octave:javachk:feature-not-available");

%!testif HAVE_JAVA
%! msg = javachk ("desktop", "Java DESKTOP");
%! assert (msg.message, 'javachk: Java DESKTOP is not supported, Java feature "desktop" is not available');
%! assert (msg.identifier, "Octave:javachk:feature-not-available");

%!testif HAVE_JAVA
%! msg = javachk ("nosuchfeature");
%! assert (msg.message, 'javachk: this function is not supported, Java feature "nosuchfeature" is not available');
%! assert (msg.identifier, "Octave:javachk:feature-not-available");

%!testif HAVE_JAVA; usejava ("jvm")
%! stnul = resize (struct ("message", "", "identifier", ""), 0, 1);
%! assert (javachk ("jvm"), stnul);

## Test input validation
%!error <Invalid call> javachk ()
%!error <FEATURE must be a string> javachk (1)
%!error javachk ("jvm", 2)
%!error javachk ("jvm", "feature", "ok")
