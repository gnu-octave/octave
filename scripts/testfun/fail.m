## Copyright (C) 2005-2012 Paul Kienzle
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
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn  {Function File} {} fail (@var{code})
## @deftypefnx {Function File} {} fail (@var{code}, @var{pattern})
## @deftypefnx {Function File} {} fail (@var{code}, 'warning', @var{pattern})
##
## Return true if @var{code} fails with an error message matching
## @var{pattern}, otherwise produce an error.  Note that @var{code}
## is a string and if @var{code} runs successfully, the error produced is:
##
## @example
##           expected error but got none
## @end example
##
## If the code fails with a different error, the message produced is:
##
## @example
## @group
##           expected <pattern>
##           but got <text of actual error>
## @end group
## @end example
##
## The angle brackets are not part of the output.
##
## Called with three arguments, the behavior is similar to
## @code{fail(@var{code}, @var{pattern})}, but produces an error if no
## warning is given during code execution or if the code fails.
## @seealso{assert}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function ret = fail (code, pattern, warning_pattern)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  ## sort out arguments
  test_warning = (nargin > 1 && strcmp (pattern, "warning"));
  if (nargin == 3)
    pattern = warning_pattern;
  elseif (nargin == 1 || (nargin == 2 && test_warning))
    pattern = "";
  endif

  ## match any nonempty message
  if (isempty (pattern))
    pattern = ".";
  endif

  ## allow assert(fail())
  if (nargout)
    ret = 1;
  endif

  if (test_warning)
    ## Perform the warning test.
    ## Clear old warnings.
    lastwarn ();
    ## Make sure warnings are turned on.
    state = warning ("query", "quiet");
    warning ("on", "quiet");
    try
      ## printf("lastwarn before %s: %s\n",code,lastwarn);
      evalin ("caller", sprintf ("%s;", code));
      ## printf("lastwarn after %s: %s\n",code,lastwarn);
      ## Retrieve new warnings.
      err = lastwarn ();
      warning (state.state, "quiet");
      if (isempty (err))
        msg = sprintf ("expected warning <%s> but got none", pattern);
      else
        ## Transform "warning: ...\n" to "...".
        err([1:9, end]) = [];
        if (! isempty (regexp (err, pattern, "once")))
          return;
        endif
        msg = sprintf ("expected warning <%s>\nbut got <%s>", pattern, err);
      endif
    catch
      warning (state.state, "quiet");
      err = lasterr;
      ## Transform "error: ...\n", to "...".
      err([1:7, end]) = [];
      msg = sprintf ("expected warning <%s> but got error <%s>", pattern, err);
    end_try_catch

  else
    ## Perform the error test.
    try
      evalin ("caller", sprintf ("%s;", code));
      msg = sprintf ("expected error <%s> but got none", pattern);
    catch
      err = lasterr ();
      if (strcmp (err(1:7), "error:"))
         err([1:6, end]) = []; # transform "error: ...\n", to "..."
      endif
      if (! isempty (regexp (err, pattern, "once")))
        return;
      endif
      msg = sprintf ("expected error <%s>\nbut got <%s>", pattern, err);
    end_try_catch
  endif

  ## If we get here, then code didn't fail or error didn't match.
  error (msg);

endfunction


%!fail ('[1,2]*[2,3]', 'nonconformant')
%!fail ("fail('[1,2]*[2;3]', 'nonconformant')", "expected error <nonconformant> but got none")
%!fail ("fail('[1,2]*[2,3]','usage:')", "expected error <usage:>\nbut got.*nonconformant")
%!fail ("warning('test warning')", 'warning','test warning');

##% !fail ("warning('next test')",'warning','next test');  ## only allowed one warning test?!?

%% Test that fail() itself will generate an error
%!error fail ("1");
%!error <undefined> fail ('a*[2;3]', 'nonconformant')
%!error <expected error>  fail ('a*[2,3]', 'usage:')
%!error <warning failure> fail ("warning('warning failure')", 'warning', 'success')
