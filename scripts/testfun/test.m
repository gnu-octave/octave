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

## -*- texinfo -*-
## @deftypefn  {Command} {} test @var{name}
## @deftypefnx {Command} {} test @var{name} quiet|normal|verbose
## @deftypefnx {Function File} {} test ('@var{name}', 'quiet|normal|verbose', @var{fid})
## @deftypefnx {Function File} {} test ([], 'explain', @var{fid})
## @deftypefnx {Function File} {@var{success} =} test (@dots{})
## @deftypefnx {Function File} {[@var{n}, @var{max}] =} test (@dots{})
## @deftypefnx {Function File} {[@var{code}, @var{idx}] =} test ('@var{name}', 'grabdemo')
##
## Perform tests from the first file in the loadpath matching @var{name}.
## @code{test} can be called as a command or as a function.  Called with
## a single argument @var{name}, the tests are run interactively and stop
## after the first error is encountered.
##
## With a second argument the tests which are performed and the amount of
## output is selected.
##
## @table @asis
## @item 'quiet'
##  Don't report all the tests as they happen, just the errors.
##
## @item 'normal'
## Report all tests as they happen, but don't do tests which require
## user interaction.
##
## @item 'verbose'
## Do tests which require user interaction.
## @end table
##
## The argument @var{fid} can be used to allow batch processing.  Errors
## can be written to the already open file defined by @var{fid}, and
## hopefully when Octave crashes this file will tell you what was happening
## when it did.  You can use @code{stdout} if you want to see the results as
## they happen.  You can also give a file name rather than an @var{fid}, in
## which case the contents of the file will be replaced with the log from
## the current test.
##
## Called with a single output argument @var{success}, @code{test} returns
## true if all of the tests were successful.  Called with two output arguments
## @var{n} and @var{max}, the number of successful tests and the total number
## of tests in the file @var{name} are returned.
##
## If the second argument is the string 'grabdemo', the contents of the demo
## blocks are extracted but not executed.  Code for all code blocks is
## concatenated and returned as @var{code} with @var{idx} being a vector of
## positions of the ends of the demo blocks.
##
## If the second argument is 'explain', then @var{name} is ignored and an
## explanation of the line markers used is written to the file @var{fid}.
## @seealso{assert, fail, error, demo, example}
## @end deftypefn

## FIXME: * Consider using keyword fail rather then error?  This allows us
## to make a functional form of error blocks, which means we
## can include them in test sections which means that we can use
## octave flow control for both kinds of tests.

function [__ret1, __ret2, __ret3, __ret4] = test (__name, __flag, __fid)
  ## Information from test will be introduced by "key".
  persistent __signal_fail =  "!!!!! ";
  persistent __signal_empty = "????? ";
  persistent __signal_block = "  ***** ";
  persistent __signal_file =  ">>>>> ";
  persistent __signal_skip = "----- ";

  __xfail = 0;
  __xskip = 0;

  if (nargin < 2 || isempty (__flag))
    __flag = "quiet";
  endif
  if (nargin < 3)
    __fid = [];
  endif
  if (nargin < 1 || nargin > 3
      || (! ischar (__name) && ! isempty (__name)) || ! ischar (__flag))
    print_usage ();
  endif
  if (isempty (__name) && (nargin != 3 || ! strcmp (__flag, "explain")))
    print_usage ();
  endif
  __batch = (! isempty (__fid));

  ## Decide if error messages should be collected.
  __close_fid = 0;
  if (__batch)
    if (ischar (__fid))
      __fid = fopen (__fid, "wt");
      if (__fid < 0)
        error ("test: could not open log file");
      endif
      __close_fid = 1;
    endif
    fprintf (__fid, "%sprocessing %s\n", __signal_file, __name);
    fflush (__fid);
  else
    __fid = stdout;
  endif

  if (strcmp (__flag, "normal"))
    __grabdemo = 0;
    __rundemo = 0;
    __verbose = __batch;
  elseif (strcmp (__flag, "quiet"))
    __grabdemo = 0;
    __rundemo = 0;
    __verbose = 0;
  elseif (strcmp (__flag, "verbose"))
    __grabdemo = 0;
    __rundemo = 1;
    __verbose = 1;
  elseif (strcmp (__flag, "grabdemo"))
    __grabdemo = 1;
    __rundemo = 0;
    __verbose = 0;
    __demo_code = "";
    __demo_idx = [];
  elseif (strcmp (__flag, "explain"))
    fprintf (__fid, "# %s new test file\n", __signal_file);
    fprintf (__fid, "# %s no tests in file\n", __signal_empty);
    fprintf (__fid, "# %s test had an unexpected result\n", __signal_fail);
    fprintf (__fid, "# %s code for the test\n", __signal_block);
    fprintf (__fid, "# Search for the unexpected results in the file\n");
    fprintf (__fid, "# then page back to find the file name which caused it.\n");
    fprintf (__fid, "# The result may be an unexpected failure (in which\n");
    fprintf (__fid, "# case an error will be reported) or an unexpected\n");
    fprintf (__fid, "# success (in which case no error will be reported).\n");
    fflush (__fid);
    if (__close_fid)
      fclose(__fid);
    endif
    return;
  else
    error ("test: unknown flag '%s'", __flag);
  endif

  ## Locate the file to test.
  __file = file_in_loadpath (__name, "all");
  if (isempty (__file))
    __file = file_in_loadpath (cstrcat (__name, ".m"), "all");
  endif
  if (isempty (__file))
    __file = file_in_loadpath (cstrcat (__name, ".cc"), "all");
  endif
  if (iscell (__file))
      ## If repeats, return first in path.
    if (isempty (__file))
      __file = "";
    else
      __file = __file{1};
    endif
  endif
  if (isempty (__file))
    if (__grabdemo)
      __ret1 = "";
      __ret2 = [];
    else
      if (exist (__name) == 3)
        fprintf (__fid, "%s%s source code with tests for dynamically linked function not found\n", __signal_empty, __name);
      else
        fprintf (__fid, "%s%s does not exist in path\n", __signal_empty, __name);
      endif
      fflush (__fid);
      if (nargout > 0)
        __ret1 = __ret2 = 0;
      endif
    endif
    if (__close_fid)
      fclose(__fid);
    endif
    return;
  endif

  ## Grab the test code from the file.
  __body = __extract_test_code (__file);

  if (isempty (__body))
    if (__grabdemo)
      __ret1 = "";
      __ret2 = [];
    else
      fprintf (__fid, "%s%s has no tests available\n", __signal_empty, __file);
      fflush (__fid);
      if (nargout > 0)
        __ret1 = __ret2 = 0;
      endif
    endif
    if (__close_fid)
      fclose(__fid);
    endif
    return;
  else
    ## Add a dummy comment block to the end for ease of indexing.
    if (__body (length(__body)) == "\n")
      __body = sprintf ("\n%s#", __body);
    else
      __body = sprintf ("\n%s\n#", __body);
    endif
  endif

  ## Chop it up into blocks for evaluation.
  __lineidx = find (__body == "\n");
  __blockidx = __lineidx(find (! isspace (__body(__lineidx+1))))+1;

  ## Ready to start tests ... if in batch mode, tell us what is happening.
  if (__verbose)
    disp (cstrcat (__signal_file, __file));
  endif

  ## Assume all tests will pass.
  __all_success = 1;

  ## Process each block separately, initially with no shared variables.
  __tests = __successes = 0;
  __shared = " ";
  __shared_r = " ";
  __clear = "";
  for __i = 1:length(__blockidx)-1

    ## Extract the block.
    __block = __body(__blockidx(__i):__blockidx(__i+1)-2);

    ## Let the user/logfile know what is happening.
    if (__verbose)
      fprintf (__fid, "%s%s\n", __signal_block, __block);
      fflush (__fid);
    endif

    ## Split __block into __type and __code.
    __idx = find (! isletter (__block));
    if (isempty (__idx))
      __type = __block;
      __code = "";
    else
      __type = __block(1:__idx(1)-1);
      __code = __block(__idx(1):length(__block));
    endif

    ## Assume the block will succeed.
    __success = 1;
    __msg = [];

### DEMO

    ## If in __grabdemo mode, then don't process any other block type.
    ## So that the other block types don't have to worry about
    ## this __grabdemo mode, the demo block processor grabs all block
    ## types and skips those which aren't demo blocks.

    __isdemo = strcmp (__type, "demo");
    if (__grabdemo || __isdemo)
      __istest = 0;

      if (__grabdemo && __isdemo)
        if (isempty(__demo_code))
          __demo_code = __code;
          __demo_idx = [1, length(__demo_code)+1];
        else
          __demo_code = cstrcat(__demo_code, __code);
          __demo_idx = [__demo_idx, length(__demo_code)+1];
        endif

      elseif (__rundemo && __isdemo)
        try
          ## process the code in an environment without variables
          eval (sprintf ("function __test__()\n%s\nendfunction", __code));
          __test__;
          input ("Press <enter> to continue: ", "s");
        catch
          __success = 0;
          __msg = sprintf ("%sdemo failed\n%s",  __signal_fail, lasterr ());
        end_try_catch
        clear __test__;

      endif
      ## Code already processed.
      __code = "";

### SHARED

    elseif (strcmp (__type, "shared"))
      __istest = 0;

      ## Separate initialization code from variables.
      __idx = find (__code == "\n");
      if (isempty (__idx))
        __vars = __code;
        __code = "";
      else
        __vars = __code (1:__idx(1)-1);
        __code = __code (__idx(1):length(__code));
      endif

      ## Strip comments off the variables.
      __idx = find (__vars == "%" | __vars == "#");
      if (! isempty (__idx))
        __vars = __vars(1:__idx(1)-1);
      endif

      ## Assign default values to variables.
      try
        __vars = deblank (__vars);
        if (! isempty (__vars))
          eval (cstrcat (strrep (__vars, ",", "=[];"), "=[];"));
          __shared = __vars;
          __shared_r = cstrcat ("[ ", __vars, "] = ");
        else
          __shared = " ";
          __shared_r = " ";
        endif
      catch
        ## Couldn't declare, so don't initialize.
        __code = "";
        __success = 0;
        __msg = sprintf ("%sshared variable initialization failed\n",
                         __signal_fail);
      end_try_catch

      ## Clear shared function definitions.
      eval (__clear, "");
      __clear = "";

      ## Initialization code will be evaluated below.

### FUNCTION

    elseif (strcmp (__type, "function"))
      __istest = 0;
      persistent __fn = 0;
      __name_position = function_name (__block);
      if (isempty (__name_position))
        __success = 0;
        __msg = sprintf ("%stest failed: missing function name\n",
                         __signal_fail);
      else
        __name = __block(__name_position(1):__name_position(2));
        __code = __block;
        try
          eval(__code); ## Define the function
          __clear = sprintf ("%sclear %s;\n", __clear, __name);
        catch
          __success = 0;
          __msg = sprintf ("%stest failed: syntax error\n%s",
                           __signal_fail, lasterr ());
        end_try_catch
      endif
      __code = "";

### ENDFUNCTION

    elseif (strcmp (__type, "endfunction"))
      ## endfunction simply declares the end of a previous function block.
      ## There is no processing to be done here, just skip to next block.
      __istest = 0;
      __code = "";

### ASSERT/FAIL

    elseif (strcmp (__type, "assert") || strcmp (__type, "fail"))
      __istest = 1;
      ## Put the keyword back on the code.
      __code = __block;
      ## The code will be evaluated below as a test block.

### ERROR/WARNING

    elseif (strcmp (__type, "error") || strcmp(__type, "warning"))
      __istest = 1;
      __warning = strcmp (__type, "warning");
      [__pattern, __id, __code] = getpattern (__code);
      if (__id)
        __patstr = ["id=",__id];
      else
        __patstr = ["<",__pattern,">"];
      endif
      try
        eval (sprintf ("function __test__(%s)\n%s\nendfunction",
                       __shared, __code));
      catch
        __success = 0;
        __msg = sprintf ("%stest failed: syntax error\n%s",
                         __signal_fail, lasterr ());
      end_try_catch

      if (__success)
        __success = 0;
        __warnstate = warning ("query", "quiet");
        warning ("on", "quiet");
        try
          eval (sprintf ("__test__(%s);", __shared));
          if (! __warning)
            __msg = sprintf ("%sexpected %s but got no error\n",
                             __signal_fail, __patstr);
          else
            if (! isempty (__id))
              [~, __err] = lastwarn;
              __mismatch = ! strcmp (__err, __id);
            else
              __err = trimerr (lastwarn, "warning");
              __mismatch = isempty (regexp (__err, __pattern, "once"));
            endif
            warning (__warnstate.state, "quiet");
            if (isempty (__err))
              __msg = sprintf ("%sexpected %s but got no warning\n",
                             __signal_fail, __patstr);
            elseif (__mismatch)
              __msg = sprintf ("%sexpected %s but got %s\n",
                               __signal_fail, __patstr, __err);
            else
              __success = 1;
            endif
          endif

        catch
          if (! isempty (__id))
            [~, __err] = lasterr;
            __mismatch = ! strcmp (__err, __id);
          else
            __err = trimerr (lasterr, "error");
            __mismatch = isempty (regexp (__err, __pattern, "once"));
          endif
          warning (__warnstate.state, "quiet");
          if (__warning)
            __msg = sprintf ("%sexpected warning %s but got error %s\n",
                             __signal_fail, __patstr, __err);
          elseif (__mismatch)
            __msg = sprintf ("%sexpected %s but got %s\n",
                             __signal_fail, __patstr, __err);
          else
            __success = 1;
          endif
        end_try_catch
        clear __test__;
      endif
      ## Code already processed.
      __code = "";

### TESTIF

    elseif (strcmp (__type, "testif"))
      __e = regexp (__code, '.$', 'lineanchors', 'once');
      ## Strip comment any comment from testif line before looking for features
      __feat_line = strtok (__code(1:__e), '#%'); 
      __feat = regexp (__feat_line, '\w+', 'match');
      __have_feat = strfind (octave_config_info ("DEFS"), __feat); 
      if (any (cellfun ("isempty", __have_feat)))
        __xskip++;
        __istest = 0;
        __code = ""; # Skip the code.
        __msg = sprintf ("%sskipped test\n", __signal_skip);
      else
        __istest = 1;
        __code = __code(__e + 1 : end);
      endif

### TEST

    elseif (strcmp (__type, "test") || strcmp (__type, "xtest"))
      __istest = 1;
      ## Code will be evaluated below.

### Comment block.

    elseif (strcmp (__block(1:1), "#"))
      __istest = 0;
      __code = ""; # skip the code

### Unknown block.

    else
      __istest = 1;
      __success = 0;
      __msg = sprintf ("%sunknown test type!\n", __signal_fail);
      __code = ""; # skip the code
    endif

    ## evaluate code for test, shared, and assert.
    if (! isempty(__code))
      try
        ## FIXME: need to check for embedded test functions, which cause
        ## segfaults, until issues with subfunctions in functions are resolved.
        embed_func = regexp (__code, '^\s*function ', 'once', 'lineanchors');
        if (isempty (embed_func))
          eval (sprintf ("function %s__test__(%s)\n%s\nendfunction",
                         __shared_r,__shared, __code));
          eval (sprintf ("%s__test__(%s);", __shared_r, __shared));
        else
          error (["Functions embedded in %!test blocks are not allowed.\n", ...
                  "Use the %!function/%!endfunction syntax instead to define shared functions for testing.\n"]);
        endif
      catch
        if (strcmp (__type, "xtest"))
           __msg = sprintf ("%sknown failure\n%s", __signal_fail, lasterr ());
           __xfail++;
        else
           __msg = sprintf ("%stest failed\n%s", __signal_fail, lasterr ());
           __success = 0;
        endif
        if (isempty (lasterr ()))
          error ("empty error text, probably Ctrl-C --- aborting");
        endif
      end_try_catch
      clear __test__;
    endif

    ## All done.  Remember if we were successful and print any messages.
    if (! isempty (__msg))
      ## Make sure the user knows what caused the error.
      if (! __verbose)
        fprintf (__fid, "%s%s\n", __signal_block, __block);
        fflush (__fid);
      endif
      fputs (__fid, __msg);
      fputs (__fid, "\n");
      fflush (__fid);
      ## Show the variable context.
      if (! strcmp (__type, "error") && ! strcmp (__type, "testif")
          && ! all (__shared == " "))
        fputs (__fid, "shared variables ");
        eval (sprintf ("fdisp(__fid,bundle(%s));", __shared));
        fflush (__fid);
      endif
    endif
    if (__success == 0)
      __all_success = 0;
      ## Stop after one error if not in batch mode.
      if (! __batch)
        if (nargout > 0)
          __ret1 = __ret2 = 0;
        endif
        if (__close_fid)
          fclose(__fid);
        endif
        return;
      endif
    endif
    __tests += __istest;
    __successes += __success * __istest;
  endfor
  eval (__clear, "");

  if (nargout == 0)
    if (__tests || __xfail || __xskip)
      if (__xfail)
        printf ("PASSES %d out of %d tests (%d expected failures)\n",
                __successes, __tests, __xfail);
      else
        printf ("PASSES %d out of %d tests\n", __successes, __tests);
      endif
      if (__xskip)
        printf ("Skipped %d tests due to missing features\n", __xskip);
      endif
    else
      printf ("%s%s has no tests available\n", __signal_empty, __file);
    endif
  elseif (__grabdemo)
    __ret1 = __demo_code;
    __ret2 = __demo_idx;
  elseif (nargout == 1)
    __ret1 = __all_success;
  else
    __ret1 = __successes;
    __ret2 = __tests;
    __ret3 = __xfail;
    __ret4 = __xskip;
  endif
endfunction

## Create structure with fieldnames the name of the input variables.
function s = varstruct (varargin)
  for i = 1:nargin
    s.(deblank (argn(i,:))) = varargin{i};
  endfor
endfunction

## Find [start,end] of fn in 'function [a,b] = fn'.
function pos = function_name (def)
  pos = [];

  ## Find the end of the name.
  right = find (def == "(", 1);
  if (isempty (right))
    return;
  endif
  right = find (def(1:right-1) != " ", 1, "last");

  ## Find the beginning of the name.
  left = max ([find(def(1:right)==" ", 1, "last"), ...
               find(def(1:right)=="=", 1, "last")]);
  if (isempty (left))
    return;
  endif
  left++;

  ## Return the end points of the name.
  pos = [left, right];
endfunction

## Strip <pattern> from '<pattern> code'.
## Also handles 'id=ID code'
function [pattern, id, rest] = getpattern (str)
  pattern = ".";
  id = [];
  rest = str;
  str = trimleft (str);
  if (! isempty (str) && str(1) == "<")
    close = index (str, ">");
    if (close)
      pattern = str(2:close-1);
      rest = str(close+1:end);
    endif
  elseif (strncmp (str, "id=", 3))
    [id, rest] = strtok (str(4:end));
  endif
endfunction

## Strip '.*prefix:' from '.*prefix: msg\n' and strip trailing blanks.
function msg = trimerr (msg, prefix)
  idx = index (msg, cstrcat (prefix, ":"));
  if (idx > 0)
    msg(1:idx+length(prefix)) = [];
  endif
  msg = trimleft (deblank (msg));
endfunction

## Strip leading blanks from string.
function str = trimleft (str)
  idx = find (isspace (str));
  leading = find (idx == 1:length(idx));
  if (! isempty (leading))
    str = str(leading(end)+1:end);
  endif
endfunction

## Make a structure out of the named variables
## (based on Etienne Grossmann's tar function).
function s = bundle (varargin)
  for i = 1:nargin
    s.(deblank (argn(i,:))) = varargin{i};
  endfor
endfunction

function body = __extract_test_code (nm)
  fid = fopen (nm, "rt");
  body = [];
  if (fid >= 0)
    while (! feof (fid))
      ln = fgetl (fid);
      if (length (ln) >= 2 && strcmp (ln(1:2), "%!"))
        body = [body, "\n"];
        if (length(ln) > 2)
          body = cstrcat (body, ln(3:end));
        endif
      endif
    endwhile
    fclose (fid);
  endif
endfunction

### Test for test for missing features
%!testif OCTAVE_SOURCE
%! ## This test should be run
%! assert (true);

### Disable this test to avoid spurious skipped test for "make check"
% !testif HAVE_FOOBAR
% ! ## missing feature. Fail if this test is run
% ! error("Failed missing feature test");

### Test for a known failure
%!xtest error("This test is known to fail")

### example from toeplitz
%!shared msg1,msg2
%! msg1="C must be a vector";
%! msg2="C and R must be vectors";
%!fail ('toeplitz([])', msg1);
%!fail ('toeplitz([1,2;3,4])', msg1);
%!fail ('toeplitz([1,2],[])', msg2);
%!fail ('toeplitz([1,2],[1,2;3,4])', msg2);
%!fail ('toeplitz ([1,2;3,4],[1,2])', msg2);
% !fail ('toeplitz','usage: toeplitz'); # usage doesn't generate an error
% !fail ('toeplitz(1, 2, 3)', 'usage: toeplitz');
%!test  assert (toeplitz ([1,2,3], [1,4]), [1,4; 2,1; 3,2]);
%!demo  toeplitz ([1,2,3,4],[1,5,6])

### example from kron
%!#error kron  # FIXME suppress these until we can handle output
%!#error kron(1,2,3)
%!test assert (isempty (kron ([], rand(3, 4))))
%!test assert (isempty (kron (rand (3, 4), [])))
%!test assert (isempty (kron ([], [])))
%!shared A, B
%!test
%! A = [1, 2, 3; 4, 5, 6];
%! B = [1, -1; 2, -2];
%!assert (size (kron (zeros (3, 0), A)), [ 3*rows(A), 0 ])
%!assert (size (kron (zeros (0, 3), A)), [ 0, 3*columns(A) ])
%!assert (size (kron (A, zeros (3, 0))), [ 3*rows(A), 0 ])
%!assert (size (kron (A, zeros (0, 3))), [ 0, 3*columns(A) ])
%!assert (kron (pi, e), pi*e)
%!assert (kron (pi, A), pi*A)
%!assert (kron (A, e), e*A)
%!assert (kron ([1, 2, 3], A), [ A, 2*A, 3*A ])
%!assert (kron ([1; 2; 3], A), [ A; 2*A; 3*A ])
%!assert (kron ([1, 2; 3, 4], A), [ A, 2*A; 3*A, 4*A ])
%!test
%! res = [1,-1,2,-2,3,-3; 2,-2,4,-4,6,-6; 4,-4,5,-5,6,-6; 8,-8,10,-10,12,-12];
%! assert (kron (A, B), res)

### an extended demo from specgram
%!#demo
%! ## Speech spectrogram
%! [x, Fs] = auload(file_in_loadpath("sample.wav")); # audio file
%! step = fix(5*Fs/1000);     # one spectral slice every 5 ms
%! window = fix(40*Fs/1000);  # 40 ms data window
%! fftn = 2^nextpow2(window); # next highest power of 2
%! [S, f, t] = specgram(x, fftn, Fs, window, window-step);
%! S = abs(S(2:fftn*4000/Fs,:)); # magnitude in range 0<f<=4000 Hz.
%! S = S/max(max(S));         # normalize magnitude so that max is 0 dB.
%! S = max(S, 10^(-40/10));   # clip below -40 dB.
%! S = min(S, 10^(-3/10));    # clip above -3 dB.
%! imagesc(flipud(20*log10(S)), 1);
%! % you should now see a spectrogram in the image window


### now test test itself

%!## usage and error testing
% !fail ('test','usage.*test')           # no args, generates usage()
% !fail ('test(1,2,3,4)','usage.*test')  # too many args, generates usage()
%!fail ('test("test", "bogus")','unknown flag')      # incorrect args
%!fail ('garbage','garbage.*undefined')  # usage on nonexistent function should be

%!error test                     # no args, generates usage()
%!error test(1,2,3,4)            # too many args, generates usage()
%!error <unknown flag> test("test", 'bogus');  # incorrect args, generates error()
%!error <garbage' undefined> garbage           # usage on nonexistent function should be

%!error test("test", 'bogus');           # test without pattern

%!test
%! lastwarn();            # clear last warning just in case

%!warning <warning message> warning('warning message');

%!## test of shared variables
%!shared a                # create a shared variable
%!test   a=3;             # assign to a shared variable
%!test   assert(a,3)      # variable should equal 3
%!shared b,c              # replace shared variables
%!test assert (!exist("a"));   # a no longer exists
%!test assert (isempty(b));    # variables start off empty
%!shared a,b,c            # recreate a shared variable
%!test assert (isempty(a));    # value is empty even if it had a previous value
%!test a=1; b=2; c=3;   # give values to all variables
%!test assert ([a,b,c],[1,2,3]); # test all of them together
%!test c=6;             # update a value
%!test assert([a, b, c],[1, 2, 6]); # show that the update sticks
%!shared                    # clear all shared variables
%!test assert(!exist("a"))  # show that they are cleared
%!shared a,b,c              # support for initializer shorthand
%! a=1; b=2; c=4;

%!function x = __test_a(y)
%! x = 2*y;
%!endfunction
%!assert(__test_a(2),4);       # Test a test function

%!function __test_a (y)
%! x = 2*y;
%!endfunction
%!test
%! __test_a(2);                # Test a test function with no return value

%!function [x,z] = __test_a (y)
%! x = 2*y;
%! z = 3*y;
%!endfunction
%!test                   # Test a test function with multiple returns
%! [x,z] = __test_a(3);
%! assert(x,6);
%! assert(z,9);

%!## test of assert block
%!assert (isempty([]))      # support for test assert shorthand

%!## demo blocks
%!demo                   # multiline demo block
%! t=[0:0.01:2*pi]; x=sin(t);
%! plot(t,x);
%! % you should now see a sine wave in your figure window
%!demo a=3               # single line demo blocks work too

%!## this is a comment block. it can contain anything.
%!##
%! it is the "#" as the block type that makes it a comment
%! and it  stays as a comment even through continuation lines
%! which means that it works well with commenting out whole tests

% !# failure tests.  All the following should fail. These tests should
% !# be disabled unless you are developing test() since users don't
% !# like to be presented with expected failures.  I use % ! to disable.
% !test   error("---------Failure tests.  Use test('test','verbose',1)");
% !test   assert([a,b,c],[1,3,6]);   # variables have wrong values
% !bogus                     # unknown block type
% !error  toeplitz([1,2,3]); # correct usage
% !test   syntax errors)     # syntax errors fail properly
% !shared garbage in         # variables must be comma separated
% !error  syntax++error      # error test fails on syntax errors
% !error  "succeeds.";       # error test fails if code succeeds
% !error <wrong pattern> error("message")  # error pattern must match
% !demo   with syntax error  # syntax errors in demo fail properly
% !shared a,b,c
% !demo                      # shared variables not available in demo
% ! assert(exist("a"))
% !error
% ! test('/etc/passwd');
% ! test("nonexistent file");
% ! ## These don't signal an error, so the test for an error fails. Note
% ! ## that the call doesn't reference the current fid (it is unavailable),
% ! ## so of course the informational message is not printed in the log.
