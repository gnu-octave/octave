## Copyright (C) 2005 Paul Kienzle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} test @var{name}
## @deftypefnx {Function File} {} test @var{name} quiet|normal|verbose
## @deftypefnx {Function File} {} test ('@var{name}', 'quiet|normal|verbose', @var{fid})
## @deftypefnx {Function File} {} test ([], 'explain', @var{fid})
## @deftypefnx {Function File} {@var{success} =} test (@dots{})
## @deftypefnx {Function File} {[@var{n}, @var{max}] =} test (@dots{})
## @deftypefnx {Function File} {[@var{code}, @var{idx}] =} test ('@var{name}','grabdemo')
##
## Perform tests from the first file in the loadpath matching @var{name}.
## @code{test} can be called as a command or as a function. Called with 
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
## The argument @var{fid} can be used to allow batch processing. Errors
## can be written to the already open file defined by @var{fid}, and 
## hopefully when octave crashes this file will tell you what was happening
## when it did. You can use @code{stdout} if you want to see the results as
## they happen.  You can also give a file name rather than an @var{fid}, in
## which case the contents of the file will be replaced with the log from 
## the current test.
##
## Called with a single output argument @var{success}, @code{test} returns
## true is all of the tests were successful. Called with two output arguments
## @var{n} and @var{max}, the number of sucessful test and the total number
## of tests in the file @var{name} are returned.
##
## If the second argument is the string 'grabdemo', the contents of the demo
## blocks are extracted but not executed. Code for all code blocks is
## concatented and returned as @var{code} with @var{idx} being a vector of
## positions of the ends of the demo blocks.
##
## If the second argument is 'explain', then @var{name} is ignored and an
## explanation of the line markers used is written to the file @var{fid}.
## @seealso{error, assert, fail, demo, example}
## @end deftypefn

## TODO: * Consider using keyword fail rather then error?  This allows us
## TODO: to make a functional form of error blocks, which means we
## TODO: can include them in test sections which means that we can use
## TODO: octave flow control for both kinds of tests.

## PKG_ADD: mark_as_command test

function [__ret1, __ret2] = test (__name, __flag, __fid)
  ## information from test will be introduced by "key" 
  persistent __signal_fail =  "!!!!! ";
  persistent __signal_empty = "????? ";
  persistent __signal_block = "  ***** ";
  persistent __signal_file =  ">>>>> ";

  if (nargin < 2 || isempty(__flag))
    __flag = "quiet";
  endif
  if (nargin < 3) 
    __fid = []; 
  endif
  if (nargin < 1 || nargin > 3 ...
      || (!ischar(__name) && !isempty(__name)) || !ischar(__flag))
    usage("success = test('name', ['quiet'|'normal'|'verbose'], fid)");
  endif
  if (isempty(__name) && (nargin != 3 || !strcmp(__flag, "explain")))
    usage("test([], 'explain', fid)");
  endif
  __batch = (!isempty(__fid));

  ## decide if error messages should be collected
  __close_fid = 0;
  if (__batch)
    if (ischar(__fid))
      __fid = fopen(__fid, "wt");
      if __fid < 0, error("could not open log file"); endif
      __close_fid = 1;
    endif
    fprintf (__fid, "%sprocessing %s\n", __signal_file, __name);
    fflush (__fid);
  else
    __fid = stdout;
  endif

  if (strcmp(__flag, "normal"))
    __grabdemo = 0;
    __rundemo = 0;
    __verbose = __batch;
  elseif (strcmp(__flag, "quiet"))
    __grabdemo = 0;
    __rundemo = 0;
    __verbose = 0;
  elseif (strcmp(__flag, "verbose"))
    __grabdemo = 0;
    __rundemo = 1;
    __verbose = 1;
  elseif (strcmp(__flag, "grabdemo"))
    __grabdemo = 1;
    __rundemo = 0;
    __verbose = 0;
    __demo_code = "";
    __demo_idx = 1;
  elseif (strcmp(__flag, "explain"))
    fprintf (__fid, "# %s new test file\n",__signal_file);
    fprintf (__fid, "# %s no tests in file\n",__signal_empty);
    fprintf (__fid, "# %s test had an unexpected result\n",__signal_fail);
    fprintf (__fid, "# %s code for the test\n",__signal_block);
    fprintf (__fid, "# Search for the unexpected results in the file\n");
    fprintf (__fid, "# then page back to find the file name which caused it.\n");
    fprintf (__fid, "# The result may be an unexpected failure (in which\n");
    fprintf (__fid, "# case an error will be reported) or an unexpected\n");
    fprintf (__fid, "# success (in which case no error will be reported).\n");
    fflush (__fid);
    if (__close_fid) fclose(__fid); endif
    return;
  else
    error("test unknown flag '%s'", __flag);
  endif

  ## locate the file to test
  __file = file_in_loadpath (__name);
  if (isempty (__file)) 
    __file = file_in_loadpath ([__name, ".m"]);
  endif
  if (isempty (__file))
    __file = file_in_loadpath ([__name, ".cc"]);
  endif
  if (isempty (__file))
    if (__grabdemo)
      __ret1 = "";
      __ret2 = [];
    else
      fprintf(__fid, "%s%s does not exist in path\n", __signal_empty, __name);
      fflush (__fid);
      if (nargout > 0) __ret1 = __ret2 = 0; endif
    endif
    if (__close_fid) fclose(__fid); endif
    return;
  endif

  ## grab the test code from the file
  __body = __extract_test_code (__file);

  if (isempty (__body))
    if (__grabdemo)
      __ret1 = "";
      __ret2 = [];
    else
      fprintf(__fid, "%s%s has no tests available\n", __signal_empty, __file);
      fflush (__fid);
      if (nargout > 0) __ret1 = __ret2 = 0; endif
    endif
    if (__close_fid) fclose(__fid); endif
    return;
  else
    ## add a dummy comment block to the end for ease of indexing
    if (__body (length(__body)) == "\n")
      __body = sprintf("\n%s#", __body); 
    else
      __body = sprintf("\n%s\n#", __body); 
    endif
  endif

  ## chop it up into blocks for evaluation
  __lineidx = find(__body == "\n");
  __blockidx = __lineidx(find(!isspace(__body(__lineidx+1))))+1;

  ## ready to start tests ... if in batch mode, tell us what is happening
  if (__verbose)
    disp ([ __signal_file, __file ]);
  endif

  ## assume all tests will pass
  __all_success = 1;

  ## process each block separately, initially with no shared variables
  __tests = __successes = 0;
  __shared = " ";
  __shared_r = " ";
  __clear = "";
  for __i=1:length(__blockidx)-1

    ## extract the block
    __block = __body(__blockidx(__i):__blockidx(__i+1)-2);

    ## let the user/logfile know what is happening
    if (__verbose)
      fprintf (__fid, "%s%s\n", __signal_block, __block);
      fflush (__fid);
    endif

    ## split __block into __type and __code
    __idx = find(!isletter(__block));
    if (isempty(__idx))
      __type = __block;
      __code = "";
    else
      __type = __block(1:__idx(1)-1);
      __code = __block(__idx(1):length(__block));
    endif

    ## assume the block will succeed;
    __success = 1;
    __msg = [];

    ## DEMO
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
	  __demo_idx = [ 1, length(__demo_code)+1 ];
	else
	  __demo_code = strcat(__demo_code, __code);
	  __demo_idx = [ __demo_idx, length(__demo_code)+1 ];
	endif

      elseif (__rundemo && __isdemo)
      	try
	  ## process the code in an environment without variables
      	  eval(sprintf("function __test__()\n%s\nendfunction",__code));
	  __test__;
	  input("Press <enter> to continue: ","s");
      	catch
	  __success = 0;
	  __msg = sprintf("%sdemo failed\n%s",  __signal_fail, __error_text__);
      	end_try_catch
      	clear __test__;

      endif
      __code = ""; # code already processed
      
    ## SHARED
    elseif strcmp (__type, "shared")
      __istest = 0;

      ## separate initialization code from variables
      __idx = find(__code == "\n");
      if (isempty(__idx))
	__vars = __code;
	__code = "";
      else
      	__vars = __code (1:__idx(1)-1);
      	__code = __code (__idx(1):length(__code));
      endif
      
      ## strip comments off the variables
      __idx = find(__vars=="%" | __vars == "#");
      if (!isempty(__idx))
	__vars = __vars(1:__idx(1)-1);
      endif
      
      ## assign default values to variables
      try
	__vars = deblank(__vars);
	if (!isempty(__vars))
	  eval([strrep(__vars,",","=[];"), "=[];"]);
	  __shared = __vars;
	  __shared_r = ["[ ", __vars, "] = "];
      	else
	  __shared = " ";
	  __shared_r = " ";
      	endif
      catch
	__code = "";  # couldn't declare, so don't initialize
	__success = 0;
	__msg = sprintf("%sshared variable initialization failed\n", ...
		        __signal_fail);
      end_try_catch

      ## clear shared function definitions
      eval(__clear,""); __clear="";
      
      ## initialization code will be evaluated below
    
    ## FUNCTION
    elseif strcmp (__type, "function")
      __istest = 0;
      persistent __fn = 0;
      __name_position = function_name(__block);
      if isempty(__name_position)
        __success = 0;
        __msg = sprintf("%stest failed: missing function name\n", ...
			__signal_fail);
      else
        __name = __block(__name_position(1):__name_position(2));
        __code = __block;
        try
          eval(__code); ## Define the function
          __clear = sprintf("%sclear %s;\n",__clear,__name);
        catch
          __success = 0;
          __msg = sprintf("%stest failed: syntax error\n%s", ...
			  __signal_fail, __error_text__);
        end_try_catch
      endif
      __code = "";
      

    ## ASSERT/FAIL
    elseif strcmp (__type, "assert") || strcmp (__type, "fail")
      __istest = 1;
      __code = __block; # put the keyword back on the code
      ## the code will be evaluated below as a test block
      
    ## ERROR/WARNING
    elseif strcmp (__type, "error") || strcmp(__type, "warning")
      __istest = 1;
      __warning = strcmp(__type, "warning");
      [__pattern, __code] = getpattern(__code);
      try
      	eval(sprintf("function __test__(%s)\n%s\nendfunction", ...
		     __shared, __code));
      catch
      	__success = 0;
      	__msg = sprintf("%stest failed: syntax error\n%s", ...
			__signal_fail, __error_text__);
      end_try_catch
      
      if (__success)
        __success = 0;
	__warnstate = warning("query","quiet");
	warning("on","quiet");
      	try
 	  eval(sprintf("__test__(%s);", __shared));
	  __err = trimerr(lastwarn,"warning");
          warning(__warnstate.state,"quiet");

          if !__warning,
       	    __msg = sprintf("%sexpected <%s> but got no error\n", ...
 			    __signal_fail, __pattern);
          elseif isempty(__err)
            __msg = sprintf("%sexpected <%s> but got no warning\n", ...
			    __signal_fail,__pattern);
          elseif isempty(regexp(__err,__pattern,"once"))
            __msg = sprintf("%sexpected <%s> but got %s\n", ...
 			     __signal_fail, __pattern, __err);
          else
            __success = 1;
          endif

      	catch
	  __err = trimerr(lasterr,"error");
          warning(__warnstate.state,"quiet");
          if __warning,
            __msg = sprintf("%sexpected warning <%s> but got error %s\n", ...
			    __signal_fail, __pattern, __err);
	  elseif isempty(regexp(__err,__pattern,"once"))
            __msg = sprintf("%sexpected <%s> but got %s\n", ...
			    __signal_fail, __pattern, __err);
          else
	    __success = 1;
          endif
      	end_try_catch
      	clear __test__;
      endif
      __code = ""; # code already processed
      
    ## TEST
    elseif strcmp(__type, "test")
      __istest = 1;
      ## code will be evaluated below
      
    ## comment block
    elseif strcmp (__block(1:1), "#")
      __istest = 0;
      __code = ""; # skip the code

    else
    ## unknown block
      __istest = 1;
      __success = 0;
      __msg = sprintf("%sunknown test type!\n", __signal_fail);
      __code = ""; # skip the code
    endif

    ## evaluate code for test, shared, and assert.
    if (!isempty(__code))
      try
      	eval(sprintf("function %s__test__(%s)\n%s\nendfunction", ...
	      __shared_r,__shared, __code));
	eval(sprintf("%s__test__(%s);", __shared_r, __shared));
      catch
	__success = 0;
	__msg = sprintf("%stest failed\n%s", __signal_fail, __error_text__);
	if isempty(__error_text__), 
	  error("empty error text, probably Ctrl-C --- aborting"); 
	endif
      end_try_catch
      clear __test__;
    endif
    
    ## All done.  Remember if we were successful and print any messages
    if (!isempty(__msg))
      ## make sure the user knows what caused the error
      if (!__verbose)
      	fprintf (__fid, "%s%s\n", __signal_block, __block);
	fflush (__fid);
      endif
      fputs (__fid, __msg);
      fflush (__fid);
      ## show the variable context
      if (!strcmp(__type, "error") && !all(__shared==" "))
	fputs(__fid, "shared variables ");
	eval (sprintf("fdisp(__fid,bundle(%s));", __shared)); 
	fflush (__fid);
      endif
    endif
    if (__success == 0)
      __all_success = 0;
      	## stop after one error if not in batch mode
      if (!__batch)
    	if (nargout > 0) __ret1 = __ret2 = 0; endif
	if (__close_fid) fclose(__fid); endif
      	return;
      endif
    endif
    __tests += __istest;
    __successes += __success*__istest;
  endfor
  eval(__clear,"");

  if (nargout == 0)
    printf("PASSES %d out of %d tests\n",__successes,__tests);
  elseif (__grabdemo)
    __ret1 = __demo_code;
    __ret2 = __demo_idx;
  elseif nargout == 1
    __ret1 = __all_success; 
  else
    __ret1 = __successes;
    __ret2 = __tests;
  endif
endfunction

## create structure with fieldnames the name of the input variables
function s = varstruct(varargin)
  for i=1:nargin
    s.(deblank(argn(i,:))) = varargin{i};
  endfor
endfunction

## find [start,end] of fn in 'function [a,b] = fn'
function pos = function_name(def)
  pos = [];

  ## Find the end of the name
  right = min(find(def=='('));
  if isempty(right), return; endif
  right = max(find(def(1:right-1) != ' '));

  ## Find the beginning of the name
  left = max([find(def(1:right)==' '),find(def(1:right)=='=')]);
  if isempty(left), return; endif
  left++;

  ## Return the end points of the name
  pos = [left,right];
endfunction

## strip <pattern> from '<pattern> code'
function [pattern,rest] = getpattern(str)
  pattern = '.';
  rest = str; 
  str = trimleft(str);
  if !isempty(str) && str(1) == '<'
    close = index(str,'>');
    if close,
      pattern = str(2:close-1);
      rest = str(close+1:end);
    endif
  endif
endfunction

## strip '.*prefix:' from '.*prefix: msg\n' and strip trailing blanks
function msg = trimerr(msg,prefix)
  idx = index(msg,[prefix,':']);
  if (idx > 0), msg(1:idx+length(prefix)) = []; end
  msg = trimleft(deblank(msg));
endfunction

## strip leading blanks from string
function str = trimleft(str)
  idx = find(isspace(str));
  leading = find(idx == [1:length(idx)]);
  if !isempty(leading)
    str = str(leading(end)+1:end);
  endif
endfunction

## make a structure out of the named variables
## (based on Etienne Grossmann's tar function)
function s = bundle(varargin)
  for i=1:nargin
    s.(deblank(argn(i,:))) = varargin{i};
  end
endfunction

function body = __extract_test_code (nm)
  fid = fopen (nm, "rt");
  body = [];
  if (fid >= 0)
    while (! feof(fid))
      ln = fgetl (fid);
      if (length(ln) >= 2 && strcmp (ln(1:2), "%!"))
        body = [body, "\n"];
        if (length(ln) > 2)
          body = [body, ln(3:end)];
        endif
      endif
    endwhile
    fclose (fid);
  endif
endfunction

### example from toeplitz
%!shared msg
%! msg="expecting vector arguments";
%!fail ('toeplitz([])', msg);
%!fail ('toeplitz([1,2],[])', msg);
%!fail ('toeplitz([1,2;3,4])', msg);
%!fail ('toeplitz([1,2],[1,2;3,4])', msg);
%!fail ('toeplitz ([1,2;3,4],[1,2])', msg);
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

%!error <usage.*test> test                     # no args, generates usage()
%!error <usage.*test> test(1,2,3,4)            # too many args, generates usage()
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
%!assert(__test_a(2),4);       # Test a test function

%!function __test_a (y)
%! x = 2*y;
%!test
%! __test_a(2);                # Test a test function with no return value

%!function [x,z] = __test_a (y)
%! x = 2*y;
%! z = 3*y;
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
