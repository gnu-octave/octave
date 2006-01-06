## -*- texinfo -*-
## @deftypefn {Function File} {} fail (@var{code},@var{pattern})
## @deftypefnx {Function File} {} fail (@var{code},'warning',@var{pattern})
##
## Return true if @var{code} fails with an error message matching
## @var{pattern}, otherwise produce an error. Note that @var{code}
## is a string and if @var{code} runs successfully, the error produced is:
##
## @example
##           expected error but got none  
## @end example
##
## If the code fails with a different error, the message produced is:
##
## @example
##           expected <pattern>
##           but got <text of actual error>
## @end example
##
## The angle brackets are not part of the output.
##
## Called with three arguments, the behavior is similar to 
## @code{fail(@var{code}, @var{pattern})}, but produces an error if no 
## warning is given during code execution or if the code fails.
##
## @end deftypefn

## This program is public domain
## Author: Paul Kienzle <pkienzle@users.sf.net>

## PKG_ADD mark_as_command fail
function ret=fail(code,pattern,warning_pattern)
  if nargin < 1 || nargin > 3
    usage("fail(code [, 'warning'] [, pattern])");
  endif

  ## sort out arguments
  test_warning =  (nargin > 1 && strcmp(pattern,'warning'));
  if nargin == 3
    pattern = warning_pattern;
  elseif nargin == 1 || (nargin==2 && test_warning)
    pattern = "";
  endif
  if isempty(pattern), pattern = "."; endif  # match any nonempty message

  ## allow assert(fail())
  if nargout, ret=1; endif  

  ## don't test failure if evalin doesn't exist
  if !exist('evalin') || !exist('lastwarn'), return; endif

  if test_warning
    ## perform the warning test
    lastwarn();  # clear old warnings
    state = warning("query","quiet"); # make sure warnings are turned on
    warning("on","quiet");
    try
      ## printf("lastwarn before %s: %s\n",code,lastwarn);
      evalin("caller",sprintf("%s;",code));
      ## printf("lastwarn after %s: %s\n",code,lastwarn);
      err = lastwarn;  # retrieve new warnings
      warning(state.state,"quiet");
      if isempty(err), 
        msg = sprintf("expected warning <%s> but got none", pattern); 
      else
        err([1:9,end]) = [];  # transform "warning: ...\n" to "..."
        if !isempty(regexp(err,pattern,"once")), return; end
        msg = sprintf("expected warning <%s>\nbut got <%s>", pattern,err);
      endif
    catch
      warning(state.state,"quiet");
      err = lasterr;
      err([1:7,end]) = [];  # transform "error: ...\n", to "..."
      msg = sprintf("expected warning <%s> but got error <%s>", pattern, err);
    end
      
  else
    ## perform the error test
    try
      evalin("caller",sprintf("%s;",code));
      msg = sprintf("expected error <%s> but got none", pattern);
    catch
      err=lasterr;
      if (strcmp(err(1:7),"error:"))
         err([1:6,end]) = []; # transform "error: ...\n", to "..."
      endif
      if !isempty(regexp(err,pattern,"once")), return; end
      msg = sprintf("expected error <%s>\nbut got <%s>",pattern,err);
    end
  endif

  ## if we get here, then code didn't fail or error didn't match
  error(msg);
endfunction

%!fail ('[1,2]*[2,3]','nonconformant')
%!fail ("fail('[1,2]*[2;3]','nonconformant')","expected error <nonconformant> but got none")
%!fail ("fail('[1,2]*[2,3]','usage:')","expected error <usage:>\nbut got.*nonconformant")
%!fail ("warning('test warning')",'warning','test warning');

%!# fail ("warning('next test')",'warning','next test');  ## only allowed one warning test?!?

## Comment out the following tests if you don't want to see what
## errors look like
% !fail ('a*[2;3]', 'nonconformant')
% !fail ('a*[2,3]', 'usage:')
% !fail ("warning('warning failure')", 'warning', 'success')
