## Copyright (C) 2000 Paul Kienzle
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
## @deftypefn {Function File} {} assert (@var{cond})
## @deftypefnx {Function File} {} assert (@var{observed},@var{expected})
## @deftypefnx {Function File} {} assert (@var{observed},@var{expected},@var{tol})
##
## Produces an error if the condition is not met. @code{assert} can be
## called in three different ways.
##
## @table @code
## @item assert (@var{cond})
## Called with a single argument @var{cond}, @code{assert} produces an
## error if @var{cond} is zero.
##
## @item assert (@var{observed}, @var{expected})
## Produce an error if observed is not the same as expected. Note that 
## observed and expected can be strings, scalars, vectors, matrices, 
## lists or structures.
##
## @item assert(@var{observed}, @var{expected}, @var{tol})
## Produce an error if relative error is less than tolerance. That is, 
## @code{abs(@var{observed} - @var{expected}) > @var{tol} * @var{expected}}.  
## Absolute error @code{abs(@var{observed} - @var{expected}) > abs(@var{tol})} 
## will be used when tolerance is negative or when the expected value is zero.
## @end table
## @seealso{test}
## @end deftypefn

## TODO: Output throttling: don't print out the entire 100x100 matrix,
## TODO: but instead give a summary; don't print out the whole list, just
## TODO: say what the first different element is, etc.  To do this, make
## TODO: the message generation type specific.
function assert(cond, expected, tol)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin < 3)
    tol = 0;
  endif

  if exist("argn") == 0, argn=" "; endif
  in = deblank(argn(1,:));
  for i=2:rows(argn)
    in = [in, ",", deblank(argn(i,:))];
  end
  in = ["(",in,")"];

  coda = "";
  iserror = 0;
  if (nargin == 1)
    if (!isnumeric(cond) || !all(cond(:)))
      error ("assert %s failed", in); # say which elements failed?
    endif
  
  elseif (is_list(cond))
    if (!is_list(expected) || length(cond) != length(expected))
      iserror = 1;
    else
      try
	for i=1:length(cond)
	  assert(nth(cond,i),nth(expected,i));
	endfor
      catch
	iserror = 1;
      end
    endif

  elseif (ischar (expected))
    iserror = (!ischar (cond) || !strcmp (cond, expected));

  elseif (iscell(expected))
    if (!iscell (cond) || any(size(cond)!=size(expected)))
      iserror = 1;
    else
      try
	for i=1:length(expected(:))
	  assert(cond{i},expected{i},tol);
	endfor
      catch
	iserror = 1;
      end
    endif

  elseif (isstruct (expected))
    if (!isstruct (cond) || any(size(cond) != size(expected)) || ...
	rows(struct_elements(cond)) != rows(struct_elements(expected)))
      iserror = 1;
    else
      try
	empty=prod(size(cond))==0;
	normal=prod(size(cond))==1;
	for [v,k] = cond
	  if !struct_contains(expected,k), error; endif
	  if empty, v = cell(1,0); endif
	  if normal, v = {v}; endif
	  assert(v,{expected.(k)},tol);
	endfor
      catch
	iserror = 1;
      end
    endif

  elseif (isempty (expected))
    iserror = (any (size (cond) != size (expected)));

  elseif (any (size (cond) != size (expected)))
    iserror = 1;
    coda = "Dimensions don't match";

  elseif tol==0 && !strcmp(typeinfo(cond),typeinfo(expected))
    iserror = 1;
    coda = ["Type ",typeinfo(cond)," != ",typeinfo(expected)];

  else # numeric
    A=cond(:); B=expected(:);
    ## Check exceptional values
    if any(isnan(A) != isnan(B))
      iserror = 1;
      coda = "NaNs don't match";
    elseif any(isna(A) != isna(B))
      iserror = 1;
      coda = "NAs don't match";
    elseif any(A(isinf(A)) != B(isinf(B)))
      iserror = 1;
      coda = "Infs don't match";
    else
      ## Check normal values
      A = A(finite(A)); B=B(finite(B));
      if tol == 0,
        err = any(A != B);
	errtype = "values do not match";
      elseif tol >= 0,
	err = max(abs(A-B));
	errtype = "maximum absolute error %g exceeds tolerance %g";
      else 
	abserr = max(abs(A(B==0)));
	A = A(B!=0); B = B(B!=0);
	relerr = max(abs(A-B)./abs(B));
	err = max([abserr;relerr]);
	errtype = "maximum relative error %g exceeds tolerance %g";
      endif
      if err > abs(tol)
	iserror = 1;
	coda = sprintf(errtype,err,abs(tol));
      endif
    endif
  endif

  if (!iserror)
    return;
  endif

  ## pretty print the "expected but got" info,
  ## trimming leading and trailing "\n"
  str = disp (expected);
  idx = find(str!="\n");
  if (!isempty(idx))
    str = str(idx(1):idx(length(idx)));
  endif
  str2 = disp (cond);
  idx = find(str2!="\n");
  if (!isempty(idx))
    str2 = str2(idx(1):idx(length(idx)));
  endif
  msg = ["assert ",in," expected\n", str, "\nbut got\n", str2];
  if (!isempty(coda))
    msg = [ msg, "\n", coda ];
  endif
  error("%s",msg);
  ## disp(msg);
  ## error("assertion failed");
endfunction

## empty
%!assert([])
%!assert(zeros(3,0),zeros(3,0))
%!error assert(zeros(3,0),zeros(0,2))
%!error assert(zeros(3,0),[])

## conditions
%!assert(isempty([]))
%!assert(1)
%!error assert(0)
%!assert(ones(3,1))
%!assert(ones(1,3))
%!assert(ones(3,4))
%!error assert([1,0,1])
%!error assert([1;1;0])
%!error assert([1,0;1,1])

## vectors
%!assert([1,2,3],[1,2,3]);
%!assert([1;2;3],[1;2;3]);
%!error assert([2;2;3],[1;2;3]);
%!error assert([1,2,3],[1;2;3]);
%!error assert([1,2],[1,2,3]);
%!error assert([1;2;3],[1;2]);
%!assert([1,2;3,4],[1,2;3,4]);
%!error assert([1,4;3,4],[1,2;3,4])
%!error assert([1,3;2,4;3,5],[1,2;3,4])

## exceptional values
%!assert([NaN, NA, Inf, -Inf, 1+eps, eps],[NaN, NA, Inf, -Inf, 1, 0],eps)
%!error assert(NaN, 1)
%!error assert(NA, 1)
%!error assert(-Inf, Inf)

## scalars
%!error assert(3, [3,3; 3,3])
%!error assert([3,3; 3,3], 3)
%!assert(3, 3);
%!assert(3+eps, 3, eps);
%!assert(3, 3+eps, eps);
%!error assert(3+2*eps, 3, eps);
%!error assert(3, 3+2*eps, eps);

%## must give a little space for floating point errors on relative
%!assert(100+100*eps, 100, -2*eps); 
%!assert(100, 100+100*eps, -2*eps);
%!error assert(100+300*eps, 100, -2*eps); 
%!error assert(100, 100+300*eps, -2*eps);
%!error assert(3, [3,3]);
%!error assert(3,4);

## structures
%!shared x,y
%! x.a = 1; x.b=[2, 2];
%! y.a = 1; y.b=[2, 2];
%!assert (x,y)
%!test y.b=3;
%!error assert (x,y)
%!error assert (3, x);
%!error assert (x, 3);

## check usage statements
%!error assert
%!error assert(1,2,3,4,5)

## strings
%!assert("dog","dog")
%!error assert("dog","cat")
%!error assert("dog",3);
%!error assert(3,"dog");
