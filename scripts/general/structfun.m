## Copyright (C) 2007, 2008, 2009 David Bateman
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
## @deftypefn {Function File} {} structfun (@var{func}, @var{s})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} structfun (@dots{})
## @deftypefnx {Function File} {} structfun (@dots{}, "ErrorHandler", @var{errfunc})
## @deftypefnx {Function File} {} structfun (@dots{}, "UniformOutput", @var{val})
## 
## Evaluate the function named @var{name} on the fields of the structure
## @var{s}.  The fields of @var{s} are passed to the function @var{func}
## individually.
##
## @code{structfun} accepts an arbitrary function @var{func} in the form of 
## an inline function, function handle, or the name of a function (in a 
## character string).  In the case of a character string argument, the 
## function must accept a single argument named @var{x}, and it must return 
## a string value.  If the function returns more than one argument, they are
## returned as separate output variables.
##
## If the parameter "UniformOutput" is set to true (the default), then the function
## must return a single element which will be concatenated into the
## return value.  If "UniformOutput" is false, the outputs are placed into a structure
## with the same fieldnames as the input structure.
## 
## @example
## @group
## s.name1 = "John Smith"; 
## s.name2 = "Jill Jones"; 
## structfun (@@(x) regexp (x, '(\w+)$', "matches")@{1@}, s, 
##            "UniformOutput", false)
##     @result{}
##    @{
##      name1 = Smith
##      name2 = Jones
##    @} 
## @end group
## @end example
## 
## Given the parameter "ErrorHandler", @var{errfunc} defines a function to
## call in case @var{func} generates an error.  The form of the function is
## 
## @example
## function [@dots{}] = errfunc (@var{se}, @dots{})
## @end example
## 
## where there is an additional input argument to @var{errfunc} relative to
## @var{func}, given by @var{se}.  This is a structure with the elements
## "identifier", "message" and "index", giving respectively the error
## identifier, the error message, and the index into the input arguments
## of the element that caused the error.  For an example on how to use
## an error handler, @pxref{doc-cellfun, @code{cellfun}}. 
##
## @seealso{cellfun, arrayfun}
## @end deftypefn

function varargout = structfun (fun, s, varargin);
  if (nargin < 2)
    print_usage ();
  endif

  varargout = cell (max ([nargout, 1]), 1);
  [varargout{:}] = cellfun (fun, struct2cell (s), varargin{:});

  if (iscell (varargout{1}))
    [varargout{:}] = cell2struct (varargout{1}, fieldnames(s), 1);
  endif
endfunction


%!test
%! s.name1 = "John Smith"; 
%! s.name2 = "Jill Jones"; 
%! l.name1 = "Smith";
%! l.name2 = "Jones";
%! o = structfun (@(x) regexp (x, '(\w+)$', "matches"){1}, s, 
%!		  "UniformOutput", false);
%! assert (o, l);
