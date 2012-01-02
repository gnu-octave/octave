## Copyright (C) 2010-2012 John W. Eaton
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
## @deftypefn {Loadable Function} {} dispatch (@var{f}, @var{r}, @var{type})
##
## Replace the function @var{f} with a dispatch so that function @var{r}
## is called when @var{f} is called with the first argument of the named
## @var{type}.  If the type is @var{any} then call @var{r} if no other type
## matches.  The original function @var{f} is accessible using
## @code{builtin (@var{f}, @dots{})}.
##
## If @var{r} is omitted, clear dispatch function associated with @var{type}.
##
## If both @var{r} and @var{type} are omitted, list dispatch functions
## for @var{f}.
## @seealso{builtin}
## @end deftypefn

## Deprecated in version 3.4

function varargout = dispatch (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "dispatch is obsolete and will be removed from a future version of Octave; please use classes instead");
  endif

  varargout = cell (nargout, 1);
  [ varargout{:} ] = __dispatch__ (varargin{:});

endfunction


%!test # builtin function replacement
%! dispatch('sin','length','string')
%! assert(sin("abc"),3)
%! assert(sin(0),0,10*eps);

%!test # 'any' function
%! dispatch('sin','exp','any')
%! assert(sin(0),1,eps);
%! assert(sin("abc"),3);

%!test # 'builtin' function
%! assert(builtin('sin',0),0,eps);
%! builtin('eval','x=1;');
%! assert(x,1);

%!test # clear function mapping
%! dispatch('sin','string')
%! dispatch('sin','any')
%! assert(sin(0),0,10*eps);

%!test # oct-file replacement
%! dispatch('fft','length','string')
%! assert(fft([1,1]),[2,0]);
%! assert(fft("abc"),3)
%! dispatch('fft','string');

%!test # m-file replacement
%! dispatch('hamming','length','string')
%! assert(hamming(1),1)
%! assert(hamming("abc"),3)
%! dispatch('hamming','string')

%!test # override preloaded builtin
%! evalin('base','cos(1);');
%! dispatch('cos','length','string')
%! evalin('base','assert(cos("abc"),3)');
%! evalin('base','assert(cos(0),1,eps)');
%! dispatch('cos','string')

%!test # override pre-loaded oct-file
%! evalin('base','qr(1);');
%! dispatch('qr','length','string')
%! evalin('base','assert(qr("abc"),3)');
%! evalin('base','assert(qr(1),1)');
%! dispatch('qr','string');

%!test # override pre-loaded m-file
%! evalin('base','hanning(1);');
%! dispatch('hanning','length','string')
%! evalin('base','assert(hanning("abc"),3)');
%! evalin('base','assert(hanning(1),1)');
%! dispatch('hanning','string');
