## Copyright (C) 2009-2012 S�ren Hauberg
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
## @deftypefn  {Command} {} type @var{name} @dots{}
## @deftypefnx {Command} {} type -q @var{name} @dots{}
## @deftypefnx {Function File} {dfns =} type ("@var{name}", @dots{})
## Display the definition of each @var{name} that refers to a function.
##
## Normally also displays whether each @var{name} is user-defined or built-in;
## the @option{-q} option suppresses this behavior.
##
## If an output argument is requested nothing is displayed.  Instead, a cell
## array of strings is returned, where each element corresponds to the
## definition of each requested function.
## @end deftypefn

function retval = type (varargin)
  ## Parse input
  if (nargin == 0)
    error ("type: not enough input arguments");
  endif

  if (!iscellstr (varargin))
    error ("type: input arguments must be strings");
  endif

  quiet = false;
  idx = strcmpi (varargin, "-q") | strcmpi (varargin, "-quiet");
  if (any (idx))
    quiet = true;
    varargin (idx) = [];
  endif

  if (nargout > 0)
    retval = cell (size (varargin));
  endif

  for n = 1:length (varargin)
    name = varargin {n};

    ## Find function and get its code
    text = "";
    cmd = sprintf ("exist ('%s')", name);
    e = evalin ("caller", cmd);
    if (e == 1)
      ## Variable
      cmd = sprintf ("disp (%s);", name);
      desc = evalin ("caller", cmd);
      if (quiet)
        text = desc;
      else
        text = sprintf ("%s is a variable\n%s", name, desc);
      endif
    elseif (e == 2)
      ## m-file or ordinary file
      file = which (name);
      if (isempty (file))
        ## 'name' is an ordinary file, and not a function name.
        ## FIXME: Should we just print it anyway?
        error ("type: '%s' undefined\n", name);
      endif

      ## Read the file
      fid = fopen (file, "r");
      if (fid < 0)
        error ("type: couldn't open '%s' for reading", file);
      endif
      contents = char (fread (fid).');
      fclose (fid);

      if (quiet)
        text = contents;
      else
        text = sprintf ("%s is the user-defined function defined from: %s\n\n%s",
                        name, file, contents);
      endif
    elseif (e == 3)
      text = sprintf ("%s is a dynamically-linked function", name);
    elseif (e == 5)
      text = sprintf ("%s is a built-in function", name);
    elseif (any (strcmp (__operators__ (), name)))
      text = sprintf ("%s is an operator", name);
    elseif (any (strcmp (__keywords__ (), name)))
      text = sprintf ("%s is a keyword", name);
    else
      error ("type: '%s' undefined\n", name);
    endif

    ## Should we return the text or print if
    if (nargout == 0)
      disp (text);
    else
      retval {n} = text;
    endif
  endfor
endfunction

%!test
%! var = 1;
%! typestr = type ("var");
%! typestr = typestr{1}(1:17);
%! assert (typestr, "var is a variable");

%!assert (type ('dot'){1}, "dot is a dynamically-linked function")
%!assert (type ('cat'){1}, "cat is a built-in function")
%!assert (type ('+'){1}, "+ is an operator")
%!assert (type ('end'){1}, "end is a keyword")
%!error (type ('NO_NAME'))
