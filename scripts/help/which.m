########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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
## @deftypefn {} {} which name @dots{}
## Display the type of each @var{name}.
##
## If @var{name} is defined from a function file, the full name of the file is
## also displayed.
## @seealso{help, lookfor}
## @end deftypefn

function varargout = which (varargin)

  if (nargin == 0 || ! iscellstr (varargin))
    print_usage ();
  endif

  ## FIXME: "-all" option not implemented.  Warn user that only the first
  ##        result found will be returned.  See bug #32088.
  if (any (isall = strcmpi (varargin, "-all")))
    warning (["which: '-all' not yet implemented - only the first result ", ...
             "will be returned\n"]);
    varargin = varargin(! isall);
    nargin = nargin - sum (isall);
  endif

  m = __which__ (varargin{:});

  ## Check whether each name is a variable, variables take precedence over
  ## functions in name resolution.
  for i = 1:nargin
    m(i).is_variable = evalin ("caller",
                               ['exist ("' undo_string_escapes(m(i).name) '", "var")'], "");
    if (m(i).is_variable)
      m(i).file = "variable";
    endif
  endfor

  if (nargout == 0)
    for i = 1:nargin
      if (m(i).is_variable)
        printf ("'%s' is a variable\n", m(i).name);
      elseif (isempty (m(i).file))
        if (! isempty (m(i).type))
          printf ("'%s' is a %s\n",
                  m(i).name, m(i).type);
        endif
      else
        if (isempty (m(i).type))
          if (isfolder (m(i).file))
            printf ("'%s' is the directory %s\n",
                    m(i).name, m(i).file);
          else
            printf ("'%s' is the file %s\n",
                    m(i).name, m(i).file);
          endif
        else
          printf ("'%s' is a %s from the file %s\n",
                  m(i).name, m(i).type, m(i).file);
        endif
      endif
    endfor
  else
    varargout = {m.file};
    ## Return type, instead of "", for built-in classes (bug #50541).
    ## FIXME: remove code if __which__ is updated to return path for classes
    idx = find (cellfun ("isempty", varargout));
    if (idx)
      varargout(idx) = m(idx).type;
    endif
  endif

endfunction


%!test
%! str = which ("ls");
%! assert (str(end-17:end), fullfile ("miscellaneous", "ls.m"));
%!test
%! str = which ("fftw");
%! assert (str(end-7:end), "fftw.oct");
%!assert <*49434> (which ("inputParser"), file_in_loadpath ("inputParser.m"))
%!test
%! x = 3;
%! str = which ("x");
%! assert (str, "variable");

%!assert (which ("__NO_SUCH_NAME__"), "")

%!test
%! str = which ("fftw");
%! assert (str(end-7:end), "fftw.oct");
%! fftw = 12;
%! str = which ("fftw");
%! assert (str, "variable");
%! clear fftw;
%! str = which ("fftw");
%! assert (str(end-7:end), "fftw.oct");

%!error <Invalid call> which ()
%!error <Invalid call> which (1)
%!warning <'-all' not yet implemented> which ("1", "-all")

