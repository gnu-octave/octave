## Copyright (C) 2018 Guillaume Flandin
##
## This filename is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the filename COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} openfig
## @deftypefnx {} {} openfig (@var{filename})
## @deftypefnx {} {} openfig (@dots{}, @var{copies})
## @deftypefnx {} {} openfig (@dots{}, @var{visibility})
## @deftypefnx {} {@var{h} =} openfig (@dots{})
## Open a saved figure window from @var{filename} and return its graphics handle
## @var{h}.
##
## By default, @var{filename} is @qcode{"Untitled.fig"}.  If a full path is not
## specified, the file opened will be the first one encountered in the load
## path.  If @var{filename} is not found and does not have an extension, a
## search will take place for the first file in the load path with extension
## @qcode{".fig"} or @qcode{".ofig"}, in that order.
##
## @var{copies} is an optional input indicating whether a new figure should
## always be created (@qcode{"new"}) or if an already opened one can be reused
## (@qcode{"reuse"}).  Default is @qcode{"new"}.
##
## @var{visibility} is an optional input indicating whether to make the figure
## visible (@qcode{"visible"}) or not (@qcode{"invisible"}).  Default is
## @qcode{"visible"} unless this setting is stored in @var{filename}, in which
## case the latter will be used instead.
##
## @seealso{open, hgload, savefig, struct2hdl}
## @end deftypefn

function h = openfig (filename = "Untitled.fig", varargin)

  if (nargin > 3)
    print_usage ();
  endif

  ## Check input filename
  [d,n,ext] = fileparts (filename);
  if (isempty (file_in_loadpath (filename)))
    if (isempty (ext))
      filename = fullfile (d, [n ".fig"]);
      if (isempty (file_in_loadpath (filename)))
        filename = fullfile (d, [n ".ofig"]);
        if (isempty (file_in_loadpath (filename)))
          error ("openfig: cannot find file '%s'", filename);
        endif
      endif
    else
      error ("openfig: cannot find file '%s'", filename);
    endif
  endif
  filename = file_in_loadpath (filename);

  ## Get optional arguments
  copies = true;
  visibility = {};
  for i = 1:numel (varargin)
    if (! ischar (varargin{i}))
      error ("openfig: input argument must be a char array");
    endif
    switch (tolower (varargin{i}))
      case "reuse"
        copies = false;
      case "new"
        copies = true;
      case "visible"
        visibility = {"Visible", "on"};
      case "invisible"
        visibility = {"Visible", "off"};
      otherwise
        error ("openfig: unknown option '%s'", varargin{i});
    endswitch
  endfor

  ## Reuse an existing figure?
  if (! copies)
    h = findobj (allchild (0), "Type", "figure", "FileName", filename);
    if (! isempty (h))
      h = h(end);
      if (! isempty (visibility))
        set (h, visibility{:});
      endif
      movegui (h, "onscreen");
      return;
    endif
  endif

  ## Load graphics objects from file
  prop_struct = cell2struct (visibility(2:2:end), visibility(1:2:end), 2);
  h = hgload (filename, prop_struct);
  set (h, "FileName", filename);

endfunction

%!error openfig (1, 2, 3, 4)
%!error openfig ("%%_A_REALLY_UNLIKELY_FILENAME_%%")

%!test
%! unwind_protect
%!   h1 = figure ("Visible", "off");
%!   ftmp = [tempname() ".ofig"];
%!   hgsave (h1, ftmp);
%!   close (h1);
%!   h2 = openfig (ftmp, "new", "invisible");
%!   h3 = openfig (ftmp, "reuse");
%!   assert (h2 == h3);
%!   close (h2);
%! unwind_protect_cleanup
%!   unlink (ftmp);
%!   try, close (h1); end_try_catch
%!   try, close (h2); end_try_catch
%!   try, close (h3); end_try_catch
%! end_unwind_protect
