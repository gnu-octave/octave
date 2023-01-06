########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{status} =} web ()
## @deftypefnx {} {@var{status} =} web (@var{url})
## @deftypefnx {} {@var{status} =} web (@var{url}, @var{option})
## @deftypefnx {} {@var{status} =} web (@var{url}, @var{option_1}, @dots{}, @var{option_N})
## @deftypefnx {} {[@var{status}, @var{h}, @var{url}] =} web (@dots{})
##
## Open @var{url} in the default system web browser.
##
## With no arguments given, the address @url{https://www.octave.org} is
## opened.
##
## Additional options can be passed for @sc{matlab} compatibility, but are
## ignored.
##
## @itemize @bullet
## @item
## @samp{-browser} Open @var{url} in the default system browser.
##
## @item
## @samp{-new} No effect on the system browser.
##
## @item
## @samp{-noaddressbox} No effect on the system browser.
##
## @item
## @samp{-notoolbar} No effect on the system browser.
##
## @end itemize
##
## The return value @var{status} has one of the values:
##
## @itemize @bullet
## @item
## @samp{0} Found and opened system browser successfully.
##
## @item
## @samp{1} Cannot find the system browser.
##
## @item
## @samp{2} System browser found, but an error occurred.
##
## @end itemize
##
## The return values @var{handle} and @var{url} are currently unimplemented
## but given for compatibility.
##
## @seealso{weboptions, webread, webwrite, urlread, urlwrite}
## @end deftypefn

function [status, h, url] = web (url, varargin)

  if (nargin == 0)
    url = "https://www.octave.org";
  endif

  if (! (ischar (url) && isrow (url)))
    error ("web: URL must be a string");
  endif

  for i = 1:numel (varargin)
    validatestring (varargin{i},
                    {"-browser", "-new", "-noaddressbox", "-notoolbar"});
  endfor

  ## Store text after "text://" to a temporary file and open it.
  if (strncmpi (url, "text://", 7))
    fname = [tempname() ".html"];
    fid = fopen (fname, "w");
    if (fid < 0)
      error ("web: could not open temporary file for text:// content");
    endif
    fprintf (fid, "%s", url(8:end));
    fclose (fid);
    url = ["file://" fname];
  endif

  sts = __open_with_system_app__ (url);
  sts = ifelse (sts == 1, 0, 2);

  h = [];  # Empty handle, as we cannot control an external browser.

  ## For Matlab compatibility.
  if (any (strcmp (varargin, "-browser")))
    url = [];
  endif

  if (nargout > 0)
    status = sts;
  endif

endfunction


%!error <URL must be a string> web (1)
%!error <URL must be a string> web ('')
%!error <'-invalid_Option' does not match>
%!  web ("https://www.octave.org", "-invalid_Option")
