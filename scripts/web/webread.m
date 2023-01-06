########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{response} =} webread (@var{url})
## @deftypefnx {} {@var{response} =} webread (@var{url}, @var{name1}, @var{value1}, @dots{})
## @deftypefnx {} {@var{response} =} webread (@dots{}, @var{options})
##
## Read content from RESTful web service.
##
## Read content from the web service specified by @var{url} and return the
## content in @var{response}.
##
## All key-value pairs given (@var{name1}, @var{value1}, @dots{}) are appended
## as query parameters to @var{url}.  To place a query in the body of the
## message, use @code{webwrite}.  The web service defines the acceptable query
## parameters.
##
## @var{options} is a @code{weboptions} object that may be used to add other
## HTTP request options.  This argument can be used with either calling form.
## See @code{help weboptions} for a complete list of supported HTTP options.
##
## @seealso{weboptions, webwrite}
## @end deftypefn

function response = webread (url, varargin)

  if (nargin == 0)
    print_usage ();
  endif

  if (! (ischar (url) && isrow (url)))
    error ("webread: URL must be a string");
  endif

  if (nargin > 1 && isa (varargin{end}, "weboptions"))
    has_weboptions = true;
    options = varargin{end};
    varargin(end) = [];
  else
    has_weboptions = false;
    options = weboptions ();
  endif

  if (strcmp (options.MediaType, "auto"))
    options.MediaType = "application/x-www-form-urlencoded";
  endif

  ## If MediaType is set by the user, append it to other headers.
  if (! strcmp (options.CharacterEncoding, "auto"))
    options.HeaderFields{end+1,1} = "Content-Type";
    options.HeaderFields{end,2} = [options.MediaType, ...
                                   "; charset=", options.CharacterEncoding];
  endif

  if (! isempty (options.KeyName))
    options.HeaderFields{end+1,1} = options.KeyName;
    options.HeaderFields{end,2} = options.KeyValue;
  endif

  if (strcmp (options.RequestMethod, "auto"))
    options.RequestMethod = "get";
  endif

  ## Flatten the cell array because the internal processing takes place on
  ## a flattened array.
  options.HeaderFields = options.HeaderFields(:)';

  nargs = 1 + numel (varargin);
  if (nargs == 1)
    response = __restful_service__ (url, cell (), options);
  elseif (rem (nargs, 2) == 1)
    if (! iscellstr (varargin))
      error ("webwrite: KEYS and VALUES must be strings");
    else
      response = __restful_service__ (url, varargin, options);
    endif
  else
    error ("webread: KEYS/VALUES must occur in pairs");
  endif

endfunction


## Test input validation
%!error <Invalid call> webread ()
%!error <URL must be a string> webread (1)
%!error <URL must be a string> webread (["a";"b"])
%!error <KEYS and VALUES must be strings> webread ("URL", "NAME1", 5)
%!error <KEYS/VALUES must occur in pairs> webread ("URL", "KEY1")
