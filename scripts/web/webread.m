## Copyright (C) 2018-2019 Sahil Yadav <yadavsahil5198@gmail.com>
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

## -*- texinfo -*-
## @deftypefn  {} {@var{response} =} webread (@var{url})
## @deftypefnx {} {@var{response} =} webread (@var{url}, @var{name1}, @var{value1},...)
## @deftypefnx {} {@var{response} =} webread (..., @var{options})
##
## Read content from RESTful web service.
##
## Reads content from the web service specified by @var{url} and returns the
## content in @var{response}.
##
## If the pairs @var{name1}, @var{value1}... are given, append these key-value
## pairs of query parameters to @var{url}.  To put a query into the body of the
## message, use @code{webwrite}.  The web service defines the query parameters.
##
## @var{options} is a @code{weboptions} object to be used to add other HTTP
## request options.  You can use this option with any of the input arguments of
## the previous syntax.
##
## See help text for @code{weboptions} to see the complete list of supported
## HTTP operations.
##
## @seealso{weboptions, webwrite, websave}
## @end deftypefn

function response = webread (url, varargin)

  if (nargin < 1)
    print_usage();
  endif

  if (! (ischar (url) && isvector (url)))
    error ("webread: URL must be a string");
  endif

  has_weboptions = false;
  options = weboptions;

  if (numel (varargin) > 0)
    if (isa (varargin{end}, "weboptions"))
      has_weboptions = true;
      options = varargin{end};
    endif
  endif

  if (strcmp (options.MediaType, "auto"))
    options.MediaType = "application/x-www-form-urlencoded";
  endif

  ## If MediaType is set by the user, append it to other headers.
  if (! strcmp (options.CharacterEncoding, "auto"))
    options.HeaderFields{end+1,1} = "Content-Type";
    options.HeaderFields{end,2} = [options.MediaType,...
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

  if (nargin == 1 || (nargin == 2 && has_weboptions))
    response = __restful_service__ (url, cell, options);
  elseif (rem (numel (varargin), 2) == 1 && has_weboptions)
    if (iscellstr (varargin(1:end-1)))
      response = __restful_service__ (url, varargin(1:end-1), options);
    else
      error ("webread: Keys and Values must be string.");
    endif
  elseif (rem (numel (varargin), 2) == 0 && ! has_weboptions)
    if (iscellstr (varargin))
      response = __restful_service__ (url, varargin, options);
    else
      error ("webread: Keys and Values must be string.");
    endif
  else
    error ("webread: Wrong input arguments");
  endif
endfunction
