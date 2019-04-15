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
## @deftypefn  {} {@var{response} =} webwrite (@var{url}, @var{name1}, @var{value1},...)
## @deftypefnx {} {@var{response} =} webwrite (@var{url}, @var{data})
## @deftypefnx {} {@var{response} =} webwrite (..., @var{options})
##
## Write data to RESTful web services.
##
## Writes content to web service specified by @var{url} and returns the
## response in @var{response}.
##
## All pairs @var{name1}, @var{value1}... are given, adds these key-value
## pairs of query parameters to the body of request method (@code{get},
## @code{post}, @code{put}, etc).
##
## @var{options} is a @code{weboptions} object to be used to add other HTTP
## request options.  You can use this option with any of the input arguments of
## the previous syntax.
##
## See @code{help weboptions} for a complete list of supported HTTP options.
##
## @seealso{weboptions, webread, websave}
## @end deftypefn

function response = webwrite (url, varargin)

  if (numel (varargin) < 1)
    print_usage();
  endif

  if (! (ischar (url) && isvector (url)))
    error ("webwrite: URL must be a string");
  endif

  options = weboptions;
  has_weboptions = false;

  if (isa (varargin{end}, "weboptions"))
    has_weboptions = true;
    options = varargin{end};
  endif

  if (strcmp (options.MediaType, "auto"))
    options.MediaType = "application/x-www-form-urlencoded";
  endif

  ## If MediaType is set by the user, append it to other headers.
  if (! strcmp (options.CharacterEncoding, "auto"))
    options.HeaderFields{end+1, 1} = "Content-Type";
    options.HeaderFields{end, 2} = [options.MediaType,...
                                  "; charset=", options.CharacterEncoding];
  endif

  if (! isempty (options.KeyName))
    options.HeaderFields{end+1, 1} = options.KeyName;
    options.HeaderFields{end, 2} = options.KeyValue;
  endif

  if (strcmp (options.RequestMethod, "auto"))
    options.RequestMethod = "post";
  endif

  ## Flatten the cell array because the internal processing takes place on
  ## a flattened array.
  options.HeaderFields = options.HeaderFields(:)';

  if (numel (varargin) == 2)
    if ((ischar (varargin{1}) && isvector (varargin{1})) && has_weboptions)
      param = strsplit (varargin{1}, {"=", "&"});
      response = __restful_service__ (url, param, options);
    elseif (! has_weboptions && iscellstr (varargin))
      response = __restful_service__ (url, varargin, options);
    else
      error ("webwrite: data should be a character array or string.");
    endif
  elseif (rem (numel (varargin), 2) == 1 && has_weboptions)
    if (iscellstr (varargin(1:end-1)))
      response = __restful_service__ (url, varargin(1:end-1), options);
    else
      error ("webwrite: Keys and Values must be string.");
    endif
  elseif (rem (numel (varargin), 2) == 0 && ! has_weboptions)
    if (iscellstr (varargin))
      response = __restful_service__ (url, varargin, options);
    else
      error ("webwrite: Keys and Values must be string.");
    endif
  else
    error ("webwrite: Wrong input arguments");
  endif
endfunction
