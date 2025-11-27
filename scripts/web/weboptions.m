########################################################################
##
## Copyright (C) 2018-2025 The Octave Project Developers
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

classdef weboptions < handle

  ## -*- texinfo -*-
  ## @deftypefn  {} {@var{options} =} weboptions ()
  ## @deftypefnx {} {@var{options} =} weboptions (@var{name1}, @var{value1}, @dots{})
  ##
  ## Specify parameters for RESTful web services.
  ##
  ## When called with with no inputs return a default @code{weboptions} object
  ## to specify parameters for a request to a web service.  A @code{weboptions}
  ## object is an optional input argument to the @code{webread} and
  ## @code{webwrite} functions.
  ##
  ## Multiple name and value pair arguments may be specified in any order as
  ## @var{name1}, @var{value1}, @var{name2}, @var{value2}, etc.
  ##
  ## The option names must match @strong{exactly} one of those specified in the
  ## table below.
  ##
  ## The following options are available:
  ##
  ## @itemize @bullet
  ## @item
  ## @samp{CharacterEncoding} --- Specify the character encoding of the data:
  ##
  ## @samp{auto} (default), @samp{UTF-8}, @samp{US-ASCII}.
  ## @samp{auto} chooses an encoding based on the content-type of the data.
  ##
  ## @item
  ## @samp{UserAgent} --- Specify the User Agent for the connection.
  ##
  ## Default value is @samp{Octave/version}, where @samp{version} is the
  ## current version of Octave as returned by @code{version}.
  ##
  ## @item
  ## @samp{Timeout} --- Specify the timeout value for the connection in
  ## seconds.
  ##
  ## Default is 5 seconds.  The special value @samp{Inf} sets the timeout to
  ## the maximum value of 2147.483647 seconds.
  ##
  ## @item
  ## @samp{Username} --- User identifier for a basic HTTP connection.
  ##
  ## Default is @qcode{''}.  It must be a string.
  ##
  ## @item
  ## @samp{Password} --- User authentication password for HTTP connection.
  ##
  ## Default is @qcode{''}.  It must be a string or character vector.
  ##
  ## Programming Note: If you display a @code{weboption} object with the
  ## Password property set, the value is displayed as a string containing
  ## @qcode{'*'}.  However, the object stores the value of the Password
  ## property as plain text.
  ##
  ## @item
  ## @samp{KeyName} --- Specify the name of an additional key to be added to
  ## the HTTP request header.
  ##
  ## It must be a string or character vector.  It should be coupled with
  ## @samp{KeyValue}.
  ##
  ## @item
  ## @samp{KeyValue} --- Specify the value of the key @samp{KeyName}.
  ##
  ## @samp{KeyName} must already be assigned in order to specify this field.
  ##
  ## @item
  ## @samp{@nospell{HeaderFields}} --- Specify header fields for the
  ## connection.
  ##
  ## Names and values of header fields, specified as an m-by-2 cell array of
  ## strings, to add to the HTTP request header.
  ## @code{@nospell{HeaderFields}@{i,1@}} is the name of a field and
  ## @code{@nospell{HeaderFields}@{i,2@}} is its value.
  ##
  ## @example
  ## @group
  ## weboptions ("HeaderFields",
  ##             @{"Content-Length", "78" ;
  ##              "Content-Type", "application/json"@})
  ## @end group
  ## @end example
  ##
  ## @noindent
  ## creates a weboptions object that contains two header fields:
  ## @code{Content-Length} with value @code{78} and @code{Content-Type} with
  ## value @code{application/json}.
  ##
  ## @item
  ## @samp{ContentType} --- Specify the content type of the data.
  ##
  ## The following values are available:
  ## @samp{auto}, @samp{text}, @samp{json}
  ##
  ## Default is @samp{auto}.  It automatically determines the content type.
  ## All other formats like @samp{audio}, @samp{binary}, etc.@: available in
  ## @sc{matlab} are not currently supported.
  ##
  ## @item
  ## @samp{ContentReader} --- Not yet implemented.  Only for @sc{matlab}
  ## compatibility.
  ##
  ## @item
  ## @samp{MediaType} --- Not yet implemented.  Only for @sc{matlab}
  ## compatibility.
  ##
  ## @item
  ## @samp{RequestMethod} --- Specifies the type of request to be made.
  ##
  ## The following methods are available:
  ## @samp{get}, @samp{put}, @samp{post}, @samp{delete}, @samp{patch}
  ##
  ## @code{webread} uses the HTTP GET method.  @code{webwrite} uses the HTTP
  ## POST method as default.
  ##
  ## @item
  ## @samp{ArrayFormat} -- Not yet implemented.  Only for @sc{matlab}
  ## compatibility.
  ##
  ## @item
  ## @samp{CertificateFilename} --- Not yet implemented.  Only for @sc{matlab}
  ## compatibility.
  ## @end itemize
  ##
  ## @seealso{webread, webwrite}
  ## @end deftypefn

  properties
    CharacterEncoding = "auto";
    UserAgent = ["Octave/", version()];
    Timeout = 5;
    Username = "";
    Password = "";
    KeyName = "";
    KeyValue = "";
    HeaderFields = {};
    ContentType = "auto";
    ContentReader = "";
    MediaType = "auto";
    RequestMethod = "auto";
    ArrayFormat = "csv";
    CertificateFilename = "";
  endproperties

  methods

    function f = weboptions (varargin)

      if (rem (numel (varargin), 2) != 0)
        error ("weboptions: invalid number of arguments");
      elseif (numel (varargin) > 28)
        error ("weboptions: invalid number of arguments");
      endif

      propnames = properties (f);
      for i = 1:2:numel (varargin)
        idx = find (strcmpi (varargin{i}, propnames), 1);
        if (isempty (idx))
          error ("weboptions: Undefined field '%s'", varargin{i});
        endif
        f.(propnames{idx}) = varargin{i+1};
      endfor

    endfunction

    function f = set.CharacterEncoding (f, value)
      ## FIXME: Why validate this?  There are many other possible encodings.
      if (! any (strcmpi (value, {'auto', 'US-ASCII', 'UTF-8'})))
        error ("weboptions: Invalid CharacterEncoding value");
      endif
      f.CharacterEncoding = value;
    endfunction

    function f = set.UserAgent (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: UserAgent must be a string");
      endif
      f.UserAgent = value;
    endfunction

    function f = set.Timeout (f, value)
      if (! (isreal (value) && isscalar (value) && value > 0))
        error ("weboptions: Timeout must be a real scalar > 0");
      endif
      if (value == Inf)
        f.Timeout = 2147.483647;
      else
        f.Timeout = value;
      endif
    endfunction

    function f = set.Username (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: Username must be a string");
      endif
      f.Username = value;
    endfunction

    function f = set.Password (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: Password must be a string");
      endif
      f.Password = value;
    endfunction

    function f = set.KeyName (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: invalid KeyName value");
      endif
      f.KeyName = value;
    endfunction

    function f = set.KeyValue (f, value)
      if (isempty (f.KeyName) && ! isempty (value))
        error ("weboptions: KeyName field empty.  Cannot set KeyValue.");
      endif
      f.KeyValue = value;
    endfunction

    function f = set.HeaderFields (f, value)

      if (! isempty (value))
        if (! iscellstr (value))
          error ("weboptions: HeaderFields must be a cell array of strings");
        elseif (ndims (value) != 2 || columns (value) != 2)
          error ("weboptions: HeaderFields must be of size m-by-2");
        endif
      endif
      ## C++ code requires row vector of "prop", "value" pairs.
      f.HeaderFields = (value')(:)';

    endfunction

    function f = set.ContentType (f, value)
      if (! isempty (value))
        if (! any (strcmpi (value, {"auto", "json", "text"})))
          error ("weboptions: invalid ContentType value");
        endif
      endif
      f.ContentType = value;
    endfunction

    function f = set.ContentReader (f, value)
      if (! is_function_handle (value))
        error ("weboptions: ContentReader must be a function handle");
      endif
        ## FIXME: Should emit a warning about unimplemented feature
      f.ContentReader = value;
    endfunction

    function f = set.MediaType (f, value)
      ## FIXME: Should emit a warning about unimplemented feature
      f.MediaType = value;
    endfunction

    function f = set.RequestMethod (f, value)
      if (! isempty (value))
        if (! any (strcmpi (value,
                            {"auto", "get", "put", "post", "delete", "patch"})))
          error ("weboptions: invalid RequestMethod value");
        endif
      endif
      f.RequestMethod = value;

    endfunction

    function f = set.ArrayFormat (f, value)
      if (! isempty (value))
        if (! any (strcmpi (value, {"csv", "json", "php", "repeating"})))
          error ("weboptions: invalid ArrayFormat value");
        endif
      endif
      f.ArrayFormat = value;
    endfunction

    function f = set.CertificateFilename (f, value)
      ## FIXME: Should emit a warning about unimplemented feature
      f.CertificateFilename = value;
    endfunction

    function disp (f)

      Timeout = num2str (f.Timeout);
      Password = repmat ("*", 1, numel (num2str (f.Password)));

      if (! isempty (f.ContentReader))
        ContentReader = disp (f.ContentReader);
      else
        ContentReader = "[]";
      endif

      if (! isempty (f.HeaderFields))
        HeaderFields = ['{"', strjoin(f.HeaderFields, '", "'), '"}'];
      else
        HeaderFields = "{}";
      endif

      if (! isempty (f.KeyValue))
        KeyValue = num2str (f.KeyValue);
      else
        KeyValue = "''";
      endif

      output = ["  weboptions with properties:\n",...
                "\n      CharacterEncoding: '", f.CharacterEncoding, "'",...
                "\n              UserAgent: '", f.UserAgent, "'",...
                "\n                Timeout: " , Timeout, "",...
                "\n               Username: '", f.Username, "'",...
                "\n               Password: '", Password, "'",...
                "\n                KeyName: '", f.KeyName, "'",...
                "\n               KeyValue: " , KeyValue,...
                "\n            ContentType: '", f.ContentType, "'",...
                "\n          ContentReader: " , ContentReader,...
                "\n              MediaType: '", f.MediaType, "'",...
                "\n          RequestMethod: '", f.RequestMethod, "'",...
                "\n            ArrayFormat: '", f.ArrayFormat, "'",...
                "\n           HeaderFields: " , HeaderFields,...
                "\n    CertificateFilename: '", f.CertificateFilename];
      disp (output);

    endfunction

  endmethods

endclassdef
