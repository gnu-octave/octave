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

classdef weboptions < handle

  ## -*- texinfo -*-
  ## @deftypefn  {} {@var{output} =} weboptions ()
  ## @deftypefnx {} {@var{output} =} weboptions (@var{name1}, @var{value1}, @dots{})
  ##
  ## Specify parameters for RESTful web services.
  ##
  ## @code{weboptions} with no inputs returns a default @code{weboptions}
  ## object to specify parameters for a request to a web service.  A
  ## @code{weboptions} object can be an optional input argument to the
  ## @code{webread} and @code{webwrite} functions.
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
  ## @samp{auto} (default), @samp{UTF-8}, @samp{US-ASCII}
  ## @samp{auto} chooses an encoding based on the content-type of the data.
  ##
  ## @item
  ## @samp{UserAgent} --- Specify the User Agent for the connection.
  ##
  ## Default value is @samp{GNU Octave/version}, where @samp{version} is the
  ## current version of Octave as returned by @code{version}.
  ##
  ## @item
  ## @samp{Timeout} --- Specify the timeout value for the connection in
  ## seconds.
  ##
  ## Default is 10 seconds.  @samp{Inf} is not currently supported.
  ##
  ## @item
  ## @samp{Username} --- User identifier for a basic HTTP connection.
  ##
  ## Default is NULL@.  It must be a string.
  ##
  ## @item
  ## @samp{Password} --- User authentication password for HTTP connection.
  ##
  ## Default is NULL@.  It must be a string or character vector.
  ## Programming Note: If you display a @code{weboption} object with the
  ## Password property set, the value is displayed as a string containing
  ## @qcode{'*'}.  However, the object stores the value of the Password
  ## property as plain text.
  ##
  ## @item
  ## @samp{KeyName} --- Specify the name of an additional key to be added to
  ## the HTTP request header.  It should be coupled with @samp{KeyValue}.  It
  ## must be a string or character vector.
  ##
  ## @item
  ## @samp{KeyValue} --- Specify the value of the key @samp{KeyName}.
  ##
  ## @samp{KeyName} must be present in order to assign to this field.
  ##
  ## @item
  ## @samp{@nospell{HeaderFields}} --- Specify the header fields for the
  ## connection.
  ##
  ## Names and values of header fields, specified as an m-by-2 array of strings
  ## or cell array of character vectors to add to the HTTP request header.
  ## @nospell{HeaderFields}@{i,1@} is the name of a field and
  ## @nospell{HeaderFields}@{i,2@} is its value.
  ##
  ## @example
  ## @group
  ## weboptions ("HeaderFields", @{"Content-Length" "78";"Content-Type" "application/json"@})
  ## Creates a weboptions object that contains two header fields:
  ## Content-Length with value 78 and Content-Type with value application/json.
  ## @end group
  ## @end example
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
    UserAgent = ["GNU Octave/", version()];
    Timeout = 10;
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
      else
        h = cell2struct (varargin(2:2:end), varargin(1:2:end), 2);
        if (numfields (h) > 14)
          error ("weboptions: invalid number of arguments");
        endif

        if (isfield (h, "CharacterEncoding"))
          f.CharacterEncoding = h.CharacterEncoding;
          h = rmfield (h, "CharacterEncoding");
        endif

        if (isfield (h, "UserAgent"))
          f.UserAgent = h.UserAgent;
          h = rmfield (h, "UserAgent");
        endif

        if (isfield (h, "Timeout"))
          f.Timeout = h.Timeout;
          h = rmfield (h, "Timeout");
        endif

        if (isfield (h, "Username"))
          f.Username = h.Username;
          h = rmfield (h, "Username");
        endif

        if (isfield (h, "Password"))
          f.Password = h.Password;
          h = rmfield (h, "Password");
        endif

        if (isfield (h, "KeyName"))
          f.KeyName = h.KeyName;
          h = rmfield (h, "KeyName");
        endif

        if (isfield (h, "KeyValue"))
          f.KeyValue = h.KeyValue;
          h = rmfield (h, "KeyValue");
        endif

        if (isfield (h, "HeaderFields"))
          f.HeaderFields = h.HeaderFields;
          h = rmfield (h, "HeaderFields");
        endif

        if (isfield (h, "ContentType"))
          f.ContentType = h.ContentType;
          h = rmfield (h, "ContentType");
        endif

        if (isfield (h, "ContentReader"))
          f.ContentReader = h.ContentReader;
          h = rmfield (h, "ContentReader");
        endif

        if (isfield (h, "MediaType"))
          f.MediaType = h.MediaType;
          h = rmfield (h, "MediaType");
        endif

        if (isfield (h, "RequestMethod"))
          f.RequestMethod = h.RequestMethod;
          h = rmfield (h, "RequestMethod");
        endif

        if (isfield (h, "ArrayFormat"))
          f.ArrayFormat = h.ArrayFormat;
          h = rmfield (h, "ArrayFormat");
        endif

        if (isfield (h, "CertificateFilename"))
          f.CertificateFilename = h.CertificateFilename;
          h = rmfield (h, "CertificateFilename");
        endif

        if (! isempty (fieldnames (h)))
          field = fieldnames (h){1};
          error (["weboptions: Undefined field " field]);
        endif
      endif

    endfunction

    function f = set.CharacterEncoding (f, value)
      if (! any (strcmpi (value, {"UTF-8", 'US-ASCII', "auto"})))
        error ("weboptions: Invalid CharacterEncoding value");
      else
        f.CharacterEncoding = value;
      endif
    endfunction

    function f = set.UserAgent (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: UserAgent must be a string");
      else
        f.UserAgent = value;
      endif
    endfunction

    function f = set.Timeout (f, value)
      if (! isreal (value) || ! isscalar (value)
          || floor (value) != value || value < 0)
        error ("weboptions: invalid Timeout value");
      else
        f.Timeout = value;
      endif
    endfunction

    function f = set.Username (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: Username must be a string");
      else
        f.Username = value;
      endif
    endfunction

    function f = set.Password (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: Password must be a string");
      else
        f.Password = value;
      endif
    endfunction

    function f = set.KeyName (f, value)
      if (! ischar (value) && ! isrow (value))
        error ("weboptions: invalid KeyName value");
      else
        f.KeyName = value;
      endif
    endfunction

    function f = set.KeyValue (f, value)
      if (isempty (f.KeyName) && ! isempty (value))
        error ("weboptions: field KeyName empty.  Cannot set KeyValue.");
      else
        f.KeyValue = value;
      endif
    endfunction

    function f = set.HeaderFields (f, value)

      if (! isempty (value))
        if (! iscellstr (value) || ! ismatrix (value))
          error ("weboptions: HeaderFields must be array of strings or a cell array");
        elseif (columns (value) != 2)
          error ("weboptions: HeaderFields must be of size m-by-2");
        endif
      endif
      f.HeaderFields = value;

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
      f.ContentReader = value;
    endfunction

    function f = set.MediaType (f, value)
      f.MediaType = value;
    endfunction

    function f = set.RequestMethod (f, value)

      if (! isempty (value))
        if (! any (strcmpi (value, {"auto", "get", "put", "post",...
                                    "delete", "patch"})))
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
      f.CertificateFilename = value;
    endfunction

    function display (f)

      Timeout = int2str (f.Timeout);
      Password = repmat ("*", 1, numel (num2str (f.Password)));

      if (! isempty (f.ContentReader))
        ContentReader = ['["', strjoin(f.ContentReader, '", "'), '"]'];
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

      printf ("%s =", inputname (1));
      output = ["\n\n   weboptions with properties:     \n",...
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
                "\n    CertificateFilename: '", f.CertificateFilename, "'\n"];
      disp (output);

    endfunction

  endmethods

endclassdef

