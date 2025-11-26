////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2025 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>
#include <fstream>
#include <iomanip>
#include <algorithm>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "glob-match.h"
#include "oct-env.h"
#include "oct-handle.h"
#include "oct-sysdep.h"
#include "url-transfer.h"

#include "defun.h"
#include "error.h"
#include "interpreter.h"
#include "oct-map.h"
#include "oct-refcount.h"
#include "ov-cell.h"
#include "ov-classdef.h"
#include "ovl.h"
#include "pager.h"
#include "unwind-prot.h"
#include "url-handle-manager.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Helper function to parse property/value pairs into weboptions struct
static void
parse_property_value_pairs (const octave_value_list& args, int start_idx,
                            struct weboptions& options,
                            std::string& method, Array<std::string>& param,
                            const std::string& who)
{
  int nargin = args.length ();

  if ((nargin - start_idx) % 2 != 0)
    error ("%s: property/value arguments must occur in pairs", who.c_str ());

  // Parse property/value pairs
  for (int i = start_idx; i < nargin; i += 2)
    {
      if (! args(i).is_string ())
        error ("%s: property names must be strings", who.c_str ());

      std::string property = args(i).string_value ();

      // Convert property name to lowercase for case-insensitive comparison
      std::transform (property.begin (), property.end (), property.begin (),
                     [] (unsigned char c) { return std::tolower(c); });

      if (property == "get")
        {
          method = "get";
          std::string err_msg = who + ": 'Get' value must be a cell array of strings";
          param = args(i+1).xcellstr_value (err_msg.c_str ());
          if (param.numel () % 2 == 1)
            error ("%s: number of elements in 'Get' cell array must be even", who.c_str ());
        }
      else if (property == "post")
        {
          method = "post";
          std::string err_msg = who + ": 'Post' value must be a cell array of strings";
          param = args(i+1).xcellstr_value (err_msg.c_str ());
          if (param.numel () % 2 == 1)
            error ("%s: number of elements in 'Post' cell array must be even", who.c_str ());
        }
      else if (property == "timeout")
        {
          std::string err_msg = who + ": 'Timeout' value must be numeric";
          double timeout = args(i+1).xdouble_value (err_msg.c_str ());
          if (timeout <= 0)
            error ("%s: 'Timeout' value must be positive", who.c_str ());
          // Convert to milliseconds
          options.Timeout = static_cast<long> (timeout * 1000);
        }
      else if (property == "useragent")
        {
          std::string err_msg = who + ": 'UserAgent' value must be a string";
          options.UserAgent = args(i+1).xstring_value (err_msg.c_str ());
        }
      else if (property == "username")
        {
          std::string err_msg = who + ": 'Username' value must be a string";
          options.Username = args(i+1).xstring_value (err_msg.c_str ());
        }
      else if (property == "password")
        {
          std::string err_msg = who + ": 'Password' value must be a string";
          options.Password = args(i+1).xstring_value (err_msg.c_str ());
        }
      else if (property == "charset")
        {
          // Store for potential future use, but not used by curl_transfer yet
          std::string err_msg = who + ": 'Charset' value must be a string";
          options.CharacterEncoding = args(i+1).xstring_value (err_msg.c_str ());
        }
      else
        {
          error ("%s: unknown property '%s'", who.c_str (), args(i).string_value ().c_str ());
        }
    }
}

DEFUN (urlwrite, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} urlwrite (@var{url}, @var{localfile})
@deftypefnx {} {} urlwrite (@var{url}, @var{localfile}, @var{Name}, @var{Value}, @dots{})
@deftypefnx {} {@var{f} =} urlwrite (@var{url}, @var{localfile}, @dots{})
@deftypefnx {} {[@var{f}, @var{success}] =} urlwrite (@var{url}, @var{localfile}, @dots{})
@deftypefnx {} {[@var{f}, @var{success}, @var{message}] =} urlwrite (@var{url}, @var{localfile}, @dots{})
Download a remote file specified by its @var{url} and save it as
@var{localfile}.

For example:

@example
@group
urlwrite ("http://ftp.octave.org/pub/README",
          "README.txt");
@end group
@end example

The full path of the downloaded file is returned in @var{f}.

The variable @var{success} is 1 if the download was successful,
otherwise it is 0 in which case @var{message} contains an error message.

If no output argument is specified and an error occurs, then the error is
signaled through Octave's error handling mechanism.

This function uses libcurl.  The curl library supports, among others, the HTTP,
FTP, and FILE protocols.  Username and password may be specified in the URL,
for example:

@example
@group
urlwrite ("http://username:password@@example.com/file.txt",
          "file.txt");
@end group
@end example

Additional options can be specified using property/value pairs:

@table @code
@item Get
Cell array of name/value pairs for GET request parameters.

@item Post
Cell array of name/value pairs for POST request parameters.

@item Timeout
Timeout value in seconds.

@item UserAgent
User agent string for the request.

@item Username
Username for authentication.

@item Password
Password for authentication.
@end table

Example with property/value pairs:
@example
@group
urlwrite ("http://www.example.com/data.txt", "data.txt",
          "Timeout", 10, "UserAgent", "Octave");
@end group
@end example

For backward compatibility, the old calling form is also supported:
@example
@group
urlwrite ("http://www.google.com/search", "search.html",
          "get", @{"query", "octave"@});
@end group
@end example
@seealso{urlread, weboptions}
@end deftypefn */)
{
  int nargin = args.length ();

  // verify arguments
  if (nargin < 2)
    print_usage ();

  std::string url = args(0).xstring_value ("urlwrite: URL must be a string");

  // name to store the file if download is successful
  std::string filename = args(1).xstring_value ("urlwrite: LOCALFILE must be a string");

  std::string method = "get";
  Array<std::string> param;

  // Create a weboptions struct to hold option values
  struct weboptions options;
  // Set default timeout to match weboptions default
  options.Timeout = 5000;  // 5 seconds in milliseconds

  // Check if old calling form with 4 arguments (backward compatibility)
  if (nargin == 4 && args(2).is_string ()
      && (args(2).string_value () == "get" || args(2).string_value () == "post"))
    {
      // Old calling form: urlwrite(url, file, method, param)
      method = args(2).xstring_value ("urlwrite: METHOD must be a string");

      if (method != "get" && method != "post")
        error (R"(urlwrite: METHOD must be "get" or "post")");

      param = args(3).xcellstr_value ("urlwrite: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlwrite: number of elements in PARAM must be even");
    }
  else if (nargin > 2)
    {
      // New property/value pairs form
      parse_property_value_pairs (args, 2, options, method, param, "urlwrite");
    }

  // The file should only be deleted if it doesn't initially exist, we
  // create it, and the download fails.  We use unwind_protect to do
  // it so that the deletion happens no matter how we exit the function.

  std::ofstream ofile =
    sys::ofstream (filename.c_str (), std::ios::out | std::ios::binary);

  if (! ofile.is_open ())
    error ("urlwrite: unable to open file");

  int(*unlink_fptr)(const std::string&) = sys::unlink;
  unwind_action_safe unlink_action (unlink_fptr, filename);

  url_transfer url_xfer (url, ofile);

  octave_value_list retval;

  if (! url_xfer.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  // Apply weboptions if any were set
  url_xfer.set_weboptions (options);

  // Perform the HTTP action
  url_xfer.http_action (param, method);

  ofile.close ();

  if (url_xfer.good ())
    unlink_action.discard ();

  if (nargout > 0)
    {
      if (url_xfer.good ())
        retval = ovl (sys::env::make_absolute (filename), true, "");
      else
        retval = ovl ("", false, url_xfer.lasterror ());
    }

  if (nargout < 2 && ! url_xfer.good ())
    error ("urlwrite: %s", url_xfer.lasterror ().c_str ());

  return retval;
}

DEFUN (urlread, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{s} =} urlread (@var{url})
@deftypefnx {} {@var{s} =} urlread (@var{url}, @var{Name}, @var{Value}, @dots{})
@deftypefnx {} {[@var{s}, @var{success}] =} urlread (@var{url}, @dots{})
@deftypefnx {} {[@var{s}, @var{success}, @var{message}] =} urlread (@var{url}, @dots{})
Download a remote file specified by its @var{url} and return its content
in string @var{s}.

For example:

@example
s = urlread ("http://ftp.octave.org/pub/README");
@end example

The variable @var{success} is 1 if the download was successful,
otherwise it is 0 in which case @var{message} contains an error
message.

If no output argument is specified and an error occurs, then the error is
signaled through Octave's error handling mechanism.

This function uses libcurl.  The curl library supports, among others, the HTTP,
FTP, and FILE protocols.  Username and password may be specified in the URL@.
For example:

@example
s = urlread ("http://user:password@@example.com/file.txt");
@end example

Additional options can be specified using property/value pairs:

@table @code
@item Get
Cell array of name/value pairs for GET request parameters.

@item Post
Cell array of name/value pairs for POST request parameters.

@item Timeout
Timeout value in seconds.

@item UserAgent
User agent string for the request.

@item Username
Username for authentication.

@item Password
Password for authentication.

@item Charset
Character encoding (stored but not currently used).
@end table

Example with property/value pairs:
@example
@group
s = urlread ("http://www.example.com/data",
             "Get", @{"term", "octave"@}, "Timeout", 10);
@end group
@end example

For backward compatibility, the old calling form is also supported:
@example
@group
s = urlread ("http://www.google.com/search",
             "get", @{"query", "octave"@});
@end group
@end example
@seealso{urlwrite, weboptions}
@end deftypefn */)
{
  int nargin = args.length ();

  // verify arguments
  if (nargin < 1)
    print_usage ();

  std::string url = args(0).xstring_value ("urlread: URL must be a string");

  std::string method = "get";
  Array<std::string> param;

  // Create a weboptions struct to hold option values
  struct weboptions options;
  // Set default timeout to match weboptions default
  options.Timeout = 5000;  // 5 seconds in milliseconds

  // Check if old calling form with 3 arguments (backward compatibility)
  if (nargin == 3 && args(1).is_string ()
      && (args(1).string_value () == "get" || args(1).string_value () == "post"))
    {
      // Old calling form: urlread(url, method, param)
      method = args(1).xstring_value ("urlread: METHOD must be a string");

      if (method != "get" && method != "post")
        error (R"(urlread: METHOD must be "get" or "post")");

      param = args(2).xcellstr_value ("urlread: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlread: number of elements in PARAM must be even");
    }
  else if (nargin > 1)
    {
      // New property/value pairs form
      parse_property_value_pairs (args, 1, options, method, param, "urlread");
    }

  std::ostringstream buf;

  url_transfer url_xfer = url_transfer (url, buf);

  if (! url_xfer.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  // Apply weboptions if any were set
  url_xfer.set_weboptions (options);

  // Perform the HTTP action
  url_xfer.http_action (param, method);

  if (nargout < 2 && ! url_xfer.good ())
    error ("urlread: %s", url_xfer.lasterror ().c_str ());

  octave_value_list retval (std::max (1, std::min (nargout, 3)));

  retval(0) = buf.str ();
  if (nargout > 1)
    retval(1) = url_xfer.good ();
  if (nargout > 2)
    retval(2) = url_xfer.good () ? "" : url_xfer.lasterror ();

  return retval;
}

DEFUN (__restful_service__, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{response} =} __restful_service__ (@var{url}, @var{param}, @var{weboptions})
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1)
    print_usage ();

  std::string url = args(0).xstring_value ("__restful_service__: URL must be a string");

  std::ostringstream content;

  url_transfer url_xfer (url, content);

  if (! url_xfer.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  Array<std::string> param = args(1).cellstr_value ();

  std::string data, method;

  struct weboptions options;

  cdef_object object
    = args (nargin - 1).classdef_object_value () -> get_object ();

  // We could've used object.map_value () instead to return a map but that
  // shows a warning about about overriding access restrictions.
  // Nevertheless, we are keeping checking that here if the keys are not
  // equal to "delete" and "display", getting away with the warning.
  string_vector keys = object.map_keys ();

  for (int i = 0; i < keys.numel (); i++)
    {
      if (keys(i) == "Timeout")
        {
          float timeout = object.get (keys(i)).float_value ();
          options.Timeout = static_cast<long> (timeout * 1000);
        }

      if (keys(i) == "HeaderFields")
        {
          options.HeaderFields = object.get (keys(i)).cellstr_value ();
        }

      // FIXME: 'delete' and 'display', auto-generated, probably by cdef_object
      // class?  Remaining fields have already been adjusted elsewhere in the
      // m-script.  Set 'value' as the Value of the Key wherever it's a string.
      if (keys(i) != "Timeout" && keys(i) != "HeaderFields"
          && keys(i) != "delete" && keys(i) != "display")
        {
          std::string value = object.get (keys(i)).string_value ();

          if (keys(i) == "UserAgent")
            options.UserAgent = value;

          if (keys(i) == "Username")
            options.Username = value;

          if (keys(i) == "Password")
            options.Password = value;

          if (keys(i) == "ContentReader")
            // Unimplemented.  Only for MATLAB compatibility.
            options.ContentReader = "";

          if (keys(i) == "RequestMethod")
            method = value;

          if (keys(i) == "ArrayFormat")
            options.ArrayFormat = value;

          if (keys(i) == "CertificateFilename")
            options.CertificateFilename = "";
        }
    }

  url_xfer.set_weboptions (options);

  url_xfer.http_action (param, method);

  if (nargout < 2 && ! url_xfer.good ())
    error ("__restful_service__: %s", url_xfer.lasterror ().c_str ());

  return ovl (content.str ());
}

OCTAVE_END_NAMESPACE(octave)
