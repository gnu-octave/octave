////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2023 The Octave Project Developers
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

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-sysdep.h"
#include "oct-env.h"
#include "oct-handle.h"
#include "glob-match.h"
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

DEFUN (urlwrite, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} urlwrite (@var{url}, @var{localfile})
@deftypefnx {} {@var{f} =} urlwrite (@var{url}, @var{localfile})
@deftypefnx {} {[@var{f}, @var{success}] =} urlwrite (@var{url}, @var{localfile})
@deftypefnx {} {[@var{f}, @var{success}, @var{message}] =} urlwrite (@var{url}, @var{localfile})
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

GET and POST requests can be specified by @var{method} and @var{param}.
The parameter @var{method} is either @samp{get} or @samp{post} and
@var{param} is a cell array of parameter and value pairs.
For example:

@example
@group
urlwrite ("http://www.google.com/search", "search.html",
          "get", @{"query", "octave"@});
@end group
@end example
@seealso{urlread}
@end deftypefn */)
{
  int nargin = args.length ();

  // verify arguments
  if (nargin != 2 && nargin != 4)
    print_usage ();

  std::string url = args(0).xstring_value ("urlwrite: URL must be a string");

  // name to store the file if download is successful
  std::string filename = args(1).xstring_value ("urlwrite: LOCALFILE must be a string");

  std::string method;
  Array<std::string> param;

  if (nargin == 4)
    {
      method = args(2).xstring_value ("urlwrite: METHOD must be a string");

      if (method != "get" && method != "post")
        error (R"(urlwrite: METHOD must be "get" or "post")");

      param = args(
                3).xcellstr_value ("urlwrite: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlwrite: number of elements in PARAM must be even");
    }

  // The file should only be deleted if it doesn't initially exist, we
  // create it, and the download fails.  We use unwind_protect to do
  // it so that the deletion happens no matter how we exit the function.

  sys::file_stat fs (filename);

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
@deftypefnx {} {[@var{s}, @var{success}] =} urlread (@var{url})
@deftypefnx {} {[@var{s}, @var{success}, @var{message}] =} urlread (@var{url})
@deftypefnx {} {[@dots{}] =} urlread (@var{url}, @var{method}, @var{param})
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

GET and POST requests can be specified by @var{method} and @var{param}.
The parameter @var{method} is either @samp{get} or @samp{post} and
@var{param} is a cell array of parameter and value pairs.
For example:

@example
@group
s = urlread ("http://www.google.com/search",
             "get", @{"query", "octave"@});
@end group
@end example
@seealso{urlwrite}
@end deftypefn */)
{
  int nargin = args.length ();

  // verify arguments
  if (nargin != 1 && nargin != 3)
    print_usage ();

  std::string url = args(0).xstring_value ("urlread: URL must be a string");

  std::string method;
  Array<std::string> param;

  if (nargin == 3)
    {
      method = args(1).xstring_value ("urlread: METHOD must be a string");

      if (method != "get" && method != "post")
        error (R"(urlread: METHOD must be "get" or "post")");

      param = args(
                2).xcellstr_value ("urlread: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlread: number of elements in PARAM must be even");
    }

  std::ostringstream buf;

  url_transfer url_xfer = url_transfer (url, buf);

  if (! url_xfer.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  url_xfer.http_action (param, method);

  octave_value_list retval;

  if (nargout > 0)
    {
      // Return empty string if no error occurred.
      retval = ovl (buf.str (), url_xfer.good (),
                    url_xfer.good () ? "" : url_xfer.lasterror ());
    }

  if (nargout < 2 && ! url_xfer.good ())
    error ("urlread: %s", url_xfer.lasterror ().c_str ());

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
          options.Timeout = static_cast<long>(timeout * 1000);
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
