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

#include <fstream>
#include <iomanip>
#include <iostream>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-sysdep.h"
#include "oct-env.h"
#include "unwind-prot.h"
#include "url-transfer.h"
#include "version.h"

#if defined (HAVE_CURL)
#  include <curl/curl.h>
#  include <curl/curlver.h>
#  include <curl/easy.h>
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

base_url_transfer::base_url_transfer (void)
  : m_host_or_url (), m_valid (false), m_ftp (false),
    m_ascii_mode (false), m_ok (true), m_errmsg (),
    m_curr_istream (&std::cin), m_curr_ostream (&std::cout)
{ }

base_url_transfer::base_url_transfer (const std::string& host,
                                      const std::string& /* user_arg */,
                                      const std::string& /* passwd */,
                                      std::ostream& os)
  : m_host_or_url (host), m_valid (false), m_ftp (true),
    m_ascii_mode (false), m_ok (true), m_errmsg (),
    m_curr_istream (&std::cin), m_curr_ostream (&os)
{ }

base_url_transfer::base_url_transfer (const std::string& url,
                                      std::ostream& os)
  : m_host_or_url (url), m_valid (false), m_ftp (false),
    m_ascii_mode (false), m_ok (true), m_errmsg (),
    m_curr_istream (&std::cin), m_curr_ostream (&os)
{ }

void
base_url_transfer::mget_directory (const std::string& directory,
                                   const std::string& target)
{
  std::string sep = sys::file_ops::dir_sep_str ();
  sys::file_stat fs (directory);

  if (! fs || ! fs.is_dir ())
    {
      std::string msg;
      int status = sys::mkdir (directory, 0777, msg);

      if (status < 0)
        {
          m_ok = false;
          m_errmsg = "__ftp_mget__: can not create directory '"
                     + target + sep + directory + "': " + msg;
          return;
        }
    }

  cwd (directory);

  if (good ())
    {
      unwind_action_safe reset_path (&base_url_transfer::cwd, this, "..");

      string_vector sv = list ();

      for (octave_idx_type i = 0; i < sv.numel (); i++)
        {
          OCTAVE_TIME_T ftime;
          bool fisdir;
          double fsize;

          get_fileinfo (sv(i), fsize, ftime, fisdir);

          if (fisdir)
            mget_directory (sv(i), target + directory + sep);
          else
            {
              std::string realfile = target + directory + sep + sv(i);

              std::ofstream ofile =
                sys::ofstream (realfile.c_str (),
                               std::ios::out | std::ios::binary);

              if (! ofile.is_open ())
                {
                  m_ok = false;
                  m_errmsg = "__ftp_mget__: unable to open file";
                  break;
                }
              int(*unlink_fptr)(const std::string&) = sys::unlink;
              unwind_action_safe delete_file (unlink_fptr, realfile);

              get (sv(i), ofile);

              ofile.close ();

              if (good ())
                delete_file.discard ();
            }

          if (! good ())
            break;
        }
    }
}

string_vector
base_url_transfer::mput_directory (const std::string& base,
                                   const std::string& directory)
{
  string_vector file_list;

  std::string realdir
    = (base.empty ()
       ? directory : base + sys::file_ops::dir_sep_str () + directory);

  mkdir (directory);

  if (! good ())
    return file_list;

  cwd (directory);

  if (good ())
    {
      unwind_action_safe reset_path (&base_url_transfer::cwd, this, "..");

      string_vector files;
      std::string msg;

      if (sys::get_dirlist (realdir, files, msg))
        for (octave_idx_type i = 0; i < files.numel (); i++)
          {
            std::string file = files (i);

            if (file == "." || file == "..")
              continue;

            std::string realfile
              = realdir + sys::file_ops::dir_sep_str () + file;

            sys::file_stat fs (realfile);

            if (! fs.exists ())
              {
                m_ok = false;
                m_errmsg = "__ftp__mput: file '" + realfile
                           + "' does not exist";
                break;
              }

            if (fs.is_dir ())
              {
                file_list.append (mput_directory (realdir, file));

                if (! good ())
                  break;
              }
            else
              {
                // FIXME: Does ascii mode need to be flagged here?
                std::ifstream ifile =
                  sys::ifstream (realfile.c_str (),
                                 std::ios::in | std::ios::binary);

                if (! ifile.is_open ())
                  {
                    m_ok = false;
                    m_errmsg = "__ftp_mput__: unable to open file '"
                               + realfile + "'";
                    break;
                  }

                put (file, ifile);

                ifile.close ();

                if (! good ())
                  break;

                file_list.append (realfile);
              }
          }
      else
        {
          m_ok = false;
          m_errmsg = "__ftp_mput__: can not read the directory '"
                     + realdir + "'";
        }
    }

  return file_list;
}

#if defined (HAVE_CURL)

static int
write_data (void *buffer, std::size_t size, std::size_t nmemb, void *streamp)
{
  std::ostream& stream = *(static_cast<std::ostream *> (streamp));
  stream.write (static_cast<const char *> (buffer), size*nmemb);
  return (stream.fail () ? 0 : size * nmemb);
}

static int
read_data (void *buffer, std::size_t size, std::size_t nmemb, void *streamp)
{
  std::istream& stream = *(static_cast<std::istream *> (streamp));
  stream.read (static_cast<char *> (buffer), size*nmemb);
  if (stream.eof ())
    return stream.gcount ();
  else
    return (stream.fail () ? 0 : size * nmemb);
}

static std::size_t
throw_away (void *, std::size_t size, std::size_t nmemb, void *)
{
  return static_cast<std::size_t> (size * nmemb);
}

// I'd love to rewrite this as a private method of the url_transfer
// class, but you can't pass the va_list from the wrapper SETOPT to
// the curl_easy_setopt function.
#define SETOPT(option, parameter)                                       \
  do                                                                    \
    {                                                                   \
      CURLcode res = curl_easy_setopt (m_curl, option, parameter);      \
      if (res != CURLE_OK)                                              \
        {                                                               \
          m_ok = false;                                                 \
          m_errmsg = curl_easy_strerror (res);                          \
          return;                                                       \
        }                                                               \
    }                                                                   \
  while (0)

// Same as above but with a return value.
#define SETOPTR(option, parameter)                                      \
  do                                                                    \
    {                                                                   \
      CURLcode res = curl_easy_setopt (m_curl, option, parameter);      \
      if (res != CURLE_OK)                                              \
        {                                                               \
          m_ok = false;                                                 \
          m_errmsg = curl_easy_strerror (res);                          \
          return retval;                                                \
        }                                                               \
    }                                                                   \
  while (0)

class curl_transfer : public base_url_transfer
{
public:

  curl_transfer (void)
    : base_url_transfer (), m_curl (curl_easy_init ()), m_errnum (), m_url (),
      m_userpwd ()
  {
    if (m_curl)
      m_valid = true;
    else
      m_errmsg = "can not create curl object";
  }

  curl_transfer (const std::string& host, const std::string& user_arg,
                 const std::string& passwd, std::ostream& os)
    : base_url_transfer (host, user_arg, passwd, os),
      m_curl (curl_easy_init ()), m_errnum (), m_url (), m_userpwd ()
  {
    if (m_curl)
      m_valid = true;
    else
      {
        m_errmsg = "can not create curl object";
        return;
      }

    init (user_arg, passwd, std::cin, os);

    m_url = "ftp://" + host;
    SETOPT (CURLOPT_URL, m_url.c_str ());

    // Set up the link, with no transfer.
    perform ();
  }

  curl_transfer (const std::string& url_str, std::ostream& os)
    : base_url_transfer (url_str, os), m_curl (curl_easy_init ()),
      m_errnum (), m_url (), m_userpwd ()
  {
    if (m_curl)
      m_valid = true;
    else
      {
        m_errmsg = "can not create curl object";
        return;
      }

    init ("", "", std::cin, os);

    std::string cainfo = sys::env::getenv ("CURLOPT_CAINFO");
    if (! cainfo.empty ())
      SETOPT (CURLOPT_CAINFO, cainfo.c_str ());

    std::string capath = sys::env::getenv ("CURLOPT_CAPATH");
    if (! capath.empty ())
      SETOPT (CURLOPT_CAPATH, capath.c_str ());

    SETOPT (CURLOPT_NOBODY, 0);

    // Restore the default HTTP request method to GET after setting
    // NOBODY to true (in the init method) and back to false (above).
    // This is needed for backward compatibility with versions of
    // libcurl < 7.18.2.
    SETOPT (CURLOPT_HTTPGET, 1);
  }

  // No copying!

  curl_transfer (const curl_transfer&) = delete;

  curl_transfer& operator = (const curl_transfer&) = delete;

  ~curl_transfer (void)
  {
    if (m_curl)
      curl_easy_cleanup (m_curl);
  }

  void perform (void)
  {
    m_errnum = curl_easy_perform (m_curl);

    if (m_errnum != CURLE_OK)
      {
        m_ok = false;
        m_errmsg = curl_easy_strerror (m_errnum);
      }
  }

  std::string lasterror (void) const
  {
    return std::string (curl_easy_strerror (m_errnum));
  }

  std::ostream& set_ostream (std::ostream& os)
  {
    std::ostream& retval = *m_curr_ostream;
    m_curr_ostream = &os;
    SETOPTR (CURLOPT_WRITEDATA, static_cast<void *> (m_curr_ostream));
    return retval;
  }

  std::istream& set_istream (std::istream& is)
  {
    std::istream& retval = *m_curr_istream;
    m_curr_istream = &is;
    SETOPTR (CURLOPT_READDATA, static_cast<void *> (m_curr_istream));
    return retval;
  }

  void ascii (void)
  {
    m_ascii_mode = true;
    SETOPT (CURLOPT_TRANSFERTEXT, 1);
  }

  void binary (void)
  {
    m_ascii_mode = false;
    SETOPT (CURLOPT_TRANSFERTEXT, 0);
  }

  void cwd (const std::string& path)
  {
    ftp_file_or_dir_action (path, "cwd");
  }

  void del (const std::string& file)
  {
    ftp_file_or_dir_action (file, "dele");
  }

  void rmdir (const std::string& path)
  {
    ftp_file_or_dir_action (path, "rmd");
  }

  void mkdir (const std::string& path)
  {
    ftp_file_or_dir_action (path, "mkd");
  }

  void rename (const std::string& oldname, const std::string& newname)
  {
    struct curl_slist *slist = nullptr;

    unwind_action cleanup_slist ([=] () { curl_slist_free_all (slist); });

    std::string cmd = "rnfr " + oldname;
    slist = curl_slist_append (slist, cmd.c_str ());
    cmd = "rnto " + newname;
    slist = curl_slist_append (slist, cmd.c_str ());
    SETOPT (CURLOPT_POSTQUOTE, slist);

    perform ();
    if (! good ())
      return;

    SETOPT (CURLOPT_POSTQUOTE, 0);
  }

  void put (const std::string& file, std::istream& is)
  {
    m_url = "ftp://" + m_host_or_url + '/' + file;
    SETOPT (CURLOPT_URL, m_url.c_str ());
    SETOPT (CURLOPT_UPLOAD, 1);
    SETOPT (CURLOPT_NOBODY, 0);
    std::istream& old_is = set_istream (is);

    perform ();
    if (! good ())
      return;

    set_istream (old_is);
    SETOPT (CURLOPT_NOBODY, 1);
    SETOPT (CURLOPT_UPLOAD, 0);
    m_url = "ftp://" + m_host_or_url;
    SETOPT (CURLOPT_URL, m_url.c_str ());
  }

  void get (const std::string& file, std::ostream& os)
  {
    m_url = "ftp://" + m_host_or_url + '/' + file;
    SETOPT (CURLOPT_URL, m_url.c_str ());
    SETOPT (CURLOPT_NOBODY, 0);
    std::ostream& old_os = set_ostream (os);

    perform ();
    if (! good ())
      return;

    set_ostream (old_os);
    SETOPT (CURLOPT_NOBODY, 1);
    m_url = "ftp://" + m_host_or_url;
    SETOPT (CURLOPT_URL, m_url.c_str ());
  }

  void dir (void)
  {
    m_url = "ftp://" + m_host_or_url + '/';
    SETOPT (CURLOPT_URL, m_url.c_str ());
    SETOPT (CURLOPT_NOBODY, 0);

    perform ();
    if (! good ())
      return;

    SETOPT (CURLOPT_NOBODY, 1);
    m_url = "ftp://" + m_host_or_url;
    SETOPT (CURLOPT_URL, m_url.c_str ());
  }

  string_vector list (void)
  {
    string_vector retval;

    std::ostringstream buf;
    m_url = "ftp://" + m_host_or_url + '/';
    SETOPTR (CURLOPT_WRITEDATA, static_cast<void *> (&buf));
    SETOPTR (CURLOPT_URL, m_url.c_str ());
    SETOPTR (CURLOPT_DIRLISTONLY, 1);
    SETOPTR (CURLOPT_NOBODY, 0);

    perform ();
    if (! good ())
      return retval;

    SETOPTR (CURLOPT_NOBODY, 1);
    m_url = "ftp://" + m_host_or_url;
    SETOPTR (CURLOPT_WRITEDATA, static_cast<void *> (m_curr_ostream));
    SETOPTR (CURLOPT_DIRLISTONLY, 0);
    SETOPTR (CURLOPT_URL, m_url.c_str ());

    // Count number of directory entries
    std::string str = buf.str ();
    octave_idx_type n = 0;
    std::size_t pos = 0;
    while (true)
      {
        pos = str.find_first_of ('\n', pos);
        if (pos == std::string::npos)
          break;
        pos++;
        n++;
      }
    retval.resize (n);
    pos = 0;
    for (octave_idx_type i = 0; i < n; i++)
      {
        std::size_t newpos = str.find_first_of ('\n', pos);
        if (newpos == std::string::npos)
          break;

        std::string name = str.substr (pos, newpos - pos);
        // remove trailing \r if there is one
        if (name.length () > 0 && name.back () == '\r')
          name.pop_back ();

        retval(i) = name;
        pos = newpos + 1;
      }

    return retval;
  }

  void get_fileinfo (const std::string& filename, double& filesize,
                     OCTAVE_TIME_T& filetime, bool& fileisdir)
  {
    std::string path = pwd ();

    m_url = "ftp://" + m_host_or_url + '/' + path + '/' + filename;
    SETOPT (CURLOPT_URL, m_url.c_str ());
    SETOPT (CURLOPT_FILETIME, 1);
    SETOPT (CURLOPT_HEADERFUNCTION, throw_away);
    SETOPT (CURLOPT_WRITEFUNCTION, throw_away);

    // FIXME
    // The MDTM command fails for a directory on the servers I tested
    // so this is a means of testing for directories.  It also means
    // I can't get the date of directories!

    perform ();
    if (! good ())
      {
        fileisdir = true;
        filetime = -1;
        filesize = 0;

        return;
      }

    fileisdir = false;
    OCTAVE_TIME_T ft;
    curl_easy_getinfo (m_curl, CURLINFO_FILETIME, &ft);
    filetime = ft;
    double fs;
    curl_easy_getinfo (m_curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &fs);
    filesize = fs;

    SETOPT (CURLOPT_WRITEFUNCTION, write_data);
    SETOPT (CURLOPT_HEADERFUNCTION, 0);
    SETOPT (CURLOPT_FILETIME, 0);
    m_url = "ftp://" + m_host_or_url;
    SETOPT (CURLOPT_URL, m_url.c_str ());

    // The MDTM command seems to reset the path to the root with the
    // servers I tested with, so cd again into the correct path.  Make
    // the path absolute so that this will work even with servers that
    // don't end up in the root after an MDTM command.
    cwd ('/' + path);
  }

  std::string pwd (void)
  {
    std::string retval;

    struct curl_slist *slist = nullptr;

    unwind_action cleanup_slist ([=] () { curl_slist_free_all (slist); });

    slist = curl_slist_append (slist, "pwd");
    SETOPTR (CURLOPT_POSTQUOTE, slist);
    SETOPTR (CURLOPT_HEADERFUNCTION, write_data);

    std::ostringstream buf;
    SETOPTR (CURLOPT_WRITEHEADER, static_cast<void *>(&buf));

    perform ();
    if (! good ())
      return retval;

    retval = buf.str ();

    // Can I assume that the path is always in "" on the last line
    std::size_t pos2 = retval.rfind ('"');
    std::size_t pos1 = retval.rfind ('"', pos2 - 1);
    retval = retval.substr (pos1 + 1, pos2 - pos1 - 1);

    SETOPTR (CURLOPT_HEADERFUNCTION, 0);
    SETOPTR (CURLOPT_WRITEHEADER, 0);
    SETOPTR (CURLOPT_POSTQUOTE, 0);

    return retval;
  }

  void http_get (const Array<std::string>& param)
  {
    http_action (param, "get");
  }

  void http_post (const Array<std::string>& param)
  {
    http_action (param, "post");
  }

  void http_action (const Array<std::string>& param, const std::string& action)
  {
    m_url = m_host_or_url;

    std::string query_string;

    query_string = form_query_string (param);

    if (action.empty () || action == "get")
      {
        if (! query_string.empty ())
          m_url += '?' + query_string;

        SETOPT (CURLOPT_URL, m_url.c_str ());
      }
    else if (action == "post" || action == "put" || action == "delete")
      {
        SETOPT (CURLOPT_POSTFIELDS, query_string.c_str ());

        if (action == "put")
          {
            SETOPT (CURLOPT_CUSTOMREQUEST, "PUT");
          }

        if (action == "delete")
          {
            SETOPT (CURLOPT_CUSTOMREQUEST, "DELETE");
          }

        SETOPT (CURLOPT_URL, m_url.c_str ());
      }
    else
      {
        m_ok = false;
        m_errmsg = "curl_transfer: unknown http action";
      }

    if (m_ok)
      perform ();
  }

  void cookie_jar (const std::string& filename)
  {
    SETOPT (CURLOPT_COOKIEJAR, filename.c_str ());

    SETOPT (CURLOPT_COOKIEFILE, filename.c_str ());
  }

  // Sets the header fields in a transfer.  Input should be in the form
  // of an array of strings with pairs of keys and values together
  void set_header_fields (const Array<std::string>& param)
  {
    struct curl_slist *slist = nullptr;

    unwind_action cleanup_slist ([=] () { curl_slist_free_all (slist); });

    if (param.numel () >= 2)
      {
        for (int i = 0; i < param.numel (); i += 2)
          {
            std::string header = param(i) + ": " + param(i+1);

            slist = curl_slist_append (slist, header.c_str ());
          }

        SETOPT (CURLOPT_HTTPHEADER, slist);
      }
  }

  // Sets and sends the form data associated with a transfer.
  // Input should be an array of strings with each pair of strings
  // corresponding to the fieldname and it's value.
  // To attach a file, you should use 'file' as the fieldname with the
  // path of the file as its value.
  void form_data_post (const Array<std::string>& param)
  {
    struct curl_httppost *post = nullptr;
    struct curl_httppost *last = nullptr;

    SETOPT (CURLOPT_URL, m_host_or_url.c_str ());

    unwind_action cleanup_httppost ([=] () { curl_formfree (post); });

    if (param.numel () >= 2)
      {
        for (int i = 0; i < param.numel (); i += 2)
          {
            std::string name = param(i);
            std::string data = param(i+1);

            if (name == "file")
              curl_formadd (&post, &last, CURLFORM_COPYNAME, name.c_str (),
                            CURLFORM_FILE, data.c_str (), CURLFORM_END);
            else
              curl_formadd(&post, &last, CURLFORM_COPYNAME, name.c_str (),
                           CURLFORM_COPYCONTENTS, data.c_str (), CURLFORM_END);
          }

        SETOPT (CURLOPT_HTTPPOST, post);
      }

    perform ();
  }

  // Sets the various options specified by weboptions object.
  void set_weboptions (const struct weboptions& options)
  {
    // Remove this after completing fixmes.
    std::string temp = "";

    set_header_fields (options.HeaderFields);

    SETOPT (CURLOPT_TIMEOUT, options.Timeout);

    if (! options.UserAgent.empty ())
      SETOPT (CURLOPT_USERAGENT, options.UserAgent.c_str ());

    if (! options.Username.empty ())
      {
        if (! options.Password.empty ())
          {
            std::string tmp = options.Username + ":" + options.Password;
            SETOPT (CURLOPT_USERPWD, tmp.c_str ());
          }
        else
          {
            std::string tmp = options.Username + ":";
            SETOPT (CURLOPT_USERPWD, tmp.c_str ());
          }
      }

    // Unimplemented.  Only for MATLAB compatibility.
    if (! options.ContentReader.empty ())
      temp = options.ContentReader;

    // Unimplemented.  Only for MATLAB compatibility.
    if (! options.ArrayFormat.empty ())
      temp = options.ArrayFormat;

    // Unimplemented.  Only for MATLAB compatibility.
    if (! options.CertificateFilename.empty ())
      temp = options.CertificateFilename;
  }

private:

  // Pointer to cURL object.
  CURL *m_curl;

  // cURL error code.
  CURLcode m_errnum;

  // The cURL library changed the curl_easy_setopt call to make an
  // internal copy of string parameters in version 7.17.0.  Prior
  // versions only held a pointer to a string provided by the caller
  // that must persist for the lifetime of the CURL handle.
  //
  // The associated API did not change, only the behavior of the library
  // implementing the function call.
  //
  // To be compatible with any version of cURL, the caller must keep a
  // copy of all string parameters associated with a CURL handle until
  // the handle is released.  The curl_handle::curl_handle_rep class
  // contains the pointer to the CURL handle and so is the best
  // candidate for storing the strings as well. (bug #36717)
  std::string m_url;
  std::string m_userpwd;

  void init (const std::string& user, const std::string& passwd,
             std::istream& is, std::ostream& os)
  {
    // No data transfer by default
    SETOPT (CURLOPT_NOBODY, 1);

    // Set the username and password
    m_userpwd = user;
    if (! passwd.empty ())
      m_userpwd += ':' + passwd;
    if (! m_userpwd.empty ())
      SETOPT (CURLOPT_USERPWD, m_userpwd.c_str ());

    // Define our callback to get called when there's data to be written.
    SETOPT (CURLOPT_WRITEFUNCTION, write_data);

    // Set a pointer to our struct to pass to the callback.
    SETOPT (CURLOPT_WRITEDATA, static_cast<void *> (&os));

    // Define our callback to get called when there's data to be read
    SETOPT (CURLOPT_READFUNCTION, read_data);

    // Set a pointer to our struct to pass to the callback.
    SETOPT (CURLOPT_READDATA, static_cast<void *> (&is));

    // Follow redirects.
    SETOPT (CURLOPT_FOLLOWLOCATION, true);

    // Don't use EPSV since connecting to sites that don't support it
    // will hang for some time (3 minutes?) before moving on to try PASV
    // instead.
    SETOPT (CURLOPT_FTP_USE_EPSV, false);

    // Set the user agent for the curl request
    // Needed by mediaWiki API.
    curl_version_info_data *data = curl_version_info(CURLVERSION_NOW);
    const char *lib_ver = data->version;
    std::string user_agent
    ("GNU Octave/"
     + std::string (OCTAVE_VERSION)
     + " (https://www.gnu.org/software/octave/ ; help@octave.org) libcurl/"
     + std::string (lib_ver));

    SETOPT (CURLOPT_USERAGENT, user_agent.c_str ());

    SETOPT (CURLOPT_NOPROGRESS, true);
    SETOPT (CURLOPT_FAILONERROR, true);

    SETOPT (CURLOPT_POSTQUOTE, 0);
    SETOPT (CURLOPT_QUOTE, 0);
  }

  std::string form_query_string (const Array<std::string>& param)
  {
    std::ostringstream query;

    if (param.numel () >= 2)
      for (int i = 0; i < param.numel (); i += 2)
        {
          std::string name = param(i);
          std::string text = param(i+1);

          // Encode strings.
          char *enc_name = curl_easy_escape (m_curl, name.c_str (),
                                             name.length ());
          char *enc_text = curl_easy_escape (m_curl, text.c_str (),
                                             text.length ());

          query << enc_name << '=' << enc_text;

          curl_free (enc_name);
          curl_free (enc_text);

          if (i < param.numel ()-2)
            query << '&';
        }

    query.flush ();

    return query.str ();
  }

  void ftp_file_or_dir_action (const std::string& file_or_dir,
                               const std::string& action)
  {
    struct curl_slist *slist = nullptr;

    unwind_action cleanup_slist ([=] () { curl_slist_free_all (slist); });

    std::string cmd = action + ' ' + file_or_dir;

    slist = curl_slist_append (slist, cmd.c_str ());

    SETOPT (CURLOPT_POSTQUOTE, slist);

    perform ();

    if (! good ())
      return;

    SETOPT (CURLOPT_POSTQUOTE, 0);
  }
};

#undef SETOPT

#endif

#if defined (HAVE_CURL)
#  define REP_CLASS curl_transfer
#else
#  define REP_CLASS base_url_transfer
#endif

url_transfer::url_transfer (void) : m_rep (new REP_CLASS ())
{ }

url_transfer::url_transfer (const std::string& host, const std::string& user,
                            const std::string& passwd, std::ostream& os)
  : m_rep (new REP_CLASS (host, user, passwd, os))
{ }

url_transfer::url_transfer (const std::string& url, std::ostream& os)
  : m_rep (new REP_CLASS (url, os))
{ }

#undef REP_CLASS

OCTAVE_END_NAMESPACE(octave)
