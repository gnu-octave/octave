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

#if ! defined (octave_url_transfer_h)
#define octave_url_transfer_h 1

#include "octave-config.h"

#include <istream>
#include <memory>
#include <ostream>
#include <string>

#include "Array-fwd.h"
#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

struct weboptions
{
  std::string UserAgent;
  long Timeout;
  std::string Username;
  std::string Password;
  Array<std::string> HeaderFields;
  std::string ContentReader;
  std::string RequestMethod;
  std::string ArrayFormat;
  std::string CertificateFilename;
};

class
OCTAVE_API
base_url_transfer
{
public:

  friend class url_transfer;

  base_url_transfer (void);

  base_url_transfer (const std::string& host,
                     const std::string& /* user_arg */,
                     const std::string& /* passwd */,
                     std::ostream& os);

  base_url_transfer (const std::string& url, std::ostream& os);

  // No copying!

  base_url_transfer (const base_url_transfer&) = delete;

  base_url_transfer& operator = (const base_url_transfer&) = delete;

  virtual ~base_url_transfer (void) = default;

  bool is_valid (void) const { return m_valid; }

  bool good (void) const { return m_valid && m_ok; }

  virtual void perform (void) { }

  virtual std::string lasterror (void) const { return m_errmsg; }

  virtual std::ostream& set_ostream (std::ostream& /* os */)
  {
    return *m_curr_ostream;
  }

  virtual std::istream& set_istream (std::istream& /* is */)
  {
    return *m_curr_istream;
  }

  virtual void ascii (void) { }

  virtual void binary (void) { }

  bool is_ascii (void) const { return m_ascii_mode; }

  bool is_binary (void) const { return ! m_ascii_mode; }

  virtual void cwd (const std::string& /* path */) { }

  virtual void del (const std::string& /* file */) { }

  virtual void rmdir (const std::string& /* path */) { }

  virtual void mkdir (const std::string& /* path */) { }

  virtual void rename (const std::string& /* oldname */,
                       const std::string& /* newname */) { }

  virtual void put (const std::string& /* file */,
                    std::istream& /* is */) { }

  virtual void get (const std::string& /* file */,
                    std::ostream& /* os */) { }

  void mget_directory (const std::string& directory,
                       const std::string& target);

  string_vector mput_directory (const std::string& base,
                                const std::string& directory);

  virtual void dir (void) { }

  virtual string_vector list (void) { return string_vector (); }

  virtual void get_fileinfo (const std::string& /* filename */,
                             double& /* filesize */,
                             OCTAVE_TIME_T& /* filetime */,
                             bool& /* fileisdir */) { }

  virtual std::string pwd (void) { return ""; }

  virtual void http_get (const Array<std::string>& /* param */) { }

  virtual void http_post (const Array<std::string>& /* param */) { }

  virtual void http_action (const Array<std::string>& /* param */,
                            const std::string& /* action */) { }

  virtual void cookie_jar (const std::string& /* filename */) { }

  virtual void set_header_fields (const Array<std::string>& /* param */) { }

  virtual void form_data_post (const Array<std::string>& /* param */) { }

  virtual void set_weboptions (const struct weboptions& /* param */) { }

protected:

  // Host for ftp transfers or full URL for http requests.
  std::string m_host_or_url;
  bool m_valid;
  bool m_ftp;
  bool m_ascii_mode;
  bool m_ok;
  std::string m_errmsg;
  std::istream *m_curr_istream;
  std::ostream *m_curr_ostream;
};

class
OCTAVE_API
url_transfer
{
public:

  url_transfer (void);

  url_transfer (const std::string& host, const std::string& user,
                const std::string& passwd, std::ostream& os);

  url_transfer (const std::string& url, std::ostream& os);

  url_transfer (const url_transfer&) = default;

  url_transfer& operator = (const url_transfer&) = default;

  ~url_transfer (void) = default;

  bool is_valid (void) const { return m_rep->is_valid (); }

  bool good (void) const { return m_rep->good (); }

  std::string lasterror (void) const { return m_rep->lasterror (); }

  std::ostream& set_ostream (std::ostream& os)
  {
    return m_rep->set_ostream (os);
  }

  std::istream& set_istream (std::istream& is)
  {
    return m_rep->set_istream (is);
  }

  void ascii (void) { m_rep->ascii (); }

  void binary (void) { m_rep->binary (); }

  bool is_ascii (void) const { return m_rep->is_ascii (); }

  bool is_binary (void) const { return m_rep->is_binary (); }

  void cwd (const std::string& path) { m_rep->cwd (path); }

  void del (const std::string& file) { m_rep->del (file); }

  void rmdir (const std::string& path) { m_rep->rmdir (path); }

  void mkdir (const std::string& path) { m_rep->mkdir (path); }

  void rename (const std::string& oldname, const std::string& newname)
  {
    m_rep->rename (oldname, newname);
  }

  void put (const std::string& file, std::istream& is)
  {
    m_rep->put (file, is);
  }

  void get (const std::string& file, std::ostream& os)
  {
    m_rep->get (file, os);
  }

  void mget_directory (const std::string& directory,
                       const std::string& target)
  {
    m_rep->mget_directory (directory, target);
  }

  string_vector mput_directory (const std::string& base,
                                const std::string& directory)
  {
    return m_rep->mput_directory (base, directory);
  }

  void dir (void) { m_rep->dir (); }

  string_vector list (void) { return m_rep->list (); }

  void get_fileinfo (const std::string& filename, double& filesize,
                     OCTAVE_TIME_T& filetime, bool& fileisdir)
  {
    m_rep->get_fileinfo (filename, filesize, filetime, fileisdir);
  }

  std::string pwd (void) { return m_rep->pwd (); }

  void http_get (const Array<std::string>& param)
  {
    m_rep->http_get (param);
  }

  void http_post (const Array<std::string>& param)
  {
    m_rep->http_post (param);
  }

  void http_action (const Array<std::string>& param,
                    const std::string& action)
  {
    m_rep->http_action (param, action);
  }

  void cookie_jar (const std::string& filename)
  {
    m_rep->cookie_jar (filename);
  }

  void set_header_fields (const Array<std::string>& param)
  {
    m_rep->set_header_fields (param);
  }

  void form_data_post (const Array<std::string>& param)
  {
    m_rep->form_data_post (param);
  }

  void set_weboptions (const struct weboptions& param)
  {
    m_rep->set_weboptions (param);
  }

private:

  std::shared_ptr<base_url_transfer> m_rep;
};

OCTAVE_END_NAMESPACE(octave)

#endif
