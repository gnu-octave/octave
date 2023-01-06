////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_procstream_h)
#define octave_procstream_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

#include <sys/types.h>

#include "oct-procbuf.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
OCTINTERP_API
procstreambase : virtual public std::ios
{
public:

  procstreambase (void) : m_pb () { pb_init (); }

  procstreambase (const std::string& name, int mode);

  procstreambase (const char *name, int mode);

  ~procstreambase (void) { close (); }

  void open (const std::string& name, int mode)
  {
    open (name.c_str (), mode);
  }

  void open (const char *name, int mode);

  int is_open (void) const { return m_pb.is_open (); }

  int close (void);

  pid_t pid (void) const { return m_pb.pid (); }

  int file_number (void) const { return m_pb.file_number (); }

private:

  procbuf m_pb;

  void pb_init (void)
  {
    // Explicit initialization of the std::ios object is needed.
    // FIXME: is there a better way to organize these classes?
    init (&m_pb);
  }

  procstreambase (const procstreambase&);

  procstreambase& operator = (const procstreambase&);
};

class
OCTINTERP_API
iprocstream : public std::istream, public procstreambase
{
public:

  iprocstream (void) : std::istream (nullptr), procstreambase () { }

  iprocstream (const std::string& name, int mode = std::ios::in)
    : std::istream (nullptr), procstreambase (name, mode)
  { }

  iprocstream (const char *name, int mode = std::ios::in)
    : std::istream (nullptr), procstreambase (name, mode)
  { }

  ~iprocstream (void) = default;

  void open (const std::string& name, int mode = std::ios::in)
  {
    procstreambase::open (name, mode);
  }

  void open (const char *name, int mode = std::ios::in)
  {
    procstreambase::open (name, mode);
  }

private:

  iprocstream (const iprocstream&);

  iprocstream& operator = (const iprocstream&);
};

class
OCTINTERP_API
oprocstream : public std::ostream, public procstreambase
{
public:

  oprocstream (void) : std::ostream (nullptr), procstreambase () { }

  oprocstream (const std::string& name, int mode = std::ios::out)
    : std::ostream (nullptr), procstreambase (name, mode) { }

  oprocstream (const char *name, int mode = std::ios::out)
    : std::ostream (nullptr), procstreambase (name, mode) { }

  ~oprocstream (void) = default;

  void open (const std::string& name, int mode = std::ios::out)
  {
    procstreambase::open (name, mode);
  }

  void open (const char *name, int mode = std::ios::out)
  {
    procstreambase::open (name, mode);
  }

private:

  oprocstream (const oprocstream&);

  oprocstream& operator = (const oprocstream&);
};

class
OCTINTERP_API
procstream : public std::iostream, public procstreambase
{
public:

  procstream (void) : std::iostream (nullptr), procstreambase () { }

  procstream (const std::string& name, int mode)
    : std::iostream (nullptr), procstreambase (name, mode)
  { }

  procstream (const char *name, int mode)
    : std::iostream (nullptr), procstreambase (name, mode)
  { }

  ~procstream (void) = default;

  void open (const std::string& name, int mode)
  {
    procstreambase::open (name, mode);
  }

  void open (const char *name, int mode)
  {
    procstreambase::open (name, mode);
  }

private:

  procstream (const procstream&);

  procstream& operator = (const procstream&);
};

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::procstreambase' instead")
typedef octave::procstreambase procstreambase;

OCTAVE_DEPRECATED (7, "use 'octave::iprocstream' instead")
typedef octave::iprocstream iprocstream;

OCTAVE_DEPRECATED (7, "use 'octave::oprocstream' instead")
typedef octave::oprocstream oprocstream;

OCTAVE_DEPRECATED (7, "use 'octave::procstream' instead")
typedef octave::procstream procstream;

#endif

#endif
