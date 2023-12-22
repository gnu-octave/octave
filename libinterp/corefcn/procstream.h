////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2024 The Octave Project Developers
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

class OCTINTERP_API procstreambase : virtual public std::ios
{
public:

  procstreambase () : m_pb () { pb_init (); }

  procstreambase (const std::string& name, int mode);

  procstreambase (const char *name, int mode);

  OCTAVE_DISABLE_COPY_MOVE (procstreambase)

  ~procstreambase () { close (); }

  void open (const std::string& name, int mode)
  {
    open (name.c_str (), mode);
  }

  void open (const char *name, int mode);

  int is_open () const { return m_pb.is_open (); }

  int close ();

  pid_t pid () const { return m_pb.pid (); }

  int file_number () const { return m_pb.file_number (); }

private:

  procbuf m_pb;

  void pb_init ()
  {
    // Explicit initialization of the std::ios object is needed.
    // FIXME: is there a better way to organize these classes?
    init (&m_pb);
  }
};

class OCTINTERP_API iprocstream : public std::istream, public procstreambase
{
public:

  iprocstream () : std::istream (nullptr), procstreambase () { }

  iprocstream (const std::string& name, int mode = std::ios::in)
    : std::istream (nullptr), procstreambase (name, mode)
  { }

  iprocstream (const char *name, int mode = std::ios::in)
    : std::istream (nullptr), procstreambase (name, mode)
  { }

  OCTAVE_DISABLE_COPY_MOVE (iprocstream)

  ~iprocstream () = default;

  void open (const std::string& name, int mode = std::ios::in)
  {
    procstreambase::open (name, mode);
  }

  void open (const char *name, int mode = std::ios::in)
  {
    procstreambase::open (name, mode);
  }
};

class OCTINTERP_API oprocstream : public std::ostream, public procstreambase
{
public:

  oprocstream () : std::ostream (nullptr), procstreambase () { }

  oprocstream (const std::string& name, int mode = std::ios::out)
    : std::ostream (nullptr), procstreambase (name, mode) { }

  oprocstream (const char *name, int mode = std::ios::out)
    : std::ostream (nullptr), procstreambase (name, mode) { }

  OCTAVE_DISABLE_COPY_MOVE (oprocstream)

  ~oprocstream () = default;

  void open (const std::string& name, int mode = std::ios::out)
  {
    procstreambase::open (name, mode);
  }

  void open (const char *name, int mode = std::ios::out)
  {
    procstreambase::open (name, mode);
  }
};

class OCTINTERP_API procstream : public std::iostream, public procstreambase
{
public:

  procstream () : std::iostream (nullptr), procstreambase () { }

  procstream (const std::string& name, int mode)
    : std::iostream (nullptr), procstreambase (name, mode)
  { }

  procstream (const char *name, int mode)
    : std::iostream (nullptr), procstreambase (name, mode)
  { }

  OCTAVE_DISABLE_COPY_MOVE (procstream)

  ~procstream () = default;

  void open (const std::string& name, int mode)
  {
    procstreambase::open (name, mode);
  }

  void open (const char *name, int mode)
  {
    procstreambase::open (name, mode);
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
