/*

Copyright (C) 1993-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_pager_h)
#define octave_pager_h 1

#include "octave-config.h"

#include <iosfwd>
#include <sstream>
#include <string>

#include <sys/types.h>

namespace octave
{
  class
  OCTINTERP_API
  pager_buf : public std::stringbuf
  {
  public:

    pager_buf (void) : std::stringbuf (), diary_skip (0) { }

    void flush_current_contents_to_diary (void);

    void set_diary_skip (void);

  protected:

    int sync (void);

  private:

    size_t diary_skip;
  };

  class
  OCTINTERP_API
  pager_stream : public std::ostream
  {
  protected:

    pager_stream (void);

  public:

    // No copying!

    pager_stream (const pager_stream&) = delete;

    pager_stream& operator = (const pager_stream&) = delete;

    ~pager_stream (void);

    static void flush_current_contents_to_diary (void);

    static void set_diary_skip (void);

    static std::ostream& stream (void);

    static void reset (void);

  private:

    void do_flush_current_contents_to_diary (void);

    void do_set_diary_skip (void);

    void do_reset (void);

    static pager_stream *instance;

    static bool instance_ok (void);

    static void cleanup_instance (void) { delete instance; instance = 0; }

    pager_buf *pb;
  };

  class
  OCTINTERP_API
  diary_buf : public std::stringbuf
  {
  public:

    diary_buf (void) : std::stringbuf () { }

  protected:

    int sync (void);
  };

  class
  OCTINTERP_API
  diary_stream : public std::ostream
  {
  protected:

    diary_stream (void);

  public:

    // No copying!

    diary_stream (const diary_stream&) = delete;

    diary_stream& operator = (const diary_stream&) = delete;

    ~diary_stream (void);

    static std::ostream& stream (void);

    static void reset (void);

  private:

    void do_reset (void);

    static diary_stream *instance;

    static bool instance_ok (void);

    static void cleanup_instance (void) { delete instance; instance = 0; }

    diary_buf *db;
  };

  extern OCTAVE_API void flush_stdout (void);
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::diary_buf' instead")
typedef octave::diary_buf octave_diary_buf;

OCTAVE_DEPRECATED ("use 'octave::diary_stream' instead")
typedef octave::diary_stream octave_diary_stream;

OCTAVE_DEPRECATED ("use 'octave::pager_buf' instead")
typedef octave::pager_buf octave_pager_buf;

OCTAVE_DEPRECATED ("use 'octave::pager_stream' instead")
typedef octave::pager_stream octave_pager_stream;

OCTAVE_DEPRECATED ("use 'octave::flush_stdout' instead")
static inline void
flush_octave_stdout (void)
{
  return octave::flush_stdout ();
}

#endif

#define octave_stdout (octave::pager_stream::stream ())

#define octave_diary (octave::diary_stream::stream ())

#endif
