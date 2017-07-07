/*

Copyright (C) 2009-2017 John W. Eaton

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

#if ! defined (octave_display_h)
#define octave_display_h 1

#include "octave-config.h"

#include <string>

class Matrix;

namespace octave
{
  class
  OCTINTERP_API
  display_info
  {
  protected:

    display_info (void)
      : m_ht (1), m_wd (1), m_dp (0),
        m_rx (72), m_ry (72), m_dpy_avail (false), m_err_msg ()
    {
      init ();
    }

    explicit display_info (const std::string& dpy_name)
      : m_ht (1), m_wd (1), m_dp (0),
        m_rx (72), m_ry (72), m_dpy_avail (false), m_err_msg ()
    {
      init (dpy_name);
    }

    explicit display_info (bool query)
      : m_ht (1), m_wd (1), m_dp (0),
        m_rx (72), m_ry (72), m_dpy_avail (false), m_err_msg ()
    {
      init ("", query);
    }

    explicit display_info (const std::string& dpy_name, bool query)
      : m_ht (1), m_wd (1), m_dp (0),
        m_rx (72), m_ry (72), m_dpy_avail (false), m_err_msg ()
    {
      init (dpy_name, query);
    }

  public:

    static int height (void)
    {
      return instance_ok () ? instance->do_height () : 0;
    }

    static int width (void)
    {
      return instance_ok () ? instance->do_width () : 0;
    }

    static int depth (void)
    {
      return instance_ok () ? instance->do_depth () : 0;
    }

    static double x_dpi (void)
    {
      return instance_ok () ? instance->do_x_dpi () : 0;
    }

    static double y_dpi (void)
    {
      return instance_ok () ? instance->do_y_dpi () : 0;
    }

    static bool display_available (void)
    {
      std::string msg;
      return instance_ok () ? instance->do_display_available (msg) : false;
    }

    static bool display_available (std::string& msg)
    {
      return instance_ok () ? instance->do_display_available (msg) : false;
    }

    // To disable querying the window system for defaults, this function
    // must be called before any other display_info function.
    static void no_window_system (void)
    {
      instance_ok (false);
    }

  private:

    static display_info *instance;

    static void cleanup_instance (void) { delete instance; instance = 0; }

    // Height, width, and depth of the display.
    int m_ht;
    int m_wd;
    int m_dp;

    // X- and Y- Resolution of the display in dots (pixels) per inch.
    double m_rx;
    double m_ry;

    bool m_dpy_avail;

    std::string m_err_msg;

    int do_height (void) const { return m_ht; }
    int do_width (void) const { return m_wd; }
    int do_depth (void) const { return m_dp; }

    double do_x_dpi (void) const { return m_rx; }
    double do_y_dpi (void) const { return m_ry; }

    bool do_display_available (std::string& msg) const
    {
      msg = m_err_msg;

      return m_dpy_avail;
    }

    void init (const std::string& dpy_name = "", bool query = true);

    static bool instance_ok (bool query = true);
  };
}

#endif
