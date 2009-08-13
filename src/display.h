/*

Copyright (C) 2009 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_display_h)
#define octave_display_h 1

class Matrix;

class display_info
{
protected:

  display_info (bool query = true)
    : ht (1), wd (1), dp (0), rx (72), ry (72)
  {
    init (query);
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

  // To disable querying the window system for defaults, this function
  // must be called before any other display_info function.
  static void no_window_system (void)
  {
    instance_ok (false);
  }

private:

  static display_info *instance;

  // Height, width, and depth of the display.
  int ht;
  int wd;
  int dp;

  // X- and Y- Resolution of the display in dots (pixels) per inch.
  double rx;
  double ry;

  int do_height (void) const { return ht; }
  int do_width (void) const { return wd; }
  int do_depth (void) const { return dp; }

  double do_x_dpi (void) const { return rx; }
  double do_y_dpi (void) const { return ry; }

  void init (bool query = true);

  static bool instance_ok (bool query = true);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
