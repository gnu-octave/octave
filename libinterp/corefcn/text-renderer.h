/*

Copyright (C) 2016 John W. Eaton
Copyright (C) 2009-2016 Michael Goffioul

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

#if ! defined (octave_text_renderer_h)
#define octave_text_renderer_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "caseless-str.h"
#include "dMatrix.h"
#include "uint8NDArray.h"

#include "txt-eng.h"

namespace octave
{

class base_text_renderer;

class
OCTINTERP_API
text_renderer
{
public:

  text_renderer (void);

  ~text_renderer (void);

  bool ok (void) const;

  Matrix get_extent (text_element *elt, double rotation = 0.0);

  Matrix get_extent (const std::string& txt, double rotation = 0.0,
                     const caseless_str& interpreter = "tex");

  void set_font (const std::string& name, const std::string& weight,
                 const std::string& angle, double size);

  void set_color (const Matrix& c);

  void text_to_pixels (const std::string& txt,
                       uint8NDArray& pxls, Matrix& bbox,
                       int halign, int valign, double rotation = 0.0,
                       const caseless_str& interpreter = "tex",
                       bool handle_rotation = true);

  class font
  {
  public:

    font (void)
      : name (), weight (), angle (), size (0)
    { }

    font (const std::string& nm, const std::string& wt,
          const std::string& ang, double sz)
      : name (nm), weight (wt), angle (ang), size (sz)
    { }

    font (const font& ft)
      : name (ft.name), weight (ft.weight), angle (ft.angle),
        size (ft.size)
    { }

    ~font (void) { }

    font& operator = (const font& ft)
    {
      if (&ft != this)
        {
          name = ft.name;
          weight = ft.weight;
          angle = ft.angle;
          size = ft.size;
        }

      return *this;
    }

    std::string get_name (void) const { return name; }

    std::string get_weight (void) const { return weight; }

    std::string get_angle (void) const { return angle; }

    double get_size (void) const { return size; }

  protected:

    std::string name;
    std::string weight;
    std::string angle;
    double size;
  };

  // Container for substrings after parsing.

  class string
  {
  public:

    string (const std::string& s, font& f, const double x0, const double y0)
      : str (s), fnt (f), x (x0), y (y0), z (0.0), code (0),
        color (Matrix (1,3,0.0))
    { }

    string (const string& s)
      : str (s.str), fnt (s.fnt), x (s.x), y (s.y), code (s.code),
        color (s.color)
    { }

    ~string (void) { }

    string& operator = (const string& s)
    {
      if (&s != this)
        {
          str = s.str;
          fnt = s.fnt;
          x = s.x;
          y = s.y;
          code = s.code;
          color = s.color;
        }

      return *this;
    }

    void set_string (const std::string& s) { str = s; }

    std::string get_string (void) const { return str; }

    std::string get_name (void) const { return fnt.get_name (); }

    std::string get_weight (void) const { return fnt.get_weight (); }

    std::string get_angle (void) const { return fnt.get_angle (); }

    double get_size (void) const { return fnt.get_size (); }

    void set_x (const double x0) { x = x0; }

    double get_x (void) const { return x; }

    void set_y (const double y0) { y = y0; }

    double get_y (void) const { return y; }

    void set_z (const double z0) { z = z0; }

    double get_z (void) const { return z; }

    void set_code (const uint32_t c) { code = c; }

    uint32_t get_code (void) const { return code; }

    void set_color (const uint8NDArray& c)
    {
      color(0) = static_cast<double> (c(0)) / 255;
      color(1) = static_cast<double> (c(1)) / 255;
      color(2) = static_cast<double> (c(2)) / 255;
    }

    Matrix get_color (void) const { return color; }

  private:

    std::string str;
    font fnt;
    double x, y, z;
    uint32_t code;
    Matrix color;
  };

  void text_to_strlist (const std::string& txt,
                        std::list<string>& lst, Matrix& box,
                        int halign, int valign, double rotation = 0.0,
                        const caseless_str& interpreter = "tex");

private:

  base_text_renderer *rep;

  // No copying!

  text_renderer (const text_renderer&);

  text_renderer& operator = (const text_renderer&);
};

}

#endif
