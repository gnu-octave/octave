////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#if ! defined (octave_text_renderer_h)
#define octave_text_renderer_h 1

#include "octave-config.h"

#include <list>
#include <string>
#include <vector>

#include "caseless-str.h"
#include "dMatrix.h"
#include "uint8NDArray.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_text_renderer;
class text_element;

class
OCTINTERP_API
text_renderer
{
public:

  text_renderer (void);

  // No copying!

  text_renderer (const text_renderer&) = delete;

  text_renderer& operator = (const text_renderer&) = delete;

  ~text_renderer (void);

  bool ok (void) const;

  Matrix get_extent (text_element *elt, double rotation = 0.0);

  Matrix get_extent (const std::string& txt, double rotation = 0.0,
                     const caseless_str& interpreter = "tex");

  void set_anti_aliasing (bool val);

  void set_font (const std::string& name, const std::string& weight,
                 const std::string& angle, double size);

  octave_map get_system_fonts (void);

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
      : m_name (), m_weight (), m_angle (), m_size (0)
    { }

    font (const std::string& nm, const std::string& wt,
          const std::string& ang, double sz)
      : m_name (nm), m_weight (wt), m_angle (ang), m_size (sz)
    { }

    font (const font& ft)
      : m_name (ft.m_name), m_weight (ft.m_weight), m_angle (ft.m_angle),
        m_size (ft.m_size)
    { }

    ~font (void) = default;

    font& operator = (const font& ft)
    {
      if (&ft != this)
        {
          m_name = ft.m_name;
          m_weight = ft.m_weight;
          m_angle = ft.m_angle;
          m_size = ft.m_size;
        }

      return *this;
    }

    std::string get_name (void) const { return m_name; }

    std::string get_weight (void) const { return m_weight; }

    std::string get_angle (void) const { return m_angle; }

    double get_size (void) const { return m_size; }

  protected:

    std::string m_name;
    std::string m_weight;
    std::string m_angle;
    double m_size;
  };

  // Container for substrings after parsing.

  class string
  {
  public:

    string (const std::string& s, font& f, const double x, const double y)
      : m_str (s), m_family (f.get_name ()), m_fnt (f), m_x (x), m_y (y),
        m_z (0.0), m_xdata (), m_code (0), m_color (Matrix (1, 3, 0.0)),
        m_svg_element ()
    { }

    string (const string& s)
      : m_str (s.m_str), m_family (s.m_family), m_fnt (s.m_fnt), m_x (s.m_x),
        m_y (s.m_y), m_z (s.m_z), m_xdata (s.m_xdata), m_code (s.m_code),
        m_color (s.m_color), m_svg_element (s.m_svg_element)
    { }

    ~string (void) = default;

    string& operator = (const string& s)
    {
      if (&s != this)
        {
          m_str = s.m_str;
          m_family = s.m_family;
          m_fnt = s.m_fnt;
          m_x = s.m_x;
          m_y = s.m_y;
          m_z = s.m_z;
          m_xdata = s.m_xdata;
          m_code = s.m_code;
          m_color = s.m_color;
        }

      return *this;
    }

    void set_string (const std::string& s) { m_str = s; }

    std::string get_string (void) const { return m_str; }

    std::string get_name (void) const { return m_fnt.get_name (); }

    std::string get_family (void) const { return m_family; }

    void set_family (const std::string& nm) { m_family = nm; }

    std::string get_weight (void) const { return m_fnt.get_weight (); }

    std::string get_angle (void) const { return m_fnt.get_angle (); }

    double get_size (void) const { return m_fnt.get_size (); }

    void set_x (const double x) { m_x = x; }

    double get_x (void) const { return m_x; }

    void set_xdata (const std::vector<double>& x) { m_xdata = x; }

    std::vector<double> get_xdata (void) const { return m_xdata; }

    void set_y (const double y) { m_y = y; }

    double get_y (void) const { return m_y; }

    void set_z (const double z) { m_z = z; }

    double get_z (void) const { return m_z; }

    void set_code (const uint32_t code) { m_code = code; }

    uint32_t get_code (void) const { return m_code; }

    void set_svg_element (const std::string& svg) { m_svg_element = svg; }

    std::string get_svg_element (void) const { return m_svg_element; }

    void set_color (const uint8NDArray& c)
    {
      m_color(0) = static_cast<double> (c(0)) / 255;
      m_color(1) = static_cast<double> (c(1)) / 255;
      m_color(2) = static_cast<double> (c(2)) / 255;
    }

    Matrix get_color (void) const { return m_color; }

  private:

    std::string m_str;
    std::string m_family;
    font m_fnt;
    double m_x, m_y, m_z;
    std::vector<double> m_xdata;
    uint32_t m_code;
    Matrix m_color;
    std::string m_svg_element;
  };

  void text_to_strlist (const std::string& txt,
                        std::list<string>& lst, Matrix& box,
                        int halign, int valign, double rotation = 0.0,
                        const caseless_str& interpreter = "tex");

private:

  base_text_renderer *m_rep;
  base_text_renderer *m_latex_rep;
};

OCTAVE_END_NAMESPACE(octave)

#endif
