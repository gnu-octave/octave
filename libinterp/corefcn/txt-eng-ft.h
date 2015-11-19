/*

Copyright (C) 2009-2015 Michael Goffioul

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

#if ! defined (txt_eng_ft_h)
#define txt_eng_ft_h 1

#if HAVE_FREETYPE

#include <list>
#include <vector>

#include <ft2build.h>
#include FT_FREETYPE_H

#include <dMatrix.h>
#include <uint8NDArray.h>
#include "txt-eng.h"

class
OCTINTERP_API
ft_render : public text_processor
{
public:
  enum
  {
    MODE_BBOX   = 0,
    MODE_RENDER = 1
  };

  enum
  {
    ROTATION_0   = 0,
    ROTATION_90  = 1,
    ROTATION_180 = 2,
    ROTATION_270 = 3
  };

public:

  ft_render (void);

  ~ft_render (void);

  void visit (text_element_string& e);

  void visit (text_element_list& e);

  void visit (text_element_subscript& e);

  void visit (text_element_superscript& e);

  void visit (text_element_color& e);

  void visit (text_element_fontsize& e);

  void visit (text_element_fontname& e);

  void visit (text_element_fontstyle& e);

  void visit (text_element_symbol& e);

  void visit (text_element_combined& e);

  void reset (void);

  uint8NDArray get_pixels (void) const { return pixels; }

  Matrix get_boundingbox (void) const { return bbox; }

  uint8NDArray render (text_element* elt, Matrix& box,
                       int rotation = ROTATION_0);

  Matrix get_extent (text_element *elt, double rotation = 0.0);
  Matrix get_extent (const std::string& txt, double rotation = 0.0,
                     const caseless_str& interpreter = "tex");

  void set_font (const std::string& name, const std::string& weight,
                 const std::string& angle, double size);

  void set_color (Matrix c);

  void set_mode (int m);

  void text_to_pixels (const std::string& txt,
                       uint8NDArray& pixels_, Matrix& bbox,
                       int halign, int valign, double rotation,
                       const caseless_str& interpreter = "tex",
                       bool handle_rotation = true);

private:
  int rotation_to_mode (double rotation) const;

  // No copying!

  ft_render (const ft_render&);

  ft_render& operator = (const ft_render&);

  // Class to hold information about fonts and a strong
  // reference to the font objects loaded by FreeType.
  class ft_font
  {
  public:
    ft_font (void)
      : name (), weight (), angle (), size (0), face (0) { }

    ft_font (const std::string& nm, const std::string& wt,
             const std::string& ang, double sz, FT_Face f = 0)
      : name (nm), weight (wt), angle (ang), size (sz), face (f) { }

    ft_font (const ft_font& ft);

    ~ft_font (void)
    {
      if (face)
        FT_Done_Face (face);
    }

    ft_font& operator = (const ft_font& ft);

    bool is_valid (void) const { return get_face (); }

    std::string get_name (void) const { return name; }

    std::string get_weight (void) const { return weight; }

    std::string get_angle (void) const { return angle; }

    double get_size (void) const { return size; }

    FT_Face get_face (void) const;

  private:
    std::string name;
    std::string weight;
    std::string angle;
    double size;
    mutable FT_Face face;
  };

  void push_new_line (void);

  void update_line_bbox (void);

  void compute_bbox (void);

  int compute_line_xoffset (const Matrix& lb) const;

  FT_UInt process_character (FT_ULong code, FT_UInt previous = 0);

public:
  // A class to store informations on substrings after parsing.
  class ft_string : public ft_font
  {
  public:
    ft_string (const std::string s, const std::string fontang,
              const std::string fontwgt, const std::string nm,
              const double fontsz, const double x0, const double y0)
      : ft_font (nm, fontwgt, fontang, fontsz),
        string(s), x(x0), y(y0), z(0.0), code(0),
        color(Matrix (1,3,0.0)){ }

    void set_string (const std::string str) { string = str; }

    std::string get_string (void) const { return string; }

    void set_x (const double x0) { x = x0; }

    double get_x (void) const { return x; }

    void set_y (const double y0) { y = y0; }

    double get_y (void) const { return y; }

    void set_z (const double z0) { z = z0; }

    double get_z (void) const { return z; }

    void set_code (const uint32_t c) { code = c; }

    uint32_t get_code (void) const { return code; }

    void set_color (const uint8NDArray c)
    {
      color(0) = static_cast<double> (c(0)) / 255;
      color(1) = static_cast<double> (c(1)) / 255;
      color(2) = static_cast<double> (c(2)) / 255;
    }

    Matrix get_color (void) const { return color; }

  private:
    std::string  string;
    double x, y, z;
    uint32_t code;
    Matrix color;
  };

 void text_to_strlist (const std::string& txt,
                        std::list<ft_string>& lst, Matrix& box,
                        int ha, int va, double rot,
                        const caseless_str& interp = "tex")
  {
    uint8NDArray pixels_;
    // First run text_to_pixels which will also build the string list
    text_to_pixels (txt, pixels_, box, ha, va, rot, interp, false);

    lst = strlist;
  }

private:
  // The current font used by the renderer.
  ft_font font;

  // Used to stored the bounding box corresponding to the rendered text.
  // The bounding box has the form [x, y, w, h] where x and y represent the
  // coordinates of the bottom left corner relative to the anchor point of
  // the text (== start of text on the baseline). Due to font descent or
  // multiple lines, the value y is usually negative.
  Matrix bbox;

  // Used to stored the rendered text. It's a 3D matrix with size MxNx4
  // where M and N are the width and height of the bounding box.
  uint8NDArray pixels;

  // Used to store the bounding box of each line. This is used to layout
  // multiline text properly.
  std::list<Matrix> line_bbox;

  // The current horizontal alignment. This is used to align multi-line text.
  int halign;

  // The X offset for the next glyph.
  int xoffset;

  // The Y offset of the baseline for the current line.
  int line_yoffset;

  // The Y offset of the baseline for the next glyph. The offset is relative
  // to line_yoffset. The total Y offset is computed with:
  // line_yoffset + yoffset.
  int yoffset;

  // The current mode of the rendering process (box computing or rendering).
  int mode;

  // The base color of the rendered text.
  uint8NDArray color;

  // A list of parsed strings to be used for printing.
  std::list<ft_string> strlist;

  // The X offset of the baseline for the current line.
  int line_xoffset;

};

#endif // HAVE_FREETYPE

#endif
