/*

Copyright (C) 2009-2012 Michael Goffioul

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
  enum {
      MODE_BBOX   = 0,
      MODE_RENDER = 1
  };

  enum {
      ROTATION_0   = 0,
      ROTATION_90  = 1,
      ROTATION_180 = 2,
      ROTATION_270 = 3
  };

public:
  ft_render (void);

  ~ft_render (void);

  void visit (text_element_string& e);

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
                       const caseless_str& interpreter = "tex");

private:
  int rotation_to_mode (double rotation) const;

  // No copying!

  ft_render (const ft_render&);

  ft_render& operator = (const ft_render&);

  // Class to hold information about fonts and a strong
  // reference to the font objects loaded by freetype.
  class ft_font
    {
    public:
      std::string name;
      std::string weight;
      std::string angle;
      double size;
      FT_Face face;

      ft_font (const std::string& nm, const std::string& wt,
               const std::string& ang, double sz, FT_Face f)
        : name (nm), weight (wt), angle (ang), size (sz), face (f) { }

      ft_font (const ft_font& ft)
        : name (ft.name), weight (ft.weight), angle (ft.angle), size (ft.size)
        {
          if (FT_Reference_Face (ft.face) == 0)
            face = ft.face;
        }

      ~ft_font (void)
        {
          if (face)
            FT_Done_Face (face);
        }

      ft_font& operator = (const ft_font& ft)
        {
          if (&ft != this)
            {
              name = ft.name;
              weight = ft.weight;
              angle = ft.angle;
              size = ft.size;
              FT_Done_Face (face);
              if (FT_Reference_Face (ft.face))
                face = ft.face;
              else
                face = 0;
            }

          return *this;
        }

    private:
      ft_font (void);
    };

  FT_Face current_face (void)
    { return fonts.size () ? fonts.back ().face : 0; }

  void push_new_line (void);

  void compute_bbox (void);

  int compute_line_xoffset (const Matrix& lb) const;

private:
  // The stack of fonts currently used by the renderer.
  std::list<ft_font> fonts;

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

  // The current X offset for the next glyph.
  int xoffset;

  // The current Y offset for the next glyph.
  int yoffset;

  // The current mode of the rendering process (box computing or rendering).
  int mode;

  // The base color of the rendered text.
  uint8_t red, green, blue;
};

#endif // HAVE_FREETYPE

#endif
