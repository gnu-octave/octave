/*

Copyright (C) 2009-2011 Michael Goffioul

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

  void set_font (const std::string& name, const std::string& weight,
                 const std::string& angle, double size);

  void set_color (Matrix c);

  void set_mode (int m);

  void text_to_pixels (const std::string& txt,
                       uint8NDArray& pixels_, Matrix& bbox,
                       int halign, int valign, double rotation);

private:
  int rotation_to_mode (double rotation) const;

private:
  FT_Face face;
  Matrix bbox;
  uint8NDArray pixels;
  int xoffset;
  int yoffset;  
  int mode;
  uint8_t red, green, blue;
};

#endif // HAVE_FREETYPE

#endif
