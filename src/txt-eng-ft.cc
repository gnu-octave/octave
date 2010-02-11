/*

Copyright (C) 2009 Michael Goffioul

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_FREETYPE

#if HAVE_FONTCONFIG
#include <fontconfig/fontconfig.h>
#endif

#include <iostream>

#include "error.h"
#include "pr-output.h"
#include "txt-eng-ft.h"

class
ft_manager
{
public:
  static bool instance_ok (void)
    {
      bool retval = true;

      if (! instance)
        instance = new ft_manager ();

      if (! instance)
        {
          ::error ("unable to create ft_manager!");

          retval = false;
        }

      return retval;
    }

  static FT_Face get_font (const std::string& name, const std::string& weight,
                           const std::string& angle, double size)
    { return (instance_ok ()
              ? instance->do_get_font (name, weight, angle, size)
              : 0); }

private:
  static ft_manager *instance;

private:
  ft_manager (void)
    {
      if (FT_Init_FreeType (&library))
        {
          ::error ("unable to initialize freetype library");
        }

#if HAVE_FONTCONFIG
      fc_init_done = false;
      if (! FcInit ())
        {
          ::error ("unable to initialize fontconfig library");
        }
      else
        {
          fc_init_done = true;
        }
#endif
    }

  ~ft_manager (void)
    {
#if HAVE_FONTCONFIG
      FcFini ();
      fc_init_done = false;
#endif
    }

  FT_Face do_get_font (const std::string& name, const std::string& weight,
                       const std::string& angle, double size)
    {
      FT_Face retval = 0;

      std::string file;

#if HAVE_FONTCONFIG
      if (fc_init_done)
        {
          int fc_weight, fc_angle;

          if (weight == "bold")
            fc_weight = FC_WEIGHT_BOLD;
          else if (weight == "light")
            fc_weight = FC_WEIGHT_LIGHT;
          else if (weight == "demi")
            fc_weight = FC_WEIGHT_DEMIBOLD;
          else
            fc_weight = FC_WEIGHT_NORMAL;

          if (angle == "italic")
            fc_angle = FC_SLANT_ITALIC;
          else if (angle == "oblique")
            fc_angle = FC_SLANT_OBLIQUE;
          else
            fc_angle = FC_SLANT_ROMAN;

          FcPattern *pat = FcPatternCreate ();

          FcPatternAddString (pat, FC_FAMILY,
                              (reinterpret_cast<const FcChar8*>
                               (name == "*" ? "sans" : name.c_str ())));

          FcPatternAddInteger (pat, FC_WEIGHT, fc_weight);
          FcPatternAddInteger (pat, FC_SLANT, fc_angle);
          FcPatternAddDouble (pat, FC_PIXEL_SIZE, size);

          if (FcConfigSubstitute (0, pat, FcMatchPattern))
            {
              FcResult res;
              FcPattern *match;

              FcDefaultSubstitute (pat);
              match = FcFontMatch (0, pat, &res);

              // FIXME -- originally, this test also required that
              // res != FcResultNoMatch.  Is that really needed?
              if (match)
                {
                  unsigned char *tmp;

                  FcPatternGetString (match, FC_FILE, 0, &tmp);
                  file = reinterpret_cast<char*> (tmp);
                }
              else
                ::warning ("could not match any font: %s-%s-%s-%g",
                         name.c_str (), weight.c_str (), angle.c_str (),
                         size);

              if (match)
                FcPatternDestroy (match);
            }

          FcPatternDestroy (pat);
        }
#endif

      if (file.empty ())
        {
#ifdef __WIN32__
          file = "C:/WINDOWS/Fonts/verdana.ttf";
#else
          // FIXME: find a "standard" font for UNIX platforms
#endif
        }

      if (! file.empty () && FT_New_Face (library, file.c_str (), 0, &retval))
        ::warning ("ft_manager: unable to load font: %s", file.c_str ());
      
      return retval;
    }

private:
  FT_Library library;
#if HAVE_FONTCONFIG
  bool fc_init_done;
#endif
};

ft_manager* ft_manager::instance = 0;

// ---------------------------------------------------------------------------

ft_render::ft_render (void)
    : text_processor (), face (0), bbox (1, 4, 0.0),
      xoffset (0), yoffset (0), mode (MODE_BBOX),
      red (0), green (0), blue (0)
{
}

ft_render::~ft_render (void)
{
  if (face)
    FT_Done_Face (face);
}

void
ft_render::set_font (const base_properties& props)
{
  if (face)
    FT_Done_Face (face);

  // FIXME: take "fontunits" into account
  double font_size = props.get ("fontsize").double_value ();

  face = ft_manager::get_font (props.get ("fontname").string_value (),
                               props.get ("fontweight").string_value (),
                               props.get ("fontangle").string_value (),
                               font_size);

  if (face)
    {
      if (FT_Set_Char_Size (face, 0, font_size*64, 0, 0))
        ::warning ("ft_render: unable to set font size to %d", font_size);
    }
  else
    ::warning ("ft_render: unable to load appropriate font");
}

void
ft_render::set_mode (int m)
{
  mode = m;

  switch (mode)
    {
    case MODE_BBOX:
      xoffset = yoffset = 0;
      bbox = Matrix (1, 4, 0.0);
      break;
    case MODE_RENDER:
      if (bbox.numel () != 4)
        {
          ::warning ("ft_render: invalid bounding box, cannot render");

          xoffset = yoffset = 0;
          pixels = uint8NDArray ();
        }
      else
        {
          pixels = uint8NDArray (dim_vector (4, bbox(2), bbox(3)),
                                 static_cast<uint8_t> (0));
          xoffset = 0;
          yoffset = -bbox(1)-1;
        }
      break;
    default:
      ::error ("ft_render: invalid mode `%d'", mode);
      break;
    }
}

void
ft_render::visit (text_element_string& e)
{
  if (face)
    {
      std::string str = e.string_value ();
      FT_UInt glyph_index, previous = 0;

      for (int i = 0; i < str.length (); i++)
        {
          glyph_index = FT_Get_Char_Index (face, str[i]);

          if (! glyph_index
              || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT))
            ::warning ("ft_render: skipping missing glyph for character `%c'",
                       str[i]);
          else
            {
              switch (mode)
                {
                case MODE_RENDER:
                  if (FT_Render_Glyph (face->glyph, FT_RENDER_MODE_NORMAL))
                    ::warning ("ft_render: unable to render glyph for character `%c'",
                               str[i]);
                  else
                    {
                      FT_Bitmap& bitmap = face->glyph->bitmap;
                      int x0, y0;

                      if (previous)
                        {
                          FT_Vector delta;

                          FT_Get_Kerning (face, previous, glyph_index, FT_KERNING_DEFAULT, &delta);
                          xoffset += (delta.x >> 6);
                        }

                      x0 = xoffset+face->glyph->bitmap_left;
                      y0 = yoffset+face->glyph->bitmap_top;
                      for (int r = 0; r < bitmap.rows; r++)
                        for (int c = 0; c < bitmap.width; c++)
                          {
                            unsigned char pix = bitmap.buffer[r*bitmap.width+c];
                            if (x0+c < 0 || x0+c >= pixels.dim2()
                                || y0-r < 0 || y0-r >= pixels.dim3())
                              {
                                //::error ("out-of-bound indexing!!");
                              }
                            else if (pixels(3, x0+c, y0-r).value () == 0)
                              {
                                pixels(0, x0+c, y0-r) = red;
                                pixels(1, x0+c, y0-r) = green;
                                pixels(2, x0+c, y0-r) = blue;
                                pixels(3, x0+c, y0-r) = pix;
                              }
                          }

                      xoffset += (face->glyph->advance.x >> 6);
                    }
                  break;

                case MODE_BBOX:
                  // width
                  if (previous)
                    {
                      FT_Vector delta;

                      FT_Get_Kerning (face, previous, glyph_index, FT_KERNING_DEFAULT, &delta);
                      bbox(2) += (delta.x >> 6);
                    }
                  bbox(2) += (face->glyph->advance.x >> 6);

                  int asc, desc;

                  if (false /*tight*/)
                    {
                      desc = face->glyph->metrics.horiBearingY - face->glyph->metrics.height;
                      asc = face->glyph->metrics.horiBearingY;
                    }
                  else
                    {
                      asc = face->size->metrics.ascender;
                      desc = face->size->metrics.descender;
                    }

                  asc = yoffset + (asc >> 6);
                  desc = yoffset + (desc >> 6);

                  if (desc < bbox(1))
                    {
                      bbox(3) += (bbox(1) - desc);
                      bbox(1) = desc;
                    }
                  if (asc > (bbox(3)+bbox(1)))
                    bbox(3) = asc-bbox(1);
                  break;
                }

              previous = glyph_index;
            }
        }
    }
}

void
ft_render::reset (void)
{
  set_mode (MODE_BBOX);
  set_color (Matrix (1, 3, 0.0));
}

void
ft_render::set_color (Matrix c)
{
  if (c.numel () == 3)
    {
      red = static_cast<uint8_t> (c(0)*255);
      green = static_cast<uint8_t> (c(1)*255);
      blue = static_cast<uint8_t> (c(2)*255);
    }
  else
    ::warning ("ft_render::set_color: invalid color");
}

uint8NDArray
ft_render::render (text_element* elt, Matrix& box, int rotation)
{
  set_mode (MODE_BBOX);
  elt->accept (*this);
  box = bbox;

  set_mode (MODE_RENDER);
  if (pixels.numel () > 0)
    {
      elt->accept (*this);

      switch (rotation)
        {
        case ROTATION_0:
          break;
        case ROTATION_90:
            {
              Array<octave_idx_type> perm (3);
              perm(0) = 0;
              perm(1) = 2;
              perm(2) = 1;
              pixels = pixels.permute (perm);

              Array<idx_vector> idx (3);
              idx(0) = idx_vector (':');
              idx(1) = idx_vector (pixels.dim2()-1, -1, -1);
              idx(2) = idx_vector (':');
              pixels = uint8NDArray (pixels.index (idx));
            }
          break;
        case ROTATION_180:
            {
              Array<idx_vector> idx (3);
              idx(0) = idx_vector (':');
              idx(1) = idx_vector (pixels.dim2()-1, -1, -1);
              idx(2)=  idx_vector (pixels.dim3()-1, -1, -1);
              pixels = uint8NDArray (pixels.index (idx));
            }
          break;
        case ROTATION_270:
            {
              Array<octave_idx_type> perm (3);
              perm(0) = 0;
              perm(1) = 2;
              perm(2) = 1;
              pixels = pixels.permute (perm);

              Array<idx_vector> idx (3);
              idx(0) = idx_vector (':');
              idx(1) = idx_vector (':');
              idx(2) = idx_vector (pixels.dim3()-1, -1, -1);
              pixels = uint8NDArray (pixels.index (idx));
            }
          break;
        }
    }

  return pixels;
}

#endif // HAVE_FREETYPE
