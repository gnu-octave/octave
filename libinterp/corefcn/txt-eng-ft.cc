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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (HAVE_FREETYPE)

#if defined (HAVE_FONTCONFIG)
#include <fontconfig/fontconfig.h>
#endif

#include <iostream>
#include <map>
#include <utility>

#include "singleton-cleanup.h"

#include "error.h"
#include "pr-output.h"
#include "txt-eng-ft.h"

// FIXME -- maybe issue at most one warning per glyph/font/size/weight
// combination.

static void
gripe_missing_glyph (char c)
{
  warning_with_id ("Octave:missing-glyph",
                   "ft_render: skipping missing glyph for character '%c'",
                   c);
}

static void
gripe_glyph_render (char c)
{
  warning_with_id ("Octave:glyph-render",
                   "ft_render: unable to render glyph for character '%c'",
                   c);
}

#ifdef _MSC_VER
// This is just a trick to avoid multiply symbols definition.
// PermMatrix.h contains a dllexport'ed Array<octave_idx_type>
// that will make MSVC not to generate new instantiation and
// use the imported one.
#include "PermMatrix.h"
#endif

// Forward declaration
static void ft_face_destroyed (void* object);

class
ft_manager
{
public:
  static bool instance_ok (void)
    {
      bool retval = true;

      if (! instance)
        {
          instance = new ft_manager ();

          if (instance)
            singleton_cleanup_list::add (cleanup_instance);
        }

      if (! instance)
        {
          ::error ("unable to create ft_manager!");

          retval = false;
        }

      return retval;
    }

  static void cleanup_instance (void) { delete instance; instance = 0; }

  static FT_Face get_font (const std::string& name, const std::string& weight,
                           const std::string& angle, double size)
    { return (instance_ok ()
              ? instance->do_get_font (name, weight, angle, size)
              : 0); }

  static void font_destroyed (FT_Face face)
    {
      if (instance_ok ())
        instance->do_font_destroyed (face);
    }

private:

  static ft_manager *instance;

  typedef std::pair<std::string, double> ft_key;
  typedef std::map<ft_key, FT_Face> ft_cache;

  // Cache the fonts loaded by freetype. This cache only contains
  // weak references to the fonts, strong references are only present
  // in class ft_render.
  ft_cache cache;

private:

  // No copying!

  ft_manager (const ft_manager&);

  ft_manager& operator = (const ft_manager&);

  ft_manager (void)
    : library (), freetype_initialized (false), fontconfig_initialized (false)
    {
      if (FT_Init_FreeType (&library))
        ::error ("unable to initialize freetype library");
      else
        freetype_initialized = true;

#if defined (HAVE_FONTCONFIG)
      if (! FcInit ())
        ::error ("unable to initialize fontconfig library");
      else
        fontconfig_initialized = true;
#endif
    }

  ~ft_manager (void)
    {
      if (freetype_initialized)
        FT_Done_FreeType (library);

#if defined (HAVE_FONTCONFIG)
      // FIXME -- Skip the call to FcFini because it can trigger the
      // assertion
      //
      //   octave: fccache.c:507: FcCacheFini: Assertion 'fcCacheChains[i] == ((void *)0)' failed.
      //
      // if (fontconfig_initialized)
      //   FcFini ();
#endif
    }


  FT_Face do_get_font (const std::string& name, const std::string& weight,
                       const std::string& angle, double size)
    {
      FT_Face retval = 0;

      // Look first into the font cache, then use fontconfig. If the font
      // is present in the cache, simply add a reference and return it.

      ft_key key (name + ":" + weight + ":" + angle, size);
      ft_cache::const_iterator it = cache.find (key);

      if (it != cache.end ())
        {
          FT_Reference_Face (it->second);
          return it->second;
        }

      std::string file;

#if defined (HAVE_FONTCONFIG)
      if (fontconfig_initialized)
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

      if (! file.empty ())
        {
          if (FT_New_Face (library, file.c_str (), 0, &retval))
            ::warning ("ft_manager: unable to load font: %s", file.c_str ());
          else
            {
              // Install a finalizer to notify ft_manager that the font is
              // being destroyed. The class ft_manager only keeps weak
              // references to font objects.

              retval->generic.data = new ft_key (key);
              retval->generic.finalizer = ft_face_destroyed;

              // Insert loaded font into the cache.

              cache[key] = retval;
            }
        }

      return retval;
    }

  void do_font_destroyed (FT_Face face)
    {
      if (face->generic.data)
        {
          ft_key* pkey =
            reinterpret_cast<ft_key*> (face->generic.data);

          cache.erase (*pkey);
          delete pkey;
          face->generic.data = 0;
        }
    }

private:
  FT_Library library;
  bool freetype_initialized;
  bool fontconfig_initialized;
};

ft_manager* ft_manager::instance = 0;

static void
ft_face_destroyed (void* object)
{ ft_manager::font_destroyed (reinterpret_cast<FT_Face> (object)); }

// ---------------------------------------------------------------------------

ft_render::ft_render (void)
    : text_processor (), fonts (), bbox (1, 4, 0.0), halign (0), xoffset (0),
      yoffset (0), mode (MODE_BBOX), red (0), green (0), blue (0)
{
}

ft_render::~ft_render (void)
{
  fonts.clear ();
}

void
ft_render::set_font (const std::string& name, const std::string& weight,
                     const std::string& angle, double size)
{
  if (fonts.size () > 1)
    ::warning ("ft_render: resetting font parameters while the font stack "
               "contains more than 1 element.");

  // In all cases, we only replace the first/bottom font in the stack, if any.
  // Calling this method while there's more than 1 font in the stack does
  // not make sense: we're not gonna reconstruct the entire font stack.

  if (fonts.size ())
    fonts.pop_front ();

  // FIXME: take "fontunits" into account
  FT_Face face = ft_manager::get_font (name, weight, angle, size);

  if (face)
    {
      if (FT_Set_Char_Size (face, 0, size*64, 0, 0))
        ::warning ("ft_render: unable to set font size to %d", size);

      fonts.push_front (ft_font (name, weight, angle, size, face));
    }
  else
    ::warning ("ft_render: unable to load appropriate font");
}

void
ft_render::push_new_line (void)
{
  switch (mode)
    {
    case MODE_BBOX:
        {
          // Create a new bbox entry based on the current font.

          FT_Face face = current_face ();

          if (face)
            {
              int asc = face->size->metrics.ascender >> 6;
              int desc = face->size->metrics.descender >> 6;
              int h = face->size->metrics.height >> 6;

              Matrix bb (1, 5, 0.0);

              bb(1) = desc;
              bb(3) = asc - desc;
              bb(4) = h;

              line_bbox.push_back (bb);
            }
        }
      break;

    case MODE_RENDER:
        {
          // Move to the next line bbox, adjust xoffset based on alignment
          // and yoffset based on the old and new line bbox.

          Matrix old_bbox = line_bbox.front ();
          line_bbox.pop_front ();
          Matrix new_bbox = line_bbox.front ();

          xoffset = compute_line_xoffset (new_bbox);
          yoffset += (old_bbox(1) - (new_bbox(1) + new_bbox(3)));
        }
      break;
    }
}

int
ft_render::compute_line_xoffset (const Matrix& lb) const
{
  if (! bbox.is_empty ())
    {
      switch (halign)
        {
        case 0:
          return 0;
        case 1:
          return (bbox(2) - lb(2)) / 2;
        case 2:
          return (bbox(2) - lb(2));
        }
    }

  return 0;
}

void
ft_render::compute_bbox (void)
{
  // Stack the various line bbox together and compute the final
  // bounding box for the entire text string.

  bbox = Matrix ();

  switch (line_bbox.size ())
    {
    case 0:
      break;
    case 1:
      bbox = line_bbox.front ().extract (0, 0, 0, 3);
      break;
    default:
      for (std::list<Matrix>::const_iterator it = line_bbox.begin ();
           it != line_bbox.end (); ++it)
        {
          if (bbox.is_empty ())
            bbox = it->extract (0, 0, 0, 3);
          else
            {
              bbox(1) -= (*it)(3);
              bbox(3) += (*it)(3);
              bbox(2) = xmax (bbox(2), (*it)(2));
            }
        }
      break;
    }
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
      line_bbox.clear ();
      push_new_line ();
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
          xoffset = compute_line_xoffset (line_bbox.front ());
          yoffset = -bbox(1)-1;
        }
      break;
    default:
      ::error ("ft_render: invalid mode '%d'", mode);
      break;
    }
}

void
ft_render::visit (text_element_string& e)
{
  FT_Face face = current_face ();

  if (face)
    {
      std::string str = e.string_value ();
      FT_UInt glyph_index, previous = 0;

      for (size_t i = 0; i < str.length (); i++)
        {
          glyph_index = FT_Get_Char_Index (face, str[i]);

          if (str[i] != '\n'
              && (! glyph_index
              || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT)))
            gripe_missing_glyph (str[i]);
          else
            {
              switch (mode)
                {
                case MODE_RENDER:
                  if (str[i] == '\n')
                    {
                      glyph_index = FT_Get_Char_Index (face, ' ');
                      if (!glyph_index || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT))
                        {
                          gripe_missing_glyph (' ');
                        }
                      else
                        push_new_line ();
                    }
                  else if (FT_Render_Glyph (face->glyph, FT_RENDER_MODE_NORMAL))
                    {
                      gripe_glyph_render (str[i]);
                    }
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

                      // 'w' seems to have a negative -1
                      // face->glyph->bitmap_left, this is so we don't
                      // index out of bound, and assumes we we allocated
                      // the right amount of horizontal space in the bbox.
                      if (x0 < 0)
                        x0 = 0;

                      for (int r = 0; r < bitmap.rows; r++)
                        for (int c = 0; c < bitmap.width; c++)
                          {
                            unsigned char pix = bitmap.buffer[r*bitmap.width+c];
                            if (x0+c < 0 || x0+c >= pixels.dim2 ()
                                || y0-r < 0 || y0-r >= pixels.dim3 ())
                              {
                                //::warning ("ft_render: pixel out of bound (char=%d, (x,y)=(%d,%d), (w,h)=(%d,%d)",
                                //           str[i], x0+c, y0-r, pixels.dim2 (), pixels.dim3 ());
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
                  if (str[i] == '\n')
                    {
                      glyph_index = FT_Get_Char_Index (face, ' ');
                      if (! glyph_index
                          || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT))
                        {
                          gripe_missing_glyph (' ');
                        }
                      else
                        push_new_line ();
                    }
                  else
                    {
                      Matrix& bb = line_bbox.back ();

                      // If we have a previous glyph, use kerning information.
                      // This usually means moving a bit backward before adding
                      // the next glyph. That is, "delta.x" is usually < 0.
                      if (previous)
                        {
                          FT_Vector delta;

                          FT_Get_Kerning (face, previous, glyph_index,
                                          FT_KERNING_DEFAULT, &delta);

                          bb(2) += (delta.x >> 6);
                        }

                      // Extend current line bounding box by the width of the
                      // current glyph.
                      bb(2) += (face->glyph->advance.x >> 6);
                    }
                  break;
                }

                if (str[i] == '\n')
                  previous = 0;
                else
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
  compute_bbox ();
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
              Array<octave_idx_type> perm (dim_vector (3, 1));
              perm(0) = 0;
              perm(1) = 2;
              perm(2) = 1;
              pixels = pixels.permute (perm);

              Array<idx_vector> idx (dim_vector (3, 1));
              idx(0) = idx_vector (':');
              idx(1) = idx_vector (pixels.dim2 ()-1, -1, -1);
              idx(2) = idx_vector (':');
              pixels = uint8NDArray (pixels.index (idx));
            }
          break;
        case ROTATION_180:
            {
              Array<idx_vector> idx (dim_vector (3, 1));
              idx(0) = idx_vector (':');
              idx(1) = idx_vector (pixels.dim2 ()-1, -1, -1);
              idx(2)=  idx_vector (pixels.dim3 ()-1, -1, -1);
              pixels = uint8NDArray (pixels.index (idx));
            }
          break;
        case ROTATION_270:
            {
              Array<octave_idx_type> perm (dim_vector (3, 1));
              perm(0) = 0;
              perm(1) = 2;
              perm(2) = 1;
              pixels = pixels.permute (perm);

              Array<idx_vector> idx (dim_vector (3, 1));
              idx(0) = idx_vector (':');
              idx(1) = idx_vector (':');
              idx(2) = idx_vector (pixels.dim3 ()-1, -1, -1);
              pixels = uint8NDArray (pixels.index (idx));
            }
          break;
        }
    }

  return pixels;
}

// Note:
// x-extent accurately measures width of glyphs.
// y-extent is overly large because it is measured from baseline-to-baseline.
// Calling routines, such as ylabel, may need to account for this mismatch.

Matrix
ft_render::get_extent (text_element *elt, double rotation)
{
  set_mode (MODE_BBOX);
  elt->accept (*this);
  compute_bbox ();

  Matrix extent (1, 2, 0.0);

  switch (rotation_to_mode (rotation))
    {
    case ROTATION_0:
    case ROTATION_180:
      extent(0) = bbox(2);
      extent(1) = bbox(3);
      break;
    case ROTATION_90:
    case ROTATION_270:
      extent(0) = bbox(3);
      extent(1) = bbox(2);
    }

  return extent;
}

Matrix
ft_render::get_extent (const std::string& txt, double rotation,
                       const caseless_str& interpreter)
{
  text_element *elt = text_parser::parse (txt, interpreter);
  Matrix extent = get_extent (elt, rotation);
  delete elt;

  return extent;
}

int
ft_render::rotation_to_mode (double rotation) const
{
  if (rotation == 0.0)
    return ROTATION_0;
  else if (rotation == 90.0)
    return ROTATION_90;
  else if (rotation == 180.0)
    return ROTATION_180;
  else if (rotation == 270.0)
    return ROTATION_270;
  else
    return ROTATION_0;
}

void
ft_render::text_to_pixels (const std::string& txt,
                           uint8NDArray& pixels_, Matrix& box,
                           int _halign, int valign, double rotation,
                           const caseless_str& interpreter)
{
  // FIXME: clip "rotation" between 0 and 360
  int rot_mode = rotation_to_mode (rotation);

  halign = _halign;

  text_element *elt = text_parser::parse (txt, interpreter);
  pixels_ = render (elt, box, rot_mode);
  delete elt;

  if (pixels_.numel () == 0)
    {
      // nothing to render
      return;
    }

  switch (halign)
    {
    default: box(0) = 0; break;
    case 1: box(0) = -box(2)/2; break;
    case 2: box(0) = -box(2); break;
    }
  switch (valign)
    {
    default: box(1) = 0; break;
    case 1: box(1) = -box(3)/2; break;
    case 2: box(1) = -box(3); break;
    case 3: break;
    case 4: box(1) = -box(3)-box(1); break;
    }

  switch (rot_mode)
    {
    case ROTATION_90:
      std::swap (box(0), box(1));
      std::swap (box(2), box(3));
      box(0) = -box(0)-box(2);
      break;
    case ROTATION_180:
      box(0) = -box(0)-box(2);
      box(1) = -box(1)-box(3);
      break;
    case ROTATION_270:
      std::swap (box(0), box(1));
      std::swap (box(2), box(3));
      box(1) = -box(1)-box(3);
      break;
    }
}

#endif // HAVE_FREETYPE
