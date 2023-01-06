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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "base-text-renderer.h"
#include "ft-text-renderer.h"

#if defined (HAVE_FREETYPE)

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#if defined (HAVE_FONTCONFIG)
#  include <fontconfig/fontconfig.h>
#endif

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif

#include <clocale>
#include <cwchar>
#include <map>
#include <utility>

#include "singleton-cleanup.h"
#include "unistr-wrappers.h"

#include "defaults.h"
#include "error.h"
#include "file-ops.h"
#include "oct-env.h"
#include "pr-output.h"
#include "sysdep.h"
#include "text-renderer.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// FIXME: maybe issue at most one warning per glyph/font/size/weight
//        combination.

static void
warn_missing_glyph (FT_ULong c)
{
  warning_with_id ("Octave:missing-glyph",
                   "text_renderer: skipping missing glyph for character '%lx'", c);
}

static void
warn_glyph_render (FT_ULong c)
{
  warning_with_id ("Octave:glyph-render",
                   "text_renderer: unable to render glyph for character '%lx'", c);
}

#if defined (_MSC_VER)
// FIXME: is this really needed?
//
// This is just a trick to avoid multiple symbol definitions.
// PermMatrix.h contains a dllexport'ed Array<octave_idx_type>
// that will cause MSVC not to generate a new instantiation and
// use the imported one instead.
#  include "PermMatrix.h"
#endif

// Forward declaration
static void ft_face_destroyed (void *object);

class
ft_manager
{
private:

  ft_manager (void)
    : m_library (), m_freetype_initialized (false),
      m_fontconfig_initialized (false)
  {
    if (FT_Init_FreeType (&m_library))
      error ("unable to initialize FreeType library");
    else
      m_freetype_initialized = true;

#if defined (HAVE_FONTCONFIG)
    if (! FcInit ())
      error ("unable to initialize fontconfig library");
    else
      m_fontconfig_initialized = true;
#endif
  }

public:

  // No copying!

  ft_manager (const ft_manager&) = delete;

  ft_manager& operator = (const ft_manager&) = delete;

private:

  ~ft_manager (void)
  {
    if (m_freetype_initialized)
      FT_Done_FreeType (m_library);

#if defined (HAVE_FONTCONFIG)
    // FIXME: Skip the call to FcFini because it can trigger the assertion
    //
    //   octave: fccache.c:507: FcCacheFini: Assertion 'fcCacheChains[i] == ((void *)0)' failed.
    //
    // if (m_fontconfig_initialized)
    //   FcFini ();
#endif
  }

public:

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! m_instance)
      {
        m_instance = new ft_manager ();
        singleton_cleanup_list::add (cleanup_instance);
      }

    return retval;
  }

  static void cleanup_instance (void)
  { delete m_instance; m_instance = nullptr; }

  static FT_Face get_font (const std::string& name, const std::string& weight,
                           const std::string& angle, double size,
                           FT_ULong c = 0)
  {
    return (instance_ok ()
            ? m_instance->do_get_font (name, weight, angle, size, c)
            : nullptr);
  }

  static octave_map get_system_fonts (void)
  {
    return (instance_ok ()
            ? m_instance->do_get_system_fonts ()
            : octave_map ());
  }

  static void font_destroyed (FT_Face face)
  {
    if (instance_ok ())
      m_instance->do_font_destroyed (face);
  }

private:

  typedef std::pair<std::string, double> ft_key;
  typedef std::map<ft_key, FT_Face> ft_cache;

  static octave_map do_get_system_fonts (void)
  {
    static octave_map font_map;

    if (font_map.isempty ())
      {
#if defined (HAVE_FONTCONFIG)
        FcConfig *config = FcConfigGetCurrent();
        FcPattern *pat = FcPatternCreate ();
        FcObjectSet *os = FcObjectSetBuild (FC_FAMILY, FC_SLANT, FC_WEIGHT,
                                            FC_CHARSET, nullptr);
        FcFontSet *fs = FcFontList (config, pat, os);

        if (fs->nfont > 0)
          {
            // Mark fonts that have at least all printable ASCII chars
            FcCharSet *minimal_charset =  FcCharSetCreate ();
            for (int i = 32; i < 127; i++)
              FcCharSetAddChar (minimal_charset, static_cast<FcChar32> (i));

            string_vector fields (4);
            fields(0) = "family";
            fields(1) = "angle";
            fields(2) = "weight";
            fields(3) = "suitable";

            dim_vector dv (1, fs->nfont);
            Cell families (dv);
            Cell angles (dv);
            Cell weights (dv);
            Cell suitable (dv);

            unsigned char *family;
            int val;
            for (int i = 0; fs && i < fs->nfont; i++)
              {
                FcPattern *font = fs->fonts[i];
                if (FcPatternGetString (font, FC_FAMILY, 0, &family)
                    == FcResultMatch)
                  families(i) = std::string (reinterpret_cast<char *> (family));
                else
                  families(i) = "unknown";

                if (FcPatternGetInteger (font, FC_SLANT, 0, &val)
                    == FcResultMatch)
                  angles(i) = (val == FC_SLANT_ITALIC
                               || val == FC_SLANT_OBLIQUE)
                              ? "italic" : "normal";
                else
                  angles(i) = "unknown";

                if (FcPatternGetInteger (font, FC_WEIGHT, 0, &val)
                    == FcResultMatch)
                  weights(i) = (val == FC_WEIGHT_BOLD
                                || val == FC_WEIGHT_DEMIBOLD)
                               ? "bold" : "normal";
                else
                  weights(i) = "unknown";

                FcCharSet *cset;
                if (FcPatternGetCharSet (font, FC_CHARSET, 0, &cset)
                    == FcResultMatch)
                  suitable(i) = (FcCharSetIsSubset (minimal_charset, cset)
                                 ? true : false);
                else
                  suitable(i) = false;
              }

            font_map = octave_map (dv, fields);

            font_map.assign ("family", families);
            font_map.assign ("angle", angles);
            font_map.assign ("weight", weights);
            font_map.assign ("suitable", suitable);

            if (fs)
              FcFontSetDestroy (fs);
          }
#endif
      }

    return font_map;
  }

  FT_Face do_get_font (const std::string& name, const std::string& weight,
                       const std::string& angle, double size,
                       FT_ULong search_code_point)
  {
    FT_Face retval = nullptr;

#if defined (HAVE_FT_REFERENCE_FACE)
    // Look first into the font cache, then use fontconfig.  If the font
    // is present in the cache, simply add a reference and return it.

    ft_key key (name + ':' + weight + ':' + angle + ':'
                + std::to_string (search_code_point), size);
    ft_cache::const_iterator it = m_cache.find (key);

    if (it != m_cache.end ())
      {
        FT_Reference_Face (it->second);
        return it->second;
      }
#endif

    static std::string fonts_dir;

    if (fonts_dir.empty ())
      {
        fonts_dir = sys::env::getenv ("OCTAVE_FONTS_DIR");

        if (fonts_dir.empty ())
#if defined (SYSTEM_FREEFONT_DIR)
          fonts_dir = SYSTEM_FREEFONT_DIR;
#else
          fonts_dir = config::oct_fonts_dir ();
#endif
      }


    // Default font file
    std::string file;

    if (! fonts_dir.empty ())
      {
        file = fonts_dir + sys::file_ops::dir_sep_str () + "FreeSans";

        if (weight == "bold")
          file += "Bold";

        if (angle == "italic" || angle == "oblique")
          file += "Oblique";

        file += ".otf";
      }

#if defined (HAVE_FONTCONFIG)
    if ((search_code_point != 0 || name != "*") && m_fontconfig_initialized)
      {
        int fc_weight, fc_angle;

        if (weight == "bold")
          fc_weight = FC_WEIGHT_BOLD;
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
                            (reinterpret_cast<const FcChar8 *>
                             (name.c_str ())));

        FcPatternAddInteger (pat, FC_WEIGHT, fc_weight);
        FcPatternAddInteger (pat, FC_SLANT, fc_angle);
        FcPatternAddDouble (pat, FC_PIXEL_SIZE, size);

        if (search_code_point > 0)
          {
            FcCharSet *minimal_charset =  FcCharSetCreate ();
            FcCharSetAddChar (minimal_charset,
                              static_cast<FcChar32> (search_code_point));
            FcPatternAddCharSet (pat, FC_CHARSET, minimal_charset);
          }

        if (FcConfigSubstitute (nullptr, pat, FcMatchPattern))
          {
            FcResult res;
            FcPattern *match;

            FcDefaultSubstitute (pat);

            match = FcFontMatch (nullptr, pat, &res);

            // FIXME: originally, this test also required that
            // res != FcResultNoMatch.  Is that really needed?
            if (match)
              {
                unsigned char *tmp;

                FcPatternGetString (match, FC_FILE, 0, &tmp);
                file = reinterpret_cast<char *> (tmp);
              }
            else
              ::warning ("could not match any font: %s-%s-%s-%g, using default font",
                         name.c_str (), weight.c_str (), angle.c_str (),
                         size);

            if (match)
              FcPatternDestroy (match);
          }

        FcPatternDestroy (pat);
      }
#endif

    if (file.empty ())
      ::warning ("unable to find default font files");
    else
      {
        std::string ascii_file = sys::get_ASCII_filename (file);

        if (FT_New_Face (m_library, ascii_file.c_str (), 0, &retval))
          ::warning ("ft_manager: unable to load font: %s", file.c_str ());
#if defined (HAVE_FT_REFERENCE_FACE)
        else
          {
            // Install a finalizer to notify ft_manager that the font is
            // being destroyed.  The class ft_manager only keeps weak
            // references to font objects.

            retval->generic.data = new ft_key (key);
            retval->generic.finalizer = ft_face_destroyed;

            // Insert loaded font into the cache.
            if (FT_Reference_Face (retval) == 0)
              m_cache[key] = retval;
          }
#endif
      }

    return retval;
  }

  void do_font_destroyed (FT_Face face)
  {
    if (face->generic.data)
      {
        ft_key *pkey = reinterpret_cast<ft_key *> (face->generic.data);

        m_cache.erase (*pkey);
        delete pkey;
        face->generic.data = nullptr;
        FT_Done_Face (face);
      }
  }

  //--------

  static ft_manager *m_instance;

  // Cache the fonts loaded by FreeType.  This cache only contains
  // weak references to the fonts, strong references are only present
  // in class text_renderer.
  ft_cache m_cache;

  FT_Library m_library;
  bool m_freetype_initialized;
  bool m_fontconfig_initialized;
};

ft_manager *ft_manager::m_instance = nullptr;

static void
ft_face_destroyed (void *object)
{
  ft_manager::font_destroyed (reinterpret_cast<FT_Face> (object));
}

class
OCTINTERP_API
ft_text_renderer : public base_text_renderer
{
public:

  enum
  {
    MODE_BBOX   = 0,
    MODE_RENDER = 1
  };

public:

  ft_text_renderer (void)
    : base_text_renderer (), m_font (), m_bbox (1, 4, 0.0), m_halign (0),
      m_xoffset (0), m_line_yoffset (0), m_yoffset (0), m_mode (MODE_BBOX),
      m_color (dim_vector (1, 3), 0), m_do_strlist (false), m_strlist (),
      m_line_xoffset (0), m_ymin (0), m_ymax (0), m_deltax (0),
      m_max_fontsize (0), m_antialias (true)
  { }

  // No copying!

  ft_text_renderer (const ft_text_renderer&) = delete;

  ft_text_renderer& operator = (const ft_text_renderer&) = delete;

  ~ft_text_renderer (void) = default;

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

  uint8NDArray get_pixels (void) const { return m_pixels; }

  Matrix get_boundingbox (void) const { return m_bbox; }

  uint8NDArray render (text_element *elt, Matrix& box,
                       int rotation = ROTATION_0);

  Matrix get_extent (text_element *elt, double rotation = 0.0);
  Matrix get_extent (const std::string& txt, double rotation,
                     const caseless_str& interpreter);

  void set_anti_aliasing (bool val) { m_antialias = val; };

  void set_font (const std::string& name, const std::string& weight,
                 const std::string& angle, double size);

  octave_map get_system_fonts (void);

  void set_color (const Matrix& c);

  void set_mode (int m);

  void text_to_pixels (const std::string& txt,
                       uint8NDArray& pxls, Matrix& bbox,
                       int halign, int valign, double rotation,
                       const caseless_str& interpreter,
                       bool handle_rotation);

private:

  // Class to hold information about fonts and a strong
  // reference to the font objects loaded by FreeType.

  class ft_font : public text_renderer::font
  {
  public:

    ft_font (void)
      : text_renderer::font (), m_face (nullptr) { }

    ft_font (const std::string& nm, const std::string& wt,
             const std::string& ang, double sz, FT_Face f = nullptr)
      : text_renderer::font (nm, wt, ang, sz), m_face (f)
    { }

    ft_font (const ft_font& ft);

    ~ft_font (void)
    {
      if (m_face)
        FT_Done_Face (m_face);
    }

    ft_font& operator = (const ft_font& ft);

    bool is_valid (void) const { return get_face (); }

    FT_Face get_face (void) const;

  private:

    mutable FT_Face m_face;
  };

  void push_new_line (void);

  void update_line_bbox (void);

  void compute_bbox (void);

  int compute_line_xoffset (const Matrix& lb) const;

  FT_UInt process_character (FT_ULong code, FT_UInt previous,
                             std::string& sub_font);

public:

  void text_to_strlist (const std::string& txt,
                        std::list<text_renderer::string>& lst, Matrix& bbox,
                        int halign, int valign, double rotation,
                        const caseless_str& interp);

private:

  // The current font used by the renderer.
  ft_font m_font;

  // Used to stored the bounding box corresponding to the rendered text.
  // The bounding box has the form [x, y, w, h] where x and y represent the
  // coordinates of the bottom left corner relative to the anchor point of
  // the text (== start of text on the baseline).  Due to font descent or
  // multiple lines, the value y is usually negative.
  Matrix m_bbox;

  // Used to stored the rendered text.  It's a 3D matrix with size MxNx4
  // where M and N are the width and height of the bounding box.
  uint8NDArray m_pixels;

  // Used to store the bounding box of each line.  This is used to layout
  // multiline text properly.
  std::list<Matrix> m_line_bbox;

  // The current horizontal alignment.  This is used to align multi-line text.
  int m_halign;

  // The X offset for the next glyph.
  int m_xoffset;

  // The Y offset of the baseline for the current line.
  int m_line_yoffset;

  // The Y offset of the baseline for the next glyph.  The offset is relative
  // to line_yoffset.  The total Y offset is computed with:
  // line_yoffset + yoffset.
  int m_yoffset;

  // The current mode of the rendering process (box computing or rendering).
  int m_mode;

  // The base color of the rendered text.
  uint8NDArray m_color;

  // A list of parsed strings to be used for printing.
  bool m_do_strlist;
  std::list<text_renderer::string> m_strlist;

  // The X offset of the baseline for the current line.
  int m_line_xoffset;

  // Min and max y coordinates of all glyphs in a line.
  FT_Pos m_ymin;
  FT_Pos m_ymax;

  // Difference between the advance and the actual extent of the latest glyph
  FT_Pos m_deltax;

  // Used for computing the distance between lines.
  double m_max_fontsize;

  // Anti-aliasing.
  bool m_antialias;

};

void
ft_text_renderer::set_font (const std::string& name,
                            const std::string& weight,
                            const std::string& angle, double size)
{
  // FIXME: take "fontunits" into account
  m_font = ft_font (name, weight, angle, size, nullptr);
}

octave_map
ft_text_renderer::get_system_fonts (void)
{
  return ft_manager::get_system_fonts ();
}

void
ft_text_renderer::push_new_line (void)
{
  switch (m_mode)
    {
    case MODE_BBOX:
      {
        // Create a new bbox entry based on the current font.

        FT_Face face = m_font.get_face ();

        if (face)
          {
            Matrix bb (1, 5, 0.0);

            m_line_bbox.push_back (bb);

            m_xoffset = m_yoffset = 0;
            m_ymin = m_ymax = m_deltax = 0;
          }
      }
      break;

    case MODE_RENDER:
      {
        // Move to the next line bbox, adjust xoffset based on alignment
        // and yoffset based on the old and new line bbox.

        Matrix old_bbox = m_line_bbox.front ();
        m_line_bbox.pop_front ();
        Matrix new_bbox = m_line_bbox.front ();

        m_xoffset = m_line_xoffset = compute_line_xoffset (new_bbox);
        m_line_yoffset -= (-old_bbox(1) + math::round (0.4 * m_max_fontsize)
                           + (new_bbox(3) + new_bbox(1)));
        m_yoffset = 0;
        m_ymin = m_ymax = m_deltax = 0;
      }
      break;
    }
}

int
ft_text_renderer::compute_line_xoffset (const Matrix& lb) const
{
  if (! m_bbox.isempty ())
    {
      switch (m_halign)
        {
        case 0:
          return 0;
        case 1:
          return (m_bbox(2) - lb(2)) / 2;
        case 2:
          return (m_bbox(2) - lb(2));
        }
    }

  return 0;
}

void
ft_text_renderer::compute_bbox (void)
{
  // Stack the various line bbox together and compute the final
  // bounding box for the entire text string.

  m_bbox = Matrix ();

  switch (m_line_bbox.size ())
    {
    case 0:
      break;

    case 1:
      m_bbox = m_line_bbox.front ().extract (0, 0, 0, 3);
      break;

    default:
      for (const auto& lbox : m_line_bbox)
        {
          if (m_bbox.isempty ())
            m_bbox = lbox.extract (0, 0, 0, 3);
          else
            {
              double delta = math::round (0.4 * m_max_fontsize) + lbox(3);
              m_bbox(1) -= delta;
              m_bbox(3) += delta;
              m_bbox(2) = math::max (m_bbox(2), lbox(2));
            }
        }
      break;
    }
}

void
ft_text_renderer::update_line_bbox (void)
{
  // Called after a font change, when in MODE_BBOX mode, to update the
  // current line bbox with the new font metrics.  This also includes the
  // current yoffset, that is the offset of the current glyph's baseline
  // the line's baseline.

  if (m_mode == MODE_BBOX)
    {
      Matrix& bb = m_line_bbox.back ();
      bb(1) = m_ymin;
      // Add one pixel to the bbox height to avoid occasional text clipping.
      // See bug #55328.
      bb(3) = (m_ymax + 1) - m_ymin;
      if (m_deltax > 0)
        bb(2) += m_deltax;
    }
}

void
ft_text_renderer::set_mode (int m)
{
  m_mode = m;

  switch (m_mode)
    {
    case MODE_BBOX:
      m_xoffset = m_line_yoffset = m_yoffset = 0;
      m_max_fontsize = 0.0;
      m_bbox = Matrix (1, 4, 0.0);
      m_line_bbox.clear ();
      push_new_line ();
      break;

    case MODE_RENDER:
      if (m_bbox.numel () != 4)
        {
          ::error ("ft_text_renderer: invalid bounding box, cannot render");

          m_xoffset = m_line_yoffset = m_yoffset = 0;
          m_pixels = uint8NDArray ();
        }
      else
        {
          dim_vector d (4, octave_idx_type (m_bbox(2)),
                        octave_idx_type (m_bbox(3)));
          m_pixels = uint8NDArray (d, static_cast<uint8_t> (0));
          m_xoffset = compute_line_xoffset (m_line_bbox.front ());
          m_line_yoffset = -m_bbox(1);
          m_yoffset = 0;
        }
      break;

    default:
      error ("ft_text_renderer: invalid mode '%d'", m_mode);
      break;
    }
}

bool is_opaque (const FT_GlyphSlot& glyph, const int x, const int y)
{
  // Borrowed from https://stackoverflow.com/questions/14800827/
                         //    indexing-pixels-in-a-monochrome-freetype-glyph-buffer
  int pitch = std::abs (glyph->bitmap.pitch);
  unsigned char *row = &glyph->bitmap.buffer[pitch * y];
  char cvalue = row[x >> 3];

  return ((cvalue & (128 >> (x & 7))) != 0);
}

FT_UInt
ft_text_renderer::process_character (FT_ULong code, FT_UInt previous,
                                     std::string& sub_name)
{
  FT_Face face = m_font.get_face ();

  sub_name = face->family_name;

  FT_UInt glyph_index = 0;

  if (face)
    {
      glyph_index = FT_Get_Char_Index (face, code);

      if (code != '\n' && code != '\t'
          && (! glyph_index
              || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT)))
        {
#if defined (HAVE_FONTCONFIG)
          // Try to substitue font
          FT_Face sub_face = ft_manager::get_font (m_font.get_name (),
                             m_font.get_weight (),
                             m_font.get_angle (),
                             m_font.get_size (),
                             code);

          if (sub_face)
            {
              FT_Set_Char_Size (sub_face, 0, m_font.get_size ()*64, 0, 0);

              glyph_index = FT_Get_Char_Index (sub_face, code);

              if (glyph_index
                  && (FT_Load_Glyph (sub_face, glyph_index, FT_LOAD_DEFAULT)
                      == 0))
                {
                  static std::string prev_sub_name;

                  if (prev_sub_name.empty ()
                      || prev_sub_name != std::string (sub_face->family_name))
                    {
                      prev_sub_name = sub_face->family_name;
                      warning_with_id ("Octave:substituted-glyph",
                                       "text_renderer: substituting font to '%s' for some characters",
                                       sub_face->family_name);
                    }

                  ft_font saved_font = m_font;

                  m_font = ft_font (m_font.get_name (), m_font.get_weight (),
                                    m_font.get_angle (), m_font.get_size (),
                                    sub_face);

                  process_character (code, previous, sub_name);

                  m_font = saved_font;
                }
              else
                {
                  glyph_index = 0;
                  warn_missing_glyph (code);
                }
            }
          else
            {
              glyph_index = 0;
              warn_missing_glyph (code);
            }
#else
          glyph_index = 0;
          warn_missing_glyph (code);
#endif
        }
      else if ((code == '\n') || (code == '\t'))
        {
          glyph_index = FT_Get_Char_Index (face, ' ');
          if (! glyph_index
              || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT))
            {
              glyph_index = 0;
              warn_missing_glyph (' ');
            }
          else if (code == '\n')
            push_new_line ();
          else
            {
              // Advance to next multiple of 4 times the width of the "space"
              // character.
              int x_tab = 4 * (face->glyph->advance.x >> 6);
              m_xoffset = (1 + std::floor (1. * m_xoffset / x_tab)) * x_tab;
            }
        }
      else
        {
          switch (m_mode)
            {
            case MODE_RENDER:
              if (FT_Render_Glyph (face->glyph, (m_antialias
                                                 ? FT_RENDER_MODE_NORMAL
                                                 : FT_RENDER_MODE_MONO)))
                {
                  glyph_index = 0;
                  warn_glyph_render (code);
                }
              else
                {
                  FT_Bitmap& bitmap = face->glyph->bitmap;
                  int x0, y0;

                  if (previous)
                    {
                      FT_Vector delta;

                      FT_Get_Kerning (face, previous, glyph_index,
                                      FT_KERNING_DEFAULT, &delta);

                      m_xoffset += (delta.x >> 6);
                    }

                  x0 = m_xoffset + face->glyph->bitmap_left;
                  y0 = m_line_yoffset + m_yoffset
                       + (face->glyph->bitmap_top - 1);

                  // 'w' seems to have a negative -1
                  // face->glyph->bitmap_left, this is so we don't index out
                  // of bound, and assumes we've allocated the right amount of
                  // horizontal space in the bbox.
                  if (x0 < 0)
                    x0 = 0;

                  for (int r = 0; static_cast<unsigned int> (r) < bitmap.rows; r++)
                    for (int c = 0; static_cast<unsigned int> (c) < bitmap.width; c++)
                      {
                        unsigned char pix
                          = (m_antialias
                             ? bitmap.buffer[r*bitmap.width+c]
                             : (is_opaque (face->glyph, c, r) ? 255 : 0));

                        if (x0+c < 0 || x0+c >= m_pixels.dim2 ()
                            || y0-r < 0 || y0-r >= m_pixels.dim3 ())
                          {
                            // ::warning ("ft_text_renderer: x %d,  y %d",
                            //            x0+c, y0-r);
                          }
                        else if (m_pixels(3, x0+c, y0-r).value () == 0)
                          {
                            m_pixels(0, x0+c, y0-r) = m_color(0);
                            m_pixels(1, x0+c, y0-r) = m_color(1);
                            m_pixels(2, x0+c, y0-r) = m_color(2);
                            m_pixels(3, x0+c, y0-r) = pix;
                          }
                      }

                  m_xoffset += (face->glyph->advance.x >> 6);
                }
              break;

            case MODE_BBOX:
              Matrix& bb = m_line_bbox.back ();

              // If we have a previous glyph, use kerning information.  This
              // usually means moving a bit backward before adding the next
              // glyph.  That is, "delta.x" is usually < 0.
              if (previous)
                {
                  FT_Vector delta;

                  FT_Get_Kerning (face, previous, glyph_index,
                                  FT_KERNING_DEFAULT, &delta);

                  m_xoffset += (delta.x >> 6);
                }

              // Extend current X offset box by the width of the current
              // glyph.  Then extend the line bounding box if necessary.

              m_xoffset += (face->glyph->advance.x >> 6);
              bb(2) = math::max (bb(2), m_xoffset);

              // Store the actual bbox vertical coordinates of this character
              FT_Glyph glyph;
              if (FT_Get_Glyph (face->glyph, &glyph))
                warn_glyph_render (code);
              else
                {
                  FT_BBox  glyph_bbox;
                  FT_Glyph_Get_CBox (glyph, FT_GLYPH_BBOX_UNSCALED,
                                     &glyph_bbox);
                  m_deltax = (glyph_bbox.xMax - face->glyph->advance.x) >> 6;
                  m_ymin = math::min ((glyph_bbox.yMin >> 6) + m_yoffset,
                                      m_ymin);
                  m_ymax = math::max ((glyph_bbox.yMax >> 6) + m_yoffset,
                                      m_ymax);
                  FT_Done_Glyph (glyph);
                  update_line_bbox ();
                }
              break;
            }
        }
    }

  return glyph_index;
}

void
ft_text_renderer::text_to_strlist (const std::string& txt,
                                   std::list<text_renderer::string>& lst,
                                   Matrix& box,
                                   int ha, int va, double rot,
                                   const caseless_str& interp)
{
  uint8NDArray pxls;

  // First run text_to_pixels which will also build the string list

  m_strlist = std::list<text_renderer::string> ();

  unwind_protect_var<bool> restore_var1 (m_do_strlist);
  unwind_protect_var<std::list<text_renderer::string>>
      restore_var2 (m_strlist);

  m_do_strlist = true;

  text_to_pixels (txt, pxls, box, ha, va, rot, interp, false);

  lst = m_strlist;
}

void
ft_text_renderer::visit (text_element_string& e)
{
  if (m_font.is_valid ())
    {
      m_max_fontsize = std::max (m_max_fontsize, m_font.get_size ());
      FT_UInt glyph_index, previous = 0;

      std::string str = e.string_value ();
      const uint8_t *c = reinterpret_cast<const uint8_t *> (str.c_str ());
      uint32_t u32_c;

      std::size_t n = str.size ();
      std::size_t icurr = 0;
      std::size_t ibegin = 0;

      // Initialize a new string
      text_renderer::string fs (str, m_font, m_xoffset, m_yoffset);

      std::string fname = m_font.get_face ()->family_name;

      if (fname.find (" ") != std::string::npos)
        fname = "'" + fname + "'";

      fs.set_family (fname);

      std::vector<double> xdata;
      std::string sub_name;

      while (n > 0)
        {
          // Retrieve the length and the u32 representation of the current
          // character
          int mblen = octave_u8_strmbtouc_wrapper (&u32_c, c + icurr);
          if (mblen < 1)
            {
              // This is not an UTF-8 character, use a replacement character
              mblen = 1;
              u32_c = 0xFFFD;
            }

          n -= mblen;

          if (m_do_strlist && m_mode == MODE_RENDER)
            {
              if (u32_c == 10)
                {
                  // Finish previous string in m_strlist before processing
                  // the newline character
                  fs.set_y (m_line_yoffset + m_yoffset);
                  fs.set_color (m_color);

                  std::string s = str.substr (ibegin, icurr - ibegin);
                  if (! s.empty ())
                    {
                      fs.set_string (s);
                      fs.set_y (m_line_yoffset + m_yoffset);
                      fs.set_xdata (xdata);
                      fs.set_family (fname);
                      m_strlist.push_back (fs);
                    }
                }
              else
                xdata.push_back (m_xoffset);
            }

          glyph_index = process_character (u32_c, previous, sub_name);

          if (m_do_strlist && m_mode == MODE_RENDER && ! sub_name.empty ())
            {
              // Add substitution font to the family name stack
              std::string tmp_family = fs.get_family ();

              if (tmp_family.find (sub_name) == std::string::npos)
                {
                  if (sub_name.find (" ") != std::string::npos)
                    sub_name = "'" + sub_name + "'";

                  fs.set_family (tmp_family + ", " + sub_name);
                }
            }

          if (u32_c == 10)
            {
              previous = 0;

              if (m_do_strlist && m_mode == MODE_RENDER)
                {
                  // Start a new string in m_strlist
                  ibegin = icurr+1;
                  xdata.clear ();
                  fs = text_renderer::string (str.substr (ibegin), m_font,
                                              m_line_xoffset, m_yoffset);
                }
            }
          else
            previous = glyph_index;

          icurr += mblen;
        }

      if (m_do_strlist && m_mode == MODE_RENDER
          && ! fs.get_string ().empty ())
        {
          fs.set_y (m_line_yoffset + m_yoffset);
          fs.set_color (m_color);
          fs.set_xdata (xdata);
          m_strlist.push_back (fs);
        }
    }
}

void
ft_text_renderer::visit (text_element_list& e)
{
  // Save and restore (after processing the list) the current font and color.

  ft_font saved_font (m_font);
  uint8NDArray saved_color (m_color);

  text_processor::visit (e);

  m_font = saved_font;
  m_color = saved_color;
}

void
ft_text_renderer::visit (text_element_subscript& e)
{
  ft_font saved_font (m_font);
  int saved_line_yoffset = m_line_yoffset;
  int saved_yoffset = m_yoffset;

  double sz = m_font.get_size ();

  // Reducing font size by 70% produces decent results.
  set_font (m_font.get_name (), m_font.get_weight (), m_font.get_angle (),
            std::max (5.0, sz * 0.7));

  if (m_font.is_valid ())
    {
      // Shifting the baseline by 15% of the font size gives decent results.
      m_yoffset -= std::ceil (sz * 0.15);

      if (m_mode == MODE_BBOX)
        update_line_bbox ();
    }

  text_processor::visit (e);

  m_font = saved_font;
  // If line_yoffset changed, this means we moved to a new line; hence yoffset
  // cannot be restored, because the saved value is not relevant anymore.
  if (m_line_yoffset == saved_line_yoffset)
    m_yoffset = saved_yoffset;
}

void
ft_text_renderer::visit (text_element_superscript& e)
{
  ft_font saved_font (m_font);
  int saved_line_yoffset = m_line_yoffset;
  int saved_yoffset = m_yoffset;

  double sz = m_font.get_size ();

  // Reducing font size by 70% produces decent results.
  set_font (m_font.get_name (), m_font.get_weight (), m_font.get_angle (),
            std::max (5.0, sz * 0.7));

  if (saved_font.is_valid ())
    {
      // Shifting the baseline by 40% of the font size gives decent results.
      m_yoffset += std::ceil (sz * 0.4);

      if (m_mode == MODE_BBOX)
        update_line_bbox ();
    }

  text_processor::visit (e);

  m_font = saved_font;
  // If line_yoffset changed, this means we moved to a new line; hence yoffset
  // cannot be restored, because the saved value is not relevant anymore.
  if (m_line_yoffset == saved_line_yoffset)
    m_yoffset = saved_yoffset;
}

void
ft_text_renderer::visit (text_element_color& e)
{
  if (m_mode == MODE_RENDER)
    set_color (e.get_color ());
}

void
ft_text_renderer::visit (text_element_fontsize& e)
{
  double sz = e.get_fontsize ();

  // FIXME: Matlab documentation says that the font size is expressed
  //        in the text object FontUnit.

  set_font (m_font.get_name (), m_font.get_weight (),
            m_font.get_angle (), sz);

  if (m_mode == MODE_BBOX)
    update_line_bbox ();
}

void
ft_text_renderer::visit (text_element_fontname& e)
{
  set_font (e.get_fontname (), m_font.get_weight (), m_font.get_angle (),
            m_font.get_size ());

  if (m_mode == MODE_BBOX)
    update_line_bbox ();
}

void
ft_text_renderer::visit (text_element_fontstyle& e)
{
  switch (e.get_fontstyle ())
    {
    case text_element_fontstyle::normal:
      set_font (m_font.get_name (), "normal", "normal", m_font.get_size ());
      break;

    case text_element_fontstyle::bold:
      set_font (m_font.get_name (), "bold", "normal", m_font.get_size ());
      break;

    case text_element_fontstyle::italic:
      set_font (m_font.get_name (), "normal", "italic", m_font.get_size ());
      break;

    case text_element_fontstyle::oblique:
      set_font (m_font.get_name (), "normal", "oblique", m_font.get_size ());
      break;
    }

  if (m_mode == MODE_BBOX)
    update_line_bbox ();
}

void
ft_text_renderer::visit (text_element_symbol& e)
{
  uint32_t code = e.get_symbol_code ();

  std::vector<double> xdata (1, m_xoffset);
  text_renderer::string fs ("-", m_font, m_xoffset, m_yoffset);

  if (code != text_element_symbol::invalid_code && m_font.is_valid ())
    {
      std::string sub_name;

      process_character (code, 0, sub_name);

      if (m_do_strlist && m_mode == MODE_RENDER)
        {
          if (! sub_name.empty ())
            {
              // Add substitution font to the family name
              std::string tmp_family = fs.get_family ();

              if (tmp_family.find (sub_name) == std::string::npos)
                {
                  if (sub_name.find (" ") != std::string::npos)
                    sub_name = "'" + sub_name + "'";

                  fs.set_family (tmp_family + ", " + sub_name);
                }
            }

          fs.set_code (code);
          fs.set_xdata (xdata);
        }
    }
  else if (m_font.is_valid ())
    ::warning ("ignoring unknown symbol: %d", e.get_symbol ());

  if (m_do_strlist && m_mode == MODE_RENDER && fs.get_code ())
    {
      fs.set_y (m_line_yoffset + m_yoffset);
      fs.set_color (m_color);
      fs.set_family (m_font.get_face ()->family_name);
      m_strlist.push_back (fs);
    }
}

void
ft_text_renderer::visit (text_element_combined& e)
{
  int saved_xoffset = m_xoffset;
  int max_xoffset = m_xoffset;

  for (auto *txt_elt : e)
    {
      m_xoffset = saved_xoffset;
      txt_elt->accept (*this);
      max_xoffset = math::max (m_xoffset, max_xoffset);
    }

  m_xoffset = max_xoffset;
}

void
ft_text_renderer::reset (void)
{
  set_mode (MODE_BBOX);
  set_color (Matrix (1, 3, 0.0));
  m_strlist = std::list<text_renderer::string> ();
}

void
ft_text_renderer::set_color (const Matrix& c)
{
  if (c.numel () == 3)
    {
      m_color(0) = static_cast<uint8_t> (c(0)*255);
      m_color(1) = static_cast<uint8_t> (c(1)*255);
      m_color(2) = static_cast<uint8_t> (c(2)*255);
    }
  else
    ::warning ("ft_text_renderer::set_color: invalid color");
}

uint8NDArray
ft_text_renderer::render (text_element *elt, Matrix& box, int rotation)
{
  set_mode (MODE_BBOX);
  elt->accept (*this);
  compute_bbox ();
  box = m_bbox;

  set_mode (MODE_RENDER);

  if (m_pixels.numel () > 0)
    {
      elt->accept (*this);

      rotate_pixels (m_pixels, rotation);
    }

  return m_pixels;
}

// Note:
// x-extent accurately measures width of glyphs.
// y-extent is overly large because it is measured from baseline-to-baseline.
// Calling routines, such as ylabel, may need to account for this mismatch.

Matrix
ft_text_renderer::get_extent (text_element *elt, double rotation)
{
  set_mode (MODE_BBOX);
  elt->accept (*this);
  compute_bbox ();

  Matrix extent (1, 2, 0.0);

  switch (rotation_to_mode (rotation))
    {
    case ROTATION_0:
    case ROTATION_180:
      extent(0) = m_bbox(2);
      extent(1) = m_bbox(3);
      break;

    case ROTATION_90:
    case ROTATION_270:
      extent(0) = m_bbox(3);
      extent(1) = m_bbox(2);
    }

  return extent;
}

Matrix
ft_text_renderer::get_extent (const std::string& txt, double rotation,
                              const caseless_str& interpreter)
{
  text_element *elt = text_parser::parse (txt, interpreter);
  Matrix extent = get_extent (elt, rotation);
  delete elt;

  return extent;
}

void
ft_text_renderer::text_to_pixels (const std::string& txt,
                                  uint8NDArray& pxls, Matrix& box,
                                  int _halign, int valign, double rotation,
                                  const caseless_str& interpreter,
                                  bool handle_rotation)
{
  int rot_mode = rotation_to_mode (rotation);

  m_halign = _halign;

  text_element *elt = text_parser::parse (txt, interpreter);
  pxls = render (elt, box, rot_mode);
  delete elt;

  if (pxls.isempty ())
    return;  // nothing to render

  // Move X0 and Y0 depending on alignments and eventually swap all values
  // for text rotated 90° 180° or 270°
  fix_bbox_anchor (box, m_halign, valign, rot_mode, handle_rotation);
}

ft_text_renderer::ft_font::ft_font (const ft_font& ft)
  : text_renderer::font (ft), m_face (nullptr)
{
#if defined (HAVE_FT_REFERENCE_FACE)
  FT_Face ft_face = ft.get_face ();

  if (ft_face && FT_Reference_Face (ft_face) == 0)
    m_face = ft_face;
#endif
}

ft_text_renderer::ft_font&
ft_text_renderer::ft_font::operator = (const ft_font& ft)
{
  if (&ft != this)
    {
      text_renderer::font::operator = (ft);

      if (m_face)
        {
          FT_Done_Face (m_face);
          m_face = nullptr;
        }

#if defined (HAVE_FT_REFERENCE_FACE)
      FT_Face ft_face = ft.get_face ();

      if (ft_face && FT_Reference_Face (ft_face) == 0)
        m_face = ft_face;
#endif
    }

  return *this;
}

FT_Face
ft_text_renderer::ft_font::get_face (void) const
{
  if (! m_face && ! m_name.empty ())
    {
      m_face = ft_manager::get_font (m_name, m_weight, m_angle, m_size);

      if (m_face)
        {
          if (FT_Set_Char_Size (m_face, 0, m_size*64, 0, 0))
            ::warning ("ft_text_renderer: unable to set font size to %g", m_size);
        }
      else
        ::warning ("ft_text_renderer: unable to load appropriate font");
    }

  return m_face;
}

OCTAVE_END_NAMESPACE(octave)

#endif

OCTAVE_BEGIN_NAMESPACE(octave)

base_text_renderer *
make_ft_text_renderer (void)
{
#if defined (HAVE_FREETYPE)
  return new ft_text_renderer ();
#else
  return 0;
#endif
}

OCTAVE_END_NAMESPACE(octave)
