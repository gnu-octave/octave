////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2023 The Octave Project Developers
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <fstream>

#include "base-text-renderer.h"
#include "builtin-defun-decls.h"
#include "dim-vector.h"
#include "error.h"
#include "graphics.h"
#include "file-ops.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "oct-env.h"
#include "oct-process.h"

OCTAVE_BEGIN_NAMESPACE(octave)

std::string
quote_string (std::string str)
{
  return ('"' + str + '"');
}

class
OCTINTERP_API
latex_renderer : public base_text_renderer
{

public:

  latex_renderer (void)
    : m_fontsize (10.0), m_fontname ("cmr"), m_tmp_dir (),
      m_color (dim_vector (1, 3), 0), m_latex_binary ("latex"),
      m_dvipng_binary ("dvipng"), m_dvisvg_binary ("dvisvgm"),
      m_debug (false), m_testing (true)
  {
    std::string bin = sys::env::getenv ("OCTAVE_LATEX_BINARY");
    if (! bin.empty ())
      m_latex_binary = quote_string (bin);

    bin = sys::env::getenv ("OCTAVE_DVIPNG_BINARY");
    if (! bin.empty ())
      m_dvipng_binary = quote_string (bin);

    bin = sys::env::getenv ("OCTAVE_DVISVG_BINARY");
    if (! bin.empty ())
      m_dvisvg_binary = quote_string (bin);

    m_debug = ! sys::env::getenv ("OCTAVE_LATEX_DEBUG_FLAG").empty ();
  }

  ~latex_renderer (void)
  {
    if (! m_tmp_dir.empty () && ! m_debug)
      sys::recursive_rmdir (m_tmp_dir);
  }

  void set_font (const std::string& /*name*/, const std::string& /*weight*/,
                 const std::string& /*angle*/, double size)
  {
    m_fontsize = size;
  }

  void set_color (const Matrix& c)
  {
    if (c.numel () == 3)
      {
        m_color(0) = static_cast<uint8_t> (c (0) * 255);
        m_color(1) = static_cast<uint8_t> (c (1) * 255);
        m_color(2) = static_cast<uint8_t> (c (2) * 255);
      }
  }

  Matrix get_extent (text_element * /*elt*/, double /*rotation*/)
  {
    return Matrix (1, 2, 0.0);
  }

  Matrix get_extent (const std::string& txt, double rotation,
                     const caseless_str& interpreter)
  {
    Matrix bbox;
    uint8NDArray pixels;

    text_to_pixels (txt, pixels, bbox, 0, 0, rotation, interpreter, false);

    return bbox.extract_n (0, 2, 1, 2);
  }

  void text_to_strlist (const std::string& txt,
                        std::list<text_renderer::string>& lst,
                        Matrix& bbox, int halign, int valign, double rotation,
                        const caseless_str& interp)
  {
    uint8NDArray pixels;
    text_to_pixels (txt, pixels, bbox, halign, valign, rotation,
                    interp, false);

    text_renderer::font fnt;
    text_renderer::string str ("", fnt, 0.0, 0.0);
    str.set_color (m_color);

    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    gh_manager::latex_data ldata = gh_mgr.get_latex_data (key (txt, halign));

    str.set_svg_element (ldata.second);

    lst.push_back (str);
  }

  void text_to_pixels (const std::string& txt, uint8NDArray& pxls,
                       Matrix& bbox, int halign, int valign, double rotation,
                       const caseless_str& interpreter,
                       bool handle_rotation);

  void set_anti_aliasing (bool /*val*/) { }

  octave_map get_system_fonts (void) { return octave_map (); }

  bool ok (void);

private:

  std::string key (const std::string& txt, int halign)
  {
    return (txt + ":"
            + std::to_string (m_fontsize) + ":"
            + std::to_string (halign) + ":"
            + std::to_string (m_color(0)) + ":"
            + std::to_string (m_color(1)) + ":"
            + std::to_string (m_color(2)));
  }

  void warn_helper (std::string caller, std::string txt, std::string cmd,
                    process_execution_result result);

  uint8NDArray render (const std::string& txt, int halign = 0);

  bool read_image (const std::string& png_file, uint8NDArray& data) const;

  std::string write_tex_file (const std::string& txt, int halign);

private:
  double m_fontsize;
  std::string m_fontname;
  std::string m_tmp_dir;
  uint8NDArray m_color;
  std::string m_latex_binary;
  std::string m_dvipng_binary;
  std::string m_dvisvg_binary;
  bool m_debug;
  bool m_testing;

};

bool
latex_renderer::ok (void)
{
  // Only run the test once in a session
  static bool tested = false;

  static bool isok = false;

  if (! tested)
    {
      tested = true;

      // For testing, render a questoin mark
      uint8NDArray pixels = render ("?");

      if (! pixels.isempty ())
        isok = true;
      else
        warning_with_id ("Octave:LaTeX:internal-error",
                         "latex_renderer: a run-time test failed and the 'latex' interpreter has been disabled.");
    }

  m_testing = false;

  return isok;
}

std::string
latex_renderer::write_tex_file (const std::string& txt, int halign)
{
  if (m_tmp_dir.empty ())
    {
      //Create the temporary directory
#if defined (OCTAVE_USE_WINDOWS_API)
      static std::string base_tmp_dir;

      if (base_tmp_dir.empty ())
        {
          base_tmp_dir = sys::env::get_temp_directory ();

          // Make sure we don't get short 8.3 path on Windows since some
          // versions of latex on that platform don't support them
          // (see bug #62779)
          if (base_tmp_dir.find ('~') != std::string::npos)
            base_tmp_dir = sys::canonicalize_file_name (base_tmp_dir);
        }

      m_tmp_dir = sys::tempnam (base_tmp_dir, "latex");
#else
      m_tmp_dir = sys::tempnam ("", "latex");
#endif

      if (sys::mkdir (m_tmp_dir, 0700) != 0)
        {
          warning_with_id ("Octave:LaTeX:internal-error",
                           "latex_renderer: unable to create temp directory");
          return std::string ();
        }
    }

  std::string base_file_name
    = sys::file_ops::concat (m_tmp_dir, "default");

  // Duplicate \n characters and align multi-line strings based on
  // horizontalalignment
  std::string latex_txt (txt);
  std::size_t pos = 0;

  while (true)
    {
      pos =  txt.find_first_of ("\n", pos);

      if (pos == std::string::npos)
        break;

      latex_txt.replace (pos, 1, "\n\n");

      pos += 1;
    }

  std::string env ("flushleft");
  if (halign == 1)
    env = "center";
  else if (halign == 2)
    env = "flushright";

  latex_txt = std::string ("\\begin{" ) + env + "}\n"
              + latex_txt + "\n"
              + "\\end{" + env + "}\n";

  // Write to temporary .tex file
  std::ofstream file;
  file.open (base_file_name + ".tex");
  file << "\\documentclass[10pt, varwidth]{standalone}\n"
       << "\\usepackage{amsmath}\n"
       << "\\usepackage[utf8]{inputenc}\n"
       << "\\begin{document}\n"
       << latex_txt << "\n"
       << "\\end{document}";
  file.close ();

  return base_file_name;
}

bool
latex_renderer::read_image (const std::string& png_file,
                            uint8NDArray& data) const
{
  uint8NDArray alpha;
  uint8NDArray rgb;
  int height;
  int width;

  try
    {
      // First get the image size to build the argument to __magick_read__
      octave_value_list retval = F__magick_ping__ (ovl (png_file), 1);

      octave_scalar_map info
        = retval(0).xscalar_map_value ("latex_renderer::read_image: "
                                       "Wrong type for info");
      height = info.getfield ("rows").int_value ();
      width = info.getfield ("columns").int_value ();
      Cell region (dim_vector(1, 2));
      region(0) = range<double> (1.0, height);
      region(1) = range<double> (1.0, width);
      info.setfield ("region", region);
      info.setfield ("index", octave_value (1));

      // Retrieve the alpha map
      retval = F__magick_read__ (ovl (png_file, info), 3);

      alpha = retval(2).xuint8_array_value ("latex_renderer::read_image: "
                                            "Wrong type for alpha");
    }
  catch (const execution_exception& ee)
    {
      warning_with_id ("Octave:LaTeX:internal-error",
                       "latex_renderer:: failed to read png data. %s",
                       ee.message ().c_str ());

      interpreter& interp = __get_interpreter__ ();

      interp.recover_from_exception ();

      return false;
    }

  data = uint8NDArray (dim_vector (4, width, height),
                       static_cast<uint8_t> (0));

  for (int i = 0; i < height; i++)
    {
      for (int j = 0; j < width; j++)
        {
          data(0, j, i) = m_color(0);
          data(1, j, i) = m_color(1);
          data(2, j, i) = m_color(2);
          data(3, j, i) = alpha(height-i-1, j);
        }
    }

  return true;
}

void
latex_renderer::warn_helper (std::string caller, std::string txt,
                             std::string cmd, process_execution_result result)
{
  if (m_testing && ! m_debug)
    return;

  if (! m_debug)
    warning_with_id ("Octave:LaTeX:internal-error",
                     "latex_renderer: unable to compile \"%s\"",
                     txt.c_str ());
  else
    warning_with_id ("Octave:LaTeX:internal-error",
                     "latex_renderer: %s failed for string \"%s\"\n\
* Command:\n\t%s\n\n* Error:\n%s\n\n* Stdout:\n%s",
                     caller.c_str (), txt.c_str (), cmd.c_str (),
                     result.err_msg ().c_str (),
                     result.stdout_output ().c_str ());
}

uint8NDArray
latex_renderer::render (const std::string& txt, int halign)
{
  // Render if it was not already done
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  gh_manager::latex_data ldata = gh_mgr.get_latex_data (key (txt, halign));

  if (! ldata.first.isempty ())
    return ldata.first;

  uint8NDArray data;

  // First write the base .tex file
  std::string base_file_name = write_tex_file (txt, halign);

  if (base_file_name.empty ())
    return data;

  // Generate DVI file
  std::string tex_file = quote_string (base_file_name + ".tex");
  std::string dvi_file = quote_string (base_file_name + ".dvi");
  std::string log_file = quote_string (base_file_name + ".log");

  process_execution_result result;
  std::string cmd = (m_latex_binary + " -interaction=nonstopmode "
                     + "-output-directory=" + quote_string (m_tmp_dir) + " "
                     + tex_file);

#if defined (OCTAVE_USE_WINDOWS_API)
  cmd = quote_string (cmd);
#endif

  result = run_command_and_return_output (cmd);

  if (result.exit_status () != 0)
    {
      warn_helper ("latex", txt, cmd, result);

      if (txt != "?")
        {
          write_tex_file ("?", halign);

          result = run_command_and_return_output (cmd);
          if (result.exit_status () != 0)
            return data;
        }
      else
        return data;
    }

  double size_factor = m_fontsize / 10.0;


  // Convert DVI to SVG, read file and store its content for later use in
  // gl2ps_print
  std::string svg_file = base_file_name + ".svg";

  cmd = (m_dvisvg_binary + " -n "
         + "-TS" + std::to_string (size_factor) + " "
         + "-v1 -o " + quote_string (svg_file) + " "
         + dvi_file);

#if defined (OCTAVE_USE_WINDOWS_API)
  cmd = quote_string (cmd);
#endif

  result = run_command_and_return_output (cmd);

  if (result.exit_status () != 0)
    {
      warn_helper ("dvisvg", txt, cmd, result);
      return data;
    }

  std::ifstream svg_stream (svg_file);
  std::string svg_string;
  svg_string.assign (std::istreambuf_iterator<char> (svg_stream),
                     std::istreambuf_iterator<char> ());

  // Convert DVI to PNG, read file and format pixel data for later use in
  // OpenGL
  std::string png_file = base_file_name + ".png";

  cmd = (m_dvipng_binary + " " + dvi_file + " "
         + "-q -o " + quote_string (png_file) + " "
         + "-bg Transparent -D "
         + std::to_string (std::floor (72.0 * size_factor)));

#if defined (OCTAVE_USE_WINDOWS_API)
  cmd = quote_string (cmd);
#endif

  result = run_command_and_return_output (cmd);

  if (result.exit_status () != 0)
    {
      warn_helper ("dvipng", txt, cmd, result);
      return data;
    }

  if (! read_image (png_file, data))
    return data;

  // Cache pixel and svg data for this string
  ldata.first = data;
  ldata.second = svg_string;

  gh_mgr.set_latex_data (key (txt, halign), ldata);

  if (m_debug)
    std::cout << "* Caching " << key (txt, halign) << std::endl;

  return data;
}

void
latex_renderer::text_to_pixels (const std::string& txt, uint8NDArray& pixels,
                                Matrix& bbox, int halign, int valign,
                                double rotation,
                                const caseless_str& /*interpreter*/,
                                bool handle_rotation)
{
  // Return early for empty strings
  if (txt.empty ())
    {
      bbox = Matrix (1, 4, 0.0);
      return;
    }

  if (ok ())
    pixels = render (txt, halign);
  else
    pixels = uint8NDArray (dim_vector (4, 1, 1), static_cast<uint8_t> (0));

  if (pixels.ndims () < 3 || pixels.isempty ())
    return;  // nothing to render

  // Store unrotated bbox size
  bbox = Matrix (1, 4, 0.0);
  bbox (2) = pixels.dim2 ();
  bbox (3) = pixels.dim3 ();

  // Now rotate pixels if necessary
  int rot_mode = rotation_to_mode (rotation);

  if (! pixels.isempty ())
    rotate_pixels (pixels, rot_mode);

  // Move X0 and Y0 depending on alignments and eventually swap values
  // for text rotated 90° 180° or 270°
  fix_bbox_anchor (bbox, halign, valign, rot_mode, handle_rotation);
}

base_text_renderer *
make_latex_text_renderer (void)
{
  latex_renderer *renderer = new latex_renderer ();

  return renderer;
}

OCTAVE_END_NAMESPACE(octave)
