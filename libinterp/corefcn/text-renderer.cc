/*

Copyright (C) 2016-2017 John W. Eaton
Copyright (C) 2009-2016 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "base-text-renderer.h"
#include "errwarn.h"
#include "ft-text-renderer.h"
#include "text-renderer.h"

static octave::base_text_renderer *
make_text_renderer (void)
{
  // Allow the possibility of choosing different text rendering
  // implementations.

  return octave::make_ft_text_renderer ();
}

namespace octave
{
  text_renderer::text_renderer (void)
    : rep (make_text_renderer ())
  { }

  text_renderer::~text_renderer (void)
  {
    delete rep;
  }

  bool
  text_renderer::ok (void) const
  {
    static bool warned = false;

    if (! rep)
      {
        if (! warned)
          {
            warn_disabled_feature ("opengl_renderer::render_text",
                                   "rendering text (FreeType)");

            warned = true;
          }
      }

    return rep != 0;
  }

  Matrix
  text_renderer::get_extent (text_element *elt, double rotation)
  {
    static Matrix empty_extent (1, 4, 0.0);

    return ok () ? rep->get_extent (elt, rotation) : empty_extent;
  }

  Matrix
  text_renderer::get_extent (const std::string& txt, double rotation,
                             const caseless_str& interpreter)
  {
    static Matrix empty_extent (1, 4, 0.0);

    return ok () ? rep->get_extent (txt, rotation, interpreter) : empty_extent;
  }

  void
  text_renderer::set_font (const std::string& name, const std::string& weight,
                           const std::string& angle, double size)
  {
    if (ok ())
      rep->set_font (name, weight, angle, size);
  }

  void
  text_renderer::set_color (const Matrix& c)
  {
    if (ok ())
      rep->set_color (c);
  }

  void
  text_renderer::text_to_pixels (const std::string& txt,
                                 uint8NDArray& pxls, Matrix& bbox,
                                 int halign, int valign, double rotation,
                                 const caseless_str& interpreter,
                                 bool handle_rotation)
  {
    static Matrix empty_bbox (1, 4, 0.0);
    static uint8NDArray empty_pxls;

    if (ok ())
      rep->text_to_pixels (txt, pxls, bbox, halign, valign, rotation,
                           interpreter, handle_rotation);
    else
      {
        bbox = empty_bbox;
        pxls = empty_pxls;
      }
  }

  void
  text_renderer::text_to_strlist (const std::string& txt,
                                  std::list<text_renderer::string>& lst,
                                  Matrix& bbox, int halign, int valign,
                                  double rotation,
                                  const caseless_str& interpreter)
  {
    static Matrix empty_bbox (1, 4, 0.0);
    static std::list<text_renderer::string> empty_lst;

    if (ok ())
      rep->text_to_strlist (txt, lst, bbox, halign, valign, rotation,
                            interpreter);
    else
      {
        bbox = empty_bbox;
        lst = empty_lst;
      }
  }
}
