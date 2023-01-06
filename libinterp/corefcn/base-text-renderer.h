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

#if ! defined (octave_base_text_renderer_h)
#define octave_base_text_renderer_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "dMatrix.h"
#include "oct-map.h"
#include "uint8NDArray.h"

#include "text-engine.h"
#include "text-renderer.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
base_text_renderer : public text_processor
{
public:

  enum
  {
    ROTATION_0   = 0,
    ROTATION_90  = 1,
    ROTATION_180 = 2,
    ROTATION_270 = 3
  };

  base_text_renderer (void) : text_processor () { }

  // No copying!

  base_text_renderer (const base_text_renderer&) = delete;

  base_text_renderer& operator = (const base_text_renderer&) = delete;

  virtual ~base_text_renderer (void) = default;

  virtual void
  set_anti_aliasing (bool val) = 0;

  virtual Matrix
  get_extent (text_element *elt, double rotation) = 0;

  virtual Matrix
  get_extent (const std::string& txt, double rotation,
              const caseless_str& interpreter) = 0;

  virtual void
  set_font (const std::string& name, const std::string& weight,
            const std::string& angle, double size) = 0;

  virtual bool
  ok (void) { return true; };

  virtual octave_map
  get_system_fonts (void) = 0;

  virtual void set_color (const Matrix& c) = 0;

  virtual void
  text_to_pixels (const std::string& txt, uint8NDArray& pxls,
                  Matrix& bbox, int halign, int valign, double rotation,
                  const caseless_str& interpreter,
                  bool handle_rotation) = 0;

  virtual void
  text_to_strlist (const std::string& txt,
                   std::list<text_renderer::string>& lst,
                   Matrix& box, int halign, int valign, double rotation,
                   const caseless_str& interpreter = "tex") = 0;

  void rotate_pixels (uint8NDArray& pixels, int rot_mode) const;

  int rotation_to_mode (double rotation) const;

  void fix_bbox_anchor (Matrix& bbox, int halign,
                        int valign, int rot_mode,
                        bool handle_rotation) const;

};

OCTAVE_END_NAMESPACE(octave)

#endif
