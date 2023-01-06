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

OCTAVE_BEGIN_NAMESPACE(octave)

void
base_text_renderer::rotate_pixels (uint8NDArray& pixels, int rot_mode) const
{
  switch (rot_mode)
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
        idx(2) = idx_vector (pixels.dim3 ()-1, -1, -1);
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

int
base_text_renderer::rotation_to_mode (double rotation) const
{
  // Wrap rotation to range [0, 360]
  while (rotation < 0)
    rotation += 360.0;
  while (rotation > 360.0)
    rotation -= 360.0;

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
base_text_renderer::fix_bbox_anchor (Matrix& bbox, int halign,
                                     int valign, int rot_mode,
                                     bool handle_rotation) const
{
  switch (halign)
    {
    case 1:
      bbox(0) = -bbox(2)/2;
      break;

    case 2:
      bbox(0) = -bbox(2);
      break;

    default:
      bbox(0) = 0;
      break;
    }

  switch (valign)
    {
    case 1:
      bbox(1) = -bbox(3)/2;
      break;

    case 2:
      bbox(1) = -bbox(3);
      break;

    case 3:
      break;

    case 4:
      bbox(1) = -bbox(3)-bbox(1);
      break;

    default:
      bbox(1) = 0;
      break;
    }

  if (handle_rotation)
    {
      switch (rot_mode)
        {
        case ROTATION_90:
          std::swap (bbox(0), bbox(1));
          std::swap (bbox(2), bbox(3));
          bbox(0) = -bbox(0)-bbox(2);
          break;

        case ROTATION_180:
          bbox(0) = -bbox(0)-bbox(2);
          bbox(1) = -bbox(1)-bbox(3);
          break;

        case ROTATION_270:
          std::swap (bbox(0), bbox(1));
          std::swap (bbox(2), bbox(3));
          bbox(1) = -bbox(1)-bbox(3);
          break;
        }
    }
}

OCTAVE_END_NAMESPACE(octave)
