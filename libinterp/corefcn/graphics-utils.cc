////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#include "caseless-str.h"

#include "gh-manager.h"
#include "graphics-utils.h"
#include "graphics.h"
#include "input.h"
#include "interpreter-private.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Flag to stop redraws due to callbacks while deletion is in progress.
bool delete_executing = false;

void
xset (const graphics_handle& h, const caseless_str& pname,
      const octave_value& val)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  go.set (pname, val);
}

void
xset (const graphics_handle& h, const octave_value_list& args)
{
  if (args.length () > 0)
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (h);

      go.set (args);
    }
}

octave_value
xget (const graphics_handle& h, const caseless_str& pname)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  return go.get (pname);
}

bool isfigure (double val)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (val);

  return go && go.isa ("figure");
}

graphics_handle
reparent (const octave_value& ov, const std::string& who,
          const std::string& pname, const graphics_handle& new_parent,
          bool adopt)
{
  double hv = ov.xdouble_value ("%s: %s must be a graphics handle",
                                who.c_str (), pname.c_str ());

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_handle h = gh_mgr.lookup (hv);

  if (! h.ok ())
    error ("%s: invalid graphics handle (= %g) for %s",
           who.c_str (), hv, pname.c_str ());

  graphics_object go = gh_mgr.get_object (h);

  graphics_handle parent_h = go.get_parent ();

  graphics_object parent_go = gh_mgr.get_object (parent_h);

  parent_go.remove_child (h);

  if (adopt)
    go.set ("parent", new_parent.value ());
  else
    go.reparent (new_parent);

  return h;
}

void
delete_graphics_object (const graphics_handle& h, bool from_root)
{
  if (h.ok ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (h);

      // Don't do recursive deleting, due to callbacks
      if (! go.get_properties ().is_beingdeleted ())
        {
          // NOTE: Freeing the handle also calls any deletefcn.  It also calls
          //       the parent's delete_child function.

          gh_mgr.free (h, from_root || go.isa ("figure"));

          Vdrawnow_requested = true;
        }
    }
}

void
delete_graphics_object (double val, bool from_root)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  delete_graphics_object (gh_mgr.lookup (val), from_root || isfigure (val));
}

void
delete_graphics_objects (const NDArray vals, bool from_root)
{
  // Prevent redraw of partially deleted objects.
  octave::unwind_protect_var<bool> restore_var (delete_executing, true);

  for (octave_idx_type i = 0; i < vals.numel (); i++)
    delete_graphics_object (vals.elem (i), from_root);
}

void
close_figure (const graphics_handle& h)
{
  octave_value closerequestfcn = xget (h, "closerequestfcn");

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  gh_mgr.execute_callback (h, closerequestfcn);
}

void
force_close_figure (const graphics_handle& h)
{
  // Remove the deletefcn and closerequestfcn callbacks
  // and delete the object directly.

  xset (h, "deletefcn", Matrix ());
  xset (h, "closerequestfcn", Matrix ());

  delete_graphics_object (h, true);
}

OCTAVE_END_NAMESPACE(octave)
