/*

Copyright (C) 2007-2012 John W. Eaton

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

/*

To initialize:

  graphics_toolkit ("gnuplot");
  plot (randn (1e3, 1));

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "graphics.h"
#include "parse.h"
#include "variables.h"

// PKG_ADD: register_graphics_toolkit ("gnuplot");

static bool toolkit_loaded = false;

class gnuplot_graphics_toolkit : public base_graphics_toolkit
{
public:
  gnuplot_graphics_toolkit (void)
      : base_graphics_toolkit ("gnuplot") { }

  ~gnuplot_graphics_toolkit (void) { }

  bool is_valid (void) const { return true; }

  bool initialize (const graphics_object& go)
    {
      return go.isa ("figure");
    }

  void finalize (const graphics_object& go)
    {
      if (go.isa ("figure"))
        {
          const figure::properties& props =
              dynamic_cast<const figure::properties&> (go.get_properties ());

          send_quit (props.get___plot_stream__ ());
        }
    }

  void update (const graphics_object& go, int id)
    {
      if (go.isa ("figure"))
        {
          graphics_object obj (go);

          figure::properties& props =
              dynamic_cast<figure::properties&> (obj.get_properties ());

          switch (id)
            {
            case base_properties::ID_VISIBLE:
              if (! props.is_visible ())
                {
                  send_quit (props.get___plot_stream__ ());
                  props.set___plot_stream__ (Matrix ());
                  props.set___enhanced__ (false);
                }
              break;
            }
        }
    }

  void redraw_figure (const graphics_object& go) const
    {
      octave_value_list args;
      args(0) = go.get_handle ().as_octave_value ();
      feval ("__gnuplot_drawnow__", args);
    }

  void print_figure (const graphics_object& go, const std::string& term,
                     const std::string& file, bool mono,
                     const std::string& debug_file) const
    {
      octave_value_list args;
      if (! debug_file.empty ())
        args(4) = debug_file;
      args(3) = mono;
      args(2) = file;
      args(1) = term;
      args(0) = go.get_handle ().as_octave_value ();
      feval ("__gnuplot_drawnow__", args);
    }

  Matrix get_canvas_size (const graphics_handle&) const
    {
      Matrix sz (1, 2, 0.0);
      return sz;
    }

  double get_screen_resolution (void) const
    { return 72.0; }

  Matrix get_screen_size (void) const
    { return Matrix (1, 2, 0.0); }

  void close (void)
  {
    if (toolkit_loaded)
      {
        munlock ("__init_gnuplot__");

        gtk_manager::unload_toolkit ("gnuplot");

        toolkit_loaded = false;
      }
  }

private:

  void send_quit (const octave_value& pstream) const
    {
      if (! pstream.is_empty ())
        {
          octave_value_list args;
          Matrix fids = pstream.matrix_value ();

          if (! error_state)
            {
              args(1) = "\nquit;\n";
              args(0) = fids(0);
              feval ("fputs", args);

              args.resize (1);
              feval ("fflush", args);
              feval ("pclose", args);

              if (fids.numel () > 1)
                {
                  args(0) = fids(1);
                  feval ("pclose", args);

                  if (fids.numel () > 2)
                    {
                      args(0) = fids(2);
                      feval ("waitpid", args);
                    }
                }
            }
        }
    }
};

// Initialize the fltk graphics toolkit.

DEFUN_DLD (__init_gnuplot__, , , "")
{
  octave_value retval;

  if (! toolkit_loaded)
    {
      mlock ();

      graphics_toolkit tk (new gnuplot_graphics_toolkit ());
      gtk_manager::load_toolkit (tk);

      toolkit_loaded = true;
    }

  return retval;
}

