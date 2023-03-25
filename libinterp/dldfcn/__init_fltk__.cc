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

/*

To initialize:

  graphics_toolkit ("fltk");
  plot (randn (1e3, 1));

*/

// PKG_ADD: if (__have_feature__ ("FLTK") && __have_feature__ ("OPENGL") && have_window_system () && ! (ismac () && __event_manager_enabled__ ())) register_graphics_toolkit ("fltk"); endif

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"

#if defined (HAVE_X_WINDOWS)
#  include <X11/Xlib.h>
#endif

#include <cmath>

#include <locale>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#if defined (WIN32)
#  define WIN32_LEAN_AND_MEAN
#endif

#if defined (HAVE_FLTK)
#  include <FL/Fl.H>
#  include <FL/Fl_Box.H>
#  include <FL/Fl_Button.H>
#  include <FL/Fl_Choice.H>
#  include <FL/Fl_File_Chooser.H>
#  include <FL/Fl_Gl_Window.H>
#  include <FL/names.h>
#  include <FL/Fl_Menu_Bar.H>
#  include <FL/Fl_Menu_Button.H>
#  include <FL/Fl_Output.H>
#  include <FL/Fl_Window.H>
#  include <FL/fl_ask.H>
#  include <FL/fl_draw.H>
#  include <FL/gl.h>
#endif

// FLTK headers may include X11/X.h which defines Complex, and that
// conflicts with Octave's Complex typedef.  We don't need the X11
// Complex definition in this file, so remove it before including Octave
// headers which may require Octave's Complex typedef.
#undef Complex

#include "Array.h"
#include "cmd-edit.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "lo-ieee.h"
#include "oct-env.h"

#include "Cell.h"
#include "builtin-defun-decls.h"
#include "display.h"
#include "gl-render.h"
#include "gl2ps-print.h"
#include "graphics.h"
#include "gtk-manager.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "oct-map.h"
#include "oct-opengl.h"
#include "ov-fcn-handle.h"
#include "ov.h"
#include "ovl.h"
#include "parse.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_FLTK)

#define FLTK_GRAPHICS_TOOLKIT_NAME "fltk"

const char *help_text = "\
Keyboard Shortcuts\n\
a - autoscale\n\
p - pan/zoom\n\
r - rotate\n\
g - toggle grid\n\
\n\
Mouse\n\
left drag - pan\n\
mouse wheel - zoom\n\
right drag - rectangle zoom\n\
left double click - autoscale\n\
";

class OpenGL_fltk : public Fl_Gl_Window
{
public:

  OpenGL_fltk (int xx, int yy, int ww, int hh, double num)
    : Fl_Gl_Window (xx, yy, ww, hh, nullptr), m_number (num),
      m_glfcns (), m_renderer (m_glfcns), m_in_zoom (false), m_zoom_box ()
  {
#if defined (HAVE_OPENGL)

    // Ask for double buffering and a depth buffer.
    mode (FL_DEPTH | FL_DOUBLE | FL_MULTISAMPLE);

#else

    err_disabled_feature ("OpenGL_fltk", "OpenGL");

#endif
  }

  ~OpenGL_fltk (void) = default;

  void zoom (bool z)
  {
    m_in_zoom = z;
    if (! m_in_zoom)
      hide_overlay ();
  }

  bool zoom (void) { return m_in_zoom; }
  void set_zoom_box (const Matrix& zb) { m_zoom_box = zb; }

  void print (const std::string& cmd, const std::string& term)
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    octave::gl2ps_print (m_glfcns, gh_mgr.get_object (m_number), cmd, term);
  }

  uint8NDArray get_pixels (void)
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    m_renderer.draw (gh_mgr.get_object (m_number));

    return m_renderer.get_pixels (w (), h ());
  }

  void resize (int xx, int yy, int ww, int hh)
  {
#if defined (HAVE_OPENGL)

    Fl_Gl_Window::resize (xx, yy, ww, hh);

#else

    octave_unused_parameter (xx);
    octave_unused_parameter (yy);
    octave_unused_parameter (ww);
    octave_unused_parameter (hh);

    // This shouldn't happen because construction of Opengl_fltk
    // objects is supposed to be impossible if OpenGL is not available.

    panic_impossible ();

#endif
  }

  bool renumber (double new_number)
  {
    bool retval = false;

    if (m_number != new_number)
      {
        m_number = new_number;
        retval = true;
      }

    return retval;
  }

private:

  double m_number;

  octave::opengl_functions m_glfcns;
  octave::opengl_renderer m_renderer;

  bool m_in_zoom;

  // (x1,y1,x2,y2)
  Matrix m_zoom_box;

  void draw (void)
  {
#if defined (HAVE_OPENGL)

    if (! valid ())
      {
        m_glfcns.glMatrixMode (GL_PROJECTION);
        m_glfcns.glLoadIdentity ();
        m_glfcns.glViewport (0, 0, w (), h ());
      }

    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    m_renderer.draw (gh_mgr.get_object (m_number));

    if (zoom ())
      overlay ();

#else

    // This shouldn't happen because construction of Opengl_fltk
    // objects is supposed to be impossible if OpenGL is not available.

    panic_impossible ();

#endif
  }

  void overlay (void)
  {
    Matrix overlaycolor (3, 1);
    overlaycolor(0) = 0.45;
    overlaycolor(1) = 0.62;
    overlaycolor(2) = 0.81;
    double overlayalpha = 0.1;
    Matrix bordercolor = overlaycolor;
    double borderalpha = 0.9;
    double borderwidth = 1.5;

    m_renderer.draw_zoom_box (w (), h (),
                              m_zoom_box(0), m_zoom_box(1),
                              m_zoom_box(2), m_zoom_box(3),
                              overlaycolor, overlayalpha,
                              bordercolor, borderalpha, borderwidth);
  }

  int handle (int event)
  {
#if defined (HAVE_OPENGL)

    switch (event)
      {
      case FL_ENTER:
        cursor (FL_CURSOR_CROSS);
        return 1;

      case FL_LEAVE:
        cursor (FL_CURSOR_DEFAULT);
        return 1;
      }

    return Fl_Gl_Window::handle (event);

#else

    octave_unused_parameter (event);

    // This shouldn't happen because construction of Opengl_fltk
    // objects is supposed to be impossible if OpenGL is not available.

    panic_impossible ();

#endif
  }
};

static void script_cb (Fl_Widget *, void *data)
{
  static_cast<uimenu::properties *> (data)->execute_menuselectedfcn ();
}

class fltk_uimenu
{
public:

  fltk_uimenu (int xx, int yy, int ww, int hh)
    : m_menubar (new Fl_Menu_Bar (xx, yy, ww, hh))
  { }

  int items_to_show (void)
  {
    //returns the number of visible menu items
    int len = m_menubar->size ();
    int n = 0;
    for (int t = 0; t < len; t++)
      {
        const Fl_Menu_Item *m
          = static_cast<const Fl_Menu_Item *> (&(m_menubar->menu ()[t]));

        if (m->label () && m->visible ())
          n++;
      }

    return n;
  }

  void show (void)
  {
    m_menubar->show ();
    m_menubar->redraw ();
  }

  void hide (void)
  {
    m_menubar->hide ();
    m_menubar->redraw ();
  }

  bool is_visible (void)
  {
    return m_menubar->visible ();
  }

  int find_index_by_name (const std::string& findname)
  {
    // This function is derived from Greg Ercolano's function
    // int GetIndexByName(...), see:
    // http://seriss.com/people/erco/fltk/#Menu_ChangeLabel
    // He agreed via PM that it can be included in octave using GPLv3
    // Kai Habel (14.10.2010)

    std::string menupath;
    for (int t = 0; t < m_menubar->size (); t++)
      {
        Fl_Menu_Item *m = const_cast<Fl_Menu_Item *> (&(m_menubar->menu ()[t]));
        if (m->submenu ())
          {
            // item has submenu
            if (! menupath.empty ())
              menupath += '/';
            menupath += m->label ();

            if (menupath == findname)
              return (t);
          }
        else
          {
            // End of submenu? Pop back one level.
            if (! m->label ())
              {
                std::size_t idx = menupath.find_last_of ('/');
                if (idx != std::string::npos)
                  menupath.erase (idx);
                else
                  menupath.clear ();
                continue;
              }
            // Menu item?
            std::string itempath = menupath;
            if (! itempath.empty ())
              itempath += '/';
            itempath += m->label ();

            if (itempath == findname)
              return (t);
          }
      }
    return (-1);
  }

  Matrix find_uimenu_children (uimenu::properties& uimenup) const
  {
    Matrix uimenu_childs = uimenup.get_all_children ();
    Matrix retval = do_find_uimenu_children (uimenu_childs);
    return retval;
  }

  Matrix find_uimenu_children (figure::properties& figp) const
  {
    Matrix uimenu_childs = figp.get_all_children ();
    Matrix retval = do_find_uimenu_children (uimenu_childs);
    return retval;
  }

  Matrix do_find_uimenu_children (Matrix uimenu_childs) const
  {
    octave_idx_type k = 0;

    Matrix pos = Matrix (uimenu_childs.numel (), 1);

    for (octave_idx_type ii = 0; ii < uimenu_childs.numel (); ii++)
      {
        gh_manager& gh_mgr = octave::__get_gh_manager__ ();

        graphics_object kidgo = gh_mgr.get_object (uimenu_childs (ii));

        if (kidgo.valid_object () && kidgo.isa ("uimenu"))
          {
            uimenu_childs(k) = uimenu_childs(ii);
            pos(k++) = dynamic_cast<uimenu::properties&> (kidgo.get_properties ()).get_position ();
          }
      }

    uimenu_childs.resize (k, 1);
    pos.resize (k, 1);
    Matrix retval = Matrix (k, 1);
    // Don't know if this is the best method to sort.
    // Can we avoid the for loop?
    Array<octave_idx_type> sidx = pos.sort_rows_idx (DESCENDING);
    for (octave_idx_type ii = 0; ii < k; ii++)
      retval(ii) = uimenu_childs (sidx(ii));

    return retval;
  }

  void delete_entry (uimenu::properties& uimenup)
  {
    std::string fltk_label = uimenup.get___fltk_label__ ();
    int idx = find_index_by_name (fltk_label.c_str ());

    if (idx >= 0)
      m_menubar->remove (idx);
  }

  void update_accelerator (uimenu::properties& uimenup)
  {
    std::string fltk_label = uimenup.get___fltk_label__ ();
    if (! fltk_label.empty ())
      {
        Fl_Menu_Item *item = const_cast<Fl_Menu_Item *> (m_menubar->find_item (fltk_label.c_str ()));

        if (item)
          {
            std::string acc = uimenup.get_accelerator ();
            if (acc.length () > 0)
              {
                int key = FL_CTRL + acc[0];
                item->shortcut (key);
              }
          }
      }
  }

  void update_menuselectedfcn (uimenu::properties& uimenup)
  {
    std::string fltk_label = uimenup.get___fltk_label__ ();
    if (! fltk_label.empty ())
      {
        Fl_Menu_Item *item
          = const_cast<Fl_Menu_Item *> (m_menubar->find_item (fltk_label.c_str ()));
        if (item)
          {
            if (! uimenup.get_menuselectedfcn ().isempty ())
              item->callback (static_cast<Fl_Callback *> (script_cb),
                              static_cast<void *> (&uimenup));
            else
              item->callback (nullptr, static_cast<void *> (nullptr));
          }
      }
  }

  void update_enable (uimenu::properties& uimenup)
  {
    std::string fltk_label = uimenup.get___fltk_label__ ();
    if (! fltk_label.empty ())
      {
        Fl_Menu_Item *item
          = const_cast<Fl_Menu_Item *> (m_menubar->find_item (fltk_label.c_str ()));
        if (item)
          {
            if (uimenup.is_enable ())
              item->activate ();
            else
              item->deactivate ();
          }
      }
  }

  void update_foregroundcolor (uimenu::properties& uimenup)
  {
    std::string fltk_label = uimenup.get___fltk_label__ ();
    if (! fltk_label.empty ())
      {
        Fl_Menu_Item *item
          = const_cast<Fl_Menu_Item *> (m_menubar->find_item (fltk_label.c_str ()));
        if (item)
          {
            Matrix rgb = uimenup.get_foregroundcolor_rgb ();

            uchar r = static_cast<uchar> (std::floor (rgb (0) * 255));
            uchar g = static_cast<uchar> (std::floor (rgb (1) * 255));
            uchar b = static_cast<uchar> (std::floor (rgb (2) * 255));

            item->labelcolor (fl_rgb_color (r, g, b));
          }
      }
  }

  void update_seperator (const uimenu::properties& uimenup)
  {
    // Matlab places the separator before the current
    // menu entry, while fltk places it after.  So we need to find
    // the previous item in this menu/submenu. (Kai)
    std::string fltk_label = uimenup.get___fltk_label__ ();
    if (! fltk_label.empty ())
      {
        int itemflags = 0, idx;
        int curr_idx = find_index_by_name (fltk_label.c_str ());

        for (idx = curr_idx - 1; idx >= 0; idx--)
          {
            Fl_Menu_Item *item
              = const_cast<Fl_Menu_Item *> (&m_menubar->menu () [idx]);
            itemflags = item->flags;
            if (item->label ())
              break;
          }

        if (idx >= 0 && idx < m_menubar->size ())
          {
            if (uimenup.is_separator ())
              {
                if (! (itemflags & FL_SUBMENU))
                  m_menubar->mode (idx, itemflags | FL_MENU_DIVIDER);
              }
            else
              m_menubar->mode (idx, itemflags & (~FL_MENU_DIVIDER));
          }
      }
  }

  void update_visible (uimenu::properties& uimenup)
  {
    std::string fltk_label = uimenup.get___fltk_label__ ();
    if (! fltk_label.empty ())
      {
        Fl_Menu_Item *item
          = const_cast<Fl_Menu_Item *> (m_menubar->find_item (fltk_label.c_str ()));
        if (item)
          {
            if (uimenup.is_visible ())
              item->show ();
            else
              item->hide ();
          }
      }
  }

  void update_position (uimenu::properties& uimenup, int pos)
  {
    uimenup.get_property ("position").set (octave_value (static_cast<double> (pos)),
                                           true, false);
  }

  void add_entry (uimenu::properties& uimenup)
  {

    std::string fltk_label = uimenup.get___fltk_label__ ();

    if (! fltk_label.empty ())
      {
        bool item_added = false;
        do
          {
            const Fl_Menu_Item *item
              = m_menubar->find_item (fltk_label.c_str ());

            if (item)
              {
                //avoid duplicate menulabels
                std::size_t idx1 = fltk_label.find_last_of ('(');
                std::size_t idx2 = fltk_label.find_last_of (')');
                int len = idx2 - idx1;
                int val = 1;
                if (len > 0)
                  {
                    std::string valstr = fltk_label.substr (idx1 + 1, len - 1);
                    fltk_label.erase (idx1, len + 1);
                    val = atoi (valstr.c_str ());
                    if (val > 0 && val < 99)
                      val++;
                  }
                std::ostringstream valstream;
                valstream << val;
                fltk_label += '(' + valstream.str () + ')';
              }
            else
              {
                Matrix uimenu_ch = find_uimenu_children (uimenup);
                int len = uimenu_ch.numel ();
                int flags = 0;
                if (len > 0)
                  flags = FL_SUBMENU;
                if (len == 0 && uimenup.is_checked ())
                  flags += FL_MENU_TOGGLE + FL_MENU_VALUE;
                m_menubar->add (fltk_label.c_str (),
                                0, nullptr, nullptr, flags);
                item_added = true;
              }
          }
        while (! item_added);
        uimenup.set___fltk_label__ (fltk_label);
      }
  }

  void add_to_menu (uimenu::properties& uimenup)
  {
    std::vector<int> delayed_menus;
    Matrix kids = find_uimenu_children (uimenup);
    int len = kids.numel ();
    std::string fltk_label = uimenup.get___fltk_label__ ();
    int count = 0;

    add_entry (uimenup);
    update_foregroundcolor (uimenup);
    update_menuselectedfcn (uimenup);
    update_accelerator (uimenup);
    update_enable (uimenup);
    update_visible (uimenup);
    update_seperator (uimenup);

    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    for (octave_idx_type ii = 0; ii < len; ii++)
      {
        graphics_object kgo = gh_mgr.get_object (kids (len - (ii + 1)));

        if (kgo.valid_object ())
          {
            uimenu::properties& kprop = dynamic_cast<uimenu::properties&>
                                        (kgo.get_properties ());

            // if no pos yet, delay adding menu until after other menus
            int pos = kprop.get_position ();
            if (pos <= 0)
              delayed_menus.push_back ((len - (ii + 1)));
            else
              {
                add_to_menu (kprop);
              }
          }
      }

    // create any delayed menus
    for (std::size_t ii = 0; ii < delayed_menus.size (); ii++)
      {
        graphics_object kgo = gh_mgr.get_object (kids (delayed_menus[ii]));

        if (kgo.valid_object ())
          {
            uimenu::properties& kprop = dynamic_cast<uimenu::properties&>
                                        (kgo.get_properties ());
            add_to_menu (kprop);
            update_position (kprop, ++count);
          }
      }
  }

  void add_to_menu (figure::properties& figp)
  {
    std::vector<int> delayed_menus;
    Matrix kids = find_uimenu_children (figp);
    int len = kids.numel ();
    int count = 0;

    m_menubar->clear ();

    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    for (octave_idx_type ii = 0; ii < len; ii++)
      {
        graphics_object kgo = gh_mgr.get_object (kids (len - (ii + 1)));

        if (kgo.valid_object ())
          {
            uimenu::properties& kprop = dynamic_cast<uimenu::properties&>
                                        (kgo.get_properties ());

            // if no pos yet, delay adding menu until after other menus
            int pos = kprop.get_position ();
            if (pos <= 0)
              delayed_menus.push_back ((len - (ii + 1)));
            else
              {
                add_to_menu (kprop);
                update_position (kprop, ++count);
              }
          }
      }

    // create any delayed menus
    for (std::size_t ii = 0; ii < delayed_menus.size (); ii++)
      {
        graphics_object kgo = gh_mgr.get_object (kids (delayed_menus[ii]));

        if (kgo.valid_object ())
          {
            uimenu::properties& kprop = dynamic_cast<uimenu::properties&>
                                        (kgo.get_properties ());
            add_to_menu (kprop);
            update_position (kprop, ++count);
          }
      }
  }

  template <typename T_prop>
  void remove_from_menu (T_prop& prop)
  {
    Matrix kids;
    std::string type = prop.get_type ();
    kids = find_uimenu_children (prop);
    int len = kids.numel ();

    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    for (octave_idx_type ii = 0; ii < len; ii++)
      {
        graphics_object kgo = gh_mgr.get_object (kids (len - (ii + 1)));

        if (kgo.valid_object ())
          {
            uimenu::properties kprop = dynamic_cast<uimenu::properties&>
                                       (kgo.get_properties ());
            remove_from_menu (kprop);
          }
      }

    if (type == "uimenu")
      delete_entry (dynamic_cast<uimenu::properties&> (prop));
    else if (type == "figure")
      m_menubar->clear ();
  }

  // No copying!

  fltk_uimenu (const fltk_uimenu&) = delete;

  fltk_uimenu operator = (const fltk_uimenu&) = delete;

  ~fltk_uimenu (void)
  {
    // FLTK is supposed to manage memory for widgets.
  }

private:

  Fl_Menu_Bar *m_menubar;
};

#if defined (HAVE_X_WINDOWS)
static int
xerror_handler (Display *, XErrorEvent *)
{
  return 0;
}
#endif

class plot_window : public Fl_Window
{
  friend class fltk_uimenu;

public:

  plot_window (int xx, int yy, int ww, int hh, figure::properties& xfp,
               bool internal)
    : Fl_Window (xx, yy, ww, hh + m_menu_h + m_status_h + 2, "octave"),
      m_window_label (), m_fp (xfp), m_uimenu (nullptr), m_canvas (nullptr),
      m_autoscale (nullptr), m_togglegrid (nullptr), m_panzoom (nullptr),
      m_rotate (nullptr), m_help (nullptr), m_status (nullptr),
      m_resize_dummy (nullptr), m_ax_obj (), m_pos_x (0), m_pos_y (0)
  {
    callback (window_close, static_cast<void *> (this));

    // The size of the resize_dummy box also determines the minimum
    // window size.
    m_resize_dummy = new Fl_Box (5 * m_status_h, m_menu_h,
                                 ww - 5 * m_status_h, hh);

    // See http://fltk.org/articles.php?L415+I0+T+M1000+P1
                // for how resizable works
    resizable (m_resize_dummy);

    // FIXME: The function below is only available in FLTK >= 1.3
    // At some point support for FLTK 1.1 will be dropped in Octave.
    // At that point this function should be uncommented.
    // The current solution is to call xclass() before show() for each window.
    // Set WM_CLASS which allows window managers to properly group related
    // windows.  Otherwise, the class is just "FLTK"
    //default_xclass ("Octave");

    m_uimenu = new fltk_uimenu (0, 0, ww, m_menu_h);
    m_canvas = new OpenGL_fltk (0, m_menu_h, ww, hh, number ());

    // The bottom toolbar is a composite of "autoscale", "togglegrid",
    // "panzoom", "rotate", "help", and "status".
    // Only "status" should be resized.

    int toolbar_y = m_menu_h + hh + 1;
    m_status = new Fl_Output (5 * m_status_h, toolbar_y,
                              ww - 5 * m_status_h, m_status_h, "");

    m_status->textcolor (FL_BLACK);
    m_status->color (FL_GRAY);
    m_status->textfont (FL_COURIER);
    m_status->textsize (10);
    m_status->box (FL_ENGRAVED_BOX);

    m_autoscale = new Fl_Button (0, toolbar_y, m_status_h, m_status_h, "A");
    m_autoscale->callback (button_callback, static_cast<void *> (this));
    m_autoscale->tooltip ("Autoscale");

    m_togglegrid = new Fl_Button (m_status_h, toolbar_y, m_status_h, m_status_h, "G");
    m_togglegrid->callback (button_callback, static_cast<void *> (this));
    m_togglegrid->tooltip ("Toggle Grid");

    m_panzoom = new Fl_Button (2* m_status_h, toolbar_y, m_status_h, m_status_h, "P");
    m_panzoom->callback (button_callback, static_cast<void *> (this));
    m_panzoom->tooltip ("Mouse Pan/Zoom");

    m_rotate = new Fl_Button (3 * m_status_h, toolbar_y, m_status_h, m_status_h, "R");
    m_rotate->callback (button_callback, static_cast<void *> (this));
    m_rotate->tooltip ("Mouse Rotate");

    m_help = new Fl_Button (4 * m_status_h, toolbar_y, m_status_h, m_status_h, "?");
    m_help->callback (button_callback, static_cast<void *> (this));
    m_help->tooltip ("Help");

    end ();

    set_name ();
    m_uimenu->add_to_menu (m_fp);
    if (m_fp.menubar_is ("none") || ! m_uimenu->items_to_show ())
      hide_menubar ();

    update_boundingbox (internal);

    if (m_fp.is_visible ())
      {
        // FIXME: This code should be removed when Octave drops support
        // for FLTK 1.1.  Search for default_xclass in this file to find
        // code that should be uncommented to take its place.
        //
        // Set WM_CLASS which allows window managers to properly group
        // related windows.  Otherwise, the class is just "FLTK"
        xclass ("Octave");

        show ();

#if defined (HAVE_X_WINDOWS)
        std::string show_gui_msgs
          = octave::sys::env::getenv ("OCTAVE_SHOW_GUI_MESSAGES");

        // Installing our handler suppresses the messages.
        if (show_gui_msgs.empty ())
          XSetErrorHandler (xerror_handler);
#endif

        if (m_fp.get_currentaxes ().ok ())
          show_canvas ();
        else
          hide_canvas ();
      }
  }

  // No copying!

  plot_window (const plot_window&) = delete;

  plot_window& operator = (const plot_window&) = delete;

  ~plot_window (void)
  {
    this->hide ();
    Fl::check ();

    delete m_uimenu;

    // FLTK is supposed to manage memory for widgets.
  }

  double number (void) { return m_fp.get___myhandle__ ().value (); }

  void renumber (double new_number)
  {
    if (! m_canvas)
      error ("unable to renumber figure");

    if (m_canvas->renumber (new_number))
      mark_modified ();
  }

  void print (const std::string& cmd, const std::string& term)
  {
    m_canvas->print (cmd, term);
  }

  uint8NDArray get_pixels ()
  {
    return m_canvas->get_pixels ();
  }

  void show_menubar (void)
  {
    m_uimenu->show ();
    update_toolbar_position ();
  }

  void hide_menubar (void)
  {
    m_uimenu->hide ();
    update_toolbar_position ();
  }

  void uimenu_update (const graphics_handle& gh, int id)
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    graphics_object uimenu_obj = gh_mgr.get_object (gh);

    if (uimenu_obj.valid_object () && uimenu_obj.isa ("uimenu"))
      {
        uimenu::properties& uimenup
          = dynamic_cast<uimenu::properties&> (uimenu_obj.get_properties ());
        std::string fltk_label = uimenup.get___fltk_label__ ();
        graphics_object fig = uimenu_obj.get_ancestor ("figure");
        figure::properties& figp
          = dynamic_cast<figure::properties&> (fig.get_properties ());

        switch (id)
          {
          case base_properties::ID_BEINGDELETED:
            m_uimenu->remove_from_menu (uimenup);
            break;

          case base_properties::ID_VISIBLE:
            m_uimenu->update_visible (uimenup);
            break;

          case uimenu::properties::ID_ACCELERATOR:
            m_uimenu->update_accelerator (uimenup);
            break;

          case uimenu::properties::ID_MENUSELECTEDFCN:
            m_uimenu->update_menuselectedfcn (uimenup);
            break;

          case uimenu::properties::ID_CHECKED:
            m_uimenu->add_to_menu (figp);//rebuilding entire menu
            break;

          case uimenu::properties::ID_ENABLE:
            m_uimenu->update_enable (uimenup);
            break;

          case uimenu::properties::ID_FOREGROUNDCOLOR:
            m_uimenu->update_foregroundcolor (uimenup);
            break;

          case uimenu::properties::ID_LABEL:
            m_uimenu->add_to_menu (figp);//rebuilding entire menu
            break;

          case uimenu::properties::ID_POSITION:
            m_uimenu->add_to_menu (figp);//rebuilding entire menu
            break;

          case uimenu::properties::ID_SEPARATOR:
            m_uimenu->update_seperator (uimenup);
            break;
          }

        if (m_uimenu->items_to_show ())
          show_menubar ();
        else
          hide_menubar ();
      }
  }

  void show_canvas (void)
  {
    if (! m_canvas->can_do ())
      error ("unable to plot due to insufficient OpenGL support");
    else if (m_fp.is_visible ())
      {
        m_canvas->show ();
        m_canvas->make_current ();
      }
  }

  void hide_canvas (void)
  {
    m_canvas->hide ();
  }

  // Move the toolbar at the bottom of the plot_window.
  // The only reason for moving the toolbar is hiding and
  // showing the menubar.  All other resizing is done by fltk.

  void update_toolbar_position ()
  {
    int old_canvas_h = m_canvas->h ();

    // keep position fix, change outerposition accordingly
    update_boundingbox (true);
    m_canvas->resize (0, menu_dy (), w (), old_canvas_h);

    int toolbar_y = m_canvas->h () + menu_dy () + 1;
    m_autoscale->position (0, toolbar_y);
    m_togglegrid->position (m_status_h, toolbar_y);
    m_panzoom->position (2 * m_status_h, toolbar_y);
    m_rotate->position (3 * m_status_h, toolbar_y);
    m_help->position (4 * m_status_h, toolbar_y);
    m_status->resize (5 * m_status_h, toolbar_y,
                      w () - 5 * m_status_h, m_status_h);
    init_sizes ();
    redraw ();
  }

  Matrix outerposition2position (const Matrix& outerpos)
  {
    Matrix pos = outerpos;
    pos(1) += menu_dy ();
    pos(3) -= menu_dy () + m_status_h + 2;
    return pos;
  }

  Matrix position2outerposition (const Matrix& pos)
  {
    Matrix outerpos = pos;
    outerpos(1) -= menu_dy ();
    outerpos(3) += menu_dy () + m_status_h + 2;
    return outerpos;
  }

  // Called from figure::properties::ID_POSITION if internal = true
  // or ID_OUTERPOSITION if false.
  // (someone has requested a position change with set (h, "position", [...])
  // or set (h, "outerposition", [...])

  void update_boundingbox (bool internal)
  {
    Matrix bb = m_fp.get_boundingbox (internal);
    if (internal)
      bb = position2outerposition (bb);
    resize (bb(0), bb(1), bb(2), bb(3));
  }

  void mark_modified (void)
  {
    m_canvas->redraw ();
  }

  void set_name (void)
  {
    m_window_label = m_fp.get_title ();
    label (m_window_label.c_str ());
  }

private:

  // window name -- this must exists for the duration of the window's
  // life
  std::string m_window_label;

  // Figure properties.
  figure::properties& m_fp;

  // Status area height.
  static const int m_status_h = 20;

  // Menu height
  static const int m_menu_h = 25;

  fltk_uimenu *m_uimenu;

  OpenGL_fltk *m_canvas;

  Fl_Button *m_autoscale;
  Fl_Button *m_togglegrid;
  Fl_Button *m_panzoom;
  Fl_Button *m_rotate;
  Fl_Button *m_help;
  Fl_Output *m_status;

  Fl_Box *m_resize_dummy;

  graphics_object m_ax_obj;

  int m_pos_x;
  int m_pos_y;

  // Window callback.
  static void window_close (Fl_Widget *, void *data)
  {
    octave_value_list args;
    args(0) = static_cast<plot_window *> (data)->number ();
    octave::feval ("close", args);
  }

  // Button callbacks.
  static void button_callback (Fl_Widget *ww, void *data)
  {
    static_cast<plot_window *> (data)->button_press (ww, data);
  }

  void button_press (Fl_Widget *widg, void *)
  {
    if (widg == m_autoscale)
      axis_auto ();
    else if (widg == m_togglegrid)
      toggle_grid ();
    else if (widg == m_panzoom)
      m_fp.set___mouse_mode__ ("pan");
    else if (widg == m_rotate)
      m_fp.set___mouse_mode__ ("rotate");
    else if (widg == m_help)
      fl_message ("%s", help_text);
  }

  void set_on_ax_obj (const std::string& name, const std::string& value)
  {
    // ax_obj is the last clicked axes object
    if (m_ax_obj && m_ax_obj.isa ("axes")
        && m_ax_obj.get_properties ().get_tag () != "legend"
        && m_ax_obj.get_properties ().get_tag () != "colorbar")
      {
        axes::properties& ap
          = dynamic_cast<axes::properties&>(m_ax_obj.get_properties ());
        ap.set (name, value);
      }
    else // no axes object clicked so far, take currentaxes
      {
        graphics_handle gh = m_fp.get_currentaxes ();
        if (gh.ok ())
          {
            gh_manager& gh_mgr = octave::__get_gh_manager__ ();

            graphics_object go = gh_mgr.get_object (gh);

            axes::properties& ap
              = dynamic_cast<axes::properties&>(go.get_properties ());

            ap.set (name, value);
          }
      }
  }

  void axis_auto (void)
  {
    octave_value_list args;
    if (m_fp.get_currentaxes ().ok ())
      {
        args(0) = m_fp.get_currentaxes ().as_octave_value ();
        args(1) = "auto";
        octave::feval ("axis", args);
        mark_modified ();
      }
  }

  void toggle_grid (void)
  {
    octave_value_list args;
    if (m_fp.get_currentaxes ().ok ())
      args(0) = m_fp.get_currentaxes ().as_octave_value ();

    octave::feval ("grid", args);
    mark_modified ();
  }

  void pixel2pos (const graphics_handle& ax, int px, int py, double& xx,
                  double& yy) const
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    pixel2pos (gh_mgr.get_object (ax), px, py, xx, yy);
  }

  void pixel2pos (graphics_object ax, int px, int py, double& xx,
                  double& yy) const
  {
    if (ax && ax.isa ("axes"))
      {
        axes::properties& ap
          = dynamic_cast<axes::properties&> (ax.get_properties ());
        ColumnVector pp = ap.pixel2coord (px, py);
        xx = pp(0);
        yy = pp(1);
      }
  }

  graphics_handle pixel2axes_or_ca (int px, int py)
  {
    Matrix kids = m_fp.get_children ();
    int len = kids.numel ();

    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    for (int k = 0; k < len; k++)
      {
        graphics_handle hnd = gh_mgr.lookup (kids(k));

        if (hnd.ok ())
          {
            graphics_object kid = gh_mgr.get_object (hnd);

            if (kid.valid_object () && kid.isa ("axes"))
              {
                Matrix bb = kid.get_properties ().get_boundingbox (false);

                if (bb(0) <= px && px < (bb(0)+bb(2))
                    && bb(1) <= py && py < (bb(1)+bb(3)))
                  {
                    return hnd;
                  }
              }
          }
      }
    return m_fp.get_currentaxes ();
  }

  void pixel2status (const graphics_handle& ax, int px0, int py0,
                     int px1 = -1, int py1 = -1)
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    pixel2status (gh_mgr.get_object (ax), px0, py0, px1, py1);
  }

  void pixel2status (graphics_object ax, int px0, int py0,
                     int px1 = -1, int py1 = -1)
  {
    double x0, y0, x1, y1;
    x0 = y0 = x1 = y1 = octave::numeric_limits<double>::NaN ();
    std::stringstream cbuf;
    cbuf.precision (4);
    cbuf.width (6);
    pixel2pos (ax, px0, py0, x0, y0);
    cbuf << '[' << x0 << ", " << y0 << ']';
    if (px1 >= 0)
      {
        pixel2pos (ax, px1, py1, x1, y1);
        cbuf << " -> ["<< x1 << ", " << y1 << ']';
      }

    m_status->value (cbuf.str ().c_str ());
  }

  void view2status (graphics_object ax)
  {
    if (ax && ax.isa ("axes"))
      {
        axes::properties& ap
          = dynamic_cast<axes::properties&> (ax.get_properties ());
        std::stringstream cbuf;
        cbuf.precision (4);
        cbuf.width (6);
        Matrix v (1, 2, 0);
        v = ap.get ("view").matrix_value ();
        cbuf << "[azimuth: " << v(0) << ", elevation: " << v(1) << ']';

        m_status->value (cbuf.str ().c_str ());
      }
  }

  void set_currentpoint (int px, int py)
  {
    if (! m_fp.is_beingdeleted ())
      {
        Matrix pos = m_fp.map_from_boundingbox (px, py);
        m_fp.set_currentpoint (pos);

        gh_manager& gh_mgr = octave::__get_gh_manager__ ();

        graphics_object robj = gh_mgr.get_object (m_fp.get_parent ());

        root_figure::properties& rp

          = dynamic_cast<root_figure::properties&> (robj.get_properties ());
        rp.set_currentfigure (m_fp.get___myhandle__ ().value ());
      }
  }

  void set_axes_currentpoint (graphics_object ax, int px, int py)
  {
    if (ax.valid_object () && ax.isa ("axes"))
      {
        axes::properties& ap
          = dynamic_cast<axes::properties&> (ax.get_properties ());

        Matrix x_zlim = ap.get_transform_zlim ();
        Matrix pos (2, 3, 0.0);

        // front point (nearest to the viewer)
        ColumnVector tmp = ap.get_transform ().untransform (px, py, x_zlim(0));
        pos(0, 0) = tmp(0);
        pos(0, 1) = tmp(1);
        pos(0, 2) = tmp(2);

        // back point (furthest from the viewer)
        tmp = ap.get_transform ().untransform (px, py, x_zlim(1));
        pos(1, 0) = tmp(0);
        pos(1, 1) = tmp(1);
        pos(1, 2) = tmp(2);

        ap.set_currentpoint (pos);
        if (ap.get_tag () != "legend" && ap.get_tag () != "colorbar")
          m_fp.set_currentaxes (ap.get___myhandle__ ().value ());
      }
  }

  int menu_dy ()
  {
    if (m_uimenu->is_visible ())
      return m_menu_h;
    else
      return 0;
  }

  octave_scalar_map format_key_event (int e_key, const char *e_text,
                                      int e_state)
  {
    octave_scalar_map evt;

    evt.assign ("Character", octave_value (e_text));
    evt.assign ("Modifier", octave_value (modifier2cell (e_state)));

    std::string key_str;
    std::ostringstream tmp_str;

    if (e_key == FL_Escape)
      key_str = "escape";
    else if (e_key == FL_Tab)
      key_str = "tab";
    else if (e_key == FL_Caps_Lock)
      key_str = "capslock";
    else if (e_key == FL_Shift_L || e_key == FL_Shift_R)
      key_str = "shift";
    else if (e_key == FL_Control_L || e_key == FL_Control_R)
      key_str = "control";
    else if (e_key == FL_Meta_L || e_key == FL_Meta_R)
      key_str = "windows";
    else if (e_key == FL_Alt_L || e_key == FL_Alt_R)
      key_str = "alt";
    else if (e_key == 32)
      key_str = "space";
    else if (e_key == FL_Enter)
      key_str = "return";
    else if (e_key == FL_BackSpace)
      key_str = "backspace";
    else if (e_key == FL_Print)
      key_str = "printscreen";
    else if (e_key == FL_Pause)
      key_str = "pause";
    else if (e_key == FL_Home)
      key_str = "home";
    else if (e_key == FL_End)
      key_str = "end";
    else if (e_key == FL_Insert)
      key_str = "insert";
    else if (e_key == FL_Page_Up)
      key_str = "pageup";
    else if (e_key == FL_Delete)
      key_str = "delete";
    else if (e_key == FL_Page_Down)
      key_str = "pagedown";
    else if (e_key == FL_Left)
      key_str = "leftarrow";
    else if (e_key == FL_Up)
      key_str = "uparrow";
    else if (e_key == FL_Right)
      key_str = "rightarrow";
    else if (e_key == FL_Down)
      key_str = "downarrow";
    else if (e_key == FL_Num_Lock)
      key_str = "numlock";
    else if (e_key == 0xffaf)
      key_str = "divide";
    else if (e_key == 0xffaa)
      key_str = "multiply";
    else if (e_key == 0xffad)
      key_str = "subtract";
    else if (e_key == 0xffab)
      key_str = "add";
    else if (e_key == 0xff8d)
      key_str = "return";
    else if (e_key == 0xffac)
      key_str = "separator";
    else if (e_key >= 0xffb0 && e_key <= 0xffb9)
      {
        tmp_str << "numpad" << (e_key - 0xffb0);
        key_str = tmp_str.str ();
      }
    else if (e_key >= (FL_F + 1) && e_key <= (FL_F + 12))
      {
        tmp_str << 'f' << (e_key - FL_F);
        key_str = tmp_str.str ();
      }
    else if (e_key == ',')
      key_str = "comma";
    else if (e_key == '.')
      key_str = "period";
    else if (e_key == '-')
      key_str = "hyphen";
    else if (e_key == '^' || e_key == '+' || e_key == '#'
             || e_key == '<' || e_key == 0xfe03 /*AltGr*/)
      key_str = "0";
    else if (isalnum (e_key))
      key_str = std::tolower (e_key);
    else if (isprint (e_text[0]))
      key_str = "0";

    evt.assign ("Key", octave_value (key_str));
    return evt;
  }

  Cell modifier2cell (int e_state)
  {
    string_vector mod;

    if (e_state & FL_SHIFT)
      mod.append (std::string ("shift"));
    if (e_state & FL_CTRL)
      mod.append (std::string ("control"));
    if (e_state & FL_ALT)
      mod.append (std::string ("alt"));
    if (e_state & FL_COMMAND)
      mod.append (std::string ("command"));
    return Cell (mod);
  }

  void resize (int xx, int yy, int ww, int hh)
  {
    Fl_Window::resize (xx, yy, ww, hh);

    Matrix bb (1, 4);
    bb(0) = xx;
    bb(1) = yy;
    bb(2) = ww;
    bb(3) = hh;

    // update outerposition
    m_fp.set_boundingbox (bb, false, false);

    // update position
    m_fp.set_boundingbox (outerposition2position (bb), true, false);
  }

  bool pan_enabled (void)
  {
    // Getting pan mode property:
    octave_value ov_pm = m_fp.get___pan_mode__ ();

    octave_scalar_map pm = ov_pm.scalar_map_value ();

    return pm.contents ("Enable").string_value () == "on";
  }

  std::string pan_mode (void)
  {
    // Getting pan mode property:
    octave_value ov_pm = m_fp.get___pan_mode__ ();

    octave_scalar_map pm = ov_pm.scalar_map_value ();

    return pm.contents ("Motion").string_value ();
  }

  bool rotate_enabled (void)
  {
    // Getting rotate mode property:
    octave_value ov_rm = m_fp.get___rotate_mode__ ();

    octave_scalar_map rm = ov_rm.scalar_map_value ();

    return rm.contents ("Enable").string_value () == "on";
  }

  int handle (int event)
  {
    if (event == FL_FOCUS)
      return 1;

    graphics_handle gh;

    if (! m_fp.is_beingdeleted ())
      {
        // FLTK resends keyboard events with flipped case if all
        // widgets rejects the event.
        // See Event Propagation http://www.fltk.org/doc-1.3/events.html
        static bool key_resent_detected = false;

        gh_manager& gh_mgr = octave::__get_gh_manager__ ();

        switch (event)
          {
          case FL_SHORTCUT:
            {
              // check if it a resent event with switched case
              static int last_event_key = 0;
              static char last_event_text = 0;

              int e_key = Fl::event_key ();
              char e_text = Fl::event_text ()[0];
              key_resent_detected = (e_key == last_event_key
                                     && std::tolower (last_event_text) == std::tolower (e_text)
                                     && ((islower (last_event_text) && isupper (e_text))
                                         || (isupper (last_event_text) && islower (e_text))));

              last_event_key = e_key;
              last_event_text = e_text;
            }
            break;

          case FL_KEYDOWN:
            {
              int e_key = Fl::event_key ();
              const char *e_text = Fl::event_text ();
              int e_state = Fl::event_state ();
              octave_scalar_map evt = format_key_event (e_key, e_text, e_state);

              m_fp.set_currentcharacter (std::string (e_text));

              if (! m_fp.get_keypressfcn ().isempty ()
                  && (evt.contents ("Key").length () > 0))
                {
                  // Update CurrentPoint before callback
                  if (Fl::event_inside (m_canvas))
                    {
                      m_pos_x = Fl::event_x ();
                      m_pos_y = Fl::event_y () - menu_dy ();

                      set_currentpoint (m_pos_x, m_pos_y);

                      gh = pixel2axes_or_ca (m_pos_x, m_pos_y);

                      if (gh.ok ())
                        {
                          m_ax_obj = gh_mgr.get_object (gh);
                          set_axes_currentpoint (m_ax_obj, m_pos_x, m_pos_y);
                        }
                    }

                  m_fp.execute_keypressfcn (evt);
                }

              // Handle special keys used in toolbar
              switch (e_key)
                {
                case 'a':
                case 'A':
                  axis_auto ();
                  return 1;

                case 'g':
                case 'G':
                  toggle_grid ();
                  return 1;

                case 'p':
                case 'P':
                  m_fp.set___mouse_mode__ ("pan");
                  return 1;

                case 'r':
                case 'R':
                  m_fp.set___mouse_mode__ ("rotate");
                  return 1;
                }
            }
            break;

          case FL_KEYUP:
            {
              int e_key = Fl::event_key ();
              int e_state = Fl::event_state ();
              octave_scalar_map evt;
              if (key_resent_detected && Fl::event_length () == 1)
                {
                  // FLTK flipped the case of Fl::event_text because no
                  // widget wanted the FL_KEYDOWN event.
                  char tmp_e_text[2];
                  tmp_e_text[0] = Fl::event_text ()[0];
                  tmp_e_text[1] = 0;
                  // Undo the case flip
                  if (std::islower (tmp_e_text[0]))
                    tmp_e_text[0] = std::toupper (tmp_e_text[0]);
                  else
                    tmp_e_text[0] = std::tolower (tmp_e_text[0]);
                  evt = format_key_event (e_key, tmp_e_text, e_state);
                }
              else
                {
                  const char *e_text = Fl::event_text ();
                  evt = format_key_event (e_key, e_text, e_state);
                }

              if (! m_fp.get_keyreleasefcn ().isempty ()
                  && (evt.contents ("Key").length () > 0))
                m_fp.execute_keyreleasefcn (evt);
              return 1;
            }
            break;
          }

        // Events we only handle if they are in the canvas area.
        if (Fl::event_inside (m_canvas))
          switch (event)
            {
            case FL_MOVE:
              pixel2status (pixel2axes_or_ca (Fl::event_x (),
                                              Fl::event_y () - menu_dy ()),
                            Fl::event_x (), Fl::event_y () - menu_dy ());
              return 1;

            case FL_PUSH:
              m_pos_x = Fl::event_x ();
              m_pos_y = Fl::event_y () - menu_dy ();

              set_currentpoint (m_pos_x, m_pos_y);

              if (Fl::event_clicks ())
                m_fp.set_selectiontype ("open");
              else if (Fl::event_button () == FL_MIDDLE_MOUSE
                       || (Fl::event_button () == FL_LEFT_MOUSE
                           && Fl::event_shift ()))
                m_fp.set_selectiontype ("extend");
              else if (Fl::event_button () == FL_RIGHT_MOUSE
                       || (Fl::event_button () == FL_LEFT_MOUSE
                           && Fl::event_ctrl ()))
                m_fp.set_selectiontype ("alt");
              else
                m_fp.set_selectiontype ("normal");

              gh = pixel2axes_or_ca (m_pos_x, m_pos_y);

              if (gh.ok ())
                {
                  m_ax_obj = gh_mgr.get_object (gh);
                  set_axes_currentpoint (m_ax_obj, m_pos_x, m_pos_y);
                }

              // Ensure windowbuttondownfcn is called after currentpoint
              // is updated but before calling buttondownfcn.
              if (! m_fp.get_windowbuttondownfcn ().isempty ())
                m_fp.execute_windowbuttondownfcn (Fl::event_button ());

              if (gh.ok ())
                {
                  m_fp.set_currentobject (m_ax_obj.get_handle ().value ());

                  base_properties& props = m_ax_obj.get_properties ();
                  if (! props.get_buttondownfcn ().isempty ())
                    props.execute_buttondownfcn (Fl::event_button ());

                  return 1;
                }
              else if (! m_fp.get_buttondownfcn ().isempty ())
                m_fp.execute_buttondownfcn (Fl::event_button ());

              break;

            case FL_DRAG:
              if (! m_fp.get_windowbuttonmotionfcn ().isempty ())
                {
                  set_currentpoint (Fl::event_x (), Fl::event_y () - menu_dy ());
                  m_fp.execute_windowbuttonmotionfcn ();
                }

              if (Fl::event_button () == 1)
                {
                  if (m_ax_obj && m_ax_obj.isa ("axes"))
                    {
                      axes::properties& ap = dynamic_cast<axes::properties&> (m_ax_obj.get_properties ());

                      // Don't pan or rotate legend
                      if (ap.get_tag () != "legend")
                        {
                          if (rotate_enabled ())
                            view2status (m_ax_obj);
                          else
                            pixel2status (m_ax_obj, m_pos_x, m_pos_y,
                                          Fl::event_x (),
                                          Fl::event_y () - menu_dy ());

                          double x0, y0, x1, y1;
                          Matrix pos = m_fp.get_boundingbox (true);
                          pixel2pos (m_ax_obj, m_pos_x, m_pos_y, x0, y0);
                          pixel2pos (m_ax_obj, Fl::event_x (),
                                     Fl::event_y () - menu_dy (),
                                     x1, y1);

                          if (pan_enabled ())
                            {
                              std::string mode = pan_mode ();

                              ap.translate_view (mode, x0, x1, y0, y1);
                            }
                          else if (rotate_enabled ())
                            {
                              double daz, del;
                              daz = (Fl::event_x () - m_pos_x) / pos(2) * 360;
                              del = (Fl::event_y () - menu_dy () - m_pos_y)
                                    / pos(3) * 360;
                              ap.rotate_view (del, daz);
                            }
                        }
                      else
                        {
                          // move the position of the legend
                          Matrix pos = ap.get_position ().matrix_value ();
                          pos(0) += double (Fl::event_x () - m_pos_x)
                                    / m_canvas->w ();
                          pos(1) -= double (Fl::event_y () - menu_dy () - m_pos_y)
                                    / m_canvas->h ();
                          ap.set_position (pos);
                        }

                      m_pos_x = Fl::event_x ();
                      m_pos_y = Fl::event_y () - menu_dy ();
                      mark_modified ();
                    }
                  return 1;
                }
              else if (Fl::event_button () == 3)
                {
                  pixel2status (m_ax_obj, m_pos_x, m_pos_y,
                                Fl::event_x (), Fl::event_y () - menu_dy ());
                  Matrix zoom_box (1, 4, 0);
                  zoom_box(0) = m_pos_x;
                  zoom_box(1) = m_pos_y;
                  zoom_box(2) = Fl::event_x ();
                  zoom_box(3) = Fl::event_y () - menu_dy ();
                  m_canvas->set_zoom_box (zoom_box);
                  m_canvas->zoom (true);
                  mark_modified ();
                  return 1;
                }

              break;

            case FL_MOUSEWHEEL:
              {
                graphics_object ax
                  = gh_mgr.get_object (pixel2axes_or_ca (Fl::event_x (),
                                       Fl::event_y ()
                                       - menu_dy ()));
                if (ax && ax.isa ("axes"))
                  {
                    axes::properties& ap
                      = dynamic_cast<axes::properties&> (ax.get_properties ());

                    // Control how fast to zoom when using scroll wheel.
                    double wheel_zoom_speed = ap.get_mousewheelzoom ();

                    // Determine if we're zooming in or out.
                    const double factor = (Fl::event_dy () < 0
                                           ? 1 / (1.0 - wheel_zoom_speed)
                                           : 1.0 - wheel_zoom_speed);

                    // Get the point we're zooming about.
                    double x1, y1;
                    pixel2pos (ax, Fl::event_x (), Fl::event_y () - menu_dy (),
                               x1, y1);

                    // FIXME: should we only zoom about point for 2D plots?

                    ap.zoom_about_point ("both", x1, y1, factor, false);
                    mark_modified ();
                    return 1;
                  }
              }

              break;

            case FL_RELEASE:
              if (! m_fp.get_windowbuttonupfcn ().isempty ())
                {
                  set_currentpoint (Fl::event_x (),
                                    Fl::event_y () - menu_dy ());
                  m_fp.execute_windowbuttonupfcn ();
                }

              if ((Fl::event_button () == 1) && Fl::event_clicks ())
                {
                  // Double click
                  set_on_ax_obj ("xlimmode", "auto");
                  set_on_ax_obj ("ylimmode", "auto");
                  set_on_ax_obj ("zlimmode", "auto");
                  mark_modified ();
                  return 1;
                }
              if (Fl::event_button () == 3)
                {
                  // End of drag -- zoom.
                  if (m_canvas->zoom ())
                    {
                      m_canvas->zoom (false);
                      double x0, y0, x1, y1;
                      if (m_ax_obj && m_ax_obj.isa ("axes"))
                        {
                          axes::properties& ap = dynamic_cast<axes::properties&>
                                                 (m_ax_obj.get_properties ());
                          pixel2pos (m_ax_obj, m_pos_x, m_pos_y, x0, y0);
                          int pos_x1 = Fl::event_x ();
                          int pos_y1 = Fl::event_y () - menu_dy ();
                          pixel2pos (m_ax_obj, pos_x1, pos_y1, x1, y1);
                          Matrix xl (1, 2, 0);
                          Matrix yl (1, 2, 0);
                          int dx = abs (m_pos_x - pos_x1);
                          int dy = abs (m_pos_y - pos_y1);
                          // Smallest zoom box must be 4 pixels square
                          if ((dx > 4) && (dy > 4))
                            {
                              if (x0 < x1)
                                {
                                  xl(0) = x0;
                                  xl(1) = x1;
                                }
                              else
                                {
                                  xl(0) = x1;
                                  xl(1) = x0;
                                }
                              if (y0 < y1)
                                {
                                  yl(0) = y0;
                                  yl(1) = y1;
                                }
                              else
                                {
                                  yl(0) = y1;
                                  yl(1) = y0;
                                }
                              ap.zoom ("both", xl, yl);
                            }
                          mark_modified ();
                          return 1;
                        }
                    }
                }
              break;
            }
      }

    return Fl_Window::handle (event);
  }
};

class figure_manager
{
private:

  figure_manager (void) = default;

public:

  // No copying!

  figure_manager (const figure_manager&) = delete;

  figure_manager& operator = (const figure_manager&) = delete;

  ~figure_manager (void)
  {
    close_all ();
  }

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      instance = new figure_manager ();

    return retval;
  }

  static void close_all (void)
  {
    if (instance_ok ())
      instance->do_close_all ();
  }

  static void new_window (figure::properties& fp)
  {
    if (instance_ok ())
      instance->do_new_window (fp);
  }

  static void delete_window (int idx)
  {
    if (instance_ok ())
      instance->do_delete_window (idx);
  }

  static void delete_window (const std::string& idx_str)
  {
    delete_window (str2idx (idx_str));
  }

  static void renumber_figure (const std::string& idx_str, double new_number)
  {
    if (instance_ok ())
      instance->do_renumber_figure (str2idx (idx_str), new_number);
  }

  static void toggle_window_visibility (int idx, bool is_visible)
  {
    if (instance_ok ())
      instance->do_toggle_window_visibility (idx, is_visible);
  }

  static void toggle_window_visibility (const std::string& idx_str,
                                        bool is_visible)
  {
    toggle_window_visibility (str2idx (idx_str), is_visible);
  }

  static void mark_modified (int idx)
  {
    if (instance_ok ())
      instance->do_mark_modified (idx);
  }

  static void mark_modified (const graphics_handle& gh)
  {
    mark_modified (hnd2idx (gh));
  }

  static void set_name (int idx)
  {
    if (instance_ok ())
      instance->do_set_name (idx);
  }

  static void set_name (const std::string& idx_str)
  {
    set_name (str2idx (idx_str));
  }

  static Matrix get_size (int idx)
  {
    return instance_ok () ? instance->do_get_size (idx) : Matrix ();
  }

  static Matrix get_size (const graphics_handle& gh)
  {
    return get_size (hnd2idx (gh));
  }

  static void print (const graphics_handle& gh, const std::string& cmd,
                     const std::string& term)
  {
    if (instance_ok ())
      instance->do_print (hnd2idx (gh), cmd, term);
  }

  static uint8NDArray get_pixels (const graphics_handle& gh)
  {
    uint8NDArray retval;
    if (instance_ok ())
      retval = instance->do_get_pixels (hnd2idx (gh));

    return retval;
  }

  static void uimenu_update (const graphics_handle& figh,
                             const graphics_handle& uimenuh, int id)
  {
    if (instance_ok ())
      instance->do_uimenu_update (hnd2idx (figh), uimenuh, id);
  }

  static void update_canvas (const graphics_handle& gh,
                             const graphics_handle& ca)
  {
    if (instance_ok ())
      instance->do_update_canvas (hnd2idx (gh), ca);
  }

  static void update_boundingbox (const std::string& fig_idx_str,
                                  bool internal)
  {
    if (instance_ok ())
      instance->do_update_boundingbox (str2idx (fig_idx_str), internal);
  }

  static void toggle_menubar_visibility (const std::string& fig_idx_str,
                                         bool menubar_is_figure)
  {
    if (instance_ok ())
      instance->do_toggle_menubar_visibility (str2idx (fig_idx_str),
                                              menubar_is_figure);
  }

private:

  static figure_manager *instance;

  // Singleton -- hide all of the above.

  static int curr_index;

  typedef std::map<int, plot_window *> window_map;

  typedef window_map::iterator wm_iterator;

  window_map windows;

  static std::string fltk_idx_header;

  void do_close_all (void)
  {
    wm_iterator win;
    for (win = windows.begin (); win != windows.end (); win++)
      delete win->second;
    windows.clear ();
  }

  void do_new_window (figure::properties& fp)
  {
    int idx = figprops2idx (fp);

    if (idx >= 0 && windows.find (idx) == windows.end ())
      {
        Matrix pos = fp.get_outerposition ().matrix_value ();
        bool internal = false;
        // check if figure::properties::outerposition is default -1.0
        if (pos(2) != -1.0 && pos(3) != -1.0)
          {
            pos = fp.get_boundingbox (internal);
          }
        else
          {
            // use position
            internal = true;
            pos = fp.get_boundingbox (internal);
          }

        idx2figprops (curr_index, fp);

        windows[curr_index++] = new plot_window (pos(0), pos(1), pos(2), pos(3),
            fp, internal);
      }
  }

  void do_delete_window (int idx)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      {
        delete win->second;
        windows.erase (win);
      }
  }

  void do_renumber_figure (int idx, double new_number)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      win->second->renumber (new_number);
  }

  void do_toggle_window_visibility (int idx, bool is_visible)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      {
        if (is_visible)
          {
            win->second->show ();
            win->second->show_canvas ();
          }
        else
          win->second->hide ();

      }
  }

  void do_toggle_menubar_visibility (int fig_idx, bool menubar_is_figure)
  {
    wm_iterator win = windows.find (fig_idx);

    if (win != windows.end ())
      {
        if (menubar_is_figure)
          win->second->show_menubar ();
        else
          win->second->hide_menubar ();

        win->second->redraw ();
      }
  }

  void do_mark_modified (int idx)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      {
        win->second->mark_modified ();
      }
  }

  void do_set_name (int idx)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      win->second->set_name ();
  }

  Matrix do_get_size (int idx)
  {
    Matrix sz (1, 2, 0.0);

    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      {
        sz(0) = win->second->w ();
        sz(1) = win->second->h ();
      }

    return sz;
  }

  void do_print (int idx, const std::string& cmd, const std::string& term)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      win->second->print (cmd, term);
  }

  uint8NDArray do_get_pixels (int idx)
  {
    uint8NDArray retval;
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      retval = win->second->get_pixels ();

    return retval;
  }

  void do_uimenu_update (int idx, const graphics_handle& gh, int id)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      win->second->uimenu_update (gh, id);
  }

  void do_update_canvas (int idx, const graphics_handle& ca)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      {
        if (ca.ok ())
          win->second->show_canvas ();
        else
          win->second->hide_canvas ();
      }
  }

  void do_update_boundingbox (int idx, bool internal)
  {
    wm_iterator win = windows.find (idx);

    if (win != windows.end ())
      win->second->update_boundingbox (internal);
  }

  static int str2idx (const caseless_str& clstr)
  {
    int ind;
    if (clstr.find (fltk_idx_header, 0) == 0)
      {
        std::istringstream istr (clstr.substr (fltk_idx_header.size ()));
        if (istr >> ind)
          return ind;
      }

    error ("figure_manager: could not recognize fltk index");
  }

  void idx2figprops (int idx, figure::properties& fp)
  {
    std::ostringstream ind_str;
    ind_str << fltk_idx_header << idx;
    fp.set___plot_stream__ (ind_str.str ());
  }

  static int figprops2idx (const figure::properties& fp)
  {
    if (fp.get___graphics_toolkit__ () == FLTK_GRAPHICS_TOOLKIT_NAME)
      {
        octave_value ps = fp.get___plot_stream__ ();
        if (ps.is_string ())
          return str2idx (ps.string_value ());
        else
          return 0;
      }

    error ("figure_manager: figure is not fltk");
  }

  static int hnd2idx (double h)
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    graphics_object fobj = gh_mgr.get_object (h);

    if (fobj &&  fobj.isa ("figure"))
      {
        figure::properties& fp
          = dynamic_cast<figure::properties&> (fobj.get_properties ());
        return figprops2idx (fp);
      }

    error ("figure_manager: H (= %g) is not a figure", h);
  }

  static int hnd2idx (const graphics_handle& fh)
  {
    return hnd2idx (fh.value ());
  }
};

figure_manager *figure_manager::instance = nullptr;

std::string figure_manager::fltk_idx_header="fltk index=";
int figure_manager::curr_index = 1;

static bool toolkit_loaded = false;

class fltk_graphics_toolkit : public octave::base_graphics_toolkit
{
public:

  fltk_graphics_toolkit (octave::interpreter& interp)
    : octave::base_graphics_toolkit (FLTK_GRAPHICS_TOOLKIT_NAME),
      m_interpreter (interp), input_event_hook_fcn_id ()
  {
    Fl::visual (FL_RGB);
  }

  ~fltk_graphics_toolkit (void) = default;

  bool is_valid (void) const { return true; }

  bool initialize (const graphics_object& go)
  {
    if (go.isa ("figure")
        || go.isa ("uimenu"))
      {
        if (go.isa ("uimenu"))
          update (go, uimenu::properties::ID_LABEL);

        return true;
      }

    return false;
  }

  void finalize (const graphics_object& go)
  {
    if (go.isa ("figure"))
      {
        octave_value ov = go.get (caseless_str ("__plot_stream__"));

        if (! ov.isempty ())
          figure_manager::delete_window (ov.string_value ());
      }
  }

  void uimenu_set___fltk_label__ (graphics_object uimenu_obj)
  {
    if (uimenu_obj.valid_object ())
      {
        uimenu::properties& uimenup
          = dynamic_cast<uimenu::properties&> (uimenu_obj.get_properties ());
        std::string fltk_label = uimenup.get_label ();

        gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

        graphics_object go = gh_mgr.get_object (uimenu_obj.get_parent ());

        if (go.isa ("uimenu"))
          fltk_label = dynamic_cast<const uimenu::properties&>
                       (go.get_properties ()).get___fltk_label__ ()
                       + '/'
                       + fltk_label;
        else if (go.isa ("figure") || go.isa ("uicontextmenu"))
          ;
        else
          error ("invalid parent object\n");

        uimenup.set___fltk_label__ (fltk_label);
      }
  }

  void update (const graphics_object& go, int id)
  {
    if (go.isa ("figure"))
      {
        octave_value ov = go.get (caseless_str ("__plot_stream__"));

        if (! ov.isempty ())
          {
            const figure::properties& fp
              = dynamic_cast<const figure::properties&> (go.get_properties ());

            switch (id)
              {
              case base_properties::ID_VISIBLE:
                figure_manager::toggle_window_visibility (ov.string_value (),
                    fp.is_visible ());
                break;

              case figure::properties::ID_MENUBAR:
                figure_manager::toggle_menubar_visibility
                (ov.string_value (), fp.menubar_is ("figure"));
                break;

              case figure::properties::ID_CURRENTAXES:
                figure_manager::update_canvas (go.get_handle (),
                                               fp.get_currentaxes ());
                break;

              case figure::properties::ID_NAME:
              case figure::properties::ID_NUMBERTITLE:
                figure_manager::set_name (ov.string_value ());
                break;

              case figure::properties::ID_INTEGERHANDLE:
                {
                  std::string tmp = ov.string_value ();
                  graphics_handle gh = fp.get___myhandle__ ();
                  figure_manager::renumber_figure (tmp, gh.value ());
                  figure_manager::set_name (tmp);
                }
                break;

              case figure::properties::ID_POSITION:
                figure_manager::update_boundingbox (ov.string_value (), true);
                break;

              case figure::properties::ID_OUTERPOSITION:
                figure_manager::update_boundingbox (ov.string_value (), false);
                break;
              }
          }
      }
    else if (go.isa ("uimenu"))
      {
        if (id == uimenu::properties::ID_LABEL)
          uimenu_set___fltk_label__ (go);

        graphics_object fig = go.get_ancestor ("figure");
        figure_manager::uimenu_update (fig.get_handle (), go.get_handle (), id);
      }
  }

  void redraw_figure (const graphics_object& go) const
  {
    // We scan all figures and add those which use FLTK.

    gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

    graphics_object obj = gh_mgr.get_object (0);

    if (obj && obj.isa ("root"))
      {
        base_properties& props = obj.get_properties ();
        Matrix children = props.get_all_children ();

        for (octave_idx_type n = 0; n < children.numel (); n++)
          {
            graphics_object fobj = gh_mgr.get_object (children (n));

            if (fobj && fobj.isa ("figure"))
              {
                figure::properties& fp
                  = dynamic_cast<figure::properties&> (fobj.get_properties ());

                if (fp.get___graphics_toolkit__ ()
                    == FLTK_GRAPHICS_TOOLKIT_NAME)
                  figure_manager::new_window (fp);
              }
          }
      }

    figure_manager::mark_modified (go.get_handle ());
    Fl::check ();
  }

  void print_figure (const graphics_object& go,
                     const std::string& term,
                     const std::string& file_cmd,
                     const std::string& /*debug_file*/) const
  {
    figure_manager::print (go.get_handle (), file_cmd, term);
  }

  uint8NDArray get_pixels (const graphics_object& go) const
  {
    return figure_manager::get_pixels (go.get_handle ());
  }

  Matrix get_canvas_size (const graphics_handle& fh) const
  {
    return figure_manager::get_size (fh);
  }

  /*
    double get_screen_resolution (void) const
    {
      // FLTK doesn't give this info.
      return 72.0;

      // FIXME: FLTK >= 1.3.0 could do this with Fl::screen_dpi (h, v, n)
      // but do we need it?
    }
  */

  Matrix get_screen_size (void) const
  {
    Matrix sz (1, 2, 0.0);
    sz(0) = Fl::w ();
    sz(1) = Fl::h ();
    return sz;
  }

  void close (void)
  {
    if (toolkit_loaded)
      {
        m_interpreter.munlock ("__init_fltk__");

        octave_value_list args = input_event_hook_fcn_id;
        args.append (false);
        Fremove_input_event_hook (m_interpreter, args, 0);
        input_event_hook_fcn_id = octave_value_list ();

        figure_manager::close_all ();
      }
  }

  void set_input_event_hook_id (const octave_value_list& id)
  {
    input_event_hook_fcn_id = id;
  }

private:

  octave::interpreter& m_interpreter;

  octave_value_list input_event_hook_fcn_id;
};

#endif

DEFMETHOD_DLD (__fltk_check__, interp, , ,
               doc: /* -*- texinfo -*-
@deftypefn {} {} __fltk_check__ ()
Undocumented internal function.  Calls Fl::check ()
@end deftypefn */)
{
#if defined (HAVE_FLTK)
  Fl::check ();

  if (Vdrawnow_requested)
    Fdrawnow (interp);

  return octave_value_list ();
#else
  octave_unused_parameter (interp);

  err_disabled_feature ("__fltk_check__", "OpenGL and FLTK");
#endif
}

// Initialize the fltk graphics toolkit.

DEFMETHOD_DLD (__init_fltk__, interp, , ,
               doc: /* -*- texinfo -*-
@deftypefn {} {} __init_fltk__ ()
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_FLTK)
  octave::display_info& dpy_info = interp.get_display_info ();

  if (! dpy_info.display_available ())
    error ("__init_fltk__: no graphics DISPLAY available");
  else if (! toolkit_loaded)
    {
      interp.mlock ();

      octave::gtk_manager& gtk_mgr = interp.get_gtk_manager ();

      fltk_graphics_toolkit *fltk = new fltk_graphics_toolkit (interp);
      octave::graphics_toolkit tk (fltk);
      gtk_mgr.load_toolkit (tk);
      toolkit_loaded = true;

      octave_value fcn (new octave_builtin (F__fltk_check__));
      octave_value fcn_handle (new octave_fcn_handle (fcn));

      octave_value_list id = Fadd_input_event_hook (interp, fcn_handle, 1);

      fltk->set_input_event_hook_id (id);
    }

  return octave_value_list ();

#else
  octave_unused_parameter (interp);

  err_disabled_feature ("__init_fltk__", "OpenGL and FLTK");
#endif
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

OCTAVE_END_NAMESPACE(octave)
