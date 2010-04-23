/*

Copyright (C) 2007, 2008, 2009 Shai Ayal

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

  backend ("fltk");
  plot (randn (1e3, 1));

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (HAVE_FLTK)

#include <map>
#include <set>
#include <sstream>
#include <iostream>

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Output.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Gl_Window.H>
#include <FL/fl_ask.H>
#include <FL/fl_draw.H>
#include <FL/gl.h>

#include "cmd-edit.h"
#include "lo-ieee.h"

#include "defun-dld.h"
#include "error.h"
#include "gl-render.h"
#include "gl2ps-renderer.h"
#include "graphics.h"
#include "parse.h"
#include "toplev.h"
#include "variables.h"

#define FLTK_BACKEND_NAME "fltk"

// Give FLTK no more than 0.01 sec to do its stuff.
static double fltk_maxtime = 1e-2;

const char* help_text = "\
Keyboard Shortcuts\n\
a - autoscale\n\
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
    : Fl_Gl_Window (xx, yy, ww, hh, 0), number (num), in_zoom (false),
      print_filename ("")
  {
    // Ask for double buffering and a depth buffer.
    mode (FL_DEPTH | FL_DOUBLE);
  }

  ~OpenGL_fltk (void) { }

  void zoom (bool z) 
  {
    in_zoom = z;
    if (! in_zoom)
      hide_overlay ();
  }

  bool zoom (void) { return in_zoom; }
  void set_zoom_box (const Matrix& zb) { zoom_box = zb; }
  
  void print (const std::string& filename)
  {
    print_filename  = filename;
  }

private:
  double number;
  opengl_renderer renderer;
  bool in_zoom;
  // (x1,y1,x2,y2)
  Matrix zoom_box;

  std::string print_filename;

  void setup_viewport (int _w, int _h)
  {
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    glViewport (0, 0, _w, _h);
  }

  void draw (void)
  {
    if (! valid ())
      {
        valid (1);
        setup_viewport (w (), h ());
      }

    if (! print_filename.empty ())
      {
        opengl_renderer *rend = new glps_renderer (print_filename);
        rend->draw (gh_manager::lookup (number));
        print_filename = "";
        delete rend;
      }
    else
      {
        renderer.draw (gh_manager::lookup (number));

        if (zoom ())
          overlay ();
      }
  }

  void resize (int _x, int _y, int _w, int _h)
  {
    Fl_Gl_Window::resize (_x, _y, _w, _h);
    setup_viewport (_w, _h);
    redraw ();
  }

  void zoom_box_vertex (void)
  {
    glVertex2d (zoom_box(0), h () - zoom_box(1));
    glVertex2d (zoom_box(0), h () - zoom_box(3));
    glVertex2d (zoom_box(2), h () - zoom_box(3));
    glVertex2d (zoom_box(2), h () - zoom_box(1));
    glVertex2d (zoom_box(0), h () - zoom_box(1));
  }
 
  void overlay (void)
  {
    glPushMatrix ();

    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity ();

    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    gluOrtho2D (0.0, w (), 0.0, h ());

    glPushAttrib (GL_DEPTH_BUFFER_BIT | GL_CURRENT_BIT);
    glDisable (GL_DEPTH_TEST);

    glBegin (GL_POLYGON);
    glColor4f (0.45, 0.62, 0.81, 0.1);
    zoom_box_vertex ();
    glEnd ();

    glBegin (GL_LINE_STRIP);
    glLineWidth (1.5);
    glColor4f (0.45, 0.62, 0.81, 0.9);
    zoom_box_vertex ();
    glEnd ();

    glPopAttrib ();
    glPopMatrix ();
  }

  int handle (int event)
  {
    int retval = Fl_Gl_Window::handle (event);

    switch (event)
      {
      case FL_ENTER:
        window ()->cursor (FL_CURSOR_CROSS);
        return 1;

      case FL_LEAVE:
        window ()->cursor (FL_CURSOR_DEFAULT);
        return 1;
      }

    return retval;
  }
};

// Parameter controlling how fast we zoom when using the scrool wheel.
static double wheel_zoom_speed = 0.05;

class plot_window : public Fl_Window
{
public:
  plot_window (int _x, int _y, int _w, int _h, figure::properties& _fp)
    : Fl_Window (_x, _y, _w, _h, "octave"), window_label (), shift (0),
      fp (_fp), canvas (0), autoscale (0), togglegrid (0), help (0),
      status (0)
  {
    callback (window_close, static_cast<void*> (this));

    begin ();
    {
      canvas = new
        OpenGL_fltk (0, 0, _w , _h - status_h, number ());

      autoscale = new
        Fl_Button (0,
                   _h - status_h,
                   status_h,
                   status_h,
                   "A");
      autoscale->callback (button_callback, static_cast<void*> (this));

      togglegrid = new
        Fl_Button (status_h,
                   _h - status_h,
                   status_h,
                   status_h,
                   "G");
      togglegrid->callback (button_callback, static_cast<void*> (this));

      help = new
        Fl_Button (2*status_h,
                   _h - status_h,
                   status_h,
                   status_h,
                   "?");
      help->callback (button_callback, static_cast<void*> (this));

      status = new
        Fl_Output (3*status_h,
                   _h - status_h,
                   _w > 2*status_h ? _w - status_h : 0,
                   status_h, "");

      status->textcolor (FL_BLACK);
      status->color (FL_GRAY);
      status->textfont (FL_COURIER);
      status->textsize (10);
      status->box (FL_ENGRAVED_BOX);

      // This allows us to have a valid OpenGL context right away.
      canvas->mode (FL_DEPTH | FL_DOUBLE );
      if (fp.is_visible ())
        {
          show ();
          canvas->show ();
          canvas->make_current ();
        }
    }
    end ();

    status->show ();
    autoscale->show ();
    togglegrid->show ();

    set_name ();
    resizable (canvas);
    size_range (4*status_h, 2*status_h);

  }

  ~plot_window (void)
  {
    canvas->hide ();
    status->hide ();
    this->hide ();
    delete canvas;
    delete status;
  }

  // FIXME -- this could change.
  double number (void) { return fp.get___myhandle__ ().value (); }

  void print (const std::string& fname)
  {
    canvas->print (fname);

    // Print immediately so the output file will exist when the drawnow
    // command is done.
    mark_modified ();
    Fl::wait (fltk_maxtime);
  }

  void mark_modified (void)
  {
    damage (FL_DAMAGE_ALL);
    canvas->damage (FL_DAMAGE_ALL);
  }

  void set_name (void)
  {
    window_label = fp.get_title ();
    label (window_label.c_str ());
  }

private:
  // window name -- this must exists for the duration of the window's
  // life
  std::string window_label;

  // Mod keys status
  int shift;

  // Figure properties.
  figure::properties& fp;

  // Status area height.
  static const int status_h = 20;

  // Window callback.
  static void window_close (Fl_Widget*, void* data)
  {
    octave_value_list args;
    args(0) = static_cast<plot_window*> (data)->number ();
    feval ("close", args);
  }

  // Button callbacks.
  static void button_callback (Fl_Widget* ww, void* data)
  {
    static_cast<plot_window*> (data)->button_press (ww);
  }

  void button_press (Fl_Widget* widg)
  {
    if (widg == autoscale)
      axis_auto ();

    if (widg == togglegrid)
      toggle_grid ();

    if (widg == help)
      fl_message ("%s", help_text);
  }

  OpenGL_fltk* canvas;
  Fl_Button* autoscale;
  Fl_Button* togglegrid;
  Fl_Button* help;
  Fl_Output* status;

  void axis_auto (void)
  {
    octave_value_list args;
    args(0) = fp.get_currentaxes ().as_octave_value ();
    args(1) = "auto";
    feval ("axis", args);
    mark_modified ();
  }

  void toggle_grid (void)
  {
    octave_value_list args;
    args(0) = fp.get_currentaxes ().as_octave_value ();
    feval ("grid", args);
    mark_modified ();
  }

  void pixel2pos 
  (graphics_handle ax, int px, int py, double& xx, double& yy) const
  {
    pixel2pos ( gh_manager::get_object (ax), px, py, xx, yy);
  }

  void pixel2pos 
  (graphics_object ax, int px, int py, double& xx, double& yy) const
  {
    if (ax && ax.isa ("axes"))
      {
        axes::properties& ap =
          dynamic_cast<axes::properties&> (ax.get_properties ());
        ColumnVector pp = ap.pixel2coord (px, py);
        xx = pp(0);
        yy = pp(1);
      }
  }

  graphics_handle pixel2axes_or_ca (int px, int py )
  {
    Matrix kids = fp.get_children ();
    int len = kids.length ();

    for (int k = 0; k < len; k++)
      {
        graphics_handle hnd = gh_manager::lookup (kids(k));

        if (hnd.ok ())
          {
            graphics_object kid = gh_manager::get_object (hnd);

            if (kid.valid_object () && kid.isa ("axes"))
              {
                Matrix bb = kid.get_properties ().get_boundingbox (true);

                if (bb(0) <= px && px < (bb(0)+bb(2))
                    && bb(1) <= py && py < (bb(1)+bb(3)))
                  {
                    return hnd;
                  }
              }
          }
      }
    return fp.get_currentaxes ();
  }
  
  void pixel2status (graphics_handle ax, int px0, int py0,
                     int px1 = -1, int py1 = -1)
  {
    pixel2status (gh_manager::get_object (ax), px0, py0, px1, py1);
  }

  void pixel2status (graphics_object ax, int px0, int py0,
                     int px1 = -1, int py1 = -1)
  {
    double x0, y0, x1, y1;
    std::stringstream cbuf;

    pixel2pos (ax, px0, py0, x0, y0);
    cbuf << "[" << x0 << ", " << y0 << "]";
    if (px1 >= 0)
      {
        pixel2pos (ax, px1, py1, x1, y1);
        cbuf << " -> ["<< x1 << ", " << y1 << "]";
      }

    status->value (cbuf.str ().c_str ());
    status->redraw ();
  }

  void set_currentpoint (int px, int py)
  {
    Matrix pos (1,2,0);
    pos(0) = px;
    pos(1) = h () - status_h - py;
    fp.set_currentpoint (pos);
  }

  void set_axes_currentpoint (graphics_object ax, int px, int py)
  {
    axes::properties& ap = 
      dynamic_cast<axes::properties&> (ax.get_properties ());
    
    double xx, yy;
    pixel2pos (ax, px, py, xx, yy);

    Matrix pos (2,3,0);
    pos(0,0) = xx;
    pos(1,0) = yy;
    pos(0,1) = xx;
    pos(1,1) = yy;

    ap.set_currentpoint (pos);
  }

  int key2shift (int key)
  {
    if (key == FL_Shift_L || key == FL_Shift_R)
      return FL_SHIFT;

    if (key == FL_Control_L || key == FL_Control_R)
      return FL_CTRL;

    if (key == FL_Alt_L || key == FL_Alt_R)
      return FL_ALT;

    if (key == FL_Meta_L || key == FL_Meta_R)
      return FL_META;

    return 0;
  }

  int key2ascii (int key)
  {
    if (key < 256) return key;
    if (key == FL_Tab) return '\t';
    if (key == FL_Enter) return 0x0a;
    if (key == FL_BackSpace) return 0x08;
    if (key == FL_Escape) return 0x1b;

    return 0;
  }

  Cell modifier2cell ()
  {
    string_vector mod;
    
    if (shift & FL_SHIFT)
      mod.append (std::string ("shift"));
    if (shift & FL_CTRL)
      mod.append (std::string ("control"));
    if (shift & FL_ALT || shift & FL_META)
      mod.append (std::string ("alt"));

    return Cell (mod);
  }

  void resize (int _x,int _y,int _w,int _h)
  {
    Fl_Window::resize (_x, _y, _w, _h);

    Matrix pos (1,4,0);
    pos(0) = _x;
    pos(1) = _y;
    pos(2) = _w;
    pos(3) = _h - status_h;

    fp.set_position (pos);
  }

  void draw (void)
  {
    Matrix pos = fp.get_position ().matrix_value ();
    Fl_Window::resize (pos(0), pos(1) , pos(2), pos(3) + status_h);

    return Fl_Window::draw ();
  }

  int handle (int event)
  {
    static int px0,py0;
    static graphics_object ax0;

    int retval = Fl_Window::handle (event);

    // We only handle events which are in the canvas area.
    if (Fl::event_y () >= h() - status_h)
      return retval;

    switch (event)
      {
      case FL_KEYDOWN:
        {
          int key = Fl::event_key ();

          shift |= key2shift (key);
          int key_a = key2ascii (key);
          if (key_a && fp.get_keypressfcn ().is_defined ()) 
            {
              Octave_map evt;
              evt.assign ("Character", octave_value (key_a));
              evt.assign ("Key", octave_value (std::tolower (key_a)));
              evt.assign ("Modifier", octave_value (modifier2cell ()));
              fp.execute_keypressfcn (evt);
            }
          switch (key)
            {
            case 'a':
            case 'A':
              axis_auto ();
            break;

            case 'g':
            case 'G':
              toggle_grid ();
            break;
            }
        }
        break;

      case FL_KEYUP:
        {
          int key = Fl::event_key ();

          shift &= (~key2shift (key));
          int key_a = key2ascii (key);
          if (key_a && fp.get_keyreleasefcn ().is_defined ())
            {
              Octave_map evt;
              evt.assign ("Character", octave_value (key_a));
              evt.assign ("Key", octave_value (std::tolower (key_a)));
              evt.assign ("Modifier", octave_value (modifier2cell ()));
              fp.execute_keyreleasefcn (evt);
            }
        }
        break;

      case FL_MOVE:
        pixel2status (pixel2axes_or_ca (Fl::event_x (), Fl::event_y ()),
                      Fl::event_x (), Fl::event_y ());
        break;

      case FL_PUSH:
        px0 = Fl::event_x ();
        py0 = Fl::event_y ();
        ax0 = gh_manager::get_object (pixel2axes_or_ca (px0, py0));

        set_currentpoint (Fl::event_x (), Fl::event_y ());
        set_axes_currentpoint (ax0, px0, py0);
        fp.execute_windowbuttondownfcn ();
        

        if (Fl::event_button () == 1 || Fl::event_button () == 3)
          return 1;
        break;

      case FL_DRAG:
        pixel2status (ax0, px0, py0, Fl::event_x (), Fl::event_y ());
        if (fp.get_windowbuttonmotionfcn ().is_defined ())
          {
            set_currentpoint (Fl::event_x (), Fl::event_y ());
            fp.execute_windowbuttonmotionfcn ();
          }
        
        if (Fl::event_button () == 1)
          {
            if (ax0 && ax0.isa ("axes"))
              {
                axes::properties& ap = 
                  dynamic_cast<axes::properties&> (ax0.get_properties ());
              
                double x0, y0, x1, y1;
                pixel2pos (ax0, px0, py0, x0, y0);
                pixel2pos (ax0, Fl::event_x (), Fl::event_y (), x1, y1);
                px0 = Fl::event_x ();
                py0 = Fl::event_y ();

                ap.translate_view (x0 - x1, y0 - y1);
                mark_modified ();
              }
            return 1;
          }
        else if (Fl::event_button () == 3)
          {
            Matrix zoom_box (1,4,0);
            zoom_box (0) = px0;
            zoom_box (1) = py0;
            zoom_box (2) =  Fl::event_x ();
            zoom_box (3) =  Fl::event_y ();
            canvas->set_zoom_box (zoom_box);
            canvas->zoom (true);
            canvas->redraw ();
          }

        break;

      case FL_MOUSEWHEEL:
        {
          graphics_object ax = 
            gh_manager::get_object (pixel2axes_or_ca (Fl::event_x (), 
                                                      Fl::event_y ()));                                                                      
          if (ax && ax.isa ("axes"))
            {
              axes::properties& ap = 
                dynamic_cast<axes::properties&> (ax.get_properties ());
              
              // Determine if we're zooming in or out.
              const double factor = 
                (Fl::event_dy () > 0) ? 1.0 + wheel_zoom_speed : 1.0 - wheel_zoom_speed;
              
              // Get the point we're zooming about.
              double x1, y1;
              pixel2pos (ax, Fl::event_x (), Fl::event_y (), x1, y1);
              
              ap.zoom_about_point (x1, y1, factor, false);
              mark_modified ();
            }
        }
      return 1;

      case FL_RELEASE:
        if (fp.get_windowbuttonupfcn ().is_defined ())
          {
            set_currentpoint (Fl::event_x (), Fl::event_y ());
            fp.execute_windowbuttonupfcn ();
          }
       
        if (Fl::event_button () == 1)
          {
            if ( Fl::event_clicks () == 1)
              {
                if (ax0 && ax0.isa ("axes"))
                  {
                    axes::properties& ap =
                      dynamic_cast<axes::properties&> (ax0.get_properties ());
                    ap.set_xlimmode("auto");
                    ap.set_ylimmode("auto");
                    ap.set_zlimmode("auto");
                    mark_modified ();
                  }
              }
          }
        if (Fl::event_button () == 3)
          {
            // End of drag -- zoom.
            if (canvas->zoom ())
              {
                canvas->zoom (false);
                double x0,y0,x1,y1;
                if (ax0 && ax0.isa ("axes"))
                  {
                    axes::properties& ap =
                      dynamic_cast<axes::properties&> (ax0.get_properties ());
                    pixel2pos (ax0, px0, py0, x0, y0);
                    pixel2pos (ax0, Fl::event_x (), Fl::event_y (), x1, y1);
                    Matrix xl (1,2,0);
                    Matrix yl (1,2,0);
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
                    ap.zoom (xl, yl);
                    mark_modified ();
                  }
              }
          }
        break;
      }

    return retval;
  }
};

class figure_manager
{
public:

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      instance = new figure_manager ();

    if (! instance)
      {
        ::error ("unable to create figure_manager object!");

        retval = false;
      }

    return retval;
  }

  ~figure_manager (void)
  {
    close_all ();
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

  static void delete_window (std::string idx_str)
  {
    delete_window (str2idx (idx_str));
  }

  static void toggle_window_visibility (int idx, bool is_visible)
  {
    if (instance_ok ())
      instance->do_toggle_window_visibility (idx, is_visible);
  }

  static void toggle_window_visibility (std::string idx_str, bool is_visible)
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

  static void set_name (std::string idx_str)
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

  static void print (const graphics_handle& gh , const std::string& filename)
  {
    if (instance_ok ())
      instance->do_print (hnd2idx(gh), filename);
  }

private:

  static figure_manager *instance;

  figure_manager (void) { }

  // No copying!
  figure_manager (const figure_manager&);
  figure_manager& operator = (const figure_manager&);

  // Singelton -- hide all of the above.

  static int curr_index;
  typedef std::map<int, plot_window*> window_map;
  typedef window_map::iterator wm_iterator;;
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
    int x, y, w, h;

    int idx = figprops2idx (fp);
    if (idx >= 0 && windows.find (idx) == windows.end ())
      {
        default_size (x, y, w, h);
        idx2figprops (curr_index , fp);
        windows[curr_index++] = new plot_window (x, y, w, h, fp);
      }
  }

  void do_delete_window (int idx)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        delete win->second;
        windows.erase (win);
      }
  }

  void do_toggle_window_visibility (int idx, bool is_visible)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        if (is_visible)
          win->second->show ();
        else
          win->second->hide ();

        win->second->redraw ();
      }
  }

  void do_mark_modified (int idx)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        win->second->mark_modified ();
      }
  }

  void do_set_name (int idx)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        win->second->set_name ();
      }
  }

  Matrix do_get_size (int idx)
  {
    Matrix sz (1, 2, 0.0);

    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        sz(0) = win->second->w ();
        sz(1) = win->second->h ();
      }

    return sz;
  }

  void do_print (int idx, const std::string& filename)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        win->second->print (filename);
      }
  }

  // FIXME -- default size should be configurable.
  void default_size (int& x, int& y, int& w, int& h)
  {
    x = 0;
    y = 0;
    w = 640;
    h = 480;
  }

  static int str2idx (const caseless_str clstr)
  {
    int ind;
    if (clstr.find (fltk_idx_header,0) == 0)
      {
        std::istringstream istr (clstr.substr (fltk_idx_header.size ()));
        if (istr >> ind)
          return ind;
      }
    error ("fltk_backend: could not recognize fltk index");
    return -1;
  }

  void idx2figprops (int idx, figure::properties& fp)
  {
    std::ostringstream ind_str;
    ind_str << fltk_idx_header << idx;
    fp.set___plot_stream__ (ind_str.str ());
  }

  static int figprops2idx (const figure::properties& fp)
  {
    if (fp.get___backend__ () == FLTK_BACKEND_NAME)
      {
        octave_value ps = fp.get___plot_stream__ ();
        if (ps.is_string ())
          return str2idx (ps.string_value ());
        else
          return 0;
      }
    error ("fltk_backend:: figure is not fltk");
    return -1;
  }

  static int hnd2idx (const double h)
  {
    graphics_object fobj = gh_manager::get_object (h);
    if (fobj &&  fobj.isa ("figure"))
      {
        figure::properties& fp =
          dynamic_cast<figure::properties&> (fobj.get_properties ());
        return figprops2idx (fp);
      }
    error ("fltk_backend:: not a figure");
    return -1;
  }

  static int hnd2idx (const graphics_handle& fh)
  {
    return hnd2idx (fh.value ());
  }
};

figure_manager *figure_manager::instance = 0;

std::string figure_manager::fltk_idx_header="fltk index=";
int figure_manager::curr_index = 1;

static bool backend_registered = false;

static int
__fltk_redraw__ (void)
{
  if (backend_registered)
    {
      // we scan all figures and add those which use FLTK as a backend
      graphics_object obj = gh_manager::get_object (0);
      if (obj && obj.isa ("root"))
        {
          base_properties& props = obj.get_properties ();
          Matrix children = props.get_children ();

          for (octave_idx_type n = 0; n < children.numel (); n++)
            {
              graphics_object fobj = gh_manager::get_object (children (n));
              if (fobj && fobj.isa ("figure"))
                {
                  figure::properties& fp =
                      dynamic_cast<figure::properties&> (fobj.get_properties ());
                  if (fp.get___backend__ () == FLTK_BACKEND_NAME)
                    figure_manager::new_window (fp);
                }
            }
        }

      // it seems that we have to call Fl::check twice to get everything drawn
      Fl::check ();
      Fl::check ();
    }

  return 0;
}

class fltk_backend : public base_graphics_backend
{
public:
  fltk_backend (void)
    : base_graphics_backend (FLTK_BACKEND_NAME) { }

  ~fltk_backend (void) { }

  bool is_valid (void) const { return true; }

  void object_destroyed (const graphics_object& go)
  {
    if (go.isa ("figure"))
      {
        octave_value ov = go.get (caseless_str ("__plot_stream__"));
        figure_manager::delete_window (ov.string_value ());
      }
  }

  void property_changed (const graphics_object& go, int id)
  {
    if (go.isa ("figure"))
      {
        octave_value ov = go.get (caseless_str ("__plot_stream__"));
        
        if (! ov.is_empty ())
          {
            const figure::properties& fp =
              dynamic_cast<const figure::properties&> (go.get_properties ());
            
            switch (id)
              {
              case base_properties::VISIBLE:
                figure_manager::toggle_window_visibility (ov.string_value (), fp.is_visible ());
                break;

              case figure::properties::NAME:
              case figure::properties::NUMBERTITLE:
                figure_manager::set_name (ov.string_value ());
                break;
              }
          }
      }
  }

  void redraw_figure (const graphics_object& go) const
  {
    figure_manager::mark_modified (go.get_handle ());

    __fltk_redraw__ ();
  }

  void print_figure (const graphics_object& go,
                     const std::string& /*term*/,
                     const std::string& file, bool /*mono*/,
                     const std::string& /*debug_file*/) const 
  { 
    figure_manager::print (go.get_handle (), file);
  }

  Matrix get_canvas_size (const graphics_handle& fh) const
  {
    return figure_manager::get_size (fh);
  }

  double get_screen_resolution (void) const
  {
    // FLTK doesn't give this info.
    return 72.0;
  }

  Matrix get_screen_size (void) const
  {
    Matrix sz (1, 2, 0.0);
    sz(0) = Fl::w ();
    sz(1) = Fl::h ();
    return sz;
  }
};

DEFUN_DLD (__fltk_redraw__, , , "")
{
  __fltk_redraw__ ();

  return octave_value ();
}

// Initialize the fltk backend.

DEFUN_DLD (__init_fltk__, , , "")
{
  static bool remove_fltk_registered = false;

  if (! backend_registered)
    {
      mlock ();

      graphics_backend::register_backend (new fltk_backend);
      backend_registered = true;
      
      octave_value_list args;
      args(0) = "__fltk_redraw__";
      feval ("add_input_event_hook", args, 0);

      if (! remove_fltk_registered)
        {
          octave_add_atexit_function ("__remove_fltk__");

          remove_fltk_registered = true;
        }
    }

  octave_value retval;
  return retval;
}


// Delete the fltk backend.

DEFUN_DLD (__remove_fltk__, , , "")
{
  if (backend_registered)
    {
      munlock ("__init_fltk__");

      figure_manager::close_all ();
      graphics_backend::unregister_backend (FLTK_BACKEND_NAME);
      backend_registered = false;

      octave_value_list args;
      args(0) = "__fltk_redraw__";
      feval ("remove_input_event_hook", args, 0);

      // FIXME ???
      Fl::wait (fltk_maxtime);
    }

  octave_value retval;
  return retval;        
}

DEFUN_DLD (__fltk_maxtime__, args, ,"")
{
  octave_value retval = fltk_maxtime;

  if (args.length () == 1)
    {
      if (args(0).is_real_scalar ())
        fltk_maxtime = args(0).double_value ();
      else
        error ("argument must be a real scalar");
    }

  return retval;
}

DEFUN_DLD (fltk_mouse_wheel_zoom, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fltk_mouse_wheel_zoom ([@var{speed}])\n\
Returns the current mouse wheel zoom factor in the fltk backend.  If\n\
the @var{speed} argument is given, set the mouse zoom factor to this\n\
value.\n\
@end deftypefn")
{
  octave_value retval = wheel_zoom_speed;

  if (args.length () == 1)
    {
      if (args(0).is_real_scalar ())
        wheel_zoom_speed = args(0).double_value ();
      else
        error ("argument must be a real scalar");
    }

  return retval;
}

#endif
