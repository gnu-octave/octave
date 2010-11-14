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
#include <FL/Fl_Box.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Choice.H>
#include <FL/Fl_Gl_Window.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Menu_Button.H>
#include <FL/Fl_Output.H>
#include <Fl/Fl_File_Chooser.H>
#include <FL/Fl_Window.H>
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
    : Fl_Gl_Window (xx, yy, ww, hh, 0), number (num), renderer (),
      in_zoom (false), zoom_box (),  print_fid (-1)
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
  
  void print (const int fid, const std::string& term)
  {
    print_fid  = fid;
    print_term  = term;
  }

  void resize (int xx, int yy, int ww, int hh)
  {
    Fl_Gl_Window::resize (xx, yy, ww, hh);
    setup_viewport (ww, hh);
    redraw ();
  }
  
private:
  double number;
  opengl_renderer renderer;
  bool in_zoom;
  // (x1,y1,x2,y2)
  Matrix zoom_box;

  int print_fid;
  std::string print_term;

  void setup_viewport (int ww, int hh)
  {
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    glViewport (0, 0, ww, hh);
  }

  void draw (void)
  {
    if (! valid ())
      {
        valid (1);
        setup_viewport (w (), h ());
      }

    if ( print_fid > 0 )
      {
        opengl_renderer *rend = new glps_renderer (print_fid, print_term);
        rend->draw (gh_manager::lookup (number));
        print_fid = -1;
        delete rend;
      }
    else
      {
        renderer.draw (gh_manager::lookup (number));

        if (zoom ())
          overlay ();
      }
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

void script_cb(Fl_Widget*, void* data)
  {
    static_cast<uimenu::properties*> (data)->execute_callback ();
  }
  

class fltk_uimenu
{
public:
  fltk_uimenu (int xx, int yy, int ww, int hh)
    {
      menubar = new
        Fl_Menu_Bar(xx, yy, ww, hh);
    }

  int items_to_show (void)
    {
      //returns the number of visible menu items
      int len = menubar->size ();
      int n = 0;
      for (int t = 0; t < len; t++ )
        {
          const Fl_Menu_Item *m = static_cast<const Fl_Menu_Item*>(&(menubar->menu ()[t]));
          if ((m->label () != NULL) && m->visible ())
            n++;
        }

      return n;
    }

  void show (void)
    {
      menubar->show ();
    }

  void hide (void)
    {
      menubar->hide ();
    }

   bool is_visible (void)
    {
      return menubar->visible ();
    }

  int find_index_by_name (std::string findname)
    {
      // This function is derived from Greg Ercolano's function
      // int GetIndexByName(...), see:
      // http://seriss.com/people/erco/fltk/#Menu_ChangeLabel
      // He agreed via PM that it can be included in octave using GPLv3
      // Kai Habel (14.10.2010)

      std::string menupath;
      for (int t = 0; t < menubar->size (); t++ )
        {
          Fl_Menu_Item *m = const_cast<Fl_Menu_Item*>(&(menubar->menu ()[t]));
          if (m->submenu ())
            {
              // item has submenu
              if (!menupath.empty ())
                menupath += "/";
              menupath += m->label ();

              if (menupath.compare (findname) == 0 )
                return (t);
            }
          else
            {
              // End of submenu? Pop back one level.
              if (m->label () == NULL)
                {
                  std::size_t idx = menupath.find_last_of ("/");
                  if (idx != std::string::npos)
                    menupath.erase (idx);
                  else
                    menupath.clear ();
                  continue;
                }   
              // Menu item?
              std::string itempath = menupath;
              if (!itempath.empty ())
                itempath += "/";              
              itempath += m->label ();

              if (itempath.compare (findname) == 0)
                return (t);
            }
        }
      return (-1);
    }
  
  Matrix find_uimenu_children (uimenu::properties& uimenup) const
    {
      Matrix uimenu_childs = uimenup.get_children ();
      Matrix retval = do_find_uimenu_children (uimenu_childs);
      return retval;
    }

  Matrix find_uimenu_children (figure::properties& figp) const
    {
      Matrix uimenu_childs = figp.get_children ();
      Matrix retval = do_find_uimenu_children (uimenu_childs);
      return retval;
    }
    
  Matrix do_find_uimenu_children (Matrix uimenu_childs) const
    {
      octave_idx_type k = 0;
      
      
      Matrix pos = Matrix (uimenu_childs.numel (), 1);
      
      for (octave_idx_type ii = 0; ii < uimenu_childs.numel (); ii++)
      {
        graphics_object kidgo = gh_manager::get_object (uimenu_childs (ii));
        
        if (kidgo.valid_object() && kidgo.isa ("uimenu"))
          {
            uimenu_childs(k) = uimenu_childs(ii);
            pos(k++) =
              dynamic_cast<uimenu::properties&> (kidgo.get_properties ()).get_position ();
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
    
  void delete_entry(uimenu::properties& uimenup)
    {
      std::string fltk_label = uimenup.get_fltk_label ();
      int idx = find_index_by_name (fltk_label.c_str ());

      if (idx >= 0)
        menubar->remove (idx);
    }

  void update_accelerator (uimenu::properties& uimenup)
    {
      std::string fltk_label = uimenup.get_fltk_label ();
      if (!fltk_label.empty ())
        {
          Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(menubar->find_item (fltk_label.c_str ()));
          if (item != NULL)
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

  void update_callback (uimenu::properties& uimenup)
    {
      std::string fltk_label = uimenup.get_fltk_label ();
      if (!fltk_label.empty ())
        {
          Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(menubar->find_item (fltk_label.c_str ()));
          if (item != NULL)
            {
              if (!uimenup.get_callback ().is_empty ())
                item->callback(static_cast<Fl_Callback*>(script_cb), //callback
                              static_cast<void*>(&uimenup));        //callback data
              else
                item->callback(NULL, static_cast<void*>(0));
            }
        }
    }
            
  void update_enable (uimenu::properties& uimenup)
    {
      std::string fltk_label = uimenup.get_fltk_label ();
      if (!fltk_label.empty ())
        {
          Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(menubar->find_item (fltk_label.c_str ()));
          if (item != NULL)
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
      std::string fltk_label = uimenup.get_fltk_label ();
      if (!fltk_label.empty ())
        {
          Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(menubar->find_item (fltk_label.c_str ()));
          if (item != NULL)
            {
              Matrix rgb = uimenup.get_foregroundcolor_rgb ();
              item->labelcolor(fl_rgb_color(static_cast<uchar>(floor (rgb(0)*255)),
                                            static_cast<uchar>(floor (rgb(1)*255)),
                                            static_cast<uchar>(floor (rgb(2)*255))));
            }
        }
    }

  void update_seperator (uimenu::properties& uimenup)
    {
      // Matlab places the separator before the current
      // menu entry, while fltk places it after. So we need to find
      // the previous item in this menu/submenu. (Kai)
      std::string fltk_label = uimenup.get_fltk_label ();
      if (!fltk_label.empty ())
        {
          int itemflags = 0, idx;
          int curr_idx = find_index_by_name(fltk_label.c_str ());

          for (idx = curr_idx - 1; idx >= 0; idx--)
            { 
              Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(&menubar->menu () [idx]);
              itemflags = item->flags;
              if (item->label () != NULL)
                break;
            }

          if ((idx >= 0) && (idx < menubar->size ()))
            {
              if (uimenup.is_separator ())
                {
                  if (idx >= 0 && !(itemflags & FL_SUBMENU))
                    menubar->mode (idx, itemflags | FL_MENU_DIVIDER);
                }
              else
                menubar->mode (idx, itemflags & (~FL_MENU_DIVIDER));
            }
        }
    }

  void update_visible (uimenu::properties& uimenup)
    {
      std::string fltk_label = uimenup.get_fltk_label ();
      if (!fltk_label.empty ())
        {
          Fl_Menu_Item* item = const_cast<Fl_Menu_Item*>(menubar->find_item (fltk_label.c_str ()));
          if (item != NULL)
            {
              if (uimenup.is_visible ())
                item->show ();
              else
                item->hide ();
            }
        }
    }
    
  void add_entry (uimenu::properties& uimenup)
    {

      std::string fltk_label = uimenup.get_fltk_label ();

      if (!fltk_label.empty ())
        {
          bool item_added = false;
          do
            {  
              const Fl_Menu_Item* item = menubar->find_item(fltk_label.c_str ());
              
              if (item == NULL)
                {
                  Matrix uimenu_ch = find_uimenu_children (uimenup);
                  int len = uimenu_ch.numel ();
                  int flags = 0;
                  if (len > 0) 
                    flags = FL_SUBMENU;
                  if ((len == 0) && (uimenup.is_checked ()))
                    flags += FL_MENU_TOGGLE + FL_MENU_VALUE;
                  menubar->add(fltk_label.c_str (), 0, 0, 0, flags);      
                  item_added = true;
                }
              else
                {
                  //avoid duplicate menulabels
                  std::size_t idx1 = fltk_label.find_last_of ("(");
                  std::size_t idx2 = fltk_label.find_last_of (")");
                  int len = idx2 - idx1;
                  int val = 1;
                  if (len > 0)
                    {
                      std::string valstr = fltk_label.substr (idx1 + 1, len - 1);
                      fltk_label.erase(idx1, len + 1);
                      val = atoi (valstr.c_str ());
                      if ((val > 0) && (val < 99))
                        val++;
                    }
                  std::ostringstream valstream;
                  valstream << val;
                  fltk_label += "(" + valstream.str () + ")";
                }
            }
          while (!item_added);
          uimenup.set_fltk_label (fltk_label);
        }
    }  

  void add_to_menu (uimenu::properties& uimenup)
    {
      Matrix kids = find_uimenu_children (uimenup);
      int len = kids.length ();
      std::string fltk_label = uimenup.get_fltk_label ();
      
      add_entry (uimenup);
      update_foregroundcolor (uimenup);
      update_callback (uimenup);
      update_accelerator (uimenup);
      update_enable (uimenup);
      update_visible (uimenup);
      update_seperator (uimenup);
      
      for (octave_idx_type ii = 0; ii < len; ii++)
        {
          graphics_object kgo = gh_manager::get_object (kids (len - (ii + 1)));
          if (kgo.valid_object ())
            {
              uimenu::properties& kprop = dynamic_cast<uimenu::properties&>(kgo.get_properties ());
              add_to_menu (kprop);
            }
        }
    }

  void add_to_menu (figure::properties& figp)
    {
      Matrix kids = find_uimenu_children (figp);
      int len = kids.length ();
      menubar->clear ();      
      for (octave_idx_type ii = 0; ii < len; ii++)
        {
          graphics_object kgo = gh_manager::get_object (kids (len - (ii + 1)));

          if (kgo.valid_object ())
            {
              uimenu::properties& kprop = dynamic_cast<uimenu::properties&>(kgo.get_properties ());
              add_to_menu (kprop);
            }
        }
    }

  template <class T_prop>
  void remove_from_menu (T_prop& prop)
    {
      Matrix kids;
      std::string type = prop.get_type ();
      kids = find_uimenu_children (prop);    
      int len = kids.length ();

      for (octave_idx_type ii = 0; ii < len; ii++)
        {
          graphics_object kgo = gh_manager::get_object (kids (len - (ii + 1)));

          if (kgo.valid_object ())
            {
              uimenu::properties kprop = dynamic_cast<uimenu::properties&>(kgo.get_properties ());
              remove_from_menu (kprop);
            }
        }

      if (type.compare("uimenu") == 0)
        delete_entry(dynamic_cast<uimenu::properties&>(prop));
      else if (type.compare("figure") == 0)
        menubar->clear ();
    }

  ~fltk_uimenu()
    {
      delete menubar;
    }
    
private:
  Fl_Menu_Bar* menubar;
};

class plot_window : public Fl_Window
{
  friend class fltk_uimenu;
public:
  plot_window (int xx, int yy, int ww, int hh, figure::properties& xfp)
    : Fl_Window (xx, yy, ww, hh, "octave"), window_label (), shift (0),
      ndim (2), fp (xfp), canvas (0), autoscale (0), togglegrid (0),
      panzoom (0), rotate (0), help (0), status (0)
  {
    callback (window_close, static_cast<void*> (this));

    begin ();
    {
      
      uimenu = new
        fltk_uimenu(0, 0, ww, menu_h);
      uimenu->hide ();
      
      canvas = new
        OpenGL_fltk (0, 0, ww , hh - status_h, number ());

      bottom = new 
        Fl_Box (0, 
                hh - status_h, 
                ww, 
                status_h);
      bottom->box(FL_FLAT_BOX);

      ndim = calc_dimensions (gh_manager::get_object (fp.get___myhandle__ ()));

      autoscale = new
        Fl_Button (0,
                   hh - status_h,
                   status_h,
                   status_h,
                   "A");
      autoscale->callback (button_callback, static_cast<void*> (this));
      autoscale->tooltip ("Autoscale");

      togglegrid = new
        Fl_Button (status_h,
                   hh - status_h,
                   status_h,
                   status_h,
                   "G");
      togglegrid->callback (button_callback, static_cast<void*> (this));
      togglegrid->tooltip ("Toggle Grid");

      panzoom = new
        Fl_Button (2 * status_h,
                   hh - status_h,
                   status_h,
                   status_h,
                   "P");
      panzoom->callback (button_callback, static_cast<void*> (this));
      panzoom->tooltip ("Mouse Pan/Zoom");

      rotate = new
        Fl_Button (3 * status_h,
                   hh - status_h,
                   status_h,
                   status_h,
                   "R");
      rotate->callback (button_callback, static_cast<void*> (this));
      rotate->tooltip ("Mouse Rotate");

      if (ndim == 2)
        rotate->deactivate ();

      help = new
        Fl_Button (4 * status_h,
                   hh - status_h,
                   status_h,
                   status_h,
                   "?");
      help->callback (button_callback, static_cast<void*> (this));
      help->tooltip ("Help");

      status = new
        Fl_Output (5 * status_h,
                   hh - status_h,
                   ww > 2*status_h ? ww - status_h : 0,
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
	  if (fp.get_currentaxes ().ok())
	    show_canvas ();
	  else
            hide_canvas ();
        }
    }
    end ();

    status->show ();
    autoscale->show ();
    togglegrid->show ();
    panzoom->show ();
    rotate->show ();

    set_name ();
    resizable (canvas);
    size_range (4*status_h, 2*status_h);
    gui_mode = (ndim == 3 ? rotate_zoom : pan_zoom);
    uimenu->add_to_menu (fp);
    if (uimenu->items_to_show ())
      show_menubar ();
    else
      hide_menubar ();
  }

  ~plot_window (void)
  {
    canvas->hide ();
    status->hide ();
    uimenu->hide ();
    this->hide ();
    delete canvas;
    delete status;
    delete uimenu;
  }

  // FIXME -- this could change.
  double number (void) { return fp.get___myhandle__ ().value (); }

  void print (const int fid, const std::string& term)
  {
    canvas->print (fid, term);

    // Print immediately so the output file will exist when the drawnow
    // command is done.
    mark_modified ();
    Fl::wait (fltk_maxtime);
  }

  void show_menubar (void)
  {
    int dm = menu_h;
    if (uimenu->is_visible ())
      dm = 0;
    canvas->resize (canvas->x (),
                    canvas->y () + dm,
                    canvas->w (),
                    canvas->h () - dm);
    uimenu->show ();
    mark_modified ();
  }
  
  void hide_menubar (void)
  {
    int dm = menu_h;
    if (!uimenu->is_visible ())
      dm = 0;
    canvas->resize (canvas->x (),
                    canvas->y () - dm,
                    canvas->w (),
                    canvas->h () + dm);
    uimenu->hide ();
    mark_modified (); 
  }
  
  void uimenu_update(graphics_handle gh, int id)
  {
    graphics_object uimenu_obj = gh_manager::get_object (gh);
    
    if (uimenu_obj.valid_object () && uimenu_obj.isa ("uimenu"))
      {
        uimenu::properties& uimenup =
          dynamic_cast<uimenu::properties&> (uimenu_obj.get_properties ());
        std::string fltk_label = uimenup.get_fltk_label();
        graphics_object fig = uimenu_obj.get_ancestor("figure");
        figure::properties& figp =
          dynamic_cast<figure::properties&> (fig.get_properties ());
        
        switch(id)
          {
            case base_properties::ID_BEINGDELETED:
              uimenu->remove_from_menu (uimenup);
              break;
            case base_properties::ID_VISIBLE:
              uimenu->update_visible (uimenup);
              break;
            case uimenu::properties::ID_ACCELERATOR:
              uimenu->update_accelerator (uimenup);
              break;
            case uimenu::properties::ID_CALLBACK:
              uimenu->update_callback (uimenup);
              break;
            case uimenu::properties::ID_CHECKED:
              uimenu->add_to_menu (figp);//rebuilding entire menu
              break;
            case uimenu::properties::ID_ENABLE:
              uimenu->update_enable (uimenup);
              break;
            case uimenu::properties::ID_FOREGROUNDCOLOR:
              uimenu->update_foregroundcolor (uimenup);
              break;
            case uimenu::properties::ID_LABEL:
              uimenu->add_to_menu (figp);//rebuilding entire menu
              break;
            case uimenu::properties::ID_POSITION:
              uimenu->add_to_menu (figp);//rebuilding entire menu
              break;
            case uimenu::properties::ID_SEPARATOR:
              uimenu->update_seperator (uimenup);
              break;
          }
          
          if (uimenu->items_to_show ())
            show_menubar ();
          else
            hide_menubar ();
          
          mark_modified();
      }
  }

  void show_canvas (void)
  {
    canvas->show ();
    canvas->make_current ();
  }
  
  void hide_canvas (void)
  {
    canvas->hide ();
  }
  
  void mark_modified (void)
  {
    damage (FL_DAMAGE_ALL);
    canvas->damage (FL_DAMAGE_ALL);
    ndim = calc_dimensions (gh_manager::get_object (fp.get___myhandle__ ()));

    if (ndim == 3)
      rotate->activate ();
    else
      {
        rotate->deactivate ();
        gui_mode = pan_zoom;
      }
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

  // Number of dimensions, 2 or 3.
  int ndim;

  // Interactive Mode
  enum { pan_zoom, rotate_zoom } gui_mode;
 
  // Figure properties.
  figure::properties& fp;

  // Status area height.
  static const int status_h = 20;

  // Menu height
  static const int menu_h = 20;

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
    static_cast<plot_window*> (data)->button_press (ww, data);
  }

  void button_press (Fl_Widget* widg, void*)
  {
    if (widg == autoscale)
      axis_auto ();

    if (widg == togglegrid)
      toggle_grid ();
    
    if (widg == panzoom)
      gui_mode = pan_zoom;
    
    if (widg == rotate && ndim == 3)
      gui_mode = rotate_zoom;

    if (widg == help)
      fl_message ("%s", help_text);
  }

  fltk_uimenu* uimenu;
  OpenGL_fltk* canvas;
  Fl_Box*    bottom;
  Fl_Button* autoscale;
  Fl_Button* togglegrid;
  Fl_Button* panzoom;
  Fl_Button* rotate;
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
    if (fp.get_currentaxes ().ok ())
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
    cbuf.precision (4);
    cbuf.width (6);
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

  void view2status (graphics_object ax)
  {
     if (ax && ax.isa ("axes"))
       {
         axes::properties& ap = 
           dynamic_cast<axes::properties&> (ax.get_properties ());
         std::stringstream cbuf;
         cbuf.precision (4);
         cbuf.width (6);
         Matrix v (1,2,0);
         v = ap.get("view").matrix_value();
         cbuf << "[azimuth: " << v(0) << ", elevation: " << v(1) << "]";
    
         status->value (cbuf.str ().c_str ());
         status->redraw ();
       }
  }
  
  void set_currentpoint (int px, int py)
  {
    if (!fp.is_beingdeleted ())
      {
        Matrix pos (1,2,0);
        pos(0) = px;
        pos(1) = h () - status_h - menu_h - py;
        fp.set_currentpoint (pos);
      }
  }

  void set_axes_currentpoint (graphics_object ax, int px, int py)
  {
    if (ax.valid_object ())
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

  void resize (int xx,int yy,int ww,int hh)
  {
    Fl_Window::resize (xx, yy, ww, hh);

    Matrix pos (1,4,0);
    pos(0) = xx;
    pos(1) = yy;
    pos(2) = ww;
    pos(3) = hh - status_h - menu_h;

    fp.set_position (pos);
  }

  void draw (void)
  {
    Matrix pos = fp.get_position ().matrix_value ();
    Fl_Window::resize (pos(0), pos(1) , pos(2), pos(3) + status_h + menu_h);

    return Fl_Window::draw ();
  }

  int handle (int event)
  {
    static int px0,py0;
    static graphics_object ax0;

    graphics_handle gh;

    graphics_object fig = gh_manager::get_object (fp.get___myhandle__ ());
    int retval = Fl_Window::handle (event);

    // We only handle events which are in the canvas area.
    if (!Fl::event_inside(canvas))
      return retval;

    if (!fp.is_beingdeleted ())
      {
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

                case 'p':
                case 'P':
                  gui_mode = pan_zoom;
                break;

                case 'r':
                case 'R':
                  gui_mode = rotate_zoom;
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

            set_currentpoint (Fl::event_x (), Fl::event_y ());
            
            gh = pixel2axes_or_ca (px0, py0);

            if (gh.ok ())
              {
                ax0 = gh_manager::get_object (gh);
                set_axes_currentpoint (ax0, px0, py0);
              }
              
            fp.execute_windowbuttondownfcn ();

            if (Fl::event_button () == 1 || Fl::event_button () == 3)
              return 1;

            break;

          case FL_DRAG:
            if (fp.get_windowbuttonmotionfcn ().is_defined ())
              {
                set_currentpoint (Fl::event_x (), Fl::event_y ());
                fp.execute_windowbuttonmotionfcn ();
              }
            
            if (Fl::event_button () == 1)
              {
                if (ax0 && ax0.isa ("axes"))
                  {
                    if (gui_mode == pan_zoom)
                      pixel2status (ax0, px0, py0, Fl::event_x (), Fl::event_y ());
                    else
                      view2status (ax0);
                    axes::properties& ap = 
                      dynamic_cast<axes::properties&> (ax0.get_properties ());
                  
                    double x0, y0, x1, y1;
                    Matrix pos = fp.get_position ().matrix_value ();
                    pixel2pos (ax0, px0, py0, x0, y0);
                    pixel2pos (ax0, Fl::event_x (), Fl::event_y (), x1, y1);
                    
                    if (gui_mode == pan_zoom)
                      ap.translate_view (x0 - x1, y0 - y1);
                    else if (gui_mode == rotate_zoom)
                      {
                        double daz, del;
                        daz = (Fl::event_x () - px0) / pos(2) * 360;
                        del = (Fl::event_y () - py0) / pos(3) * 360;
                        ap.rotate_view (del, daz);
                      }

                    px0 = Fl::event_x ();
                    py0 = Fl::event_y ();
                    mark_modified ();
                  }
                return 1;
              }
            else if (Fl::event_button () == 3)
              {
                pixel2status (ax0, px0, py0, Fl::event_x (), Fl::event_y ());
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

  static void print (const graphics_handle& gh , const int fid,  const std::string& term)
  {
    if (instance_ok ())
      instance->do_print (hnd2idx(gh), fid, term);
  }
  
  static void uimenu_update (const graphics_handle& figh, const graphics_handle& uimenuh, const int id)
  {
    if (instance_ok ())
      instance->do_uimenu_update (hnd2idx(figh), uimenuh, id);
  }
  
  static void update_canvas (const graphics_handle& gh, const graphics_handle& ca)
  {
    if (instance_ok ())
      instance->do_update_canvas (hnd2idx(gh), ca);
  }
  
  static void toggle_menubar_visibility (int fig_idx, bool menubar_is_figure)
  {
    if (instance_ok ())
      instance->do_toggle_menubar_visibility (fig_idx, menubar_is_figure);
  }

  static void toggle_menubar_visibility (std::string fig_idx_str, bool menubar_is_figure)
  {
    toggle_menubar_visibility (str2idx (fig_idx_str), menubar_is_figure);
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

  void do_toggle_menubar_visibility (int fig_idx, bool menubar_is_figure)
  {
    wm_iterator win;
    if ((win = windows.find (fig_idx)) != windows.end ())
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

  void do_print (int idx, const int fid,  const std::string& term)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        win->second->print (fid, term);
      }
  }

  void do_uimenu_update (int idx, graphics_handle gh, int id)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        win->second->uimenu_update (gh, id);
      }
  }
  
  void do_update_canvas (int idx, graphics_handle ca)
  {
    wm_iterator win;
    if ((win = windows.find (idx)) != windows.end ())
      {
        if (ca.ok ())
          win->second->show_canvas ();
        else
          win->second->hide_canvas ();
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

  void finalize (const graphics_object& go)
  {
    if (go.isa ("figure"))
      {
        octave_value ov = go.get (caseless_str ("__plot_stream__"));

        if (! ov.is_empty ())
          figure_manager::delete_window (ov.string_value ());
      }
  }

  void uimenu_set_fltk_label(graphics_object uimenu_obj)
  {
    if (uimenu_obj.valid_object ())
      {
        uimenu::properties& uimenup =
          dynamic_cast<uimenu::properties&> (uimenu_obj.get_properties ());
        std::string fltk_label = uimenup.get_label ();  
        graphics_object go = gh_manager::get_object (uimenu_obj.get_parent ());
        if (go.isa ("uimenu"))
          fltk_label = dynamic_cast<const uimenu::properties&> (go.get_properties ()).get_fltk_label ()
                     + "/"
                     + fltk_label;
        else if (go.isa ("figure"))
          ;
        else
          error("unexpected parent object\n");
        
        uimenup.set_fltk_label(fltk_label);
      }
  }
  
  void update (const graphics_object& go, int id)
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
                case base_properties::ID_VISIBLE:
                  figure_manager::toggle_window_visibility (ov.string_value (), fp.is_visible ());
                  break;
                case figure::properties::ID_MENUBAR:
		  figure_manager::toggle_menubar_visibility (ov.string_value (), fp.menubar_is("figure"));
                  break;
                case figure::properties::ID_NAME:
		case figure::properties::ID_CURRENTAXES:
                  figure_manager::update_canvas (go.get_handle (), fp.get_currentaxes ());
                  break;
                case figure::properties::ID_NUMBERTITLE:
                  figure_manager::set_name (ov.string_value ());
                  break;
              }
          }
      }
    else if (go.isa ("uimenu"))
      {
        if (id == uimenu::properties::ID_LABEL)
          uimenu_set_fltk_label (go);
        
        graphics_object fig = go.get_ancestor("figure");
        figure_manager::uimenu_update(fig.get_handle (), go.get_handle (), id);
      }
  }

  void redraw_figure (const graphics_object& go) const
  {
    figure_manager::mark_modified (go.get_handle ());

    __fltk_redraw__ ();
  }

  void print_figure (const graphics_object& go,
                     const std::string& term,
                     const std::string& file, bool /*mono*/,
                     const std::string& /*debug_file*/) const 
  { 
    int fid;
    std::istringstream istr (file);
    if (istr >> fid)
      {
        figure_manager::print (go.get_handle (), fid, term);
        redraw_figure (go);
      }
    else
      error ("fltk_backend: filename should be fid");
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
