/*

Copyright (C) 2007 Shai Ayal

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

#include "oct.h"
#include "parse.h"
#include "graphics.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gl-render.h"


const char* help_text = "\
Keyboard Shortcuts\n\
a - autoscale\n\
g - toggle grid\n\
\n\
Mouse\n\
left drag - zoom\n\
right click - unzoom\n\
double click - copy coordinates to clipboard\
";

class OpenGL_fltk : public Fl_Gl_Window {
public:
  OpenGL_fltk (int x, int y, int w, int h, double num) :
    Fl_Gl_Window (x, y, w, h, 0),
    number (num)
  {
    // ask for double buffering and a depth buffer
    mode(FL_DEPTH | FL_DOUBLE );
  };
  ~OpenGL_fltk () {};

private:
  double number;
  opengl_renderer renderer;

  void setup_viewport (int _w, int _h) {
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport (0, 0, _w, _h);
    //    glOrtho (0.0, 1, 0.0, 1, -1.0, 1.0);
  }    

  void draw () {
    if (!valid ()) {
      valid (1);
      setup_viewport (w (), h ());
    }

#ifndef MESA
    glDrawBuffer (GL_FRONT_AND_BACK);
#endif // !MESA

    renderer.draw (gh_manager::lookup (number));

#ifndef MESA
      glDrawBuffer (GL_BACK);
#endif // !MESA
  };

  void resize (int _x,int _y,int _w,int _h) {
    Fl_Gl_Window::resize (_x, _y, _w, _h);
    setup_viewport (_w, _h);
    redraw ();
  };

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
  };

};

class plot_window : public Fl_Window {
public:
  plot_window (int _x, int _y, int _w, int _h, double num) :
    Fl_Window (_x, _y, _w, _h, "octave"), 
    _number (num),
    err_props (graphics_handle (), graphics_handle ())
  {
    callback (window_close, static_cast<void*> (this));

    begin();
    {
      canvas = new 
	OpenGL_fltk (0, 0, _w , _h - status_h, num);

      autoscale = new
	Fl_Button (0, 
		   _h - status_h, 
		   status_h,
		   status_h,
		   "A");
      autoscale->callback (button_callback, static_cast<void*> (this));

      help = new
	Fl_Button (status_h, 
		   _h - status_h, 
		   status_h,
		   status_h,
		   "H");
      help->callback (button_callback, static_cast<void*> (this));

      status = new 
	Fl_Output (2*status_h, 
		   _h - status_h, 
		   _w > 2*status_h ? _w - status_h : 0, 
		   status_h, "");
      
      status->textcolor (FL_BLACK);
      status->color (FL_GRAY);
      status->textfont (FL_COURIER);
      status->textsize (10);
      status->box (FL_ENGRAVED_BOX);

      // This allows us to have a valid OpenGL context right away
      canvas->mode (FL_DEPTH | FL_DOUBLE );
      show ();
      canvas->show ();
      canvas->make_current ();
    }
    end ();

    status->show ();
    autoscale->show ();

    resizable (canvas);
    size_range (4*status_h, 2*status_h);

    std::stringstream name;
    name << "octave: figure " << number ();
    label (name.str ().c_str ());
  }

  ~plot_window () {
    canvas->hide();
    status->hide();
    this->hide();
    delete canvas;
    delete status;
  };

  double number () { return _number;};
  
  void mark_modified () { canvas->damage (FL_DAMAGE_ALL); }

private:
  // figure number
  double _number;
  figure::properties err_props;

  // status area height
  static const int status_h = 20;
  
  // window callback
  static void window_close (Fl_Widget* w, void* data)
  {
    octave_value_list args;    
    args(0) = static_cast<plot_window*> (data)-> number (); 
    feval("close",args);
  }

  // button callbacks
  static void button_callback (Fl_Widget* w, void* data) {
    static_cast<plot_window*> (data)-> button_press (w);
  };

  void button_press (Fl_Widget* widg) {
    if (widg == autoscale) std::cout << "AS " << number () << " pressed\n";
    if (widg == help) fl_message (help_text);
  }

  OpenGL_fltk*   canvas;
  Fl_Button*	 autoscale;
  Fl_Button*	 help;
  Fl_Output*     status;


  figure::properties& get_figure_props () {
    graphics_object obj = gh_manager::get_object (_number);
  
    if (obj && obj.isa ("figure"))
      {
	figure::properties& fprops = 
	  dynamic_cast<figure::properties&> (obj.get_properties ());
	return fprops;
      }

    error("OpenGL_fltk:: Internal error -- Not associated with figure!");
    return err_props;
  };

  void pixel2pos (int px, int py, double& x, double& y) const {
    x = static_cast<double> (px) / w ();
    y = 1. - static_cast<double> (py) / (h () - status_h);
  }    

  graphics_handle pixel2axes (int px, int py) {
    
    double x,y;

    pixel2pos (px, py, x, y);

    figure::properties pp = get_figure_props ();
    Matrix children = (get_figure_props ()).get_children ();
    for (octave_idx_type n = 0; n < children.numel (); n++) 
      {
	graphics_object ax = gh_manager::get_object (children (n));
	if (ax) 
	  {
	    if (ax.isa ("axes")) 
	      {
		axes::properties& props = 
		  dynamic_cast<axes::properties&> (ax.get_properties ());
		Matrix pos =  props.get_position (). matrix_value ();
		  
		if (x >= pos(0) && x <= pos(0) + pos(2) 
		    &&
		    y >= pos(1) && y <= pos(1) + pos(3) )
		  return props.get___myhandle__ ();
	      }
	  }
      }
    return graphics_handle ();
  }

  void pixel2status (int px, int py) {
    double x,y;
    std::stringstream cbuf;

    pixel2pos (px, py, x, y);
    cbuf << "[" << x << ", " << y <<"]";
    status->value (cbuf.str ().c_str ());
    status->redraw ();
  }    

  void resize (int _x,int _y,int _w,int _h) 
  {
    Fl_Window::resize (_x, _y, _w, _h);

    Matrix pos (1,4,0);
    pos(0) = _x;
    pos(1) = _y;
    pos(2) = _w;
    pos(3) = _h;

    octave_value_list args;    
    args(0) = number ();
    args(1) = "position";
    args(2) = pos;
    feval("set",args);
  }
 
  int handle (int event) {
    static double x0,y0;
    static graphics_handle h0 = graphics_handle ();
    static bool in_drag = false;

    int retval = Fl_Window::handle (event);

    // we only handle events which are in the canvas area
    if (Fl::event_y () >= h() - status_h)
      return retval;

    switch (event)
      {
      
      case FL_MOVE:
	pixel2status (Fl::event_x (), Fl::event_y ());
	break;
      
      case FL_PUSH:
	if (Fl::event_button () == 1)
	  {
	    pixel2pos (Fl::event_x (), Fl::event_y (), x0, y0);
	    h0 = pixel2axes (Fl::event_x (), Fl::event_y ());
	    return 1;
	  }
	break;

      case FL_DRAG:
	pixel2status (Fl::event_x (), Fl::event_y ());
	if (Fl::event_button () == 1)
	  {
	    in_drag = true;
	    return 1;
	  }
	break;

      case FL_RELEASE:
	if (Fl::event_button () == 1)
	  {
	    // end of drag -- zoom
	    if (in_drag)
	      {
		in_drag = false;
	      }
	    // one click -- select axes
	    else if ( Fl::event_clicks () == 0)
	      {
		if (h0.ok ())
		  get_figure_props ().set_currentaxes (h0. value());
		return 1;
	      }
	  }
	break;
      }

    return retval;
  }

};

class figure_manager {
public:

  static figure_manager& Instance () {
    static figure_manager fm;
    return fm;
  }

  ~figure_manager () {
    close_all ();
  }

  void close_all () {
    wm_iterator win;
    for (win = windows.begin (); win != windows.end (); win++)
      delete (*win).second;
    windows.clear ();
  }

  void new_window (double num) {
    if (windows.find (num) != windows.end ())
      return;

    int x,y,w,h;

    default_size(x,y,w,h);

    windows[num] = new plot_window (x, y, w, h, num);
  };

  void delete_window (double num) {
    wm_iterator win;
    if ( (win=windows.find (num)) != windows.end ())
      {
	delete (*win).second;
	windows.erase (win);
      }
  };

  void mark_modified (double num) {
    wm_iterator win;
    if ( (win=windows.find (num)) != windows.end ())
      {
	(*win).second->mark_modified ();
      }
  };

  Matrix get_size (double num)
  {
    Matrix sz (1, 2, 0.0);

    wm_iterator win;
    if ( (win=windows.find (num)) != windows.end ())
      {
	sz(0) = (*win).second->w ();
	sz(1) = (*win).second->h ();
      }
    
    return sz;
  }

private:
  figure_manager () {};
  figure_manager (const figure_manager& ) {};
  figure_manager& operator = (const figure_manager&) {return *this;};
  // singelton -- hide all of the above


  typedef std::map<double, plot_window*> window_map;
  typedef window_map::iterator wm_iterator;;

  window_map windows;

  void default_size (int& x, int& y, int& w, int& h) {
    x = 10;
    y = 10;
    w = 400;
    h = 300;
  }

};

#define FLTK_BACKEND_NAME "fltk"

class fltk_backend : public base_graphics_backend
{
public:
  fltk_backend (void)
      : base_graphics_backend (FLTK_BACKEND_NAME) { }

  ~fltk_backend (void) { }

  bool is_valid (void) const { return true; }
 
  void close_figure (const octave_value& fh) const
    {
      if (fh.is_real_scalar ())
	  figure_manager::Instance ().delete_window (fh.double_value ());
    }

  void redraw_figure (const graphics_handle& fh) const
    {
      figure_manager::Instance ().mark_modified (fh.value ());
    }

  void print_figure (const graphics_handle& fh, const std::string& term,
		     const std::string& file, bool mono,
		     const std::string& debug_file) const
    {
    }

  Matrix get_canvas_size (const graphics_handle& fh) const
    {
      return figure_manager::Instance ().get_size (fh.value ());
    }

  double get_screen_resolution (void) const
    { 
      // FLTK doesn't give this info
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

bool backend_registered = false;
// call this to init the fltk backend
DEFUN_DLD (__init_fltk__, args, nargout,"")
{
  graphics_backend::register_backend (new fltk_backend);
  backend_registered = true;

  octave_value retval;
  return retval;	
}

// call this to delete the fltk backend
DEFUN_DLD (__remove_fltk__, args, nargout,"")
{
  figure_manager::Instance ().close_all ();
  graphics_backend::unregister_backend (FLTK_BACKEND_NAME);
  backend_registered = false;


  // give FLTK no more than 0.01 sec to do it's stuff
  Fl::wait(1e-2);	
  octave_value retval;
  return retval;	
}

// call this from the idle_callback to refresh windows
DEFUN_DLD (__fltk_redraw__, args, nargout,"internal function for the fltk backend")
{
  octave_value retval;

  if (!backend_registered)
    return retval;

  // we scan all figures and add those which use FLTK as a backend
  graphics_object obj = gh_manager::get_object (0);
  if (obj && obj.isa ("root_figure"))
    {
      base_properties& props = obj.get_properties ();
      Matrix children = props.get_children ();

      for (octave_idx_type n = 0; n < children.numel (); n++) 
        {
          graphics_object fobj = gh_manager::get_object (children (n));
          if (fobj &&  fobj.isa ("figure")) 
	    {
	      figure::properties& fp = 
		dynamic_cast<figure::properties&> (fobj.get_properties ());
	      if (fp.get___backend__ () == FLTK_BACKEND_NAME)
		{
		  figure_manager::Instance ().new_window (children (n));

		  // put figure handle in __plot_stream__ so we know
		  // which window to close. Only do this if necesarry
		  // since it modifies the figure and causes a redraw
		  octave_value ps = fp.get___plot_stream__ ();
		  if (ps.is_real_scalar ())
		    if (ps.double_value () == fp.get___myhandle__ ().value ())
		      continue;

		  octave_value_list oargs;   
		  oargs(0) = fp.get___myhandle__ ().value ();
		  oargs(1) = "__plot_stream__";
		  oargs(2) = fp.get___myhandle__ ().value ();
		  feval ("set" , oargs);
		}
	    }
        }
    }

  // give FLTK no more than 0.01 sec to do it's stuff
  Fl::wait(1e-2);	

  return retval;	
}

/* to init
autoload("__init_fltk__",[pwd(),"/fltk_backend.oct"])
autoload("__remove_fltk__",[pwd(),"/fltk_backend.oct"])
autoload("__fltk_redraw__",[pwd(),"/fltk_backend.oct"])
input_event_hook ("__fltk_redraw__");
__init_fltk__ ();
set(gcf(),"__backend__","fltk")


*/
