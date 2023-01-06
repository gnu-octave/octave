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

#if ! defined (octave_text_engine_h)
#define octave_text_engine_h 1

#include "octave-config.h"

#include <memory>
#include <string>

#include "base-list.h"
#include "caseless-str.h"
#include "dMatrix.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class text_element;
class text_element_string;
class text_element_symbol;
class text_element_list;
class text_element_subscript;
class text_element_superscript;
class text_element_combined;
class text_element_fontname;
class text_element_fontsize;
class text_element_fontstyle;
class text_element_color;

class text_processor;

class
OCTINTERP_API
text_element
{
public:
  text_element (void) { }

  virtual ~text_element (void) = default;

  virtual void accept (text_processor& p) = 0;

private:
  text_element (const text_element&);
};

class
OCTINTERP_API
text_element_string : public text_element
{
public:
  text_element_string (const std::string& s = "")
    : text_element (), m_str (s) { }

  ~text_element_string (void) = default;

  std::string string_value (void) const { return m_str; }

  void accept (text_processor& p);

private:
  text_element_string (const text_element_string&);

  //--------

  std::string m_str;
};

class
OCTINTERP_API
text_element_symbol : public text_element
{
public:
  enum { invalid_code = 0xFFFFFFFFU };

  text_element_symbol (int sym)
    : text_element (), m_symbol (sym) { }

  ~text_element_symbol (void) = default;

  int get_symbol (void) const { return m_symbol; }

  uint32_t get_symbol_code (void) const;

  void accept (text_processor& p);

private:
  int m_symbol;
};

class
OCTINTERP_API
text_element_list
  : public text_element, public base_list<text_element *>
{
public:
  text_element_list (void)
    : text_element (), base_list<text_element*> () { }

  text_element_list (text_element *e)
    : text_element (), base_list<text_element*> ()
  {
    push_back (e);
  }

  ~text_element_list (void)
  {
    while (! empty ())
      {
        auto it = begin ();
        delete (*it);
        erase (it);
      }
  }

  void accept (text_processor& p);
};

class
OCTINTERP_API
text_element_subscript : public text_element
{
public:
  text_element_subscript (text_element *e)
    : text_element (), m_elem (e) { }

  text_element_subscript (char c)
    : text_element ()
  { m_elem = new text_element_string (std::string (1, c)); }

  ~text_element_subscript (void)
  { delete m_elem; }

  void accept (text_processor& p);

  text_element * get_element (void) { return m_elem; }

private:
  text_element_subscript (void);

  //--------

  text_element *m_elem;

};

class
OCTINTERP_API
text_element_superscript : public text_element
{
public:
  text_element_superscript (text_element *e)
    : text_element (), m_elem (e) { }

  text_element_superscript (char c)
    : text_element ()
  { m_elem = new text_element_string (std::string (1, c)); }

  ~text_element_superscript (void)
  { delete m_elem; }

  void accept (text_processor& p);

  text_element * get_element (void) { return m_elem; }

private:
  text_element_superscript (void);

  //--------

  text_element *m_elem;

};

class
OCTINTERP_API
text_element_combined : public text_element_list
{
public:
  text_element_combined (text_element *e)
    : text_element_list (e) { }

  text_element_combined (text_element *e1, text_element *e2)
    : text_element_list(e1)
  { push_back (e2); }

  void accept (text_processor& p);
};

class
OCTINTERP_API
text_element_fontstyle : public text_element
{
public:
  enum fontstyle
  {
    normal,
    bold,
    italic,
    oblique
  };

  text_element_fontstyle (fontstyle st)
    : text_element (), m_style (st) { }

  ~text_element_fontstyle (void) = default;

  fontstyle get_fontstyle (void) const { return m_style; }

  void accept (text_processor& p);

private:
  text_element_fontstyle (void);

  //--------

  fontstyle m_style;

};

class
OCTINTERP_API
text_element_fontname : public text_element
{
public:
  text_element_fontname (const std::string& fname)
    : text_element (), m_name (fname) { }

  ~text_element_fontname (void) = default;

  const std::string& get_fontname (void) const { return m_name; }

  void accept (text_processor& p);

private:
  text_element_fontname (void);

  //--------

  std::string m_name;

};

class
OCTINTERP_API
text_element_fontsize : public text_element
{
public:
  text_element_fontsize (double fsize)
    : text_element (), m_size (fsize) { }

  ~text_element_fontsize (void) = default;

  double get_fontsize (void) const { return m_size; }

  void accept (text_processor& p);

private:
  text_element_fontsize (void);

  //--------

  double m_size;

};

class
OCTINTERP_API
text_element_color : public text_element
{
public:
  text_element_color (double r, double g, double b)
    : text_element (), m_rgb (1, 3, 0.0)
  {
    m_rgb(0) = r;
    m_rgb(1) = g;
    m_rgb(2) = b;
  }

  text_element_color (const std::string& cname)
    : text_element (), m_rgb (1, 3, 0.0)
  {
#define ASSIGN_COLOR(r,g,b) { m_rgb(0) = r; m_rgb(1) = g; m_rgb(2) = b; }
    if (cname == "red") ASSIGN_COLOR(1, 0, 0)
      else if (cname == "green") ASSIGN_COLOR(0, 1, 0)
        else if (cname == "yellow") ASSIGN_COLOR(1, 1, 0)
          else if (cname == "magenta") ASSIGN_COLOR(1, 0, 1)
            else if (cname == "blue") ASSIGN_COLOR(0, 0, 1)
              else if (cname == "black") ASSIGN_COLOR(0, 0, 0)
                else if (cname == "white") ASSIGN_COLOR(1, 1, 1)
                  else if (cname == "gray") ASSIGN_COLOR(.5, .5, .5)
                    else if (cname == "darkGreen") ASSIGN_COLOR(0, .5, 0)
                      else if (cname == "orange") ASSIGN_COLOR(1, .65, 0)
                        else if (cname == "lightBlue") ASSIGN_COLOR(0.68, .85, .9)
#undef ASSIGN_COLOR
                        }

  ~text_element_color (void) = default;

  Matrix get_color (void) { return m_rgb; }

  void accept (text_processor& p);

private:
  Matrix m_rgb;
};

class
OCTINTERP_API
text_processor
{
public:
  virtual void visit (text_element_string&) { }

  virtual void visit (text_element_symbol&) { }

  virtual void visit (text_element_list& e)
  {
    for (auto& el_p : e)
      {
        el_p->accept (*this);
      }
  }

  virtual void visit (text_element_subscript& e)
  { e.get_element ()->accept (*this); }

  virtual void visit (text_element_superscript& e)
  { e.get_element ()->accept (*this); }

  virtual void visit (text_element_combined&) { }

  virtual void visit (text_element_fontstyle&) { }

  virtual void visit (text_element_fontname&) { }

  virtual void visit (text_element_fontsize&) { }

  virtual void visit (text_element_color&) { }

  virtual void reset (void) { }

protected:
  text_processor (void) { }

  virtual ~text_processor (void) = default;
};

#define TEXT_ELEMENT_ACCEPT(cls)                \
  inline void                                   \
  cls::accept (text_processor& p)               \
  {                                             \
    p.visit (*this);                            \
  }

TEXT_ELEMENT_ACCEPT(text_element_string)
TEXT_ELEMENT_ACCEPT(text_element_symbol)
TEXT_ELEMENT_ACCEPT(text_element_list)
TEXT_ELEMENT_ACCEPT(text_element_subscript)
TEXT_ELEMENT_ACCEPT(text_element_superscript)
TEXT_ELEMENT_ACCEPT(text_element_combined)
TEXT_ELEMENT_ACCEPT(text_element_fontstyle)
TEXT_ELEMENT_ACCEPT(text_element_fontname)
TEXT_ELEMENT_ACCEPT(text_element_fontsize)
TEXT_ELEMENT_ACCEPT(text_element_color)

class
OCTINTERP_API
text_parser
{
public:
  text_parser (void) { }

  virtual ~text_parser (void) = default;

  virtual text_element * parse (const std::string& s) = 0;

public:
  static text_element * parse (const std::string& s,
                              const caseless_str& interpreter);
};

class
OCTINTERP_API
text_parser_none : public text_parser
{
public:
  text_parser_none (void) : text_parser () { }

  ~text_parser_none (void) = default;

  // FIXME: is it possible to use reference counting to manage the
  // memory for the object returned by the text parser?  That would be
  // preferable to having to know when and where to delete the object it
  // creates...

  text_element * parse (const std::string& s)
  {
    return new text_element_string (s);
  }
};

class
OCTINTERP_API
text_parser_tex : public text_parser
{
public:
  text_parser_tex (void)
    : text_parser (), m_scanner (nullptr), m_buffer_state (nullptr),
      m_result (nullptr)
  { }

  ~text_parser_tex (void)
  { destroy_lexer (); }

  text_element * parse (const std::string& s);

  void * get_scanner (void) { return m_scanner; }

  void set_parse_result (text_element *e) { m_result = e; }

  text_element * get_parse_result (void) { return m_result; }

private:
  bool init_lexer (const std::string& s);

  void destroy_lexer (void);

  //--------

  void *m_scanner;

  void *m_buffer_state;

  text_element *m_result;
};

inline text_element *
text_parser::parse (const std::string& s, const caseless_str& interpreter)
{
  std::unique_ptr<text_parser> parser;

  if (interpreter.compare ("tex"))
    parser.reset (new text_parser_tex ());
  else
    parser.reset (new text_parser_none ());

  return parser->parse (s);
}

OCTAVE_END_NAMESPACE(octave)

#endif
