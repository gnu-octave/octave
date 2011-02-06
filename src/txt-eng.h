/*

Copyright (C) 2009-2011 Michael Goffioul

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

#if ! defined (txt_eng_h)
#define txt_eng_h 1

#include "base-list.h"

class text_element;
class text_element_string;
class text_element_list;
class text_subscript_element;
class text_superscript_element;

class text_processor;

class
OCTINTERP_API
text_element
{
public:
  text_element (void) { }

  virtual ~text_element (void) { }

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
      : text_element (), str (s) { }

  ~text_element_string (void) { }

  std::string string_value (void) const { return str; }

  void accept (text_processor& p);

private:
  std::string str;

private:
  text_element_string (const text_element_string &);
};

class
OCTINTERP_API
text_element_list :
    public text_element,
    public octave_base_list<text_element *>
{
public:
  text_element_list (void)
      : text_element (), octave_base_list<text_element*> () { }

  ~text_element_list (void)
    {
      while (! empty ())
        {
          iterator it = begin ();
          delete (*it);
          erase (it);
        }
    }

  void accept (text_processor& p);
};

class
OCTINTERP_API
text_subscript_element : public text_element_list
{
public:
  text_subscript_element (void)
      : text_element_list () { }

  ~text_subscript_element (void) { }

  void accept (text_processor& p);
};

class
OCTINTERP_API
text_superscript_element : public text_element_list
{
public:
  text_superscript_element (void)
      : text_element_list () { }

  ~text_superscript_element (void) { }

  void accept (text_processor& p);
};

class
OCTINTERP_API
text_processor
{
public:
  virtual void visit (text_element_string& e) = 0;

  virtual void visit (text_element_list& e)
    {
      for (text_element_list::iterator it = e.begin ();
           it != e.end (); ++it)
        {
          (*it)->accept (*this);
        }
    }

  virtual void visit (text_subscript_element& e)
    { visit (dynamic_cast<text_element_list&> (e)); }

  virtual void visit (text_superscript_element& e)
    { visit (dynamic_cast<text_element_list&> (e)); }

  virtual void reset (void) { }

protected:
  text_processor (void) { }

  virtual ~text_processor (void) { }
};

#define TEXT_ELEMENT_ACCEPT(cls) \
inline void \
cls::accept (text_processor& p) \
{ p.visit (*this); }

TEXT_ELEMENT_ACCEPT(text_element_string)
TEXT_ELEMENT_ACCEPT(text_element_list)
TEXT_ELEMENT_ACCEPT(text_subscript_element)
TEXT_ELEMENT_ACCEPT(text_superscript_element)

class
OCTINTERP_API
text_parser
{
public:
  text_parser (void) { }

  virtual ~text_parser (void) { }

  virtual text_element* parse (const std::string& s) = 0;
};

class
OCTINTERP_API
text_parser_none : public text_parser
{
public:
  text_parser_none (void) : text_parser () { }

  ~text_parser_none (void) { }

  // FIXME: is it possible to use reference counting to manage the
  // memory for the object returned by the text parser?  That would be
  // preferable to having to know when and where to delete the object it
  // creates...

  text_element* parse (const std::string& s)
    {
      return new text_element_string (s);
    }
};

class
OCTINTERP_API
text_parser_tex : public text_parser
{
public:
  text_parser_tex (void) : text_parser () { }

  ~text_parser_tex (void) { }

  // FIXME: is it possible to use reference counting to manage the
  // memory for the object returned by the text parser?  That would be
  // preferable to having to know when and where to delete the object it
  // creates...

  text_element* parse (const std::string& s);

 private:
  text_element_list lst;
  mutable size_t anchor;
  
  std::string getargument(const std::string& s, size_t start) const;
  
  size_t matchbrace(const std::string& s, size_t start) const;
  


};

#endif
