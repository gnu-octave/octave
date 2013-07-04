/*

Copyright (C) 2013 John W. Eaton

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

#if !defined (octave_workspace_element_h)
#define octave_workspace_element_h 1

#include <string>

class workspace_element
{
public:

  workspace_element (char scope_arg = 'l',
                     const std::string& symbol_arg = "<name>",
                     const std::string& class_name_arg = "<class>",
                     const std::string& value_arg = "<value>",
                     const std::string& dimension_arg = "<dimension>")
    : xscope (scope_arg), xsymbol (symbol_arg),
      xclass_name (class_name_arg), xvalue (value_arg),
      xdimension (dimension_arg)
  { }

  workspace_element (const workspace_element& ws_elt)
    : xscope (ws_elt.xscope), xsymbol (ws_elt.xsymbol),
      xclass_name (ws_elt.xclass_name), xvalue (ws_elt.xvalue),
      xdimension (ws_elt.xdimension)
  { }

  workspace_element operator = (const workspace_element& ws_elt)
  {
    if (this != &ws_elt)
      {
        xscope = ws_elt.xscope;
        xsymbol = ws_elt.xsymbol;
        xclass_name = ws_elt.xclass_name;
        xvalue = ws_elt.xvalue;
        xdimension = ws_elt.xdimension;
      }

    return *this;
  }

  ~workspace_element (void) { }

  char scope (void) const { return xscope; }

  std::string symbol (void) const { return xsymbol; }

  std::string class_name (void) const { return xclass_name; }

  std::string value (void) const { return xvalue; }

  std::string dimension (void) const { return xdimension; }

private:

  // [g]lobal, [p]ersistent, [l]ocal
  char xscope;
  std::string xsymbol;
  std::string xclass_name;
  std::string xvalue;
  std::string xdimension;
};

#endif
