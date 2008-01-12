## Copyright (C) 2007 John W. Eaton
##
## This file is part of Octave.
## 
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
## 
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
## 
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.
##
## This script is used to generate the graphics.h file from graphics.h.in.
##
## Lines between the BEGIN_PROPERTIES and END_PROPERTIES markers have
## one of the following formats:
##
##   TYPE NAME
##   TYPE NAME QUALIFIERS
##   mutable TYPE NAME
##   mutable TYPE NAME QUALIFIERS
##
## For each property, we generate a declaration for the property.
##
## If QUALIFIERS is omitted, we generate the following functions directly
## in the class declaration:
##
##   TYPE
##   get_NAME (void) const
##   {
##     return NAME;
##   }
##
##   void
##   set_NAME (const TYPE& val)
##   {
##     if (! error_state)
##       NAME = val;
##   }
##
##   void
##   set_NAME (const octave_value& val)
##   {
##     set_NAME (TYPE (val));
##   }
##
## If present, the QUALIFIERS string may include any of the characters
## g, G, m, s, S, o, O, which have the following meanings:
##
##   g:  There is a custom inline definition for the get function,
##       so we don't emit one.
##
##   G:  There is a custom extern definition for the get function,
##       so we emit only the declaration.
##
##   s:  There is a custom inline definition for the type-specific set
##       function, so we don't emit one.
##
##   S:  There is a custom extern definition for the type-specific set
##       function, so we emit only the declaration.
##
##   o:  There is a custom inline definition for the octave_value version
##       of the set function, so we don't emit one.
##
##   O:  There is a custom extern definition for the octave_value version
##       of the set function, so we emit only the declaration.
##
##   a:  The octave_value version of the set function will use assignment:
##
##         void
##         set_NAME (const octave_value& val)
##         {
##           TYPE tmp (NAME);
##           tmp = val;
##           set_NAME (tmp);
##         }
##
##       This is useful for things like the radio_value classes which
##       use an overloaded assignment operator of the form
##
##         radio_property& operator = (const octave_value& val);
##
##       that preserves the list of possible values, which is different
##       from what would happen if we simply used the
##
##         TYPE (const octave_value&)
##
##       constructor, which creates a new radio_property and so cannot
##       preserve the old list of possible values.
##
##   l:  Add the line
##
##         update_axis_limits ("NAME");
##
##       to the type-specific set function.
##
##   m:  Add the line
##
##         set_NAMEmode ("manual");
##
##       to the type-specific set function.
##
## The 'o' and 'O' qualifiers are only useful when the the property type
## is something other than octave_value.

## simple accessor

function emit_get_accessor (i, rtype, faccess)
{
  printf ("  %s get_%s (void) const", rtype, name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { return %s.%s (); }\n", name[i], faccess);
  else
    printf (";\n");
}

## bool_property

function emit_get_bool (i)
{
  printf ("  bool is_%s (void) const", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { return %s.is_on (); }\n", name[i]);
  else
    printf (";\n");

  emit_get_accessor(i, "std::string", "current_value");
}

## radio_property

function emit_get_radio (i)
{
  printf ("  bool %s_is (const std::string& v) const", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { return %s.is (v); }\n", name[i]);
  else
    printf (";\n");

  emit_get_accessor(i, "std::string", "current_value");
}

## color_property

function emit_get_color (i)
{
  printf ("  bool %s_is_rgb (void) const { return %s.is_rgb (); }\n", name[i], name[i]);

  printf ("  bool %s_is (const std::string& v) const", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { return %s.is (v); }\n", name[i]);
  else
    printf (";\n");
  
  printf ("  Matrix get_%s_rgb (void) const", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { return (%s.is_rgb () ? %s.rgb () : Matrix ()); }\n", name[i], name[i]);
  else
    printf (";\n");

  emit_get_accessor(i, "octave_value", "get");
}

## callback_property

function emit_get_callback (i)
{
  printf ("  void execute_%s (void)", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { %s.execute (); }\n", name[i]);
  else
    printf (";\n");

  emit_get_accessor(i, "octave_value", "get");
}

## data_property

function emit_get_data (i)
{
  emit_get_accessor(i, "NDArray", "array_value");

  printf ("  data_property get_%s_property (void) const { return %s; }\n",
          name[i], name[i]);
}

## common section

function emit_common_declarations ()
{
  printf ("public:\n");
  printf ("  properties (const graphics_handle& mh, const graphics_handle& p);\n\n");
  printf ("  ~properties (void) { }\n\n");
  printf ("  void set (const caseless_str& name, const octave_value& val);\n\n");
  printf ("  octave_value get (void) const;\n\n");
  printf ("  octave_value get (const caseless_str& name) const;\n\n");
  printf ("  std::string graphics_object_name (void) const { return go_name; }\n\n");
  printf ("  static property_list::pval_map_type factory_defaults (void);\n\n");
  printf ("private:\n  static std::string go_name;\n\n");
}

function emit_declarations ()
{
  if (class_name)
    emit_common_declarations();

  if (idx > 0)
    print "private:\n";

  for (i = 1; i <= idx; i++)
    printf ("  %s%s %s;\n", mutable[i] ? "mutable " : "", type[i], name[i]);

  if (idx > 0)
    print "\npublic:\n";

  for (i = 1; i <= idx; i++)
  {
    if (emit_get[i])
    {
      if (type[i] == "array_property" || type[i] == "any_property")
        emit_get_accessor(i, "octave_value", "get");
      else if (type[i] == "handle_property")
        emit_get_accessor(i, "graphics_handle", "handle_value");
      else if (type[i] == "string_property")
        emit_get_accessor(i, "std::string", "string_value");
      else if (type[i] == "double_property")
        emit_get_accessor(i, "double", "double_value");
      else if (type[i] == "data_property")
        emit_get_data(i);
      else if (type[i] == "bool_property")
        emit_get_bool(i);
      else if (type[i] == "radio_property")
        emit_get_radio(i);
      else if (type[i] == "color_property")
        emit_get_color(i);
      else if (type[i] == "callback_property")
        emit_get_callback(i);
      else
      {
        printf ("  %s get_%s (void) const", type[i], name[i]);

        if (emit_get[i] == "definition")
          printf (" { return %s; }\n", name[i]);
        else
          printf (";\n");
      }
      printf ("\n");
    }
  }

  if (idx > 0)
    printf ("\n");

  for (i = 1; i <= idx; i++)
  {
    if (emit_set[i])
    {
      printf ("  void set_%s (const octave_value& val)", name[i], type[i]);

      if (emit_set[i] == "definition")
      {
        printf ("\n  {\n    if (! error_state)\n      {\n        %s = val;\n",
          name[i]);
        if (limits[i])
          printf ("        update_axis_limits (\"%s\");\n", name[i]);
        if (mode[i])
          printf ("        set_%smode (\"manual\");\n", name[i]);
        printf ("        mark_modified ();\n      }\n  }\n\n");
      }
      else
        printf (";\n\n");
    }

##    if (emit_ov_set[i])
##    {
##      printf ("  void set_%s (const octave_value& val)", name[i]);
##
##      if (emit_ov_set[i] == "definition")
##        printf (" { set_%s (%s (val)); }\n\n", name[i], type[i]);
##      else if (emit_ov_set[i] == "assignment")
##      {
##        printf ("\n  {\n    %s tmp (%s);\n    tmp = val;\n    set_%s (tmp);\n  };\n\n",
##                type[i], name[i], name[i], name[i]);
##      }
##      else
##        printf (";\n");
##    }
  }

##  if (idx > 0)
##    print "\nprivate:";
}

function emit_source ()
{
  if (class_name)
  {
    printf ("// ******** %s ********\n\n", class_name) >> filename;

    ## constructor

    printf ("%s::properties::properties (const graphics_handle& mh, const graphics_handle& p)\n", class_name) >> filename;
    printf ("  : base_properties (go_name, mh, p),\n") >> filename;

    for (i = 1; i <= idx; i++)
    {
      printf ("    %s (\"%s\", mh, %s)", name[i], name[i], default[i]) >> filename;
      if (i < idx)
        printf (",") >> filename;
      printf ("\n") >> filename;
    }

    printf ("{\n  init ();\n") >> filename;

##    for (i = 1; i <= idx; i++)
##      printf ("  insert_static_property (\"%s\", %s);\n", name[i], name[i]) >> filename;

    printf ("}\n\n") >> filename;

    ## set method

    printf ("void\n%s::properties::set (const caseless_str& name, const octave_value& val)\n{\n",
            class_name) >> filename;

    for (i = 1; i <= idx; i++)
    {
      printf ("  %sif (name.compare (\"%s\"))\n    set_%s (val);\n",
              (i > 1 ? "else " : ""), name[i], name[i]) >> filename;
    }

    printf ("  else\n    base_properties::set (name, val);\n}\n\n") >> filename;

    ## get "all" method

    printf ("octave_value\n%s::properties::get (void) const\n{\n", class_name) >> filename;
    printf ("  Octave_map m = base_properties::get ().map_value ();\n\n") >> filename;

    for (i = 1; i <= idx; i++)
    {
      printf ("  m.assign (\"%s\", get_%s ()%s);\n", name[i], name[i],
              (type[i] == "handle_property" ? ".as_octave_value ()" : "")) >> filename;
    }

    printf ("\n  return m;\n}\n\n") >> filename;
    
    ## get "one" method

    printf ("octave_value\n%s::properties::get (const caseless_str& name) const\n{\n",
            class_name) >> filename;
    printf ("  octave_value retval;\n\n") >> filename;

    for (i = 1; i<= idx; i++)
    {
      printf ("  %sif (name.compare (\"%s\"))\n",
              (i > 1 ? "else " : ""), name[i]) >> filename;
      printf ("    retval = get_%s ()%s;\n", name[i],
              (type[i] == "handle_property" ? ".as_octave_value ()" : "")) >> filename;
    }

    printf ("  else\n    retval = base_properties::get (name);\n\n") >> filename;
    printf ("  return retval;\n}\n\n") >> filename;

    ## factory defaults method

    printf ("property_list::pval_map_type\n%s::properties::factory_defaults (void)\n{\n",
            class_name) >> filename;
    printf ("  property_list::pval_map_type m;\n\n") >> filename;

    for (i = 1; i <= idx; i++)
    {
      defval = default[i];
      if (type[i] == "radio_property" || type[i] == "color_property")
        defval = gensub (/^.*\{(.*)\}.*$/, "\"\\1\"", "g", defval);
      if (! defval)
        defval = "octave_value ()";
      if (name[i] !~ /__.*/)
        printf ("  m[\"%s\"] = %s%s;\n", name[i], defval,
                (type[i] == "handle_property" ? ".as_octave_value ()" : "")) >> filename;
    }

    printf ("\n  return m;\n}\n\n") >> filename;

    ## go_name static field

    printf ("std::string %s::properties::go_name (\"%s\");\n\n",
            class_name, class_name) >> filename;
  }
}

BEGIN {
  filename = "graphics-props.cc";
  printf ("// DO NOT EDIT!  Generated automatically by genprops.awk.\n\n");
  printf ("// DO NOT EDIT!  Generated automatically by genprops.awk.\n\n") > filename;
}

/BEGIN_PROPERTIES\(.*\)/ {
  gather = 1;
  idx = 0;
  class_name = gensub (/^.*BEGIN_PROPERTIES\((.*)\)/, "\\1", "g");
  next;
}

/BEGIN_PROPERTIES/ {
  gather = 1;
  idx = 0;
  class_name = "";
  next;
}

/END_PROPERTIES/ {
  emit_declarations();
  emit_source();
  gather = 0;
  next;
}

{
  if (gather)
  {
    if (NF < 2)
      next;

    idx++;

    field = 1;

    if ($field == "mutable")
    {
      mutable[idx] = 1;
      field++;
    }
    else
      mutable[idx] = 0;

    type[idx] = $(field++);
    name[idx] = $(field++);

    limits[idx] = 0;
    mode[idx] = 0;
    emit_get[idx] = "definition";
    emit_set[idx] = "definition";
    default[idx] = "";
##    if (type[idx] == "octave_value")
##      emit_ov_set[idx] = "";
##    else
##      emit_ov_set[idx] = "definition";

    if (NF >= field)
    {
      if ($field != ",")
      {
        quals = $(field++);

        if (index (quals, "l"))
          limits[idx] = 1;

        if (index (quals, "m"))
          mode[idx] = 1;

        ## There is a custom inline definition for the get function,
        ## so we don't emit anything.
        if (index (quals, "g"))
          emit_get[idx] = "";

        ## There is a custom extern definition for the get function,
        ## but we still emit the declaration.
        if (index (quals, "G"))
          emit_get[idx] = "declaration";

        ## There is a custom inline definition for the set function,
        ## so we don't emit anything.
        if (index (quals, "s"))
          emit_set[idx] = "";

        ## There is a custom extern definition for the set function,
        ## but we still emit the declaration.
        if (index (quals, "S"))
          emit_set[idx] = "declaration";

##        ## emmit an asignment set function
##        if (index (quals, "a"))
##          emit_ov_set[idx] = "assignment";
##
##        if (type[idx] != "octave_value")
##        {
##          ## The 'o' and 'O' qualifiers are only useful when the
##          ## the property type is something other than an
##          ## octave_value.
##
##          ## There is a custom inline definition for the
##          ## octave_value version of the set function, so we
##          ## don't emit anything.
##          if (index (quals, "o"))
##            emit_ov_set[idx] = "";
##
##          ## There is a custom extern definition for the
##          ## octave_value version of the set function, but we
##          ## still emit the declaration.
##          if (index (quals, "O"))
##            emit_ov_set[idx] = "declaration";
##        }
      }

      if (NF > field && $field == ",")
      {
        field++;

        for (i = field; i <= NF; i++)
          default[idx] = (default[idx] (i > field ? " " : "") $i);
      }
    }

  }
  else
    print $0;
}
