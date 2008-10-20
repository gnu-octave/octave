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
## g, G, m, s, S, o, O, h, which have the following meanings:
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
##   h:  Make the property hidden
##
##   r:  Make the property read-only. A read-only property is not
##       settable from the global set (caseless_str, octave_value)
##       method, but still has set_X accessor.
##
##   u:  The property has an inline updater method. This effectively
##       add the line
##
##         update_NAME ();
##
##       to the type-specific set function. This line is added before
##       any other update call (like those added by the 'l' or 'm'
##       modifiers.
##
##   U:  Like 'u' modifier except that the updater is not inline.
##       A declaration for the updater function will be emitted.
##
##   f:  The property does not have any factory default value.
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

## double_radio_property

function emit_get_double_radio (i)
{
  printf ("  bool %s_is_double (void) const { return %s.is_double (); }\n", name[i], name[i]);

  printf ("  bool %s_is (const std::string& v) const", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { return %s.is (v); }\n", name[i]);
  else
    printf (";\n");
  
  printf ("  double get_%s_double (void) const", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { return (%s.is_double () ? %s.double_value () : 0); }\n", name[i], name[i]);
  else
    printf (";\n");

  emit_get_accessor(i, "octave_value", "get");
}

## callback_property

function emit_get_callback (i)
{
  printf ("  void execute_%s (const octave_value& data = octave_value ()) const", name[i]);
  
  if (emit_get[i] == "definition")
    printf (" { %s.execute (data); }\n", name[i]);
  else
    printf (";\n");

  emit_get_accessor(i, "octave_value", "get");
}

## array_property

function emit_get_array (i)
{
  emit_get_accessor(i, "octave_value", "get");
}

## common section

function emit_common_declarations ()
{
  printf ("public:\n");
  printf ("  properties (const graphics_handle& mh, const graphics_handle& p);\n\n");
  printf ("  ~properties (void) { }\n\n");
  printf ("  void set (const caseless_str& pname, const octave_value& val);\n\n");
  printf ("  octave_value get (bool all = false) const;\n\n");
  printf ("  octave_value get (const caseless_str& pname) const;\n\n");
  printf ("  property get_property (const caseless_str& pname);\n\n");
  printf ("  std::string graphics_object_name (void) const { return go_name; }\n\n");
  printf ("  static property_list::pval_map_type factory_defaults (void);\n\n");
  printf ("private:\n  static std::string go_name;\n\n");
}

function emit_declarations ()
{
  if (class_name && ! base)
    emit_common_declarations();

  if (idx > 0)
    print (base ? "protected:\n" : "private:\n");

  for (i = 1; i <= idx; i++)
    printf ("  %s%s %s;\n", mutable[i] ? "mutable " : "", type[i], name[i]);

  if (idx > 0)
    print "\npublic:\n";
  
  if (idx > 0)
  {
    printf ("  enum\n  {");
    for (i = 1; i <= idx; i++)
    {
      printf ("%s\n    %s = %d", (i == 1 ? "" : ","), toupper(name[i]), pcount);
      pcount++;
    }
    printf ("\n  };\n\n");
    pcount = (int(pcount/1000)+1)*1000;
  }

  for (i = 1; i <= idx; i++)
  {
    if (emit_get[i])
    {
      if (type[i] == "any_property")
        emit_get_accessor(i, "octave_value", "get");
      else if (type[i] == "handle_property")
        emit_get_accessor(i, "graphics_handle", "handle_value");
      else if (type[i] == "string_property")
        emit_get_accessor(i, "std::string", "string_value");
      else if (type[i] == "double_property")
        emit_get_accessor(i, "double", "double_value");
      else if (type[i] == "double_radio_property")
        emit_get_double_radio(i);
      else if (type[i] == "array_property" \
	       || type[i] == "row_vector_property")
        emit_get_array(i);
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
	if (updaters[i] || limits[i] || mode[i])
	  has_builtin_listeners = 1;
	else
	  has_builtin_listeners = 0;

        printf ("\n  {\n    if (! error_state)\n      {\n        if (%s.set (val, %s))\n          {\n",
          name[i], (has_builtin_listeners ? "false" : "true"));
        if (mode[i])
          printf ("            set_%smode (\"manual\");\n", name[i]);
        if (updater[i])
          printf ("            update_%s ();\n", name[i]);
        if (limits[i])
          printf ("            update_axis_limits (\"%s\");\n", name[i]);
	if (has_builtin_listeners)
	  printf ("            %s.run_listeners (POSTSET);\n", name[i]);
        printf ("            mark_modified ();\n");
	printf ("          }\n");
	if (mode[i])
	  printf ("        else\n          set_%smode (\"manual\");\n", name[i]);
        printf ("      }\n  }\n\n");
      }
      else
        printf (";\n\n");
    }

    if (updater[i] == "extern")
    {
      printf ("  void update_%s (void);\n\n", name[i]);
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

    if (base)
      printf ("base_properties::base_properties (const std::string& ty, const graphics_handle& mh, const graphics_handle& p)\n  : ") >> filename;
    else
    {
      printf ("%s::properties::properties (const graphics_handle& mh, const graphics_handle& p)\n", class_name) >> filename;
      printf ("  : base_properties (go_name, mh, p),\n") >> filename;
    }

    for (i = 1; i <= idx; i++)
    {
      if (ptype[i])
        printf ("    %s (\"%s\", mh, %s)", name[i], name[i], defval[i]) >> filename;
      else
        printf ("    %s (%s)", name[i], defval[i]) >> filename;
      if (i < idx)
        printf (",") >> filename;
      printf ("\n") >> filename;
    }

    printf ("{\n") >> filename;

    for (i = 1; i <= idx; i++)
    {
##    printf ("  insert_static_property (\"%s\", %s);\n", name[i], name[i]) >> filename;
      if (ptype[i])
      {
        printf ("  %s.set_id (%s);\n", name[i], toupper(name[i])) >> filename;
        if (hidden[i])
          printf ("  %s.set_hidden (true);\n", name[i]) >> filename;
      }
    }

    printf ("  init ();\n}\n\n") >> filename;

    ## set method

    if (base)
      printf ("void\nbase_properties::set (const caseless_str& pname, const octave_value& val)\n{\n") >> filename;
    else
      printf ("void\n%s::properties::set (const caseless_str& pname, const octave_value& val)\n{\n",
              class_name) >> filename;

    first = 1;

    for (i = 1; i <= idx; i++)
    {
      if (! readonly[i])
      {
        printf ("  %sif (pname.compare (\"%s\"))\n    set_%s (val);\n",
                (first == 0 ? "else " : ""), name[i], name[i]) >> filename;
        first = 0;
      }
    }

    if (base)
      printf ("  else\n    set_dynamic (pname, val);\n}\n\n") >> filename;
    else
      printf ("  else\n    base_properties::set (pname, val);\n}\n\n") >> filename;

    ## get "all" method

    if (base)
    {
      printf ("octave_value\nbase_properties::get (bool all) const\n{\n") >> filename;
      printf ("  Octave_map m = get_dynamic (all).map_value ();\n\n") >> filename;
    }
    else
    {
      printf ("octave_value\n%s::properties::get (bool all) const\n{\n", class_name) >> filename;
      printf ("  Octave_map m = base_properties::get (all).map_value ();\n\n") >> filename;
    }

    for (i = 1; i <= idx; i++)
    {
      if (hidden[i])
        printf ("  if (all)\n    m.assign (\"%s\", get_%s ()%s);\n", name[i], name[i],
                (type[i] == "handle_property" || type[i] == "graphics_handle" ? ".as_octave_value ()" : "")) >> filename;
      else
        printf ("  m.assign (\"%s\", get_%s ()%s);\n", name[i], name[i],
                (type[i] == "handle_property" || type[i] == "graphics_handle" ? ".as_octave_value ()" : "")) >> filename;
    }

    printf ("\n  return m;\n}\n\n") >> filename;
    
    ## get "one" method

    if (base)
      printf ("octave_value\nbase_properties::get (const caseless_str& pname) const\n{\n") >> filename;
    else
      printf ("octave_value\n%s::properties::get (const caseless_str& pname) const\n{\n",
              class_name) >> filename;
    printf ("  octave_value retval;\n\n") >> filename;

    for (i = 1; i<= idx; i++)
    {
      printf ("  %sif (pname.compare (\"%s\"))\n",
              (i > 1 ? "else " : ""), name[i]) >> filename;
      printf ("    retval = get_%s ()%s;\n", name[i],
              (type[i] == "handle_property" || type[i] == "graphics_handle" ? ".as_octave_value ()" : "")) >> filename;
    }

    if (base)
      printf ("  else\n    retval = get_dynamic (pname);\n\n") >> filename;
    else
      printf ("  else\n    retval = base_properties::get (pname);\n\n") >> filename;
    printf ("  return retval;\n}\n\n") >> filename;

    ## get_property method

    if (base)
      printf ("property\nbase_properties::get_property (const caseless_str& pname)\n{\n") >> filename;
    else
      printf ("property\n%s::properties::get_property (const caseless_str& pname)\n{\n",
              class_name) >> filename;

    for (i = 1; i<= idx; i++)
    {
      if (ptype[i])
      {
        printf ("  %sif (pname.compare (\"%s\"))\n",
                (i > 1 ? "else " : ""), name[i]) >> filename;
        printf ("    return property (&%s, true);\n", name[i]) >> filename;
      }
    }

    if (base)
      printf ("  else\n    return get_property_dynamic (pname);\n") >> filename;
    else
      printf ("  else\n    return base_properties::get_property (pname);\n") >> filename;
    printf ("}\n\n") >> filename;


    ## factory defaults method

    if (base)
    {
      printf ("property_list::pval_map_type\nbase_properties::factory_defaults (void)\n{\n") >> filename;
      printf ("  property_list::pval_map_type m;\n\n") >> filename;
    }
    else
    {
      printf ("property_list::pval_map_type\n%s::properties::factory_defaults (void)\n{\n",
              class_name) >> filename;
      printf ("  property_list::pval_map_type m = base_properties::factory_defaults ();\n\n") >> filename;
    }

    for (i = 1; i <= idx; i++)
    {
      if (factory[i])
      {
        dval = defval[i];
        if (type[i] == "radio_property" || type[i] == "color_property")
      	{
      	  k = index (dval, "{");
    	    dval = substr (dval, k+1);
  	      l = index (dval, "}");
      	  if (k > 0 && l > 0)
	          dval = "\"" + substr (dval, 1, l-1) +  "\"";
    	    else
  	        dval = "octave_value ()";
      	}

        printf ("  m[\"%s\"] = %s%s;\n", name[i], dval,
                (type[i] == "handle_property" || type[i] == "graphics_handle" ? ".as_octave_value ()" : "")) >> filename;
      }
    }

    printf ("\n  return m;\n}\n\n") >> filename;

    ## go_name static field

    if (! base)
      printf ("std::string %s::properties::go_name (\"%s\");\n\n",
              class_name, object_name) >> filename;
  }
}

BEGIN {
  filename = "graphics-props.cc";
  printf ("// DO NOT EDIT!  Generated automatically by genprops.awk.\n\n");
  printf ("// DO NOT EDIT!  Generated automatically by genprops.awk.\n\n") > filename;
  pcount = 0;
}

/BEGIN_PROPERTIES *\(.*\)/ {
  gather = 1;
  idx = 0;
  str = $0;
  beg = index (str, "(") + 1;
  len = index (str, ")") - beg;
  args = substr (str, beg, len);
  n = split (args, arg_list, ",");
  if (n > 0)
      class_name = arg_list[1];
  if (n > 1)
      object_name = arg_list[2];
  else
      object_name = class_name;
  gsub (/ /, "", class_name);
  gsub (/ /, "", object_name);
  base = 0;
  next;
}

/BEGIN_PROPERTIES/ {
  gather = 1;
  idx = 0;
  class_name = "";
  base = 0;
  next;
}

/BEGIN_BASE_PROPERTIES/ {
  gather = 1;
  idx = 0;
  class_name = "base";
  base = 1;
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
    if (NF < 2 || /^[ \t]*\/\//)
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
    ptype[idx] = (type[idx] ~ /^.*_property$/);
    name[idx] = $(field++);

    limits[idx] = 0;
    mode[idx] = 0;
    hidden[idx] = 0;
    readonly[idx] = 0;
    emit_get[idx] = "definition";
    emit_set[idx] = "definition";
    defval[idx] = "";
    updater[idx] = "";
    factory[idx] = 1;
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
        
	## The property is hidden
	if (index (quals, "h"))
	  hidden[idx] = 1;

	## The property is read-only
	if (index (quals, "r"))
	  readonly[idx] = 1;

        ## There is an inline updater method that should be called
        ## from the set method
        if (index (quals, "u"))
          updater[idx] = "inline";
        
	## There is an extern updater method that should be called
        ## from the set method
        if (index (quals, "U"))
          updater[idx] = "extern";

	## There is not factory default value
        if (index (quals, "f"))
          factory[idx] = 0;

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
          defval[idx] = (defval[idx] (i > field ? " " : "") $i);
      }
    }

  }
  else
    print $0;
}
