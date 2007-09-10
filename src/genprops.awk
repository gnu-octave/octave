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
##   m:  Add the line
##
##         set_NAMEmode ("manual");
##
##       to the type-specific set function.
##
## The 'o' and 'O' qualifiers are only useful when the the property type
## is something other than octave_value.

function emit_decls ()
{
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
	  printf ("  %s get_%s (void) const", type[i], name[i]);

	  if (emit_get[i] == "defn")
	      printf (" { return %s; }\n", name[i]);
	  else
	      printf (";\n");
      }
  }

  if (idx > 0)
      printf ("\n");

  for (i = 1; i <= idx; i++)
  {
      if (emit_set[i])
      {
	  printf ("  void set_%s (const %s& val)", name[i], type[i]);

	  if (emit_set[i] == "defn")
	  {
	      printf ("\n  {\n    if (! error_state)\n      {\n        %s = val;\n",
		      name[i]);
	      if (mode[i])
		  printf ("        set_%smode (\"manual\");\n", name[i]);
	      printf ("        mark_modified ();\n      }\n  }\n\n");
	  }
	  else
	      printf (";\n");
      }

      if (emit_ov_set[i])
      {
	  printf ("  void set_%s (const octave_value& val)", name[i]);

	  if (emit_ov_set[i] == "defn")
	      printf (" { set_%s (%s (val)); }\n\n", name[i], type[i]);
	  else
	      printf (";\n");
      }
  }

  if (idx > 0)
      print "\nprivate:";
}

BEGIN {
    printf ("// DO NOT EDIT!  Generated automatically by genprops.awk.\n\n");
}

/BEGIN_PROPERTIES/ {
    gather = 1;
    idx = 0;
    next;
}

/END_PROPERTIES/ {
    emit_decls();
    gather = 0;
    next;
}

{
  if (gather)
    {
      if (NF < 2 || NF > 4)
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

      mode[idx] = 0;
      emit_get[idx] = "defn";
      emit_set[idx] = "defn";
      if (type[idx] == "octave_value")
	  emit_ov_set[idx] = "";
      else
	  emit_ov_set[idx] = "defn";

      if (NF == field)
        {
	    quals = $field;

	    if (index (quals, "m"))
		mode[idx] = 1;

	    ## There is a custom inline definition for the get function,
	    ## so we don't emit anything.
	    if (index (quals, "g"))
		emit_get[idx] = "";

	    ## There is a custom extern definition for the get function,
	    ## but we still emit the declaration.
	    if (index (quals, "G"))
		emit_get[idx] = "decl";

	    ## There is a custom inline definition for the set function,
	    ## so we don't emit anything.
	    if (index (quals, "s"))
		emit_set[idx] = "";

	    ## There is a custom extern definition for the set function,
	    ## but we still emit the declaration.
	    if (index (quals, "S"))
		emit_set[idx] = "decl";

	    if (type[idx] != "octave_value")
	    {
		## The 'o' and 'O' qualifiers are only useful when the
		## the property type is something other than an
		## octave_value.

		## There is a custom inline definition for the
		## octave_value version of the set function, so we
		## don't emit anything.
		if (index (quals, "o"))
		    emit_ov_set[idx] = "";

		## There is a custom extern definition for the
		## octave_value version of the set function, but we
		## still emit the declaration.
		if (index (quals, "O"))
		    emit_ov_set[idx] = "decl";
	    }
        }

    }
  else
      print $0;
}
