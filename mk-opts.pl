#! /usr/bin/perl
#
# Copyright (C) 2002-2011 John W. Eaton
#
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

# Generate option handling code from a simpler input files for
# Octave's functions like lsode, dassl, etc.

# FIXME:
#
# * Improve default documentation and/or individual documentation
#   in data files. 
#
# * Fix print/show code to display/return something more informative
#   for special values (for example, -1 ==> infinite in some cases).
#   Probably need more information in the data files for this.

# Input file format:
#
# CLASS = string
# FCN_NAME = string
# INCLUDE = file
# DOC_STRING doc END_DOC_STRING
# OPTION
#   NAME = string
#   DOC_ITEM doc END_DOC_ITEM
#   TYPE = string
#   SET_ARG_TYPE = string   (optional, defaults to TYPE)
#   INIT_VALUE = string | INIT_BODY code END_INIT_BODY
#   SET_EXPR = string | SET_BODY code END_SET_BODY | SET_CODE code END_SET_CODE
# END_OPTION
#
# END_* must appear at beginning of line (whitespace ignored).

use Getopt::Long;

$opt_emit_opt_class_header = 0;
$opt_emit_opt_handler_fcns = 0;
$opt_debug = 0;

GetOptions ("opt-class-header" => \$opt_emit_opt_class_header,
            "opt-handler-fcns" => \$opt_emit_opt_handler_fcns,
            "debug" => \$opt_debug);

if (@ARGV == 1)
  {
    $INFILE = shift @ARGV;
    open (INFILE) || die "unable to open input file $INFILE";
  }
else
  {
    die "usage: mk-opts.pl [options] FILE";
  }

$opt_num = 0;

&parse_input;

&process_data;

FOO:
  {
    $opt_emit_opt_class_header && do { &emit_opt_class_header; last FOO; };

    $opt_emit_opt_handler_fcns && do { &emit_opt_handler_fcns; last FOO; };

    $opt_debug && do { &emit_options_debug; last FOO; };
  }

sub parse_input
{
  local ($have_doc_string);

  while (<INFILE>)
    {
      next if (/^\s*$/);
      next if (/^\s*#.*$/);

      if (/^\s*OPTION\s*$/)
        {
          &parse_option_block;
        }
      elsif (/^\s*CLASS\s*=\s*"(\w+)"\s*$/)
        {
          die "duplicate CLASS" if ($class ne "");
          $CLASS = $1;
          $class_name = "${CLASS}_options";
          $struct_name = "${class_name}_struct";
          $static_table_name = "${class_name}_table";
        }
      elsif (/^\s*FCN_NAME\s*=\s*"(\w+)"\s*$/)
        {
          die "duplicate FCN_NAME" if ($fcn_name ne "");
          $fcn_name = $1;
        }
      elsif (/^\s*INCLUDE\s*=\s*"(\S+)"\s*$/)
        {
          $include = "${include}#include <$1>\n";
	}
      elsif (/^\s*DOC_STRING\s*$/)
        {
          die "duplicate DOC_STRING" if ($have_doc_string);
          &parse_doc_string;
          $have_doc_string = 1;
        }
      else
        {
	  die "mk-opts.pl: unknown command: $_\n"
	}
    }
}

sub parse_option_block
{
  local ($have_doc_item, $have_init_body, $have_set_body, $have_set_code);

  while (<INFILE>)
    {
      next if (/^\s*$/);

      die "missing END_OPTION" if (/^\s*OPTION\s*$/);

      last if (/^\s*END_OPTION\s*$/);

      if (/^\s*NAME\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate NAME" if ($name[$opt_num] ne "");
          $name[$opt_num] = $1;
          ($opt[$opt_num] = $name[$opt_num]) =~ s/\s+/_/g;
          $optvar[$opt_num] = "x_$opt[$opt_num]";
          $kw_tok[$opt_num] = [ split (/\s+/, $name[$opt_num]) ];
          $n_toks[$opt_num] = @{$kw_tok[$opt_num]};
        }
      elsif (/^\s*DOC_ITEM\s*$/)
        {
          die "duplicate DOC_ITEM" if ($have_doc_item);
          &parse_doc_item;
          $have_doc_item = 1;
        }
      elsif (/^\s*TYPE\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate TYPE" if ($type[$opt_num] ne "");
          $type[$opt_num] = $1;
        }
      elsif (/^\s*SET_ARG_TYPE\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate SET_ARG_TYPE" if ($set_arg_type[$opt_num] ne "");
          $set_arg_type[$opt_num] = $1;
        }
      elsif (/^\s*INIT_VALUE\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate INIT_VALUE" if ($init_value[$opt_num] ne "");
          $init_value[$opt_num] = $1;
        }
      elsif (/^\s*SET_EXPR\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate SET_EXPR" if ($set_expr[$opt_num] ne "");
          $set_expr[$opt_num] = $1;
        }
      elsif (/^\s*INIT_BODY\s*$/)
        {
          die "duplicate INIT_BODY" if ($have_init_body);
          &parse_init_body;
          $have_init_body = 1;
        }
      elsif (/^\s*SET_BODY\s*$/)
        {
          die "duplicate SET_BODY" if ($have_set_body);
          &parse_set_body;
          $have_set_body = 1;
        }
      elsif (/^\s*SET_CODE\s*$/)
        {
          die "duplicate SET_CODE" if ($have_set_code);
          &parse_set_code;
          $have_set_code = 1;
        }
    }

  if ($set_arg_type[$opt_num] eq "")
    {
      $set_arg_type[$opt_num] = $type[$opt_num]
    }
  else
    {
      $set_arg_type[$opt_num]
        = &substopt ($set_arg_type[$opt_num], $optvar[$opt_num],
                     $opt[$opt_num], $type[$opt_num]);
    }

  $opt_num++;
}

sub process_data
{
  $max_tokens = &max (@n_toks);

  &get_min_match_len_info ($max_tokens);

  $fcn_name = lc ($CLASS) if ($fcn_name eq "");
    
  $opt_fcn_name = "${fcn_name}_options" if ($opt_fcn_name eq "");

  $static_object_name = "${fcn_name}_opts";

  if ($doc_string eq "")
    {
      $doc_string = "When called with two arguments, this function\\n\\
allows you set options parameters for the function \@code{$fcn_name}.\\n\\
Given one argument, \@code{$opt_fcn_name} returns the value of the\\n\\
corresponding option.  If no arguments are supplied, the names of all\\n\\
the available options and their current values are displayed.\\n\\\n";
    }
}

sub get_min_match_len_info
{
  local ($max_tokens) = @_;

  local ($i, $j, $k);

  for ($i = 0; $i < $opt_num; $i++)
    {
      for ($j = 0; $j < $max_tokens; $j++)
        {
	  $min_tok_len_to_match[$i][$j] = 0;
        }

      $min_toks_to_match[$i] = 1;

    L1: for ($k = 0; $k < $opt_num; $k++)
        {
	  local ($duplicate) = 1;

          if ($i != $k)
            {
            L2: for ($j = 0; $j < $max_tokens; $j++)
                {
                  if ($j < $n_toks[$i])
                    {
                      if ($kw_tok[$i][$j] eq $kw_tok[$k][$j])
                        {
                          if ($min_tok_len_to_match[$i][$j] == 0)
                            {
                              $min_tok_len_to_match[$i][$j] = 1;
                            }

                          $min_toks_to_match[$i]++;
                        }
                      else
                        {
			  $duplicate = 0;

			  if ($min_tok_len_to_match[$i][$j] == 0)
			    {
			      $min_tok_len_to_match[$i][$j] = 1;
			    }

                          local (@s) = split (//, $kw_tok[$i][$j]);
                          local (@t) = split (//, $kw_tok[$k][$j]);

                          local ($n, $ii);
                          $n = scalar (@s);
                          $n = scalar (@t) if (@t < $n);

                          for ($ii = 0; $ii < $n; $ii++)
                            {
                              if ("$s[$ii]" eq "$t[$ii]")
                                {
				  if ($ii + 2 > $min_tok_len_to_match[$i][$j])
				    {
				      $min_tok_len_to_match[$i][$j]++;
				    }
                                }
                              else
                                {
                                  last L2;
                                }
                            }

                          last L1;
                        }
                    }
		  else
		    {
		      die "ambiguous options \"$name[$i]\" and \"$name[$k]\"" if ($duplicate);
		    }
                }
            }
        }
    }
}

sub parse_doc_string
{
  while (<INFILE>)
    {
      last if (/^\s*END_DOC_STRING\s*$/);

      $doc_string .= $_;
    }

  $doc_string =~ s/\n/\\n\\\n/g;
}

sub parse_doc_item
{
  while (<INFILE>)
    {
      last if (/^\s*END_DOC_ITEM\s*$/);

      $doc_item[$opt_num] .= $_;
    }

  $doc_item[$opt_num] =~ s/\n/\\n\\\n/g;
}

sub parse_init_body
{
  while (<INFILE>)
    {
      last if (/^\s*END_INIT_BODY\s*$/);

      $init_body[$opt_num] .= $_;
    }
}

sub parse_set_body
{
  while (<INFILE>)
    {
      last if (/^\s*END_SET_BODY\s*$/);

      $set_body[$opt_num] .= $_;
    }
}

sub parse_set_code
{
  while (<INFILE>)
    {
      last if (/^\s*END_SET_CODE\s*$/);

      $set_code[$opt_num] .= $_;
    }
}

sub emit_copy_body
{
  local ($pfx, $var) = @_;

  for ($i = 0; $i < $opt_num; $i++)
    {
      print "${pfx}$optvar[$i] = ${var}.$optvar[$i];\n";
    }

  print "${pfx}reset = ${var}.reset;\n";
}

## To silence GCC warnings, we create an initialization list even
## though the init function actually does the work of initialization.

sub emit_default_init_list
{
  local ($prefix) = @_;

  for ($i = 0; $i < $opt_num; $i++)
    {
      if ($i == 0)
        {
          $pfx = "";
        }
      else
        {
          $pfx = $prefix;
        }

      print "${pfx}$optvar[$i] (),\n";
    }

  print "${prefix}reset ()\n";
}

sub emit_copy_ctor_init_list
{
  local ($prefix, $var) = @_;

  for ($i = 0; $i < $opt_num; $i++)
    {
      if ($i == 0)
        {
          $pfx = "";
        }
      else
        {
          $pfx = $prefix;
        }

      print "${pfx}$optvar[$i] ($var.$optvar[$i]),\n";
    }

  print "${prefix}reset ($var.reset)\n";
}

sub emit_opt_class_header
{
  local ($i, $s);

  print "// DO NOT EDIT!
// Generated automatically from $INFILE.

#if !defined (octave_${class_name}_h)
#define octave_${class_name}_h 1

#include <cfloat>
#include <cmath>

${include}

class
${class_name}
{
public:

  ${class_name} (void)
    : ";

  &emit_default_init_list ("      ");

  print "    {
      init ();
    }

  ${class_name} (const ${class_name}& opt)
    : ";

  &emit_copy_ctor_init_list ("      ", "opt");

  print "    { }

  ${class_name}& operator = (const ${class_name}& opt)
    {
      if (this != &opt)
        {\n";

  &emit_copy_body ("          ", "opt");

  print "        }

      return *this;
    }

  ~${class_name} (void) { }\n";

  print "\n  void init (void)\n    {\n";

  for ($i = 0; $i < $opt_num; $i++)
    {
      if ($init_value[$i])
        {
          print "      $optvar[$i] = $init_value[$i];\n";
        }
      elsif ($init_body[$i])
        {
          $s = &substopt ($init_body[$i], $optvar[$i], $opt[$i], $type[$i]);
          chop ($s);
          $s =~ s/^\s*/      /g;
          $s =~ s/\n\s*/\n      /g;
          print "$s\n";
        }
    }

  print "      reset = true;
    }\n";

  ## For backward compatibility and because set_options is probably
  ## a better name in some contexts:

  print "\n  void set_options (const ${class_name}& opt)
    {\n";

  &emit_copy_body ("      ", "opt");

  print "    }\n\n  void set_default_options (void) { init (); }\n";

  for ($i = 0; $i < $opt_num; $i++)
    {
      if ($set_expr[$i])
        {
          &emit_set_decl ($i);

          print "\n    { $optvar[$i] = $set_expr[$i]; reset = true; }\n";
        }
      elsif ($set_body[$i])
        {
          &emit_set_decl ($i);

          $s = &substopt ($set_body[$i], $optvar[$i], $opt[$i], $type[$i]);
          chop ($s);
          $s =~ s/^/  /g;
          $s =~ s/\n/\n  /g;
          print "\n    {\n$s\n      reset = true;\n    }\n";
        }
      elsif ($set_code[$i])
        {
          $s = &substopt ($set_code[$i], $optvar[$i], $opt[$i], $type[$i]);
          chop ($s);
          $s =~ s/^  //g;
          $s =~ s/\n  /\n/g;
          print "\n$s\n";
        }
    }

  for ($i = 0; $i < $opt_num; $i++)
    {
      print "  $type[$i] $opt[$i] (void) const\n    { return $optvar[$i]; }\n\n";
    }

  print "private:\n\n";

  for ($i = 0; $i < $opt_num; $i++)
    {
      print "  $type[$i] $optvar[$i];\n";
    }

  print "\nprotected:\n\n  bool reset;\n};\n\n#endif\n";
}

sub emit_set_decl
{
  local ($i) = @_;

  print "
  void set_$opt[$i] ($set_arg_type[$i] val)";
}

sub emit_opt_handler_fcns
{
  local ($i);
  my $header = $INFILE;
  $header =~ s/[.]\w*$/.h/; # replace .in with .h
  $header =~ s|^.*/([^/]*)$|$1|; # strip directory part

  print "// DO NOT EDIT!\n// Generated automatically from $INFILE.\n\n";

  print "#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iomanip>
#include <iostream>

#include \"$header\"

#include \"defun-dld.h\"
#include \"pr-output.h\"

#include \"oct-obj.h\"
#include \"utils.h\"
#include \"pager.h\"

static ${class_name} ${static_object_name};\n\n";

  &emit_struct_decl;

  &emit_struct_def;

  &emit_print_function;

  &emit_set_functions;

  &emit_show_function;

  &emit_options_function;
}

sub emit_struct_decl
{
  local ($i);

  print "#define MAX_TOKENS $max_tokens\n\n";

  print "struct ${struct_name}\n{\n";

  print "  const char *keyword;\n";
  print "  const char *kw_tok[MAX_TOKENS + 1];\n";
  print "  int min_len[MAX_TOKENS + 1];\n";
  print "  int min_toks_to_match;\n";

  print "};\n\n";
}

sub emit_struct_def
{
  local ($i);

  print "#define NUM_OPTIONS $opt_num\n\n";

  print "static ${struct_name} ${static_table_name} [] =\n{\n";

  for ($i = 0; $i < $opt_num; $i++)
    {
      &emit_option_table_entry ($i, 0);

      if ($i < $opt_num - 1)
	{
	  print "\n";
	}
    }

  print "};\n\n";
}

sub emit_option_table_entry
{
  local ($i, $empty) = @_;

  local ($k);

  if ($empty)
    {
      print "  { 0,\n";
    }
  else
    {
      print "  { \"$name[$i]\",\n";
    }

  local ($n) = scalar $#{$kw_tok[$i]};
  print "    {";
  for $k (0 .. $max_tokens)
    {
      if ($empty || $k > $n)
        {
          print " 0,";
        }
      else
        {
          print " \"$kw_tok[$i][$k]\",";
        }
    }
  print " },\n";

  print "    {";
  for $k (0 .. $max_tokens)
    {
      if ($empty || $k > $n)
        {
          print " 0,";
        }
      else
        {
          print " $min_tok_len_to_match[$i][$k],";
        }
    }
  print " }, $min_toks_to_match[$i], ";

  print "},\n";
}

sub emit_print_function
{
  local ($i);

  ## FIXME -- determine the width of the table automatically.

  print "static void
print_${class_name} (std::ostream& os)
{
  std::ostringstream buf;

  os << \"\\n\"
     << \"Options for $CLASS include:\\n\\n\"
     << \"  keyword                                             value\\n\"
     << \"  -------                                             -----\\n\";

  $struct_name *list = $static_table_name;\n\n";

  for ($i = 0; $i < $opt_num; $i++)
    {
      print "  {\n    os << \"  \"
        << std::setiosflags (std::ios::left) << std::setw (50)
        << list[$i].keyword
        << std::resetiosflags (std::ios::left)
        << \"  \";\n\n";

      if ($type[$i] eq "double")
        {
          print "    double val = $static_object_name.$opt[$i] ();\n\n";
          print "    os << val << \"\\n\";\n";
        }
      elsif ($type[$i] eq "float")
        {
          print "    float val = $static_object_name.$opt[$i] ();\n\n";
          print "    os << val << \"\\n\";\n";
        }
      elsif ($type[$i] eq "int" || $type[$i] eq "octave_idx_type")
        {
          print "    int val = $static_object_name.$opt[$i] ();\n\n";
          print "    os << val << \"\\n\";\n";
        }
      elsif ($type[$i] eq "std::string")
        {
          print "    os << $static_object_name.$opt[$i] () << \"\\n\";\n";
        }
      elsif ($type[$i] eq "Array<int>" || $type[$i] eq "Array<octave_idx_type>")
        {
	  if ($type[$i] eq "Array<int>")
            {
              $elt_type = "int";
            }
          else
            {
              $elt_type = "octave_idx_type";
            }
          print "    Array<$elt_type> val = $static_object_name.$opt[$i] ();\n\n";
          print "    if (val.length () == 1)
      {
        os << val(0) << \"\\n\";
      }
    else
      {
        os << \"\\n\\n\";
	octave_idx_type len = val.length ();
	Matrix tmp (len, 1);
	for (octave_idx_type i = 0; i < len; i++)
	  tmp(i,0) = val(i);
        octave_print_internal (os, tmp, false, 2);
        os << \"\\n\\n\";
      }\n";
        }
      elsif ($type[$i] eq "Array<double>")
        {
          print "    Array<double> val = $static_object_name.$opt[$i] ();\n\n";
          print "    if (val.length () == 1)
      {
        os << val(0) << \"\\n\";
      }
    else
      {
        os << \"\\n\\n\";
        Matrix tmp = Matrix (ColumnVector (val));
        octave_print_internal (os, tmp, false, 2);
        os << \"\\n\\n\";
      }\n";
        }
      elsif ($type[$i] eq "Array<float>")
        {
          print "    Array<float> val = $static_object_name.$opt[$i] ();\n\n";
          print "    if (val.length () == 1)
      {
        os << val(0) << \"\\n\";
      }
    else
      {
        os << \"\\n\\n\";
        FloatMatrix tmp = FloatMatrix (FloatColumnVector (val));
        octave_print_internal (os, tmp, false, 2);
        os << \"\\n\\n\";
      }\n";
        }
      else
        {
          die ("unknown type $type[$i]");
        }

      print "  }\n\n";
    }

  print "  os << \"\\n\";\n}\n\n";
}

sub emit_set_functions
{
  print "static void
set_${class_name} (const std::string& keyword, const octave_value& val)
{
  $struct_name *list = $static_table_name;\n\n";

  $iftok = "if";

  for ($i = 0; $i < $opt_num; $i++)
    {
      $iftok = "else if" if ($i > 0);

      print "  $iftok (keyword_almost_match (list[$i].kw_tok, list[$i].min_len,
           keyword, list[$i].min_toks_to_match, MAX_TOKENS))
    {\n";

      if ($type[$i] eq "double")
        {
          print "      double tmp = val.double_value ();\n\n";
          print "      if (! error_state)
        $static_object_name.set_$opt[$i] (tmp);\n";
        }
      elsif ($type[$i] eq "float")
        {
          print "      float tmp = val.float_value ();\n\n";
          print "      if (! error_state)
        $static_object_name.set_$opt[$i] (tmp);\n";
        }
      elsif ($type[$i] eq "int" || $type[$i] eq "octave_idx_type")
        {
          print "      int tmp = val.int_value ();\n\n";
          print "      if (! error_state)
        $static_object_name.set_$opt[$i] (tmp);\n";
        }
      elsif ($type[$i] eq "std::string")
        {
          print "      std::string tmp = val.string_value ();\n\n";
          print "      if (! error_state)
        $static_object_name.set_$opt[$i] (tmp);\n";
        }
      elsif ($type[$i] eq "Array<int>" || $type[$i] eq "Array<octave_idx_type>")
        {
          print "      Array<int> tmp = val.int_vector_value ();\n\n";
          print "      if (! error_state)
        $static_object_name.set_$opt[$i] (tmp);\n";
        }
      elsif ($type[$i] eq "Array<double>")
        {
          print "      Array<double> tmp = val.vector_value ();\n\n";
          print "      if (! error_state)
        $static_object_name.set_$opt[$i] (tmp);\n";
        }
      elsif ($type[$i] eq "Array<float>")
        {
          print "      Array<float> tmp = val.float_vector_value ();\n\n";
          print "      if (! error_state)
        $static_object_name.set_$opt[$i] (tmp);\n";
        }
      else
        {
          die ("unknown type $type[$i]");
        }

      print "    }\n";
    }

  print "  else
    {
      warning (\"$opt_fcn_name: no match for `%s'\", keyword.c_str ());
    }
}\n\n";
}

sub emit_show_function
{
  local ($i, $iftok);

  print "static octave_value_list
show_${class_name} (const std::string& keyword)
{
  octave_value retval;

  $struct_name *list = $static_table_name;\n\n";

  $iftok = "if";

  for ($i = 0; $i < $opt_num; $i++)
    {
      $iftok = "else if" if ($i > 0);

      print "  $iftok (keyword_almost_match (list[$i].kw_tok, list[$i].min_len,
           keyword, list[$i].min_toks_to_match, MAX_TOKENS))
    {\n";

      if ($type[$i] eq "double")
        {
          print "      double val = $static_object_name.$opt[$i] ();\n\n";
          print "      retval = val;\n";
        }
      elsif ($type[$i] eq "float")
        {
          print "      float val = $static_object_name.$opt[$i] ();\n\n";
          print "      retval = val;\n";
        }
      elsif ($type[$i] eq "int" || $type[$i] eq "octave_idx_type")
        {
          print "      int val = $static_object_name.$opt[$i] ();\n\n";
          print "      retval = static_cast<double> (val);\n";
        }
      elsif ($type[$i] eq "std::string")
        {
          print "      retval = $static_object_name.$opt[$i] ();\n";
        }
      elsif ($type[$i] eq "Array<int>" || $type[$i] eq "Array<octave_idx_type>")
        {
	  if ($type[$i] eq "Array<int>")
            {
              $elt_type = "int";
            }
          else
            {
              $elt_type = "octave_idx_type";
            }
          print "      Array<$elt_type> val = $static_object_name.$opt[$i] ();\n\n";
          print "      if (val.length () == 1)
        {
          retval = static_cast<double> (val(0));
        }
      else
        {
	  octave_idx_type len = val.length ();
	  ColumnVector tmp (len);
	  for (octave_idx_type i = 0; i < len; i++)
	    tmp(i) = val(i);
          retval = tmp;
        }\n";
        }
      elsif ($type[$i] eq "Array<double>")
        {
          print "      Array<double> val = $static_object_name.$opt[$i] ();\n\n";
          print "      if (val.length () == 1)
        {
          retval = val(0);
        }
      else
        {
          retval = ColumnVector (val);
        }\n";
        }
      elsif ($type[$i] eq "Array<float>")
        {
          print "      Array<float> val = $static_object_name.$opt[$i] ();\n\n";
          print "      if (val.length () == 1)
        {
          retval = val(0);
        }
      else
        {
          retval = FloatColumnVector (val);
        }\n";
        }
      else
        {
          die ("unknown type $type[$i]");
        }

      print "    }\n";
    }

  print "  else
    {
      warning (\"$opt_fcn_name: no match for `%s'\", keyword.c_str ());
    }

  return retval;\n}\n\n";
}

sub emit_options_function
{
  print "DEFUN_DLD ($opt_fcn_name, args, ,
  \"-*- texinfo -*-\\n\\
\@deftypefn {Loadable Function} {} $opt_fcn_name (\@var{opt}, \@var{val})\\n\\
$doc_string\\n\\
Options include\\n\\
\\n\\
\@table \@code\\n\\\n";

  for ($i = 0; $i < $opt_num; $i++)
    {
      print "\@item \\\"$name[$i]\\\"\\n\\\n";
      if ($doc_item[$i] ne "")
	{
	  print "$doc_item[$i]";
	}
    }

  print "\@end table\\n\\\n\@end deftypefn\")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_${class_name} (octave_stdout);
    }
  else if (nargin == 1 || nargin == 2)
    {
      std::string keyword = args(0).string_value ();

      if (! error_state)
        {
          if (nargin == 1)
            retval = show_${class_name} (keyword);
          else
            set_${class_name} (keyword, args(1));
        }
      else
        error (\"$opt_fcn_name: expecting keyword as first argument\");
    }
  else
    print_usage ();

  return retval;
}\n";  
}

sub emit_options_debug
{
  print "CLASS = \"$class\"\n";

  for ($i = 0; $i < $opt_num; $i++)
    {
      $NAME = $name[$i];
      ($OPT = $NAME) =~ s/\s+/_/g;
      $OPTVAR = "x_$OPT";
      $TYPE = $type[$i];
      print "\n";
      print "OPTION\n";
      print "  NAME = \"$NAME\"\n";
      print "  TYPE = \"$TYPE\"\n";
      if ($set_arg_type[$i])
        {
          print eval ("\"  SET_ARG_TYPE = \\\"$set_arg_type[$i]\\\"\"") . "\n";
        }
      if ($init_value[$i])
        {
          print "  INIT_VALUE = \"$init_value[$i]\"\n";
        }
      if ($init_body[$i])
        {
          print "  INIT_BODY\n";
          print &substopt ($init_body[$i]);
          print "  END_INIT_BODY\n";
        }
      if ($set_expr[$i])
        {
          print "  SET_EXPR = \"$set_expr[$i]\"\n";
        }
      if ($set_body[$i])
        {
          print "  SET_BODY\n";
          print &substopt ($set_body[$i]);
          print "  END_SET_BODY\n";
        }
      if ($set_code[$i])
        {
          print "  SET_CODE\n";
          print &substopt ($set_code[$i]);
          print "  END_SET_CODE\n";
        }
      print "END_OPTION\n";
    }
}

sub substopt
{
  local ($string, $OPTVAR, $OPT, $TYPE) = @_;

  $string =~ s/\$OPTVAR/$OPTVAR/g;
  $string =~ s/\$OPT/$OPT/g;
  $string =~ s/\$TYPE/$TYPE/g;

  $string;
}

sub print_assoc_array
{
  local (%t) = @_;

  local ($k);

  foreach $k (keys (%t))
    {
      print "$k: $t{$k}\n";
    }
}

sub max
{
  local ($max) = shift;

  foreach (@_)
    {
      $max = $_ if $max < $_;
    }

  $max;
}
