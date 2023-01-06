#! /usr/bin/perl -w

########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

$make_header = 0;
$make_source = 0;

$have_options = 1;

while ($have_options)
{
  $opt = shift (@ARGV);

  if ($opt eq "--header")
    {
      die "mk-builtins.pl: only one of --header or --source may be specified" if ($make_source);

      $make_header = 1;
    }
  elsif ($opt eq "--source")
    {
      die "mk-builtins.pl: only one of --header or --source may be specified" if ($make_header);

      $make_source = 1;
    }
  else
    {
      $srcdir = "$opt";
      $opt = shift (@ARGV);
      die "mk-builtins.pl: '--' must separate SRCDIR from other file names" if ($opt ne "--");
      $have_options = 0;
    }
}

die "usage: mk-builtins.pl --header|--source SRCDIR -- f1 f2 ..." if (! @ARGV);

die "mk-builtins.pl: one of --header or --source must be specified" if (! $make_header && ! $make_source);

if ($make_header)
{
  @method_names = ();
  @fcn_names = ();

  while ($file = shift (@ARGV))
  {
    if (! -f $file)
    {
      $file = "$srcdir/$file";
    }

    ## Generate a list of function names to declare.  We could do
    ## this in one step, but this way keeps the sed patterns a
    ## bit smaller.

    open($fh, "<", $file) || die "mk-builtins.pl: failed to open file $file\n";

    while (<$fh>)
    {
      if (/^[ \t]*DEF(METHOD|UN)[ \t]*\( *([^ ,]*).*$/)
      {
        $name = "F$2";
        $is_method = ($1 eq "METHOD");
      }
      elsif (/^[ \t]*DEF(METHOD|UN)X[ \t]*\( *"[^"]*" *, *([^ ,]*).*$/)
      {
        $name = $2;
        $is_method = ($1 eq "METHOD");
      }

      if ($name)
      {
        if ($is_method)
        {
          push (@method_names, $name);
        }
        else
        {
          push (@fcn_names, $name);
        }

        $name = "";
        $is_method = 0;
      }
    }
  }

  print "// DO NOT EDIT!  Generated automatically by mk-builtins.pl.

#if ! defined (octave_builtin_defun_decls_h)
#define octave_builtin_defun_decls_h 1

#include \"octave-config.h\"

#include \"ovl.h\"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

";

  if ($#method_names)
  {
    print "// Methods\n\n";
  }

  foreach $name (sort (@method_names))
  {
    print "extern OCTINTERP_API octave_value_list
$name (octave::interpreter&, const octave_value_list& = octave_value_list (), int = 0);

";
  }

  if ($#fcn_names)
  {
    print "// Functions\n\n";
  }

  foreach $name (sort (@fcn_names))
  {
    print "extern OCTINTERP_API octave_value_list
$name (const octave_value_list& = octave_value_list (), int = 0);

";
  }

  print "\nOCTAVE_END_NAMESPACE(octave)\n";

  print "\n#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)\n\n";

  foreach $name (sort (@fcn_names))
  {
    print "OCTAVE_DEPRECATED (7, \"use 'octave::$name' instead\")
inline octave_value_list
$name (const octave_value_list& args = octave_value_list (), int nargout = 0)
{
  return octave::$name (args, nargout);
}

";
  }

  ## end OCTAVE_PROVIDE_DEPRECATED_SYMBOLS block
  print "\n\n#endif\n";

  print "\n#endif\n";
}
elsif ($make_source)
{
  print "// DO NOT EDIT!  Generated automatically by mk-builtins.pl.

#if defined (HAVE_CONFIG_H)
#  include \"config.h\"
#endif

#include \"builtin-defun-decls.h\"
#include \"help.h\"
#include \"ovl.h\"
#include \"symtab.h\"
#include \"variables.h\"

OCTAVE_BEGIN_NAMESPACE(octave)

";

  @installer_functions = ();

  foreach $arg (@ARGV)
  {
    $file = $arg;

    if (! -f $file)
    {
      $file = "$srcdir/$file";
    }

    ($fcn = $arg) =~ s,.*/,,;
    $fcn =~ s/\.(cc|cpp|in\.cc|yy|ll)$//;
    $fcn =~ s/-/_/g;
    $fcn = "install_${fcn}_fcns";

    $fcn_header = "\n  static void
  $fcn (symbol_table& symtab)
  {
    std::string file = \"$arg\";";

    open($fh, "<", $file) || die "mk-builtins.pl: failed to open file $file\n";

    ## Find DEFUN or DEFALIAS macros and generate the function calls that
    ## install the built-in functions or function aliases.

    $type = "";
    $const_param = "";
    $fname = "";
    $name = "";
    $alias = "";
    $fcn_body = "";

    %dispatch_map = ();

    while ($line = <$fh>)
    {
      if ($line =~ /^ *DEF(METHOD|UN) *\( *([^ ,]*) *,.*$/)
      {
        $type = "fun";
        $fname = "F$2";
        $name = "$2";
      }
      elsif ($line =~ /^ *DEF(METHOD|UN)X *\( *"([^"]*)" *, *([^ ,]*) *,.*$/)
      {
        $type = "fun";
        $fname = "$3";
        $name = "$2";
      }
      elsif ($line =~ /^ *DEFALIAS *\( *([^ ,]*) *, *([^ )]*) *\).*$/)
      {
        $type = "alias";
        $alias = "$1";
        $name = "$2";
      }

      if ($type eq "fun")
      {
        if (($line = <$fh>) =~ /^ *classes:/)
        {
          $line =~ s/\s*classes:\s*//;
          $line =~ s/\s*$//;
          @classes = split (/\s+/, $line);

          $dispatch_map{$name} = [@classes];
        }

        ## We use the name appended to the "external-doc" tag to find
        ## the docstring for aliases to this function.

        $fcn_body .= "\n    symtab.install_built_in_function (\"$name\", octave_value (new octave_builtin ($fname, \"$name\", file, \"external-doc:$name\")));";

        $type = "";
        $fname = "";
        $name = "";
        $const_param = "";
      }
      elsif ($type eq "alias")
      {
        $fcn_body .= "\n    symtab.alias_built_in_function (\"$alias\", \"$name\");";

        ## Preserve dispatch info (if any) that we have for the
        ## original function.

        if (exists $dispatch_map{$name})
        {
          @classes = @{$dispatch_map{$name}};

          if (@classes)
          {
            $dispatch_map{$alias} = [@classes];
          }
        }

        $type = "";
        $name = "";
        $alias = "";
      }
    }

    foreach $fcn (sort (keys (%dispatch_map)))
    {
      $dispatch_code = "";

      @classes =  @{$dispatch_map{$fcn}};

      foreach $class (@classes)
      {
        $dispatch_code .= "\n    symtab.install_built_in_dispatch (\"$fcn\", \"$class\");";
      }

    if ($dispatch_code)
      {
        $fcn_body .= "\n$dispatch_code";
      }
    }

    if ($fcn_body)
      {
        push (@installer_functions, $fcn);

        print "$fcn_header\n$fcn_body\n  }\n";
      }
  }

  print "
  void
  symbol_table::install_builtins (void)
  {
";

  foreach $fcn (@installer_functions)
  {
    print "    $fcn (*this);\n"
  }

  print "  }\n";
  print "OCTAVE_END_NAMESPACE(octave)\n";
}
