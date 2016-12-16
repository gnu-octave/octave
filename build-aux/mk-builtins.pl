#! /usr/bin/perl -w
#
# Copyright (C) 1996-2016 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

$defun_dld_are_built_in = 0;
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
  elsif ($opt eq "--disable-dl")
    {
      ## If DLD functions are disabled, then DEFUN_DLD functions are
      ## built-in instead of being dynamically loaded so they will also
      ## need to be installed.
      $defun_dld_are_built_in= 1;
    }
  else
    {
      $srcdir = "$opt";
      $opt = shift (@ARGV);
      die "mk-builtins.pl: '--' must separate SRCDIR from other file names" if ($opt ne "--");
      $have_options = 0;
    }
}

die "usage: mk-builtins.pl --header|--source [--disable-dl] SRCDIR -- f1 f2 ..." if (! @ARGV);

die "mk-builtins.pl: one of --header or --source must be specified" if (! $make_header && ! $make_source);

if ($make_header)
{
  print "// DO NOT EDIT!  Generated automatically by mk-builtins.pl.

#if ! defined (octave_builtin_defun_decls_h)
#define octave_builtin_defun_decls_h 1

#include \"octave-config.h\"

#include \"ovl.h\"

";

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
      if (/^[ \t]*DEF(CONSTFUN|UN)[ \t]*\( *([^ ,]*).*$/)
      {
        $name = "F$2";
      }
      elsif (/^[ \t]*DEFUNX[ \t]*\( *"[^"]*" *, *([^ ,]*).*$/)
      {
        $name = $1;
      }
      elsif ($defun_dld_are_built_in)
      {
        if (/^[ \t]*DEFUN_DLD[ \t]*\( *([^ ,]*).*$/)
        {
          $name = "F$1";
        }
        elsif (/^[ \t]*DEFUNX_DLD[ \t]*\( *"[^"]*" *, *([^ ,]*).*$/)
        {
          $name = "$1";
        }
      }

      if ($name)
      {
        print "extern OCTINTERP_API octave_value_list
$name (const octave_value_list& = octave_value_list (), int = 0);

";
        $name = "";
      }
    }
  }

  print "#endif\n";
}
elsif ($make_source)
{
  print "// DO NOT EDIT!  Generated automatically by mk-builtins.pl.

#if defined (HAVE_CONFIG_H)
#  include \"config.h\"
#endif

#include \"defun.h\"
#include \"help.h\"
#include \"ovl.h\"
#include \"variables.h\"
#include \"builtin-defun-decls.h\"
#include \"builtins.h\"

#if defined (quad)
#  undef quad
#endif
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
    $fcn =~ s/\.(cc|cpp|in\.yy|ll)$//;
    $fcn =~ s/-/_/g;
    $fcn = "install_${fcn}_fcns";

    push (@installer_functions, $fcn);

    print "
static void
$fcn (void)
{
  std::string file = \"$arg\";

";

    open($fh, "<", $file) || die "mk-builtins.pl: failed to open file $file\n";

    ## Find DEFUN or DEFALIAS macros and generate the function calls that
    ## install the built-in functions or function aliases.

    $type = "";
    $const = 0;
    $fname = "";
    $name = "";
    $alias = "";

    %dispatch_map = ();

    while ($line = <$fh>)
    {
      if ($line =~ /^ *DEFUN *\( *([^ ,]*) *,.*$/)
      {
        $type = "fun";
        $fname = "F$1";
        $name = "$1";
      }
      elsif ($line =~ /^ *DEFUNX *\( *"([^"]*)" *, *([^ ,]*) *,.*$/)
      {
        $type = "fun";
        $fname = "$2";
        $name = "$1";
      }
      elsif ($line =~ /^ *DEFCONSTFUN *\( *([^ ,]*) *,.*$/)
      {
        $type = "fun";
        $fname = "F$1";
        $name = "$1";
        $const = 1;
      }
      elsif ($line =~ /^ *DEFALIAS *\( *([^ ,]*) *, *([^ )]*) *\).*$/)
      {
        $type = "alias";
        $alias = "$1";
        $name = "$2";
      }
      elsif ($defun_dld_are_built_in)
      {
        if ($line =~ /^ *DEFUN_DLD *\( *([^ ,]*) *,.*$/)
        {
          $type = "fun";
          $fname = "F$1";
          $name = "$1";
        }
        elsif ($line =~ /^ *DEFUNX_DLD *\( *"([^"]*)" *, *([^ ,]*) *,.*$/)
        {
          $type = "fun";
          $fname = "$2";
          $name = "$1";
        }
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

        if ($const)
        {
          print "  install_builtin_function ($fname, \"$name\", file, \"external-doc:$name\", true);\n"
        }
        else
        {
          print "  install_builtin_function ($fname, \"$name\", file, \"external-doc:$name\");\n"
        }

        $type = "";
        $fname = "";
        $name = "";
        $const = 0;
      }
      elsif ($type eq "alias")
      {
        print "  alias_builtin (\"$alias\", \"$name\");\n";

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

    foreach $fcn (keys %dispatch_map)
    {
      print "\n";

      @classes =  @{$dispatch_map{$fcn}};

      foreach $class (@classes)
      {
        print "  install_builtin_dispatch (\"$fcn\", \"$class\");\n";
      }
    }

    print "}\n";
  }

  print "
void
install_builtins (void)
{
";

  foreach $fcn (@installer_functions)
  {
    print "  $fcn ();\n"
  }

  print "}\n";
}
