#!/usr/bin/perl -w

################################################################################
## File: inplace_edit.pl
## Usage: perl inplace_edit.pl 'PERL_CODE' file1 [file2] [...]
## Purpose: Run snippet of PERL_CODE on each line in a file and replace
## existing line with the results of running the code.
## This replaces perl -i -pe 'PERL_CODE' file1 [file2] ...
## due to a problem in Perl 5.28 which restricts the number of files
################################################################################

## Create Perl code from first argument (-e CODE)
eval "sub per_line_code { $ARGV[0]; }";
shift @ARGV;

## Loop over each file
foreach $fname (@ARGV)
{
  rename ($fname, "$fname.$$") or die "Rename failed:$fname:$!";
  open (my $FHI, "<", "$fname.$$") or die "Open failed:$fname.$$:$!";
  open (my $FHO, ">", "$fname") or die "Open failed:$fname:$!";

  ## Loop over each line
  while (<$FHI>)
  {
    per_line_code ();
    print $FHO $_;
  }

  close ($FHI);
  close ($FHO);
  unlink "$fname.$$" or die "Delete failed:$fname.$$:$!";
}
