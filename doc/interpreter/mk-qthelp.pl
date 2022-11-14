#!/usr/bin/env perl

################################################################################
# File    : mk-qthelp.pl
# Purpose : Transform Octave HTML documentation into intermediate formats
#           for Qt Help Project (.qhp) and Qt Help Collection Project (.qhcp).
# Usage   : mk-qthelp.pl input_htmldir output_filename
################################################################################
use warnings;              # report warnings for questionable run-time code
use strict qw(refs subs);  # check at compile-time for bad programming style
use File::Basename;        # For splitting paths between dir and file
use File::Spec;            # For combining dirs and filenames into paths

################################################################################
# Extract command line arguments
if ($#ARGV != 1)
{ die "USAGE: $0 input_htmldir output_filename"; }

$htmldir = basename ($ARGV[0]);
$basedir = File::Spec->rel2abs (dirname ($ARGV[1]));
$fname   = File::Spec->catfile ($basedir, basename ($ARGV[1]));

################################################################################
# Parse index.html to retrieve the table of contents
$htmlfname = File::Spec->catfile ($basedir, $htmldir, "index.html");
open (my $HTML, "<", $htmlfname) or die "Unable to open $htmlfname";

# Skip through preamble of file to find start of list
while (defined ($_ = <$HTML>) and ! /^<div class="contents">/ ) {;}
while (defined ($_ = <$HTML>)
         and ! /^<ul class="(?:no-bullet|toc-numbered-mark)">/ ) {;}

die "index.html: reached EOF without finding data start pattern" if eof ($HTML);

$level = 0;
while (<$HTML>)
{
  if (/^\s*<li>/)
  {
    ($href, $text) = m|href="([^"]*)"[^<>]*>(.*)</a>|;
    # Sanitize text
    $text =~ s/<[^>]*>//g;         # remove xml-looking blocks like <code>
    $text =~ s/&rsquo;/&#8217;/g;  # Code for apostrophe

    push (@toc, { "href" => $href, "text" => $text,
                  "level" => $level, "sectionstart" => 0 });
  }
  elsif (/^\s+<ul /)
  {
    $level++;
    # Get last node and amend it to have a section start
    %node = %{ $toc[-1] };
    $node{sectionstart} = 1;
    $toc[-1] = { %node };
  }
  elsif (m|^\s+</ul>|)
  {  $level--; }
  elsif (m|^</ul>$|)
  {  last;  }
  else
  {  die "error:unrecognized input:$htmlfname:$.:$_"; }
}

close ($HTML);

die "Failed to parse index.html" if ($level != 0);

################################################################################
# Parse Function-Index.html to retrieve the function reference
$htmlfname = File::Spec->catfile ($basedir, $htmldir, "Function-Index.html");
open ($HTML, "<", $htmlfname) or die "Unable to open $htmlfname";

# Skip through preamble of file to find start of list
while (defined ($_ = <$HTML>)
         and ! /^<table class="(?:index-fn|fn-entries)/ ) {;}

die "Function-Index.html: reached EOF without finding data start pattern"
  if eof ($HTML);

while (<$HTML>)
{
  if (m|<a href="([^"]*)"><code>(.*)</code>|)
  {
    $href = $1, $name = $2;
    # Keep only the first link for each entry
    if (! $kword{$name})
    { $kword{$name} = $href; }
  }
  elsif (m|^</table>$|)
  { last; }
}

close ($HTML);

die "Failed to parse Function-Index.html" if (! m|^</table>$|);

################################################################################
# Prepare Qt Help Project document (.qhp file)
open (my $FH, ">", "$fname.qhp") or die "Unable to open $fname.qhcp";

$htmlfname = File::Spec->catfile ($htmldir, "index.html");

# Add header to file
print $FH <<__EOT1__;
<?xml version="1.0" encoding="UTF-8"?>
<!--DO NOT EDIT!  Generated automatically by $0-->
<QtHelpProject version="1.0">
  <namespace>org.octave.interpreter-1.0</namespace>
  <virtualFolder>doc</virtualFolder>
  <customFilter name="Octave Manual">
    <filterAttribute>core</filterAttribute>
    <filterAttribute>manual</filterAttribute>
  </customFilter>
  <customFilter name="Octave C++ API">
    <filterAttribute>core</filterAttribute>
    <filterAttribute>cpp</filterAttribute>
  </customFilter>
  <filterSection>
    <filterAttribute>core</filterAttribute>
    <filterAttribute>manual</filterAttribute>
    <toc>
      <section title="GNU Octave Manual" ref="$htmlfname">
__EOT1__

# Print out an XML block for each TOC section
$level = 0;
$indent = 4;
foreach $hashref (@toc)
{
  %node = %{$hashref};
  while ($node{level} < $level)
  {
    # Unindent and close section
    $level--;
    print $FH "  " x ($indent + $level);
    print $FH "</section>\n";
  }
  $level = $node{level};

  print $FH "  " x ($indent + $node{level});
  print $FH qq|<section title="$node{text}" |;
  print $FH qq|ref="|, File::Spec->catfile ($htmldir, $node{href});
  print $FH ($node{sectionstart} ? qq|">\n| : qq|"/>\n|);
}

# Spacer between TOC and Keywords
print $FH <<__EOT2__;
      </section>
    </toc>
    <keywords>
__EOT2__

# Print out sorted keywords
foreach $kw (sort {lc($a) cmp lc($b)} (keys (%kword)))
{
  print $FH qq|      <keyword name="$kw" id="$kw" |;
  print $FH qq|ref="|, File::Spec->catfile ($htmldir, $kword{$kw}), qq|"/>\n|;
}

# Footer of file
$htmlfiles = File::Spec->catfile ($htmldir, "*.html");
$pngfiles = File::Spec->catfile ($htmldir, "*.png");
$cssfiles = File::Spec->catfile ($htmldir, "*.css");
print $FH <<__EOT3__;
    </keywords>
    <files>
      <file>$htmlfiles</file>
      <file>$pngfiles</file>
      <file>$cssfiles</file>
    </files>
  </filterSection>
</QtHelpProject>
__EOT3__

close ($FH);

################################################################################
# Prepare Qt Help Collection Project document (.qhcp file)
open ($FH, ">", "$fname.qhcp") or die "Unable to open $fname.qhcp";

$qhpfile = basename ($fname) . ".qhp";
$qchfile = basename ($fname) . ".qch";

# This is the entire file
print $FH <<__EOT4__;
<?xml version="1.0" encoding="UTF-8"?>
<!--DO NOT EDIT! Generated automatically by $0-->
<QHelpCollectionProject version="1.0">
  <docFiles>
    <generate>
      <file>
        <input>$qhpfile</input>
        <output>$qchfile</output>
      </file>
    </generate>
    <register>
      <file>$qchfile</file>
    </register>
  </docFiles>
</QHelpCollectionProject>
__EOT4__

