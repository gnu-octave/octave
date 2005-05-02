/*

Copyright (C) 1999 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if defined (__DECCXX)
#define __USE_STD_IOSTREAM
#endif

#include <cctype>
#include <iostream>
#include <fstream>
#include <string>
#include <map>

static const char doc_delim = '';

static std::map<std::string, std::string> help_text;

static void
fatal (const std::string& msg)
{
  std::cerr << msg << "\n";
  exit (1);
}

static void
usage (void)
{
  std::cerr << "usage: munge-texi -d DOCSTRING-FILE file ...\n";
  exit (1);
}

static std::string
extract_symbol_name (std::istream& is)
{
  std::string symbol_name;

  int c;
  while ((c = is.get ()) != EOF && c != '\n')
    symbol_name += (char) c;

  return symbol_name;
}

static std::string
extract_docstring (std::istream& is)
{
  std::string doc;

  int c;
  while ((c = is.get ()) != EOF && c != doc_delim)
    doc += (char) c;

  return doc;
}

static void
skip_comments (std::ifstream& is)
{
  int c;

  bool in_comment = false;

  while ((c = is.get ()) != EOF)
    {
      if (c == '#')
	in_comment = true;
      else if (c == '\n')
	in_comment = false;
      else if (! (in_comment || ::isspace (c)))
	{
	  is.putback (c);
	  break;
	}
    }
}

static void
process_doc_file (const std::string& fname)
{
  std::ifstream infile (fname.c_str ());

  if (infile)
    {
      skip_comments (infile);

      if (infile.get () != doc_delim)
	fatal ("invalid doc file format");

      std::string symbol_name;

      do
	{
	  symbol_name = extract_symbol_name (infile);

	  if (! symbol_name.empty ())
	    {
	      std::string doc_string = extract_docstring (infile);

	      if (help_text.find (symbol_name) != help_text.end ())
		std::cerr << "ignoring duplicate entry for "
			  << symbol_name << "\n";
	      else
		help_text[symbol_name] = doc_string;
	    }
	}
      while (! symbol_name.empty ());
    }
  else
    fatal ("unable to open docfile");
}

static void
process_texi_input_file (std::istream& is, std::ostream& os)
{
  os << "@c DO NOT EDIT!  Generated automatically by munge-texi.\n\n";

  bool bol = true;

  int c;
  while ((c = is.get ()) != EOF)
    {
      if (bol)
	{
	  if (c == '@')
	    {
	      std::string symbol_name;

	      char buf[16];
	      int i = 0;
	      buf[i++] = c;

	      if ((buf[i++] = (char) is.get ()) == 'D'
		  && (buf[i++] = (char) is.get ()) == 'O'
		  && (buf[i++] = (char) is.get ()) == 'C'
		  && (buf[i++] = (char) is.get ()) == 'S'
		  && (buf[i++] = (char) is.get ()) == 'T'
		  && (buf[i++] = (char) is.get ()) == 'R'
		  && (buf[i++] = (char) is.get ()) == 'I'
		  && (buf[i++] = (char) is.get ()) == 'N'
		  && (buf[i++] = (char) is.get ()) == 'G'
		  && (buf[i++] = (char) is.get ()) == '(')
		{
		  while ((c = is.get ()) != EOF && c != ')')
		    symbol_name += (char) c;

		  if (is.eof ())
		    fatal ("end of file while reading @DOCSTRING command");
		  else
		    {
		      std::string doc_string = help_text[symbol_name];

		      int j = 0;
		      while (doc_string[j] == ' ')
			j++;

		      if (doc_string.substr (j, 15) == "-*- texinfo -*-")
			{
			  j += 15;

			  while (isspace (doc_string[j]))
			    j++;

			  // Make `see also' references in functions
			  // possible using @anchor{TAG} (new with
			  // Texinfo 4.0).

			  os << "@anchor{doc-" << symbol_name << "}\n";

			  os << doc_string.substr (j);
			}
		      else
			os << doc_string;
		    }
		}
	      else
		{
		  buf[i] = '\0';
		  os << buf;

		  if (buf[i - 1] == '\n')
		    bol = true;
		}
	    }
	  else
	    os.put ((char) c);
	}
      else
	{
	  if (c == '\n')
	    bol = true;

	  os.put ((char) (c));
	}
    }
}

int
main (int argc, char **argv)
{
  while (*++argv)
    {
      if (! strcmp (*argv, "-d"))
	{
	  if (*++argv)
	    process_doc_file (*argv);
	  else
	    usage ();
	}
      else
	break;
    }

  process_texi_input_file (std::cin, std::cout);

  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
