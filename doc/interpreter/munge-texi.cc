/*

Copyright (C) 1999, 2000, 2002, 2003, 2005, 2007, 2008, 2009 John W. Eaton

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

#if defined (__DECCXX)
#define __USE_STD_IOSTREAM
#endif

#include <cctype>
#include <iostream>
#include <fstream>
#include <string>
#include <map>

#include <cstdlib>
#include <cstring>

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
  std::cerr << "usage: munge-texi DOCSTRING-FILE file ...\n";
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
    {
      // Expand @seealso commands to Texinfo references.
      if (c == '@')
        {
          char buf[16];
          int i = 0;
          buf[i++] = (char) c;
          
          if ((   buf[i++] = (char) is.get ()) == 's'  
              && (buf[i++] = (char) is.get ()) == 'e'
              && (buf[i++] = (char) is.get ()) == 'e'
              && (buf[i++] = (char) is.get ()) == 'a'
              && (buf[i++] = (char) is.get ()) == 'l'
              && (buf[i++] = (char) is.get ()) == 's'
              && (buf[i++] = (char) is.get ()) == 'o'
              && (buf[i++] = (char) is.get ()) == '{')
            {
              doc += "@seealso{";
              
              bool first = true;
              
              // process @seealso parameters
              while ((c = is.get ()) != EOF
                     && c != doc_delim
                     && c != '}') 
                {
                  // ignore whitespace and delimiters
                  while (   c == ' ' 
                         || c == '\t'
                         || c == '\r'
                         || c == '\n'
                         || c == ',')
                    {
                      c = is.get ();
                    }
                    
                  // test for end of @seealso
                  if (c == '}') 
                    break;
                  
                  // get function name
	          std::string function_name;
                  do 
                    function_name += (char) c;
                  while ((c = is.get ()) != EOF
                          && c != doc_delim
                          && c != ' '
                          && c != '\t'
                          && c != '\r'
                          && c != '\n'
                          && c != ','
                          && c != '}');
                  if (first)
                    first = false;
                  else
                    doc += ", ";

		  if (function_name[0] == '@')
		    function_name = "@" + function_name;

                  doc += "@ref{doc-" + function_name + ",,"
		    + function_name + "}";

                  // test for end of @seealso
                  if (c == '}') 
                    break;
                }
              if (c == '}')
                doc += (char) c;
            }
          else
            {
              for (int j = 0; j < i; j++)
                doc += buf[j];
            }
        }
      else
        doc += (char) c;
    }
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
	      buf[i++] = (char) c;

	      if ((   buf[i++] = (char) is.get ()) == 'D'
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

		      size_t len = doc_string.length ();

		      int j = 0;

		      // If there is a leading comment with the file
		      // name, copy it to the output.
		      if (len > 1
			  && doc_string[j] == '@'
			  && doc_string[j+1] == 'c')
			{
			  j = 2;
			  while (doc_string[j++] != '\n')
			    /* find eol */;

			  os << doc_string.substr (0, j);
			}

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

			  if (symbol_name[0] == '@')
			    symbol_name = "@" + symbol_name;

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
    process_doc_file (*argv);

  process_texi_input_file (std::cin, std::cout);

  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
