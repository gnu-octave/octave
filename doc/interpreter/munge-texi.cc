#include <iostream.h>
#include <fstream.h>
#include <string>
#include <cctype>
#include <map>

static const char doc_delim = '';

static map<string, string> help_text;

static void
fatal (const string& msg)
{
  cerr << msg << "\n";
  exit (1);
}

static void
usage (void)
{
  cerr << "usage: munge-texi -d DOCSTRING-FILE file ...\n";
  exit (1);
}

static string
extract_symbol_name (istream& is)
{
  string symbol_name;

  int c;
  while ((c = is.get ()) != EOF && c != '\n')
    symbol_name += (char) c;

  return symbol_name;
}

static string
extract_docstring (istream& is)
{
  string doc;

  int c;
  while ((c = is.get ()) != EOF && c != doc_delim)
    doc += (char) c;

  return doc;
}

static void
process_doc_file (const string& fname)
{
  ifstream infile (fname.c_str ());

  if (infile)
    {
      if (infile.get () != doc_delim)
	fatal ("invalid doc file format");

      string symbol_name;

      do
	{
	  symbol_name = extract_symbol_name (infile);

	  if (! symbol_name.empty ())
	    {
	      string doc_string = extract_docstring (infile);

	      if (help_text.find (symbol_name) != help_text.end ())
		cerr << "ignoring duplicate entry for " << symbol_name << "\n";
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
process_texi_input_file (istream& is, ostream& os)
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
	      string symbol_name;

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
		      string doc_string = help_text[symbol_name];

		      int i = 0;
		      while (doc_string[i] == ' ')
			i++;

		      if (doc_string.substr (i, 15) == "-*- texinfo -*-")
			{
			  i += 15;

			  while (isspace (doc_string[i]))
			    i++;

			  os << doc_string.substr (i);
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

  process_texi_input_file (cin, cout);

  return 0;
}
