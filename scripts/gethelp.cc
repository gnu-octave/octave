#include <string>
#include <iostream.h>

#ifndef NPOS
#define NPOS string::npos
#endif

static bool
looks_like_octave_copyright (const string& s)
{
  bool retval = false;

  string t = s.substr (0, 14);

  if (t == "Copyright (C) ")
    {
      size_t pos = s.find ('\n');

      if (pos != NPOS)
	{
	  pos = s.find ('\n', pos + 1);

	  if (pos != NPOS)
	    {
	      pos++;

	      t = s.substr (pos, 28);

	      if (t == "This file is part of Octave."
		  || t == "This program is free softwar")
		retval = true;
	    }
	}
    }

  return retval;
}

// Eat whitespace and comments from FFILE, returning the text of the
// first block of comments that doesn't look like a copyright notice,

static string
extract_help_text (void)
{
  string help_txt;

  bool first_comments_seen = false;
  bool begin_comment = false;
  bool have_help_text = false;
  bool in_comment = false;
  bool discard_space = true;
  int c;

  while ((c = cin.get ()) != EOF)
    {
      if (begin_comment)
	{
	  if (c == '%' || c == '#')
	    continue;
	  else if (discard_space && c == ' ')
	    {
	      discard_space = false;
	      continue;
	    }
	  else
	    begin_comment = false;
	}

      if (in_comment)
	{
	  if (! have_help_text)
	    {
	      first_comments_seen = true;
	      help_txt += (char) c;
	    }

	  if (c == '\n')
	    {
	      in_comment = false;
	      discard_space = true;

	      if ((c = cin.get ()) != EOF)
		{
		  if (c == '\n')
		    break;
		}
	      else
		break;
	    }
	}
      else
	{
	  switch (c)
	    {
	    case ' ':
	    case '\t':
	      if (first_comments_seen)
		have_help_text = true;
	      break;

	    case '\n':
	      if (first_comments_seen)
		have_help_text = true;
	      continue;

	    case '%':
	    case '#':
	      begin_comment = true;
	      in_comment = true;
	      break;

	    default:
	      goto done;
	    }
	}
    }

 done:

  if (! help_txt.empty ())
    {
      if (looks_like_octave_copyright (help_txt)) 
	help_txt.resize (0);

      if (help_txt.empty ())
	help_txt = extract_help_text ();
    }

  return help_txt;
}

int
main (int argc, char **argv)
{
  string name;

  if (argc != 2)
    {
      cerr << "usage: gethelp name\n";
      return 1;
    }
  else
    name = argv[1];

  string help_text = extract_help_text ();  

  if (! help_text.empty ())
    {
      cout << "" << name << "\n" << help_text;

      if (help_text[help_text.length () - 1] != '\n')
	cout << "\n";
    }

  return 0;
}
