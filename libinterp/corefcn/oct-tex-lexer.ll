/*

Copyright (C) 2013 Michael Goffioul

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

%option prefix = "octave_tex_"
%option noyywrap
%option reentrant
%option bison-bridge

%top {
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "txt-eng.h"
#include "oct-tex-parser.h"
}

%x	NUM_MODE
%x	MAYBE_NUM_MODE
%x	COMMAND

D       [0-9]
ID	[a-zA-Z0-9]
NUM	(({D}+\.?{D}*)|(\.{D}+))

%%

%{
// Numeric values
%}

<NUM_MODE>{NUM}		{
    int nread;

    nread = sscanf (yytext, "%lf", &(yylval->num));
    if (nread == 1)
      return NUM;
  }
<NUM_MODE>[ \t]+	{ }
<NUM_MODE>"\n"|.	{ yyless (0); BEGIN (INITIAL); }

<MAYBE_NUM_MODE>"{"	{ BEGIN (NUM_MODE); return START; }
<MAYBE_NUM_MODE>"\n"|.	{ yyless (0); BEGIN (INITIAL); }

%{
// Simple commands
%}

"\\bf"		{ return BF; }
"\\it"		{ return IT; }
"\\sl"		{ return SL; }
"\\rm"		{ return RM; }

%{
// Generic font commands
%}

"\\fontname"	{ return FONTNAME; }
"\\fontsize"	{ BEGIN (MAYBE_NUM_MODE); return FONTSIZE; }
"\\color[rgb]"	{ BEGIN (MAYBE_NUM_MODE); return COLOR_RGB; }
"\\color"	{ return COLOR; }

%{
// Special characters
%}

"{"	{ return START; }
"}"	{ return END; }
"^"	{ return SUPER; }
"_"	{ return SUB; }

"\\{"	|
"\\}"	|
"\\^"	|
"\\_"	|
"\\\\"	{ yylval->ch = yytext[1]; return CH; }

%{
// Symbols
%}

"\\"		{ BEGIN(COMMAND); return CMD; }
<COMMAND>{ID}	{ yylval->ch = yytext[0]; return ID; }
<COMMAND>"\n"|.	{ BEGIN(INITIAL); yyless (0); }

%{
// Generic character
%}

"\n"	|
.	{ yylval->ch = yytext[0]; return CH; }

%%

bool
text_parser_tex::init_lexer (const std::string& s)
{
  if (! scanner)
    octave_tex_lex_init (&scanner);

  if (scanner)
    {
      if (buffer_state)
        {
          octave_tex__delete_buffer (reinterpret_cast<YY_BUFFER_STATE> (buffer_state),
                                     scanner);
          buffer_state = 0;
        }

      buffer_state = octave_tex__scan_bytes (s.data (), s.length (), scanner);
    }

  return (scanner && buffer_state);
}

void
text_parser_tex::destroy_lexer (void)
{
  if (buffer_state)
    {
      octave_tex__delete_buffer (reinterpret_cast<YY_BUFFER_STATE> (buffer_state),
                                 scanner);
      buffer_state = 0;
    }

  if (scanner)
    {
      octave_tex_lex_destroy (scanner);
      scanner = 0;
    }
}
