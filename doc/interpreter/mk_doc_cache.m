## Copyright (C) 2009 John W. Eaton
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

args = argv ();

if (nargin < 2)
  error ("usage: mk_doc_cache OUTPUT-FILE DOCSTRINGS-FILE ...");
endif

output_file = args{1};
docstrings_files = args(2:end);

doc_delim = char (31);

function str = skip_newline (str)
  n = numel (str);
  j = 1;
  while (j <= n)
    c = str(j);
    if (c == "\r" || c == "\n")
      j++;
    else
      break;
    endif
  endwhile
  str = str(j:end);
endfunction

doc_cache = [];

for i = 1:numel (docstrings_files);

  file = docstrings_files{i};

  fid = fopen (file, "r");

  if (fid < 0)
    error ("unable to open %s for reading", file);
  else

    printf ("processing %s\n", file);

    text = fread (fid, Inf, "*char")';

    delim_idx = find (text == doc_delim);
    eof = numel (text);
    idx = [delim_idx, eof];

    n = numel (delim_idx);

    tmp_doc_cache = cell (3, n);
    k = 1;

    for i = 1:n

      tmp = text(idx(i)+1:idx(i+1)-1);

      [symbol, doc] = strtok (tmp, "\r\n");

      doc = skip_newline (doc);

      ## Skip functions that start with __ as these shouldn't be
      ## searched by lookfor.
      if (numel (symbol) > 2 && regexp (symbol, "^__.+__$"))
	continue;
      endif
  
      printf ("  %s\n", symbol);

      if (strncmp (doc, "-*- texinfo -*-", 15))
	doc = skip_newline (doc(16:end));
      else
	error ("doc string for %s is not in texinfo format", symbol);
      endif

      [formatted_text, status] = makeinfo (doc, "plain text");
    
      ## Did we get the help text?
      if (status != 0 || isempty (formatted_text))
	error ("makeinfo failed for %s doc string", symbol);
      endif

      ## Extract first line by searching for a period or a double
      ## line-end.

      period_idx = find (formatted_text == ".", 1);

      line_end_idx = strfind (formatted_text, "\n\n");

      max_first_sentence_len = 80;

      first_sentence = formatted_text (1:min ([period_idx(:); line_end_idx(:); max_first_sentence_len; numel(formatted_text)]));

      tmp_doc_cache{1,k} = symbol;
      tmp_doc_cache{2,k} = formatted_text;
      tmp_doc_cache{3,k} = first_sentence;
      k++;

    endfor

    tmp_doc_cache(:,k:end) = [];

  endif

  doc_cache = [doc_cache, tmp_doc_cache];

  save ("-text", output_file, "doc_cache");

endfor
