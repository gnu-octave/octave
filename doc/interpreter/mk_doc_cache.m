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

## Read the contents of all the DOCSTRINGS files into TEXT.

text = "";
nfiles = numel (docstrings_files);
text = cell (1, nfiles+1);
for i = 1:nfiles
  file = docstrings_files{i};
  fid = fopen (file, "r");
  if (fid < 0)
    error ("unable to open %s for reading", file);
  else
    tmp = fread (fid, Inf, "*char")';
    delim_idx = find (tmp == doc_delim, 1);
    text{i} = tmp(delim_idx:end);
  endif
endfor
text = [text{:}, doc_delim];

text = regexprep (text, "@seealso *{([^}]*)}", "See also: $1.");
text = regexprep (text, "-\\*- texinfo -\\*-[ \t]*[\r\n]*", "");
text = regexprep (text, "@", "@@");

[fid, name, msg] = mkstemp ("octave_doc_XXXXXX", true);

if (fid < 0)
  error ("%s: %s\n", name, msg);
endif

fwrite (fid, text, "char");

fclose (fid);

cmd = sprintf ("%s --no-headers --no-warn --force --no-validate --fill-column=1024 %s",
               makeinfo_program (), name);

[status, formatted_text] = system (cmd);

## Did we get the help text?
if (status != 0)
  error ("makeinfo failed with exit status %d!", status);
endif

if (isempty (formatted_text))
  error ("makeinfo produced no output!");
endif

delim_idx = find (formatted_text == doc_delim);
n = numel (delim_idx);

cache = cell (3, n);
k = 1;

for i = 2:n

  block = formatted_text(delim_idx(i-1)+1:delim_idx(i)-1);

  [symbol, doc] = strtok (block, "\r\n");

  doc = skip_newline (doc);

  ## Skip functions that start with __ as these shouldn't be
  ## searched by lookfor.
  if (numel (symbol) > 2 && regexp (symbol, "^__.+__$"))
    continue;
  endif

  if (isempty (doc))
    continue;
  endif

  [s, e] = regexp (doc, "^ -- [^\r\n]*[\r\n]", "lineanchors");

  if (isempty (s))
    continue;
  endif

  start_of_first_sentence = e(end);

  tmp = doc(start_of_first_sentence:end);

  end_of_first_sentence = regexp (tmp, '(\.|[\r\n][\r\n])');

  if (isempty (end_of_first_sentence))
    end_of_first_sentence = numel (tmp);
  else
    end_of_first_sentence = end_of_first_sentence(1);
  endif

  first_sentence = tmp(1:end_of_first_sentence);
  first_sentence = regexprep (first_sentence, "([\r\n]|  *)", " ");
  first_sentence = regexprep (first_sentence, "^ *", "");

  cache{1,k} = symbol;
  cache{2,k} = doc;
  cache{3,k} = first_sentence;
  k++;
endfor

cache(:,k:end) = [];

save ("-text", output_file, "cache");
