function outstr = __publish_html_output__ (varargin)
  ##
  ## Types to handle are:
  ##
  ## * "header" (title_str, intro_str, toc_cstr)
  ## * "footer" ()
  ## * "code" (str)
  ## * "code_output" (str)
  ## * "section" (str)
  ## * "preformatted_code" (str)
  ## * "preformatted_text" (str)
  ## * "bulleted_list" (cstr)
  ## * "numbered_list" (cstr)
  ## * "graphic" (str)
  ## * "html" (str)
  ## * "latex" (str)
  ## * "text" (str)
  ## * "bold" (str)
  ## * "italic" (str)
  ## * "monospaced" (str)
  ## * "link" (url_str, url_str, str)
  ## * "TM" ()
  ## * "R" ()
  ##
  eval (["outstr = handle_", varargin{1}, " (varargin{2:end});"]);
endfunction

function outstr = handle_header (title_str, intro_str, toc_cstr)
  mathjax_str = ["<script type=\"text/x-mathjax-config\">\n", ...
    "MathJax.Hub.Config({\n", ...
    "  tex2jax: { inlineMath: [['$','$'], ['\\\\(','\\\\)']] },\n", ...
    "  TeX: { equationNumbers: { autoNumber: 'all' } }\n", ...
    "});\n", ...
    "</script>\n", ...
    "<script type=\"text/javascript\" async ", ...
    "src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?", ...
    "config=TeX-MML-AM_CHTML\"></script>\n"];
  stylesheet_str = ["<style>\n", ...
    "body > * {\n", ...
    "  max-width: 42em;\n", ...
    "}\n", ...
    "body {\n", ...
    "  font-family: \"Roboto Condensed\", sans-serif;\n", ...
    "  padding-left: 7.5em;\n", ...
    "  padding-right: 7.5em;\n", ...
    "}\n", ...
    "pre, code {\n", ...
    "  max-width: 50em;\n", ...
    "  font-family: monospace;\n", ...
    "}\n", ...
    "pre.oct-code {\n", ...
    "  border: 1px solid Grey;\n", ...
    "  padding: 5px;\n", ...
    "}\n", ...
    "pre.oct-code-output {\n", ...
    "  margin-left: 2em;\n", ...
    "}\n", ...
    "span.comment {\n", ...
    "  color: ForestGreen;\n", ...
    "}\n",...
    "span.keyword {\n", ...
    "  color: Blue;\n", ...
    "}\n",...
    "span.string {\n", ...
    "  color: DarkOrchid;\n", ...
    "}\n",...
    "footer {\n", ...
    "  margin-top: 2em;\n", ...
    "  font-size: 80%;\n", ...
    "}\n", ...
    "a, a:visited {\n", ...
    "  color: Blue;\n", ...
    "}\n", ...
    "h2 {\n", ...
    "  font-family: \"Roboto Condensed\", serif;\n", ...
    "  margin-top: 1.5em;\n", ...
    "}\n", ...
    "h2 a, h2 a:visited {\n", ...
    "  color: Black;\n", ...
    "}\n", ...
    "</style>\n"];
  outstr = ["<!DOCTYPE html>\n", ...
    "<html>\n", ...
    "<head>\n", ...
    "<meta charset=\"UTF-8\">\n", ...
    "<title>", title_str, "</title>\n", ...
    mathjax_str, ...
    stylesheet_str, ...
    "</head>\n", ...
    "<body>\n", ...
    "<h1>", title_str, "</h1>\n", ...
    intro_str];

  if (! isempty (toc_cstr))
    for i = 1:length(toc_cstr)
      toc_cstr{i} = handle_link (["#node", num2str(i)], toc_cstr{i});
    endfor
    outstr = [outstr, "<h2>Contents</h2>", ...
      handle_bulleted_list(toc_cstr)];
  endif

  ## Reset section counter
  handle_section ();
endfunction

function outstr = handle_footer (m_source_str)
  outstr = ["\n", ...
    "<footer><hr>", ...
    "<a href=\"http://www.octave.org\">Published with GNU Octave ", ...
    version(), "</a></footer>\n", ...
    "<!--\n", ...
    "##### SOURCE BEGIN #####\n", ...
    m_source_str, ...
    "\n##### SOURCE END #####\n", ...
    "-->\n", ...
    "</body>\n", ...
    "</html>\n"];
endfunction

function outstr = handle_code (str)
  outstr = ["<pre class=\"oct-code\">", syntax_highlight(str), "</pre>"];
endfunction

function outstr = handle_code_output (str)
  outstr = ["<pre class=\"oct-code-output\">", str, "</pre>"];
endfunction

function outstr = handle_section (varargin)
  persistent counter = 1;
  if (nargin == 0)
    counter = 1;
    outstr = "";
    return;
  endif
  outstr = ["<h2><a id=\"node", num2str(counter), "\">", varargin{1}, ...
    "</a></h2>"];
  counter++;
endfunction

function outstr = handle_preformatted_code (str)
  outstr = ["<pre class=\"pre-code\">", syntax_highlight(str), "</pre>"];
endfunction

function outstr = handle_preformatted_text (str)
  outstr = ["<pre class=\"pre-text\">", str, "</pre>"];
endfunction

function outstr = handle_bulleted_list (cstr)
  outstr = "<ul>";
  for i = 1:length(cstr)
    outstr = [outstr, "<li>", cstr{i}, "</li>"];
  endfor
  outstr = [outstr, "</ul>"];
endfunction

function outstr = handle_numbered_list (cstr)
  outstr = "<ol>";
  for i = 1:length(cstr)
    outstr = [outstr, "<li>", cstr{i}, "</li>"];
  endfor
  outstr = [outstr, "</ol>"];
endfunction

function outstr = handle_graphic (str)
  outstr = ["<img src=\"", str,"\" alt=\"", str, "\">"];
endfunction

function outstr = handle_html (str)
  outstr = str;
endfunction

function outstr = handle_latex (str)
  outstr = "";
endfunction

function outstr = handle_link (url_str, str)
  outstr = ["<a href=\"", url_str,"\">", str, "</a>"];
endfunction

function outstr = handle_text (str)
  outstr = ["<p>", str, "</p>"];
endfunction

function outstr = handle_bold (str)
  outstr = ["<b>", str, "</b>"];
endfunction

function outstr = handle_italic (str)
  outstr = ["<i>", str, "</i>"];
endfunction

function outstr = handle_monospaced (str)
  outstr = ["<code>", str, "</code>"];
endfunction

function outstr = handle_TM ()
  outstr = "&trade;";
endfunction

function outstr = handle_R ()
  outstr = "&reg;";
endfunction

function outstr = syntax_highlight (str)
  ## SYNTAX_HIGHLIGHT a primitive parser to add syntax highlight via <span>
  ##   tags. Should be replaced by a better solution.
  ##

  outstr = "";
  i = 1;
  placeholder_cstr = {};
  plh = 0;
  while (i <= length(str))
    ## Block comment
    if (any (strncmp (str(i:end), {"%{", "#{"}, 2)))
      plh_str = ["<span class=\"comment\">", str(i:i+1)];
      i = i + 2;
      while ((i <= length(str)) ...
             && ! (any (strncmp (str(i:end), {"%}", "#}"}, 2))))
        plh_str = [plh_str, str(i)];
        i++;
      endwhile
      if (i < length(str))
        plh_str = [plh_str, str(i:i+1), "</span>"];
        i = i + 2;
      else
        plh_str = [plh_str, "</span>"];
      endif
      plh = plh + 1;
      placeholder_cstr{plh} = plh_str;
      outstr = [outstr, " PUBLISHPLACEHOLDER", num2str(plh), " "];
    ## Line comment
    elseif (any (strcmp (str(i), {"%", "#"})))
      plh_str = "<span class=\"comment\">";
      while ((i <= length(str)) && (! strcmp (str(i), "\n")))
        plh_str = [plh_str, str(i)];
        i++;
      endwhile
      plh_str = [plh_str, "</span>\n"];
      i++;
      plh = plh + 1;
      placeholder_cstr{plh} = plh_str;
      outstr = [outstr, " PUBLISHPLACEHOLDER", num2str(plh), " "];
    ## Single quoted string
    elseif (strcmp (str(i), "'"))
      plh_str = "<span class=\"string\">'";
      i++;
      while (i <= length(str))
        ## Ignore escaped string terminations
        if (strncmp (str(i:end), "''", 2))
          plh_str = [plh_str, "''"];
          i = i + 2;
        ## Is string termination
        elseif (strcmp (str(i), "'"))
          plh_str = [plh_str, "'</span>"];
          i++;
          break;
        ## Is string termination by line break
        elseif (strcmp (str(i), "\n"))
          plh_str = [plh_str, "</span>"];
          break;
        ## String content
        else
          plh_str = [plh_str, str(i)];
          i++;
        endif
      endwhile
      plh = plh + 1;
      placeholder_cstr{plh} = plh_str;
      outstr = [outstr, " PUBLISHPLACEHOLDER", num2str(plh), " "];
    ## Double quoted string
    elseif (strcmp (str(i), "\""))
      plh_str = "<span class=\"string\">\"";
      i++;
      while (i <= length(str))
        ## Is string termination
        if (strcmp (str(i), "\"") && ! strcmp (str(i - 1), "\\"))
          plh_str = [plh_str, "\"</span>"];
          i++;
          break;
        ## Is string termination by line break
        elseif (strcmp (str(i), "\n"))
          plh_str = [plh_str, "</span>"];
          break;
        ## String content
        else
          plh_str = [plh_str, str(i)];
          i++;
        endif
      endwhile
      plh = plh + 1;
      placeholder_cstr{plh} = plh_str;
      outstr = [outstr, " PUBLISHPLACEHOLDER", num2str(plh), " "];
    else
      outstr = [outstr, str(i)];
      i++;
    endif
  endwhile
  kwords = iskeyword ();
  ## TODO: remove hack for regexp (bug #38149)
  outstr = [" ", strrep(outstr, "\n", " \n "), " "];
  for i = 1:length(kwords)
    outstr = regexprep (outstr, ...
      ['(\s)(', kwords{i},')(\s|\()'], ...
      ["$1<span class=\"keyword\">$2</span>$3"]);
  endfor
  ## TODO: remove hack for regexp (bug #38149)
  outstr = strrep(outstr(2:end-1), " \n ", "\n");

  ## Restore placeholders
  for i = plh:-1:1
    outstr = strrep (outstr, [" PUBLISHPLACEHOLDER", num2str(i), " "], ...
      placeholder_cstr{i});
  endfor
endfunction