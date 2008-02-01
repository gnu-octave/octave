## Copyright (C) 2001, 2007 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Command} edit @var{name}
## @deftypefnx {Command} edit @var{field} @var{value}
## @deftypefnx {Command} @var{value} = edit get @var{field}
## Edit the named function, or change editor settings.
##
## If @code{edit} is called with the name of a file or function as
## its argument it will be opened in a text editor.
## 
## @itemize @bullet
## @item
## If the function @var{name} is available in a file on your path and
## that file is modifiable, then it will be edited in place.  If it 
## is a system function, then it will first be copied to the directory
## @code{HOME} (see further down) and then edited.  
##
## @item
## If @var{name} is the name of a function defined in the interpreter but 
## not in an m-file, then an m-file will be created in @code{HOME}
## to contain that function along with its current definition.  
##
## @item
## If @code{name.cc} is specified, then it will search for @code{name.cc}
## in the path and try to modify it, otherwise it will create a new
## @file{.cc} file in @code{HOME}.  If @var{name} happens to be an
## m-file or interpreter defined function, then the text of that
## function will be inserted into the .cc file as a comment.
##
## @item
## If @var{name.ext} is on your path then it will be editted, otherwise
## the editor will be started with @file{HOME/name.ext} as the
## filename.  If @file{name.ext} is not modifiable, it will be copied to
## @code{HOME} before editing.
##
## @strong{WARNING!} You may need to clear name before the new definition
## is available.  If you are editing a .cc file, you will need
## to mkoctfile @file{name.cc} before the definition will be available.
## @end itemize
##
## If @code{edit} is called with @var{field} and @var{value} variables,
## the value of the control field @var{field} will be @var{value}.
## If an output argument is requested and the first argument is @code{get}
## then @code{edit} will return the value of the control field @var{field}.
## The following control fields are used:
##
## @table @samp
## @item editor
## This is the editor to use to modify the functions.  By default it uses
## Octave's @code{EDITOR} built-in function, which comes from 
## @code{getenv("EDITOR")} and defaults to @code{emacs}.  Use @code{%s}
## In place of the function name.  For example,
## @table @samp
## @item [EDITOR, " %s"]
## Use the editor which Octave uses for @code{bug_report}.
## @item "xedit %s &"           
## pop up simple X11 editor in a separate window
## @item "gnudoit -q \"(find-file \\\"%s\\\")\""   
## Send it to current Emacs; must have @code{(gnuserv-start)} in @file{.emacs}.
## @end table
##
## On cygwin, you will need to convert the cygwin path to a windows
## path if you are using a native Windows editor.  For example
## @example
## '"C:/Program Files/Good Editor/Editor.exe" "$(cygpath -wa %s)"'
## @end example
##
## @item home
## This is the location of user local m-files. Be be sure it is in your
## path. The default is @file{~/octave}.
##
## @item author
## This is the name to put after the "## Author:" field of new functions.
## By default it guesses from the @code{gecos} field of password database.
## 
## @item email
## This is the e-mail address to list after the name in the author field.
## By default it guesses @code{<$LOGNAME@@$HOSTNAME>}, and if @code{$HOSTNAME}
## is not defined it uses @code{uname -n}.  You probably want to override this.
## Be sure to use @code{<user@@host>} as your format.
##
## @item license
## @table @samp
## @item gpl
## GNU General Public License (default).
## @item bsd
## BSD-style license without advertising clause.
## @item pd
## Public domain.
## @item "text"
## Your own default copyright and license.
## @end table
## 
## @item mode
## This value determines whether the editor should be started in async mode
## or sync mode. Set it to "async" to start the editor in async mode. The
## default is "sync" (see also "system").
## 
## Unless you specify @samp{pd}, edit will prepend the copyright statement 
## with "Copyright (C) yyyy Function Author".
## @end table
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

## Original version by Paul Kienzle distributed as free software in the
## public domain.

## PKG_ADD: mark_as_command edit

function ret = edit (file, state)

  ## Pick up globals or default them.

  persistent FUNCTION = struct ("EDITOR", strcat (EDITOR (), " %s"),
  				"HOME", fullfile (default_home, "octave"),
  				"AUTHOR", default_user(1),
  				"EMAIL",  [],
  				"LICENSE",  "GPL",
				"MODE", "sync");

  mlock; # make sure the state variables survive "clear functions"

  if (nargin == 2)
    switch (toupper (file))
    case "EDITOR"
      FUNCTION.EDITOR = state;
    case "HOME"
      if (! isempty (state) && state(1) == "~")
	state = [ default_home, state(2:end) ];
      endif
      FUNCTION.HOME = state;
    case "AUTHOR"
      FUNCTION.AUTHOR = state;
    case "EMAIL"
      FUNCTION.EMAIL = state;
    case "LICENSE"
      FUNCTION.LICENSE = state;
    case "MODE"
      if (strcmp (state, "sync") || strcmp (state, "async"))
        FUNCTION.MODE = state;
      else
    	error('expected "edit MODE sync|async"');
      endif
    case "GET"
      ret = FUNCTION.(toupper (state));
    otherwise
      error ("expected \"edit EDITOR|HOME|AUTHOR|EMAIL|LICENSE|MODE val\"");
    endswitch
    return
  endif

  ## Start the editor without a file if no file is given.
  if (nargin < 1)
    if (exist (FUNCTION.HOME, "dir") == 7 && (isunix () || ! ispc ()))
      system (strcat ("cd \"", FUNCTION.HOME, "\" ; ",
		      sprintf (FUNCTION.EDITOR, "")),
	      [], FUNCTION.MODE);
    else
      system (sprintf (FUNCTION.EDITOR,""), [], FUNCTION.MODE);
    endif
    return;
  endif

  ## Check whether the user is trying to edit a builtin of compiled function.
  switch (exist (file))
    case {3, 5}
      error ("unable to edit a built-in or compiled function");
  endswitch

  ## Find file in path.
  idx = rindex (file, ".");
  if (idx != 0)
    ## If file has an extension, use it.
    path = file_in_loadpath (file);
  else
    ## Otherwise try file.cc, and if that fails, default to file.m.
    path = file_in_loadpath (strcat (file, ".cc"));
    if (isempty (path))
      file = strcat (file, ".m");
      path = file_in_loadpath (file);
    endif
  endif

  ## If the file exists and is modifiable in place then edit it,
  ## otherwise copy it and then edit it.
  if (! isempty (path))
    fid = fopen (path, "r+t");
    if (fid < 0)
      from = path;
      path = strcat (FUNCTION.HOME, from (rindex (from, filesep):end))
      [status, msg] = copyfile (from, path, 1);
      if (status == 0)
        error (msg);
      endif
    else
      fclose(fid);
    endif
    system (sprintf (FUNCTION.EDITOR, strcat ("\"", path, "\"")),
	    [], FUNCTION.MODE);
    return;
  endif

  ## If editing something other than a m-file or an oct-file, just
  ## edit it.
  idx = rindex (file, filesep);
  if (idx != 0)
    path = file;
  else
    path = fullfile (FUNCTION.HOME, file);
  endif
  idx = rindex (file, ".");
  name = file(1:idx-1);
  ext = file(idx+1:end);
  switch (ext)
    case { "cc", "m" }
      0;
    otherwise
      system (sprintf (FUNCTION.EDITOR, strcat ("\"", path, "\"")),
	      [], FUNCTION.MODE);
      return;
  endswitch

  ## The file doesn't exist in path so create it, put in the function
  ## template and edit it.

  ## Guess the email name if it was not given.
  if (isempty (FUNCTION.EMAIL))
    host = getenv("HOSTNAME");
    if (isempty (host) && ispc ())
      host = getenv ("COMPUTERNAME");
    endif
    if (isempty (host))
      [status, host] = system ("uname -n");
      ## trim newline from end of hostname
      if (! isempty (host))
	host = host(1:end-1);
      endif
    endif
    if (isempty (host))
      FUNCTION.EMAIL = " ";
    else
      FUNCTION.EMAIL = strcat ("<", default_user(0), "@", host, ">");
    endif
  endif

  ## Fill in the revision string.
  now = localtime (time);
  revs = strcat ("Created: ", strftime ("%Y-%m-%d", now));

  ## Fill in the copyright string.
  copyright = strcat (strftime ("Copyright (C) %Y ", now), FUNCTION.AUTHOR);

  ## Fill in the author tag field.
  author = strcat ("Author: ", FUNCTION.AUTHOR, " ", FUNCTION.EMAIL);

  ## Fill in the header.
  uclicense = toupper (FUNCTION.LICENSE);
  switch (uclicense)
    case "GPL"
      head = strcat (copyright, "\n\n", "\
This program is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 2 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program; if not, write to the Free Software\n\
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA\
");
      tail = strcat (author, "\n", revs);

    case "BSD"
      head = strcat (copyright, "\n\n", "\
This program is free software; redistribution and use in source and\n\
binary forms, with or without modification, are permitted provided that\n\
the following conditions are met:\n\
\n\
   1.Redistributions of source code must retain the above copyright\n\
     notice, this list of conditions and the following disclaimer.\n\
   2.Redistributions in binary form must reproduce the above copyright\n\
     notice, this list of conditions and the following disclaimer in the\n\
     documentation and/or other materials provided with the distribution.\n\
\n\
THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND\n\
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n\
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n\
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE\n\
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n\
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS\n\
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n\
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n\
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY\n\
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF\n\
SUCH DAMAGE.\
");
      tail = strcat (author, "\n", revs);

    case "PD"
      head = "";
      tail = strcat (author, "\n", revs, "\n\n",
		     "This program is granted to the public domain.");

    otherwise
      head = "";
      tail = strcat (copyright, "\n\n", FUNCTION.LICENSE, "\n",
		     author, "\n", revs);
  endswitch

  ## Generate the function template.
  exists = exist (name);
  switch (ext)
    case {"cc", "C", "cpp"}
      if (isempty (head))
	comment = strcat ("/*\n", tail, "\n\n*/\n\n");
      else
	comment = strcat ("/*\n", head, "\n\n", tail, "\n\n*/\n\n");
      endif
      ## If we are shadowing an m-file, paste the code for the m-file.
      if (any (exists == [2, 103]))
	code = strcat ("\\ ", strrep (type (name), "\n", "\n// "));
      else
	code = " ";
      endif
      body = strcat ("#include <octave/oct.h>\n\n",
                     "DEFUN_DLD(", name, ",args,nargout,\"\\\n",
		     name, "\\n\\\n\")\n{\n",
		     "  octave_value_list retval;\n",
		     "  int nargin = args.length();\n\n",
		     code, "\n  return retval;\n}\n");

      text = strcat (comment, body);
    case "m"
      ## If we are editing a function defined on the fly, paste the
      ## code.
      if (any (exists == [2, 103]))
	body = type (name);
      else
	body = strcat ("function [ ret ] = ", name, " ()\n\nendfunction\n");
      endif
      if (isempty (head))
	comment = strcat ("## ", name, "\n\n",
			  "## ", strrep (tail, "\n", "\n## "), "\n\n");
      else
	comment = strcat ("## ", strrep(head,"\n","\n## "), "\n\n", ...
			  "## ", name, "\n\n", ...
			  "## ", strrep (tail, "\n", "\n## "), "\n\n");
      endif
      text = strcat (comment, body);
  endswitch

  ## Write the initial file (if there is anything to write)
  fid = fopen (path, "wt");
  if (fid < 0)
    error ("edit: could not create %s", path);
  endif
  fputs (fid, text);
  fclose (fid);

  ## Finally we are ready to edit it!
  system (sprintf (FUNCTION.EDITOR, strcat ("\"", path, "\"")),
	  [], FUNCTION.MODE);

endfunction

function ret = default_home ()

  ret = getenv ("HOME");
  if (isempty (ret))
    ret = glob ("~");
    if (! isempty (ret))
      ret = ret{1};
    else
      ret = "";
    endif
  endif

endfunction

## Return the name associated with the current user ID.
##
## If LONG_FORM is 1, return the full name.  This will be the
## default author.  Otherwise return the login name.
## login@host will be the default email address.

function ret = default_user (long_form)

  ent = getpwuid (getuid);
  if (! isstruct (ent))
    ret = getenv ("USER");
    if (isempty (ret))
      ret = getenv ("USERNAME");
    endif
  elseif (long_form)
    ret = ent.gecos;
    pos = strfind (ret, ",");
    if (! isempty (pos))
      ret = ret(1:pos-1);
    endif
  else
    ret = ent.name;
  endif

endfunction
