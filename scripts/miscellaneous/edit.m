## Copyright (C) 2001-2012 Paul Kienzle
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
## @deftypefn  {Command} {} edit @var{name}
## @deftypefnx {Command} {} edit @var{field} @var{value}
## @deftypefnx {Command} {@var{value} =} edit get @var{field}
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
## @env{HOME} (see further down) and then edited.
## If no file is found, then the m-file
## variant, ending with ".m", will be considered.  If still no file
## is found, then variants with a leading "@@" and then with both a
## leading "@@" and trailing ".m" will be considered.
##
## @item
## If @var{name} is the name of a function defined in the interpreter but
## not in an m-file, then an m-file will be created in @env{HOME}
## to contain that function along with its current definition.
##
## @item
## If @code{name.cc} is specified, then it will search for @code{name.cc}
## in the path and try to modify it, otherwise it will create a new
## @file{.cc} file in @env{HOME}.  If @var{name} happens to be an
## m-file or interpreter defined function, then the text of that
## function will be inserted into the .cc file as a comment.
##
## @item
## If @var{name.ext} is on your path then it will be edited, otherwise
## the editor will be started with @file{HOME/name.ext} as the
## filename.  If @file{name.ext} is not modifiable, it will be copied to
## @env{HOME} before editing.
##
## @strong{Warning:} You may need to clear name before the new definition
## is available.  If you are editing a .cc file, you will need
## to mkoctfile @file{name.cc} before the definition will be available.
## @end itemize
##
## If @code{edit} is called with @var{field} and @var{value} variables,
## the value of the control field @var{field} will be @var{value}.
## If an output argument is requested and the first argument is @code{get}
## then @code{edit} will return the value of the control field @var{field}.
## If the control field does not exist, edit will return a structure
## containing all fields and values.  Thus, @code{edit get all} returns
## a complete control structure.
## The following control fields are used:
##
## @table @samp
## @item editor
## This is the editor to use to modify the functions.  By default it uses
## Octave's @env{EDITOR} built-in function, which comes from
## @code{getenv("EDITOR")} and defaults to @code{emacs}.  Use @code{%s}
## In place of the function name.  For example,
## @table @samp
## @item [EDITOR, " %s"]
## Use the editor which Octave uses for @code{edit_history}.
##
## @item "xedit %s &"
## pop up simple X11 editor in a separate window
##
## @item "gnudoit -q \"(find-file \\\"%s\\\")\""
## Send it to current Emacs; must have @code{(gnuserv-start)} in @file{.emacs}.
## @end table
##
## See also field 'mode', which controls how the editor is run by Octave.
##
## On Cygwin, you will need to convert the Cygwin path to a Windows
## path if you are using a native Windows editor.  For example:
## @c Set example in small font to prevent overfull line in TeX
##
## @smallexample
## @exdent '"C:/Program Files/Good Editor/Editor.exe" "$(cygpath -wa %s)"'
## @end smallexample
##
## @item home
## This is the location of user local m-files.  Be be sure it is in your
## path.  The default is @file{~/octave}.
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
##
## @item bsd
## BSD-style license without advertising clause.
##
## @item pd
## Public domain.
##
## @item "text"
## Your own default copyright and license.
## @end table
##
## Unless you specify @samp{pd}, edit will prepend the copyright statement
## with "Copyright (C) yyyy Function Author".
##
## @item mode
## This value determines whether the editor should be started in async mode
## (editor is started in the background and Octave continues) or sync mode
## (Octave waits until the editor exits).  Set it to "sync" to start the editor
## in sync mode.  The default is "async" (see also "system").
##
## @item editinplace
## Determines whether files should be edited in place, without regard to
## whether they are modifiable or not.  The default is @code{false}.
## @end table
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

## Original version by Paul Kienzle distributed as free software in the
## public domain.

function ret = edit (file, state)

  ## Pick up globals or default them.

  persistent FUNCTION = struct ("EDITOR", cstrcat (EDITOR (), " %s"),
                                "HOME", fullfile (default_home, "octave"),
                                "AUTHOR", default_user(1),
                                "EMAIL",  [],
                                "LICENSE",  "GPL",
                                "MODE", "async",
                                "EDITINPLACE", false);
  ## Make sure the state variables survive "clear functions".
  mlock;

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
        error('edit: expected "edit MODE sync|async"');
      endif
    case "EDITINPLACE"
      if (ischar (state))
        if (strcmpi (state, "true"))
          state = true;
        elseif (strcmpi (state, "false"))
          state = false;
        else
          state = eval (state);
        endif
      endif
      FUNCTION.EDITINPLACE = state;
    case "GET"
      if (isfield (FUNCTION, toupper(state)))
        ret = FUNCTION.(toupper (state));
      else
        ret = FUNCTION;
      endif
    otherwise
      error ('edit: expected "edit EDITOR|HOME|AUTHOR|EMAIL|LICENSE|MODE val"');
    endswitch
    return
  endif

  ## Start the editor without a file if no file is given.
  if (nargin < 1)
    if (exist (FUNCTION.HOME, "dir") == 7 && (isunix () || ! ispc ()))
      system (cstrcat ("cd \"", FUNCTION.HOME, "\" ; ",
                      sprintf (undo_string_escapes (FUNCTION.EDITOR), "")),
              [], FUNCTION.MODE);
    else
      system (sprintf (undo_string_escapes (FUNCTION.EDITOR), ""),
              [], FUNCTION.MODE);
    endif
    return;
  endif

  ## Check whether the user is trying to edit a builtin of compiled function.
  switch (exist (file))
    case {3, 5}
      error ("edit: unable to edit a built-in or compiled function");
  endswitch

  ## Checks for whether the file is
  ## absolute or relative should be handled inside file_in_loadpath.
  ## That way, it will be possible to look up files correctly given
  ## partial path information.  For example, you should be able to
  ## edit a particular overloaded function by doing any one of
  ##
  ##   edit classname/foo
  ##   edit classname/foo.m
  ##   edit @classname/foo
  ##   edit @classname/foo.m
  ##
  ## This functionality is needed for other functions as well (at least
  ## help and type; there may be more).  So the place to fix that is in
  ## file_in_loadpath, possibly with some help from the load_path class.

  ## The code below includes a portion that serves as a place-holder for
  ## the changes suggested above.

  ## Create list of explicit and implicit file names.
  filelist = {file};
  ## If file has no extension, add file.m and file.cc to the list.
  idx = rindex (file, ".");
  if (idx == 0)
    ## Create the list of files to look for
    filelist = {file};
    if (isempty (regexp (file, '\.m$')))
      ## No ".m" at the end of the file, add to the list.
      filelist{end+1} = cat (2, file, ".m");
    endif
    if (isempty (regexp (file, '\.cc$')))
      ## No ".cc" at the end of the file, add to the list.
      filelist{end+1} = cat (2, file, ".cc");
    endif
  endif

  ## If the file includes a path, it may be an overloaded function.
  if (! strcmp (file, "@") && index (file, filesep))
    ## No "@" at the beginning of the file, add to the list.
    numfiles = numel(filelist);
    for n = 1:numfiles
      filelist{n+numfiles} = cat (2, "@", filelist{n});
    endfor
  endif

  ## Search the entire path for the 1st instance of a file in the list.
  fileandpath = "";
  for n = 1:numel(filelist)
    filetoedit = file_in_path (path, filelist{n});
    if (! isempty (filetoedit))
      ## The path is explicitly included.
      fileandpath = filetoedit;
      break;
    endif
  endfor

  if (! isempty (fileandpath))
    ## If the file exists, then edit it.
    if (FUNCTION.EDITINPLACE)
      ## Edit in place even if it is protected.
      system (sprintf (undo_string_escapes (FUNCTION.EDITOR),
                       cstrcat ("\"", fileandpath, "\"")),
              [], FUNCTION.MODE);
      return;
    else
      ## If the file is modifiable in place then edit it, otherwise make
      ## a copy in HOME and then edit it.
      fid = fopen (fileandpath, "r+t");
      if (fid < 0)
        from = fileandpath;
        fileandpath = cstrcat (FUNCTION.HOME, from (rindex (from, filesep):end));
        [status, msg] = copyfile (from, fileandpath, 1);
        if (status == 0)
          error (msg);
        endif
      else
        fclose (fid);
      endif
      system (sprintf (undo_string_escapes (FUNCTION.EDITOR),
                       cstrcat ("\"", fileandpath, "\"")),
              [], FUNCTION.MODE);
      return;
    endif
  endif

  ## If editing a new file that is neither a m-file or an oct-file,
  ## just edit it.
  fileandpath = file;
  idx = rindex (file, ".");
  name = file(1:idx-1);
  ext = file(idx+1:end);
  switch (ext)
    case {"cc", "m"}
      0;
    otherwise
      system (sprintf (undo_string_escapes (FUNCTION.EDITOR),
                       cstrcat ("\"", fileandpath, "\"")),
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
      FUNCTION.EMAIL = cstrcat ("<", default_user(0), "@", host, ">");
    endif
  endif

  ## Fill in the revision string.
  now = localtime (time);
  revs = cstrcat ("Created: ", strftime ("%Y-%m-%d", now));

  ## Fill in the copyright string.
  copyright = cstrcat (strftime ("Copyright (C) %Y ", now), FUNCTION.AUTHOR);

  ## Fill in the author tag field.
  author = cstrcat ("Author: ", FUNCTION.AUTHOR, " ", FUNCTION.EMAIL);

  ## Fill in the header.
  uclicense = toupper (FUNCTION.LICENSE);
  switch (uclicense)
    case "GPL"
      head = cstrcat (copyright, "\n\n", "\
This program is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 3 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with Octave; see the file COPYING.  If not, see\n\
<http://www.gnu.org/licenses/>.\
");
      tail = cstrcat (author, "\n", revs);

    case "BSD"
      head = cstrcat (copyright, "\n\n", "\
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
      tail = cstrcat (author, "\n", revs);

    case "PD"
      head = "";
      tail = cstrcat (author, "\n", revs, "\n\n",
                     "This program is granted to the public domain.");

    otherwise
      head = "";
      tail = cstrcat (copyright, "\n\n", FUNCTION.LICENSE, "\n",
                     author, "\n", revs);
  endswitch

  ## Generate the function template.
  exists = exist (name);
  switch (ext)
    case {"cc", "C", "cpp"}
      if (isempty (head))
        comment = cstrcat ("/*\n", tail, "\n\n*/\n\n");
      else
        comment = cstrcat ("/*\n", head, "\n\n", tail, "\n\n*/\n\n");
      endif
      ## If we are shadowing an m-file, paste the code for the m-file.
      if (any (exists == [2, 103]))
        code = cstrcat ("\\ ", strrep (type (name), "\n", "\n// "));
      else
        code = " ";
      endif
      body = cstrcat ("#include <octave/oct.h>\n\n",
                     "DEFUN_DLD(", name, ",args,nargout,\"\\\n",
                     name, "\\n\\\n\")\n{\n",
                     "  octave_value_list retval;\n",
                     "  int nargin = args.length();\n\n",
                     code, "\n  return retval;\n}\n");

      text = cstrcat (comment, body);
    case "m"
      ## If we are editing a function defined on the fly, paste the
      ## code.
      if (any (exists == [2, 103]))
        body = type (name);
      else
        body = cstrcat ("function [ ret ] = ", name, " ()\n\nendfunction\n");
      endif
      if (isempty (head))
        comment = cstrcat ("## ", name, "\n\n",
                          "## ", strrep (tail, "\n", "\n## "), "\n\n");
      else
        comment = cstrcat ("## ", strrep(head,"\n","\n## "), "\n\n", ...
                          "## ", name, "\n\n", ...
                          "## ", strrep (tail, "\n", "\n## "), "\n\n");
      endif
      text = cstrcat (comment, body);
  endswitch

  ## Write the initial file (if there is anything to write)
  fid = fopen (fileandpath, "wt");
  if (fid < 0)
    error ("edit: could not create %s", fileandpath);
  endif
  fputs (fid, text);
  fclose (fid);

  ## Finally we are ready to edit it!
  system (sprintf (undo_string_escapes (FUNCTION.EDITOR),
                   cstrcat ("\"", fileandpath, "\"")),
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

%!test
%! s.editor = edit ("get", "editor");
%! s.home = edit ("get", "home");
%! s.author = edit ("get", "author");
%! s.email = edit ("get", "email");
%! s.license = edit ("get", "license");
%! s.editinplace = edit ("get", "editinplace");
%! s.mode = edit ("get", "mode");
%! edit editor none
%! edit home none
%! edit author none
%! edit email none
%! edit license none
%! edit ("editinplace", !s.editinplace)
%! if (s.mode(1) == "a")
%!   edit mode sync
%! else
%!   edit mode async
%! endif
%! edit ("editor", s.editor);
%! edit ("home", s.home);
%! edit ("author", s.author);
%! edit ("email", s.email);
%! edit ("license", s.license);
%! edit ("editinplace", s.editinplace);
%! edit ("mode", s.mode);
%! assert (edit ("get", "editor"), s.editor);
%! assert (edit ("get", "home"), s.home);
%! assert (edit ("get", "author"), s.author);
%! assert (edit ("get", "email"), s.email);
%! assert (edit ("get", "license"), s.license);
%! assert (edit ("get", "editinplace"), s.editinplace);
%! assert (edit ("get", "mode"), s.mode);

