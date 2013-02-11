## Copyright (C) 2006-2012 John W. Eaton
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

%% test/octave.test/system/cputime-1.m
%!test
%! [t1, u1, s1] = cputime ();
%! for i = 1:200
%!   sin (i);
%! endfor
%! [t2, u2, s2] = cputime ();
%! assert (t1, u1 + s1);
%! assert (t2 == u2 + s2);
%! assert (t2 >= t1);
%! assert (u2 >= u2);
%! assert (s2 >= s2);
%!#assert (t1 == u1 + s1 && t2 == u2 + s2 && t2 >= t1 && u2 >= u2 && s2 >= s2);

%% test/octave.test/system/tic-toc-1.m
%!test
%! tic ();
%! sleep (2);
%! assert (toc () > 0);

%% test/octave.test/system/pause-1.m
%!test
%! pause (0);
%! __printf_assert__ ("ok\n");
%! assert (__prog_output_assert__ ("ok"));

%% test/octave.test/system/pause-2.m
%!error <Invalid call to pause> pause (1, 2)

%% test/octave.test/system/sleep-1.m
%!test
%! sleep (0);
%! __printf_assert__ ("ok\n");
%! assert (__prog_output_assert__ ("ok"));

%% test/octave.test/system/sleep-2.m
%!error <Invalid call to sleep> sleep ()

%% test/octave.test/system/sleep-3.m
%!error <Invalid call to sleep> sleep (1, 2)

%% test/octave.test/system/usleep-1.m
%!test
%! usleep (0);
%! __printf_assert__ ("ok\n");
%! assert (__prog_output_assert__ ("ok"));

%% test/octave.test/system/usleep-2.m
%!error <Invalid call to usleep> usleep ()

%% test/octave.test/system/usleep-3.m
%!error <Invalid call to usleep> usleep (1, 2)

%% test/octave.test/system/rename-1.m
%!test
%! from = tmpnam ();
%! to = tmpnam ();
%! id = fopen (from, "wb");
%! if (id > 0 && fclose (id) == 0)
%!   [s, e] = stat (from);
%!   if (! e)
%!     if (rename (from, to) == 0)
%!       [s, e] = stat (from);
%!       if (e < 0)
%!         [s, e] = stat (to);
%!         assert (e == 0);
%!         unlink (to);
%!       endif
%!     endif
%!   endif
%! endif

%% test/octave.test/system/rename-2.m
%!error <Invalid call to rename> rename ()

%% test/octave.test/system/rename-3.m
%!error <Invalid call to rename> rename ("foo", "bar", 1)

%% test/octave.test/system/unlink-1.m
%!test
%! nm = tmpnam ();
%! if ((id = fopen (nm, "wb")) > 0)
%!   [s, err] = stat (nm);
%!   if (! err && fclose (id) == 0 && unlink (nm) == 0)
%!     [s, err] = stat (nm);
%!     assert (err < 0);
%!   endif
%! endif

%% test/octave.test/system/unlink-2.m
%!error <Invalid call to unlink> unlink ()

%% test/octave.test/system/unlink-3.m
%!error <Invalid call to unlink> unlink ("foo", 1)

%% test/octave.test/system/readdir-1.m
%!test
%! [files, status, msg] = readdir (filesep);
%! assert (iscell (files) && status == 0 && strcmp (msg, ""));

%% test/octave.test/system/readdir-2.m
%!error <Invalid call to readdir> readdir ()

%% test/octave.test/system/readdir-3.m
%!error <Invalid call to readdir> readdir ("foo", 1)

%% test/octave.test/system/mk-rm-dir-1.m
%!test
%! nm = tmpnam ();
%! e1 = mkdir (nm);
%! [s2, e2] = stat (nm);
%! e3 = rmdir (nm);
%! [s4, e4] = stat (nm);
%! assert ((e1 && strcmp (s2.modestr(1), "d") && e3 && e4 < 0));

%% test/octave.test/system/mkdir-1.m
%!error <Invalid call to mkdir> mkdir ()

%% test/octave.test/system/mkdir-2.m
%!error <Invalid call to mkdir> mkdir ("foo", 1, 2)

%% test/octave.test/system/rmdir-1.m
%!error <Invalid call to rmdir> rmdir ()

%% test/octave.test/system/rmdir-2.m
%!test
%! crr = confirm_recursive_rmdir ();
%! confirm_recursive_rmdir (0);
%! assert (!rmdir ("foo", "s"));
%! confirm_recursive_rmdir (crr);

%% test/octave.test/system/umask-1.m
%!test
%! orig_umask = umask (0);
%! nm = tmpnam ();
%! id = fopen (nm, "wb");
%! s1 = stat (nm);
%! fclose (id);
%! unlink (nm);
%! 
%! umask (777);
%! nm = tmpnam ();
%! id = fopen (nm, "wb");
%! s2 = stat (nm);
%! fclose (id);
%! unlink (nm);
%! 
%! assert (deblank (s1.modestr), "-rw-rw-rw-");
%! assert (deblank (s2.modestr), "----------");
%! # Restore original umask value
%! umask (orig_umask);

%% test/octave.test/system/umask-2.m
%!error <Invalid call to umask> umask ()

%% test/octave.test/system/umask-3.m
%!error <Invalid call to umask> umask (1, 2)

%% test/octave.test/system/stat-1.m
%!test
%! [s, err, msg] = stat (filesep);
%! assert ((err == 0
%! && isstruct (s)
%! && isfield (s, "dev")
%! && isfield (s, "ino")
%! && isfield (s, "modestr")
%! && isfield (s, "nlink")
%! && isfield (s, "uid")
%! && isfield (s, "gid")
%! && isfield (s, "size")
%! && isfield (s, "atime")
%! && isfield (s, "mtime")
%! && isfield (s, "ctime")
%! && ischar (msg)));

%% test/octave.test/system/stat-2.m
%!error <Invalid call to stat> stat ()

%% test/octave.test/system/stat-3.m
%!error <Invalid call to stat> stat ("foo", 1)

%% test/octave.test/system/lstat-1.m
%!test
%! [s, err, msg] = lstat (filesep);
%! assert ((err == 0
%! && isstruct (s)
%! && isfield (s, "dev")
%! && isfield (s, "ino")
%! && isfield (s, "modestr")
%! && isfield (s, "nlink")
%! && isfield (s, "uid")
%! && isfield (s, "gid")
%! && isfield (s, "size")
%! && isfield (s, "atime")
%! && isfield (s, "mtime")
%! && isfield (s, "ctime")
%! && ischar (msg)));

%% test/octave.test/system/lstat-2.m
%!error <Invalid call to lstat> lstat ()

%% test/octave.test/system/lstat-3.m
%!error <Invalid call to lstat> lstat ("foo", 1)

%% test/octave.test/system/glob-1.m
%!assert (iscell (glob ([filesep "*"])))

%% test/octave.test/system/glob-2.m
%!error <Invalid call to glob> glob ()

%% test/octave.test/system/glob-3.m
%!error <Invalid call to glob> glob ("foo", 1)

%% test/octave.test/system/fnmatch-1.m
%!test
%! string_fill_char = setstr (0);
%! assert ((fnmatch ("a*a", {"aba"; "xxxba"; "aa"}) == [1; 0; 1]
%! && fnmatch ({"a*a"; "b*b"}, "bob")
%! && fnmatch ("x[0-5]*", {"x1"; "x6"}) == [1; 0]
%! && fnmatch ("x[0-5]*", {"x1"; "x6"; "x001"}) == [1; 0; 1]
%! && fnmatch ("x???y", {"xabcy"; "xy"}) == [1; 0]));

%% test/octave.test/system/fnmatch-2.m
%!error <Invalid call to fnmatch> fnmatch ()

%% test/octave.test/system/fnmatch-3.m
%!error <Invalid call to fnmatch> fnmatch ("foo", "bar", 3)

%% test/octave.test/system/file_in_path-1.m
%!assert (ischar (file_in_path (path (), "date.m")))

%% test/octave.test/system/file_in_path-2.m
%!error <invalid option> file_in_path ("foo", "bar", 1)

%% test/octave.test/system/file_in_path-3.m
%!error <Invalid call to file_in_path> file_in_path ()

%% test/octave.test/system/file_in_path-4.m
%!error <Invalid call to file_in_path> file_in_path ("foo", "bar", "baz", "ooka")

%% test/octave.test/system/tilde_expand-1.m
%!testif HAVE_GETPWUID
%! x = getpwuid (getuid ());
%! assert ((strcmp (x.dir, tilde_expand ("~"))
%! && strcmp (x.dir, tilde_expand (sprintf ("~%s", x.name)))
%! && strcmp ("foobar", tilde_expand ("foobar"))));

%% test/octave.test/system/tilde_expand-2.m
%!error <Invalid call to tilde_expand> tilde_expand ()

%% test/octave.test/system/tilde_expand-3.m
%!error <Invalid call to tilde_expand> tilde_expand ("str", 2)

%% test/octave.test/system/getpgrp-1.m
%!testif HAVE_GETPGRP
%! assert (getpgrp () > 0);

%% test/octave.test/system/getpgrp-2.m
%!error <... getpgrp> getpgrp (1)

%% test/octave.test/system/getpid-1.m
%!assert (getpid () > 0)

%% test/octave.test/system/getpid-2.m
%!error <... getpid> getpid (1)

%% test/octave.test/system/getppid-1.m
%!testif HAVE_GETPPID
%! assert (getppid () > 0);

%% test/octave.test/system/getppid-2.m
%!error <... getppid> getppid (1)

%% test/octave.test/system/geteuid-1.m
%!assert (geteuid () >= 0)

%% test/octave.test/system/geteuid-2.m
%!error <... geteuid> geteuid (1)

%% test/octave.test/system/getuid-1.m
%!assert (getuid () >= 0)

%% test/octave.test/system/getuid-2.m
%!error <... getuid> getuid (1)

%% test/octave.test/system/getegid-1.m
%!assert (getegid () >= 0)

%% test/octave.test/system/getegid-2.m
%!error <... getegid> getegid (1)

%% test/octave.test/system/getgid-1.m
%!assert (getgid () >= 0)

%% test/octave.test/system/getgid-2.m
%!error <... getgid> getgid (1)

%% test/octave.test/system/getenv-1.m
%!assert (getenv ("HOME"), tilde_expand ("~"))

%% test/octave.test/system/getenv-2.m
%!error <Invalid call to getenv> getenv ()

%% test/octave.test/system/getenv-3.m
%!error <Invalid call to getenv> getenv ("foo", 1)

%% test/octave.test/system/getenv-4.m
%!test
%! wns = warning ("query", "Octave:num-to-str");
%! warning ("on", "Octave:num-to-str");
%! fail ("getenv (1)", "warning");
%! warning (wns.state, "Octave:num-to-str");

%% test/octave.test/system/putenv-1.m
%!test
%! putenv ("foobar", "baz");
%! assert (getenv ("foobar"), "baz");

%% test/octave.test/system/putenv-2.m
%!error <Invalid call to putenv> putenv ()

%% test/octave.test/system/putenv-3.m
%!error <Invalid call to putenv> putenv ("foo", "bar", 1)

%% test/octave.test/system/putenv-4.m
%!test
%! wns = warning ("query", "Octave:num-to-str");
%! warning ("on", "Octave:num-to-str");
%! fail ("putenv (1, 2)","warning");
%! warning (wns.state, "Octave:num-to-str");

%% test/octave.test/system/cd-1.m
%!test
%! xdir = pwd ();
%! cd /
%! d1 = pwd ();
%! cd (xdir);
%! if (ispc () && ! isunix ())
%!   # should be a drive letter
%!   assert (length (d1), 3);
%!   assert (d1(2), ":");
%!   assert (d1(3), "\\");
%! else
%!   assert ("/", d1);
%! endif
%! assert (pwd(), xdir);

%% test/octave.test/system/cd-2.m
%!error cd (1)

%% test/octave.test/system/pwd-1.m
%!assert (ischar (pwd ()))

%% test/octave.test/system/getpwent-1.m
%!testif HAVE_GETPWENT
%! s = getpwent ();
%! endpwent (); 
%! assert ((isstruct (s)
%! && isfield (s, "name")
%! && isfield (s, "passwd")
%! && isfield (s, "uid")
%! && isfield (s, "gid")
%! && isfield (s, "gecos")
%! && isfield (s, "dir")
%! && isfield (s, "shell")));

%% test/octave.test/system/getpwent-2.m
%!error <Invalid call to getpwent> getpwent (1)

%% test/octave.test/system/getpwuid-1.m
%!testif HAVE_GETPWUID
%! x = getpwent ();
%! y = getpwuid (x.uid);
%! endpwent (); 
%! assert (strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid);

%% test/octave.test/system/getpwuid-2.m
%!error <Invalid call to getpwuid> getpwuid ()

%% test/octave.test/system/getpwuid-3.m
%!error <Invalid call to getpwuid> getpwuid (1, 2)

%% test/octave.test/system/getpwnam-1.m
%!testif HAVE_GETPWNAM
%! x = getpwent ();
%! y = getpwnam (x.name);
%! endpwent (); 
%! assert (strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid);

%% test/octave.test/system/getpwnam-2.m
%!error <Invalid call to getpwnam> getpwnam ()

%% test/octave.test/system/getpwnam-3.m
%!error <Invalid call to getpwnam> getpwnam ("foo", 1)

%% test/octave.test/system/setpwent-1.m
%!testif HAVE_SETPWENT
%! x = getpwent ();
%! setpwent ();
%! y = getpwent ();
%! endpwent (); 
%! assert (strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid);

%% test/octave.test/system/setpwent-2.m
%!error <Invalid call to setpwent> setpwent (1)

%% test/octave.test/system/endpwent-1.m
%!error <Invalid call to endpwent> endpwent (1)

%% test/octave.test/system/getgrent-1.m
%!testif HAVE_GETGRENT
%! x = getgrent ();
%! endgrent ();
%! assert ((isstruct (x)
%! && isfield (x, "name")
%! && isfield (x, "passwd")
%! && isfield (x, "gid")
%! && isfield (x, "mem")));

%% test/octave.test/system/getgrent-2.m
%!error <Invalid call to getgrent> getgrent (1)

%% test/octave.test/system/getgrgid-1.m
%!testif HAVE_GETGRGID
%! x = getgrent ();
%! y = getgrgid (x.gid);
%! endgrent ();
%! assert (strcmp (x.name, y.name) && x.gid == y.gid);

%% test/octave.test/system/getgrgid-2.m
%!error <Invalid call to getgrgid> getgrgid ()

%% test/octave.test/system/getgrgid-3.m
%!error <Invalid call to getgrgid> getgrgid (1, 2)

%% test/octave.test/system/getgrnam-1.m
%!testif HAVE_GETGRNAM
%! x = getgrent ();
%! y = getgrnam (x.name);
%! endgrent ();
%! assert (strcmp (x.name, y.name) && x.gid == y.gid);

%% test/octave.test/system/getgrnam-2.m
%!error <Invalid call to getgrnam> getgrnam ()

%% test/octave.test/system/getgrnam-3.m
%!error <Invalid call to getgrnam> getgrnam ("foo", 1)

%% test/octave.test/system/setgrent-1.m
%!testif HAVE_SETGRENT
%! x = getgrent ();
%! setgrent ();
%! y = getgrent ();
%! endgrent ();
%! assert (strcmp (x.name, y.name) && x.gid == y.gid);

%% test/octave.test/system/setgrent-2.m
%!error <Invalid call to setgrent> setgrent (1)

%% test/octave.test/system/endgrent-1.m
%!error <Invalid call to endgrent> endgrent (1)

%% test/octave.test/system/isieee-1.m
%!assert (isieee () == 1 || isieee () == 0)

%% test/octave.test/system/octave_config_info-1.m
%!assert (isstruct (octave_config_info ()))

%% test/octave.test/system/getrusage-1.m
%!assert (isstruct (getrusage ()))

