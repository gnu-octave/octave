## Copyright (C) 2009-2012 David Bateman
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
## @deftypefn  {Function File} {@var{f} =} ftp (@var{host})
## @deftypefnx {Function File} {@var{f} =} ftp (@var{host}, @var{username}, @var{password})
## Connect to the FTP server @var{host} with @var{username} and @var{password}.
## If @var{username} and @var{password} are not specified, user "anonymous"
## with no password is used.  The returned FTP object @var{f} represents the
## established FTP connection.
## @end deftypefn

function obj = ftp (host, username = "anonymous", password = "")
  if (nargin == 0)
    p.host = "";
    p.username = username;
    p.password = password;
    p.curlhandle = tmpnam ("ftp-");
    obj = class (p, "ftp");
  elseif (nargin == 1 && strcmp (class (host), "ftp"))
    obj = host;
  else
    p.host = host;
    p.username = username;
    p.password = password;
    p.curlhandle = tmpnam ("ftp-");
    __ftp__ (p.curlhandle, host, username, password);
    obj = class (p, "ftp");
  endif
endfunction
