## Copyright (C) 2008 Ben Abbott
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
## @deftypefn {Function File} {@var{val} =} pathdef ()
## Return the default path for Octave.
## The path information is extracted from one of three sources.
## In order of preference, those are;
##
## @enumerate
## @item @file{~/.octaverc}
## @item @file{<octave-home>/.../<version>/m/startup/octaverc}
## @item Octave's path prior to changes by any octaverc.
## @end enumerate
## @seealso{path, addpath, rmpath, genpath, savepath, pathsep}
## @end deftypefn

function val = pathdef ()

  ## Save the path present when called. This will be used to restore
  ## path when done.
  presentpath = path;

  ## Use Octave's orignal path as the default default.
  val = __pathorig__;

  ## Locate the site octaverc file (is there a better way?).
  prestr = [OCTAVE_HOME, filesep];
  poststr = [filesep, version, filesep, 'm', filesep', 'startup'];
  ncolon = [0, strfind(presentpath,':'), numel(presentpath)+1];
  site_octaverc = '';
  for nc = 1:(numel(ncolon)-1)
    pathdir = presentpath((ncolon(nc)+1):(ncolon(nc+1)-1));
    npre = strfind (pathdir, prestr);
    npost = strfind (pathdir, poststr);
    if (npre == 1 &&
        npost > numel (prestr) &&
        npost == numel (pathdir) - numel (poststr)+1)
      site_octaverc = [pathdir, filesep, 'octaverc'];
      break;
    endif
  endfor
  if isempty (site_octaverc) || ~exist (site_octaverc, 'file')
    regexp_octaverc = [prestr, '*', poststr, filesep, 'octaverc'];
    warning (['pathdef: could not locate `',regexp_octaverc,'''.'])
  endif

  ## locate the user ~\.octaverc file.
  user_octaverc = tilde_expand ("~/.octaverc");

  ## Extract the specified paths from the site and user octaverc's.
  site_pathscript = __extractpath__ (site_octaverc);
  if exist (user_octaverc, 'file')
    user_pathscript = __extractpath__ (user_octaverc);
  else
    user_pathscript = '';
  endif

  ## A path definition in the user octaverc has precedence over the site
  if numel (user_pathscript)
    try
      eval (user_pathscript);
      val = path;
    catch
      warning (['Path defined in ',user_octaverc,' produced an error'])
    end_try_catch
  elseif numel (site_pathscript)
    try
      eval (site_pathscript);
      val = path;
    catch
      warning (['Path defined in ',site_octaverc,' produced an error'])
    end_try_catch
  endif

  ## Restore the path
  path (presentpath);

endfunction


