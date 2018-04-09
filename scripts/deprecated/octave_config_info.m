## Copyright (C) 2016-2018 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} octave_config_info ()
## @deftypefnx {} {} octave_config_info (@var{option})
##
## @code{octave_config_info} is deprecated and will be removed in
## Octave version 5.  Use @code{__have_feature__ (@var{option})} or
## @code{__octave_config_info__} as a replacement.
##
## Return a structure containing configuration and installation
## information for Octave.
##
## If @var{option} is a string, return the configuration information for
## the specified option.
##
## @seealso{computer}
## @end deftypefn

## Deprecated in version 4.2

function [retval, build_env_cell] = octave_config_info (option)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "octave_config_info is obsolete and will be removed from a future version of Octave, please use __have_feature__ or __octave_config_info__ instead.");
  endif

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    info = __octave_config_info__ ();
    ## Structure layout has changed.

    dld = info.dld;
    float_format = info.float_format;
    words_big_endian = info.words_big_endian;
    words_little_endian = info.words_little_endian;

    features = info.build_features;

    env = info.build_environment;
    env_fields = fieldnames (env);
    env_vals = struct2cell (env);
    env_cell = [env_fields, env_vals]';

    info = rmfield (info, {"dld", "float_format", "words_big_endian", ...
                           "words_little_endian", "build_features", ...
                           "build_environment"});

    other_fields = fieldnames (info);
    other_vals = struct2cell (info);
    other_cell = [other_fields, other_vals]';

    retval = struct ("dld", dld,
                     "float_format", float_format,
                     "words_big_endian", words_big_endian,
                     "words_little_endian", words_little_endian,
                     "features", features,
                     env_cell{:}, other_cell{:});
  else
    if (strcmp (option, "features"))
      option = "build_features";
    endif
    retval = __octave_config_info__ (option);
  endif

endfunction
