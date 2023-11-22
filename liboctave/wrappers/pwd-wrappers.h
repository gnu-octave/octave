////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_pwd_wrappers_h)
#define octave_pwd_wrappers_h 1

#include <sys/types.h>

#if defined __cplusplus
extern "C" {
#endif

struct passwd;

// We can't include <pwd.h> without the risk of overwriting function names
// with preprocessor deinitions.  Define a struct that can hold the needed
// fields, and use a function to convert from struct passwd to it.
struct octave_passwd_wrapper
{
  char *pw_name;
  char *pw_passwd;
  uid_t pw_uid;
  gid_t pw_gid;
  char *pw_gecos;
  char *pw_dir;
  char *pw_shell;
};

extern OCTAVE_API struct passwd *octave_getpwent_wrapper (void);

extern OCTAVE_API struct passwd *octave_getpwuid_wrapper (uid_t uid);

extern OCTAVE_API struct passwd *octave_getpwnam_wrapper (const char *nam);

extern OCTAVE_API void octave_setpwent_wrapper (void);

extern OCTAVE_API void octave_endpwent_wrapper (void);

extern OCTAVE_API void octave_endpwent_wrapper (void);

extern OCTAVE_API void
octave_from_passwd (const struct passwd *pw,
                    struct octave_passwd_wrapper *oct_pw);

#if defined __cplusplus
}
#endif

#endif
