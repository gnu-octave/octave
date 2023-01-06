////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_file_ops_h)
#define octave_file_ops_h 1

#include "octave-config.h"

#include <string>

#include <sys/types.h>

#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

OCTAVE_BEGIN_NAMESPACE(file_ops)

typedef std::string (*tilde_expansion_hook) (const std::string&);

// If non-null, this contains the address of a function that the
// application wants called before trying the standard tilde
// expansions.  The function is called with the text sans tilde, and
// returns a malloc()'ed string which is the expansion, or a NULL
// pointer if the expansion fails.

extern OCTAVE_API tilde_expansion_hook tilde_expansion_preexpansion_hook;

// If non-null, this contains the address of a function to call if the
// standard meaning for expanding a tilde fails.  The function is
// called with the text (sans tilde, as in "foo"), and returns a
// malloc()'ed string which is the expansion, or a NULL pointer if
// there is no expansion.

extern OCTAVE_API tilde_expansion_hook tilde_expansion_failure_hook;

// When non-null, this is a NULL terminated array of strings which are
// duplicates for a tilde prefix.  Bash uses this to expand '=~' and
// ':~'.

extern OCTAVE_API string_vector tilde_additional_prefixes;

// When non-null, this is a NULL terminated array of strings which
// match the end of a username, instead of just "/".  Bash sets this
// to ':' and '=~'.

extern OCTAVE_API string_vector tilde_additional_suffixes;

// Find the start of a tilde expansion in S, and return the index
// of the tilde which starts the expansion.  Place the length of the
// text which identified this tilde starter in LEN, excluding the
// tilde itself.

extern OCTAVE_API char dev_sep_char (void);

extern OCTAVE_API bool is_dev_sep (char c);

extern OCTAVE_API char dir_sep_char (void);

extern OCTAVE_API std::string dir_sep_str (void);

extern OCTAVE_API std::string dir_sep_chars (void);

extern OCTAVE_API bool is_dir_sep (char c);

// If NAME has a leading ~ or ~user, Unix-style, expand it to the
// user's home directory.  If no ~, or no <pwd.h>, just return NAME.

extern OCTAVE_API std::string tilde_expand (const std::string&);

// A vector version of the above.

extern OCTAVE_API string_vector tilde_expand (const string_vector&);

extern OCTAVE_API std::string concat (const std::string&, const std::string&);

// Return the directory part of a filename or an empty string if
// there is no directory component.  Does not check to see
// whether the file exists or is a directory.

extern OCTAVE_API std::string dirname (const std::string& path);

// Return the tail member of a filename.

extern OCTAVE_API std::string tail (const std::string& path);

// Convert path from UNIX type separators to whatever is the
// system separators.

extern OCTAVE_API std::string
native_separator_path (const std::string& path);

OCTAVE_END_NAMESPACE(file_ops)

extern OCTAVE_API int
mkdir (const std::string&, mode_t);

extern OCTAVE_API int
mkdir (const std::string&, mode_t, std::string&);

extern OCTAVE_API int
recursive_mkdir (const std::string& name, mode_t mode);

extern OCTAVE_API int
recursive_mkdir (const std::string& name, mode_t mode, std::string& msg);

extern OCTAVE_API int
mkfifo (const std::string&, mode_t);

extern OCTAVE_API int
mkfifo (const std::string&, mode_t, std::string&);

extern OCTAVE_API int
link (const std::string&, const std::string&);

extern OCTAVE_API int
link (const std::string&, const std::string&, std::string&);

extern OCTAVE_API int
symlink (const std::string&, const std::string&);

extern OCTAVE_API int
symlink (const std::string&, const std::string&, std::string&);

extern OCTAVE_API int
readlink (const std::string&, std::string&);

extern OCTAVE_API int
readlink (const std::string&, std::string&, std::string&);

extern OCTAVE_API int
rename (const std::string&, const std::string&);

extern OCTAVE_API int
rename (const std::string&, const std::string&, std::string&);

extern OCTAVE_API int
rmdir (const std::string&);

extern OCTAVE_API int
rmdir (const std::string&, std::string&);

extern OCTAVE_API int
recursive_rmdir (const std::string&);

extern OCTAVE_API int
recursive_rmdir (const std::string&, std::string&);

extern OCTAVE_API int
umask (mode_t);

extern OCTAVE_API int
unlink (const std::string&);

extern OCTAVE_API int
unlink (const std::string&, std::string&);

extern OCTAVE_API std::string
tempnam (const std::string&, const std::string&);

extern OCTAVE_API std::string
tempnam (const std::string&, const std::string&, std::string&);

extern OCTAVE_API std::string
canonicalize_file_name (const std::string&);

extern OCTAVE_API std::string
canonicalize_file_name (const std::string&, std::string&);

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
