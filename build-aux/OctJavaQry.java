// Copyright (C) 2012-2023 The Octave Project Developers
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

// Code used by configure script to locate Java installation variables.
// Only compiled code, OctJavaQry.class, is distributed.

public class OctJavaQry
{
  public static void main (String[] args)
  {
    if (args.length > 0)
    {
      if (args[0].equals ("JAVA_HOME"))
      {
        System.out.println (System.getProperty ("java.home"));
      }
      else if (args[0].equals ("JAVA_LDPATH"))
      {
        System.out.println (System.getProperty ("java.library.path"));
      }
      else if (args[0].equals ("JAVA_BOOTPATH"))
      {
        System.out.println (System.getProperty ("sun.boot.library.path"));
      }
    }
  }
}
