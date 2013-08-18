/*

Copyright (C) 2013 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "txt-eng.h"

static const char* symbol_names[] = {
    "alpha",
    "beta",
    "gamma",
    "delta",
    "epsilon",
    "zeta",
    "eta",
    "theta",
    "vartheta",
    "iota",
    "kappa",
    "lambda",
    "mu",
    "nu",
    "xi",
    "pi",
    "rho",
    "sigma",
    "varsigma",
    "tau",
    "equiv",
    "Im",
    "otimes",
    "cap",
    "supset",
    "int",
    "rfloor",
    "lfloor",
    "perp",
    "wedge",
    "rceil",
    "vee",
    "langle",

    "upsilon",
    "phi",
    "chi",
    "psi",
    "omega",
    "Gamma",
    "Delta",
    "Theta",
    "Lambda",
    "Xi",
    "Pi",
    "Sigma",
    "Upsilon",
    "Phi",
    "Psi",
    "Omega",
    "forall",
    "exists",
    "ni",
    "cong",
    "approx",
    "Re",
    "oplus",
    "cup",
    "subseteq",
    "in",
    "lceil",
    "cdot",
    "neg",
    "times",
    "surd",
    "varpi",
    "rangle",

    "leq",
    "infty",
    "clubsuit",
    "diamondsuit",
    "heartsuit",
    "spadesuit",
    "leftrightarrow",
    "leftarrow",
    "uparrow",
    "rightarrow",
    "downarrow",
    "circ",
    "pm",
    "geq",
    "propto",
    "partial",
    "bullet",
    "div",
    "neq",
    "aleph",
    "wp",
    "oslash",
    "supseteq",
    "subset",
    "o",
    "nabla",
    "ldots",
    "prime",
    "0",
    "mid",
    "copyright",

    0
};

// Maps the symbol names (using index from symbol_names array) to
// character codes, using 2 mapping:
// - Unicode
// - MS symbol (using Private Use Area)
static uint32_t symbol_codes[][2] = {
      { 0x03B1, 0xF061 },		// alpha
      { 0x03B2, 0xF062 },		// beta
      { 0x03B3, 0xF067 },		// gamma
      { 0x03B4, 0xF064 },		// delta
      { 0x03B5, 0xF065 },		// epsilon
      { 0x03B6, 0xF07A },		// zeta
      { 0x03B7, 0xF068 },		// eta
      { 0x03B8, 0xF071 },		// theta
      { 0x03D1, 0xF04A },		// vartheta
      { 0x03B9, 0xF069 },		// iota
      { 0x03BA, 0xF06B },		// kappa
      { 0x03BB, 0xF06C },		// lambda
      { 0x03BC, 0xF06D },		// mu
      { 0x03BD, 0xF06E },		// nu
      { 0x03BE, 0xF078 },		// xi
      { 0x03C0, 0xF070 },		// pi
      { 0x03C1, 0xF072 },		// rho
      { 0x03C3, 0xF073 },		// sigma
      { 0x03C2, 0xF056 },		// varsigma
      { 0x03C4, 0xF074 },		// tau
      { 0x2261, 0xF0BA },		// equiv
      { 0x2111, 0xF0C1 },		// Im
      { 0x2297, 0xF0C4 },		// otimes
      { 0x2229, 0xF0C7 },		// cap
      { 0x2283, 0xF0C9 },		// supset
      { 0x222B, 0xF0F2 },		// int
      { 0x230B, 0xF0FB },		// rfloor
      { 0x230A, 0xF0EB },		// lfloor
      { 0x22A5, 0xF05E },		// perp
      { 0x2227, 0xF0D9 },		// wedge
      { 0x2309, 0xF0F9 },		// rceil
      { 0x2228, 0xF0DA },		// vee
      { 0x2220, 0xF0E1 },		// langle

      { 0x03C5, 0xF075 },		// upsilon
      { 0x03C6, 0xF066 },		// phi
      { 0x03C7, 0xF063 },		// chi
      { 0x03C8, 0xF079 },		// psi
      { 0x03C9, 0xF077 },		// omega
      { 0x0393, 0xF047 },		// Gamma
      { 0x0394, 0xF044 },		// Delta
      { 0x0398, 0xF051 },		// Theta
      { 0x039B, 0xF04C },		// Lambda
      { 0x039E, 0xF058 },		// Xi
      { 0x03A0, 0xF050 },		// Pi
      { 0x03A3, 0xF053 },		// Sigma
      { 0x03D2, 0xF055 },		// Upsilon
      { 0x03A6, 0xF046 },		// Phi
      { 0x03A8, 0xF059 },		// Psi
      { 0x03A9, 0xF057 },		// Omega
      { 0x2200, 0xF022 },		// forall
      { 0x2203, 0xF024 },		// exists
      { 0x220B, 0xF027 },		// ni
      { 0x2245, 0xF040 },		// cong
      { 0x2248, 0xF0BB },		// approx
      { 0x211C, 0xF0C2 },		// Re
      { 0x2295, 0xF0C5 },		// oplus
      { 0x222A, 0xF0C8 },		// cup
      { 0x2286, 0xF0CD },		// subseteq
      { 0x2208, 0xF0CE },		// in
      { 0x2308, 0xF0E9 },		// lceil
      { 0x22C5, 0xF0D7 },		// cdot
      { 0x00AC, 0xF0D8 },		// neg
      { 0x00D7, 0xF0B4 },		// times
      { 0x221A, 0xF0D6 },		// surd
      { 0x03D6, 0xF076 },		// varpi
      { 0x232A, 0xF0F1 },		// rangle

      { 0x2264, 0xF0A3 },		// leq
      { 0x221E, 0xF0A5 },		// infty
      { 0x2663, 0xF0A7 },		// clubsuit
      { 0x2666, 0xF0A8 },		// diamondsuit
      { 0x2665, 0xF0A9 },		// heartsuit
      { 0x2660, 0xF0AA },		// spadesuit
      { 0x2194, 0xF0AB },		// leftrightarrow
      { 0x2190, 0xF0AC },		// leftarrow
      { 0x2191, 0xF0AD },		// uparrow
      { 0x2192, 0xF0AE },		// rightarrow
      { 0x2193, 0xF0AF },		// downarrow
      { 0x25CB, 0xF0B0 },		// circ
      { 0x00B1, 0xF0B1 },		// pm
      { 0x2265, 0xF0B3 },		// geq
      { 0x221D, 0xF0B5 },		// propto
      { 0x2202, 0xF0B6 },		// partial
      { 0x2022, 0xF0B7 },		// bullet
      { 0x00F7, 0xF0B8 },		// div
      { 0x2260, 0xF0B9 },		// neq
      { 0x2135, 0xF0C0 },		// aleph
      { 0x2118, 0xF0C3 },		// wp
      { 0x2298, 0xF0C6 },		// oslash
      { 0x2287, 0xF0CA },		// supseteq
      { 0x2282, 0xF0CC },		// subset
      { 0x03BF, 0xF0B0 },		// o
      { 0x2207, 0xF0D1 },		// nabla
      { 0x2026, 0xF0BC },		// ldots
      { 0x2032, 0xF0A2 },		// prime
      { 0x2205, 0xF0C6 },		// 0
      { 0x2223, 0xF0BD },		// mid
      { 0x00A9, 0xF0E3 },		// copyright

      { 0, 0 }
};

uint32_t
text_element_symbol::get_symbol_code (void)
{
  if (code == invalid_code)
    {
      std::string sym = string_value ();

      for (int i = 0; symbol_names[i]; i++)
        {
          if (symbol_names[i] == sym)
            {
              code = symbol_codes[i][0];
              break;
            }
        }
    }

  return code;
}
