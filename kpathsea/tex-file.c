/* tex-file.c: high-level file searching by format.

Copyright (C) 1993, 94, 95, 96, 97 Karl Berry.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include <kpathsea/config.h>

#include <kpathsea/c-fopen.h>
#include <kpathsea/c-vararg.h>
#include <kpathsea/cnf.h>
#include <kpathsea/default.h>
#include <kpathsea/expand.h>
#include <kpathsea/fontmap.h>
#include <kpathsea/paths.h>
#include <kpathsea/pathsearch.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/tex-make.h>
#include <kpathsea/variable.h>


/* See tex-file.h.  */
const_string kpse_fallback_font = NULL;
const_string kpse_fallback_resolutions_string = NULL;
unsigned *kpse_fallback_resolutions = NULL;
kpse_format_info_type kpse_format_info[kpse_last_format];

/* These are not in the structure
   because it's annoying to initialize lists in C.  */
#define GF_ENVS "GFFONTS", GLYPH_ENVS
#define PK_ENVS "PKFONTS", "TEXPKS", GLYPH_ENVS
#define GLYPH_ENVS "GLYPHFONTS", "TEXFONTS"
#define AFM_ENVS "AFMFONTS"
#define BASE_ENVS "MFBASES", "TEXMFINI"
#define BIB_ENVS "BIBINPUTS", "TEXBIB"
#define BST_ENVS "BSTINPUTS"
#define CNF_ENVS "TEXMFCNF"
#define DB_ENVS "TEXMFDBS"
#define FMT_ENVS "TEXFORMATS", "TEXMFINI"
#define FONTMAP_ENVS "TEXFONTMAPS"
#define MEM_ENVS "MPMEMS", "TEXMFINI"
#define MF_ENVS "MFINPUTS"
#define MFPOOL_ENVS "MFPOOL", "TEXMFINI"
#define MFT_ENVS "MFTINPUTS"
#define MP_ENVS "MPINPUTS"
#define MPPOOL_ENVS "MPPOOL", "TEXMFINI" 
#define MPSUPPORT_ENVS "MPSUPPORT"
#define OCP_ENVS "OCPINPUTS"
#define OFM_ENVS "OFMFONTS", "TEXFONTS"
#define OPL_ENVS "OPLFONTS", "TEXFONTS"
#define OTP_ENVS "OTPINPUTS"
#define OVF_ENVS "OVFFONTS", "TEXFONTS"
#define OVP_ENVS "OVPFONTS", "TEXFONTS"
#define PICT_ENVS "TEXPICTS", TEX_ENVS
#define TEX_ENVS "TEXINPUTS"
#define TEXDOC_ENVS "TEXDOCS"
#define TEXPOOL_ENVS "TEXPOOL", "TEXMFINI"
#define TEXSOURCE_ENVS "TEXSOURCES"
#define TEX_PS_HEADER_ENVS "TEXPSHEADERS", "PSHEADERS"
#define TFM_ENVS "TFMFONTS", "TEXFONTS"
#define TROFF_FONT_ENVS "TRFONTS"
#define TYPE1_ENVS "T1FONTS", "T1INPUTS", TEX_PS_HEADER_ENVS
#define VF_ENVS "VFFONTS", "TEXFONTS"
#define DVIPS_CONFIG_ENVS "TEXCONFIG"
#define IST_ENVS "TEXINDEXSTYLE", "INDEXSTYLE"

/* The compiled-in default list, DEFAULT_FONT_SIZES, is intended to be
   set from the command line (presumably via the Makefile).  */

#ifndef DEFAULT_FONT_SIZES
#define DEFAULT_FONT_SIZES ""
#endif

void
kpse_init_fallback_resolutions P1C(string, envvar)
{
  string size, orig_size_list;
  const_string size_var = ENVVAR (envvar, "TEXSIZES");
  string size_str = getenv (size_var);
  unsigned *last_resort_sizes = NULL;
  unsigned size_count = 0;
  const_string default_sizes = kpse_fallback_resolutions_string
                         ? kpse_fallback_resolutions_string
                         : DEFAULT_FONT_SIZES; 
  string size_list = kpse_expand_default (size_str, default_sizes);
  
  orig_size_list = size_list; /* For error messages.  */

  /* Initialize the list of last-resort sizes.  */
  for (size = kpse_path_element (size_list); size != NULL;
       size = kpse_path_element (NULL))
    {
      unsigned s;
      if (! *size) /* Skip empty elements.  */
        continue;
      
      s = atoi (size);
      if (size_count && s < last_resort_sizes[size_count - 1]) {
    WARNING1 ("kpathsea: last resort size %s not in ascending order, ignored",
          size);
      } else {
        size_count++;
        XRETALLOC (last_resort_sizes, size_count, unsigned);
        last_resort_sizes[size_count - 1] = atoi (size);
      }
    }

  /* Add a zero to mark the end of the list.  */
  size_count++;
  XRETALLOC (last_resort_sizes, size_count, unsigned);
  last_resort_sizes[size_count - 1] = 0;

  /* If we didn't expand anything, we won't have allocated anything.  */
  if (size_str && size_list != size_str)
    free (size_list);
    
  kpse_fallback_resolutions = last_resort_sizes;
}

/* We should be able to set the program arguments in the same way.  Not
   to mention the path values themselves.  */

void
kpse_set_program_enabled P3C(kpse_file_format_type, fmt,  boolean, value,
                             kpse_src_type, level)
{
  kpse_format_info_type *f = &kpse_format_info[fmt];
  if (level >= f->program_enable_level) {
    f->program_enabled_p = value;
    f->program_enable_level = level;
  }
}


/* Web2c and kpsewhich have command-line options to set this stuff.  May
   as well have a common place.  */

void
kpse_maketex_option P2C(const_string, fmtname,  boolean, value)
{
  kpse_file_format_type fmt;
  
  /* Trying to match up with the suffix lists unfortunately doesn't work
     well, since that would require initializing the formats.  */
  if (FILESTRCASEEQ (fmtname, "pk")) {
    fmt = kpse_pk_format;
  } else if (FILESTRCASEEQ (fmtname, "mf")) {
    fmt = kpse_tex_format;
  } else if (FILESTRCASEEQ (fmtname, "tex")) {
    fmt = kpse_tex_format;
  } else if (FILESTRCASEEQ (fmtname, "tfm")) {
    fmt = kpse_tex_format;
  }
  kpse_set_program_enabled (fmt, value, kpse_src_cmdline);
}

/* Macro subroutines for `init_path'.  TRY_ENV checks if an envvar ENAME
   is set and non-null, and sets var to ENAME if so.  */
#define TRY_ENV(ename) do { \
  string evar = ename; \
} while (0)

/* And EXPAND_DEFAULT calls kpse_expand_default on try_path and the
   present info->path.  */
#define EXPAND_DEFAULT(try_path, source_string)			\
  if (try_path) {						\
      info->raw_path = try_path;				\
      info->path = kpse_expand_default (try_path, info->path);	\
      info->path_source = source_string;			\
  }

/* Find the final search path to use for the format entry INFO, given
   the compile-time default (DEFAULT_PATH), and the environment
   variables to check (the remaining arguments, terminated with NULL).
   We set the `path' and `path_source' members of INFO.  The
   `client_path' member must already be set upon entry.  */

static void
init_path PVAR2C(kpse_format_info_type *, info, const_string, default_path, ap)
{
  string env_name;
  string var = NULL;
  
  info->default_path = default_path;

  /* First envvar that's set to a nonempty value will exit the loop.  If
     none are set, we want the first cnf entry that matches.  Find the
     cnf entries simultaneously, to avoid having to go through envvar
     list twice -- because of the PVAR?C macro, that would mean having
     to create a str_list and then use it twice.  Yuck.  */
  while ((env_name = va_arg (ap, string)) != NULL) {
    /* Since sh doesn't like envvar names with `.', check PATH_prog
       rather than PATH.prog.  */
    if (!var) {
      string evar = concat3 (env_name, "_", program_invocation_short_name);
      string env_value = getenv (evar);
      if (env_value && *env_value) {
        var = evar;
      } else {
        free (evar);

        /* Try simply PATH.  */
        env_value = getenv (env_name);
        if (env_value && *env_value) {
          var = env_name;        
        }
      }
    }
    
    /* If we are initializing the cnf path, don't try to get any
       values from the cnf files; that's infinite loop time.  */
    if (!info->cnf_path
        && (!info->suffix || !FILESTRCASEEQ (info->suffix, ".cnf")))
      info->cnf_path = kpse_cnf_get (env_name);
      
    if (var && info->cnf_path)
      break;
  }
  va_end (ap);
  
  /* Expand any extra :'s.  For each level, we replace an extra : with
     the path at the next lower level.  For example, an extra : in a
     user-set envvar should be replaced with the path from the cnf file.
     things are complicated because none of the levels above the very
     bottom are guaranteed to exist.  */

  /* Assume we can reliably start with the compile-time default.  */
  info->path = info->raw_path = info->default_path;
  info->path_source = "compile-time paths.h";

  EXPAND_DEFAULT (info->cnf_path, "texmf.cnf");
  EXPAND_DEFAULT (info->client_path, "program config file");
  if (var)
    EXPAND_DEFAULT (getenv (var), concat (var, " environment variable"));
  EXPAND_DEFAULT (info->override_path, "application override variable");
  info->path = kpse_path_expand (info->path);
}}


/* Some file types have more than one suffix.  (We don't actually need
   the constant argument here, but ANSI requires it.)  */

static void
add_alt_suffixes PVAR1C(kpse_format_info_type *, info,  ap)
{
  const_string s;
  unsigned count = 0;
  
  while ((s = va_arg (ap, string)) != NULL) {
    count++;
    XRETALLOC (info->alt_suffix, count + 1, const_string);
    info->alt_suffix[count - 1] = s;
  }
  va_end (ap);
  info->alt_suffix[count] = NULL;
}}


/* The path spec we are defining, one element of the global array.  */
#define FMT_INFO kpse_format_info[format]


/* Call add_alt_suffixes.  */
#define ALT_SUFFIXES(args) add_alt_suffixes (&FMT_INFO, args, NULL)

/* Call `init_path', including appending the trailing NULL to the envvar
   list. Also initialize the fields not needed in setting the path.  */
#define INIT_FORMAT(ext, default_path, envs) \
  FMT_INFO.suffix = FMT_INFO.type = ext; \
  init_path (&FMT_INFO, default_path, envs, NULL)


/* A few file types allow for runtime generation by an external program.
   kpse_init_prog may have already initialized it (the `program'
   member).  Here we allow people to turn it off or on in the config
   file, by setting the variable whose name is the uppercasified program
   name to 0 or 1.  */

static void
init_maketex P3C(kpse_file_format_type, fmt,  const_string, dflt_prog,
                 const_string, args)
{
  kpse_format_info_type *f = &kpse_format_info[fmt];
  const_string prog = f->program ? f->program : dflt_prog; /* MakeTeXPK */
  string PROG = uppercasify (prog);             /* MAKETEXPK */
  string progval = kpse_var_value (PROG);       /* ENV/cnf{"MAKETEXPK"} */

  /* Doesn't hurt to always set this info.  */
  f->program = prog;
  f->program_args = args;

  if (progval && *progval) {
    /* This might actually be from an environment variable value, but in
       that case, we'll have previously set it from kpse_init_prog.  */
    kpse_set_program_enabled (fmt, *progval == '1', kpse_src_client_cnf);
  }
  
  free (PROG);
}

/* We need this twice, so ... */
#define MAKETEXPK_ARGS \
  "$KPATHSEA_DPI $MAKETEX_BASE_DPI $MAKETEX_MAG $MAKETEX_MODE"

/* Initialize everything for FORMAT.  */

const_string
kpse_init_format P1C(kpse_file_format_type, format)
{
  /* If we get called twice, don't redo all the work.  */
  if (FMT_INFO.path)
    return FMT_INFO.path;
    
  switch (format)
    { /* We might be able to avoid repeating `gf' or whatever so many
         times with token pasting, but it doesn't seem worth it.  */
    case kpse_gf_format:
      INIT_FORMAT ("gf", DEFAULT_GFFONTS, GF_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_pk_format:
      init_maketex (format, "MakeTeXPK", MAKETEXPK_ARGS);
      INIT_FORMAT ("pk", DEFAULT_PKFONTS, PK_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_any_glyph_format:
      init_maketex (format, "MakeTeXPK", MAKETEXPK_ARGS);
      INIT_FORMAT (NULL, DEFAULT_GLYPHFONTS, GLYPH_ENVS);
      FMT_INFO.type = "bitmap font";
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_afm_format:
      INIT_FORMAT (".afm", DEFAULT_AFMFONTS, AFM_ENVS);
      break;
    case kpse_base_format:
      INIT_FORMAT (".base", DEFAULT_MFBASES, BASE_ENVS);
      break;
    case kpse_bib_format:
      INIT_FORMAT (".bib", DEFAULT_BIBINPUTS, BIB_ENVS);
      break;
    case kpse_bst_format:
      INIT_FORMAT (".bst", DEFAULT_BSTINPUTS, BST_ENVS);
      break;
    case kpse_cnf_format:
      INIT_FORMAT (".cnf", DEFAULT_TEXMFCNF, CNF_ENVS);
      break;
    case kpse_db_format:
      INIT_FORMAT ("ls-R", DEFAULT_TEXMFDBS, DB_ENVS);
      break;
    case kpse_fmt_format:
      INIT_FORMAT (".fmt", DEFAULT_TEXFORMATS, FMT_ENVS);
      break;
    case kpse_fontmap_format:
      INIT_FORMAT (".map", DEFAULT_TEXFONTMAPS, FONTMAP_ENVS);
      break;
    case kpse_mem_format:
      INIT_FORMAT (".mem", DEFAULT_MPMEMS, MEM_ENVS);
      break;
    case kpse_mf_format:
      init_maketex (format, "MakeTeXMF", NULL);
      INIT_FORMAT (".mf", DEFAULT_MFINPUTS, MF_ENVS);
      break;
    case kpse_mft_format:
      INIT_FORMAT (".mft", DEFAULT_MFTINPUTS, MFT_ENVS);
      break;
    case kpse_mfpool_format:
      INIT_FORMAT (".pool", DEFAULT_MFPOOL, MFPOOL_ENVS);
      break;
    case kpse_mp_format:
      INIT_FORMAT (".mp", DEFAULT_MPINPUTS, MP_ENVS);
      break;
    case kpse_mppool_format:
      INIT_FORMAT (".pool", DEFAULT_MPPOOL, MPPOOL_ENVS);
      break;
    case kpse_mpsupport_format:
      INIT_FORMAT (NULL, DEFAULT_MPSUPPORT, MPSUPPORT_ENVS);
      FMT_INFO.type = "MetaPost support";
      break;
    case kpse_ocp_format:
      init_maketex (format, "MakeOmegaOCP", NULL);
      INIT_FORMAT (".ocp", DEFAULT_OCPINPUTS, OCP_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_ofm_format:
      init_maketex (format, "MakeOmegaOFM", NULL);
      INIT_FORMAT (".ofm", DEFAULT_OFMFONTS, OFM_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_opl_format:
      INIT_FORMAT (".opl", DEFAULT_OPLFONTS, OPL_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_otp_format:
      INIT_FORMAT (".otp", DEFAULT_OTPINPUTS, OTP_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_ovf_format:
      INIT_FORMAT (".ovf", DEFAULT_OVFFONTS, OVF_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_ovp_format:
      INIT_FORMAT (".ovp", DEFAULT_OVPFONTS, OVP_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_pict_format:
      INIT_FORMAT (NULL, DEFAULT_TEXINPUTS, PICT_ENVS);
      FMT_INFO.type = "graphic/figure";
      /* It is conveniently coincidental that this comes before
         kpse_tex_format, since if the user runs kpsewhich --format=eps,
         they'll get kpse_pict_format instead of tex_format.  */
#define PICT_SUFFIXES ".eps", ".epsi"
      ALT_SUFFIXES (PICT_SUFFIXES);
      break;
    case kpse_tex_format:
      init_maketex (format, "MakeTeXTeX", NULL);
      INIT_FORMAT (".tex", DEFAULT_TEXINPUTS, TEX_ENVS);
/* If this list changes, also change tex.ch.  */
#define TEX_SUFFIXES ".ltx", ".dtx", ".texi", ".texinfo", ".txi", PICT_SUFFIXES
      ALT_SUFFIXES (TEX_SUFFIXES);
      break;
    case kpse_tex_ps_header_format:
      INIT_FORMAT (NULL, DEFAULT_TEXPSHEADERS, TEX_PS_HEADER_ENVS);
      FMT_INFO.type = "PostScript header/font";
      ALT_SUFFIXES (".pro");
      break;
    case kpse_texdoc_format:
      INIT_FORMAT (NULL, DEFAULT_TEXDOCS, TEXDOC_ENVS);
      FMT_INFO.type = "TeX system documentation";
      break;
    case kpse_texpool_format:
      INIT_FORMAT (".pool", DEFAULT_TEXPOOL, TEXPOOL_ENVS);
      break;
    case kpse_texsource_format:
      INIT_FORMAT (NULL, DEFAULT_TEXSOURCES, TEXSOURCE_ENVS);
      FMT_INFO.type = "TeX system sources";
      break;
    case kpse_tfm_format:
      init_maketex (format, "MakeTeXTFM", NULL);
      INIT_FORMAT (".tfm", DEFAULT_TFMFONTS, TFM_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_troff_font_format:
      INIT_FORMAT (NULL, DEFAULT_TRFONTS, TROFF_FONT_ENVS);
      FMT_INFO.type = "Troff fonts";
      break;
    case kpse_type1_format:
      INIT_FORMAT (".pfa", DEFAULT_T1FONTS, TYPE1_ENVS);
      FMT_INFO.type = "type1 fonts";
      ALT_SUFFIXES (".pfb");
      break;
    case kpse_vf_format:
      INIT_FORMAT (".vf", DEFAULT_VFFONTS, VF_ENVS);
      FMT_INFO.suffix_search_only = true;
      break;
    case kpse_dvips_config_format:
      INIT_FORMAT (NULL, DEFAULT_TEXCONFIG, DVIPS_CONFIG_ENVS);
      FMT_INFO.type = "dvips config";
      break;
    case kpse_ist_format:
      INIT_FORMAT (".ist", DEFAULT_INDEXSTYLE, IST_ENVS);
      break;
    default:
      FATAL1 ("kpse_init_format: Unknown format %d", format);
    }

#ifdef KPSE_DEBUG
#define MAYBE(member) (FMT_INFO.member ? FMT_INFO.member : "(none)")

  /* Describe the monster we've created.  */
  if (KPSE_DEBUG_P (KPSE_DEBUG_PATHS))
    {
      DEBUGF2 ("Search path for %s files (from %s)\n",
              FMT_INFO.type, FMT_INFO.path_source);
      DEBUGF1 ("  = %s\n", FMT_INFO.path);
      DEBUGF1 ("  before expansion = %s\n", FMT_INFO.raw_path);
      DEBUGF1 ("  application override path = %s\n", MAYBE (override_path));
      DEBUGF1 ("  application config file path = %s\n", MAYBE (client_path));
      DEBUGF1 ("  texmf.cnf path = %s\n", MAYBE (cnf_path));
      DEBUGF1 ("  compile-time path = %s\n", MAYBE (default_path));
      DEBUGF1 ("  default suffix = %s\n", MAYBE (suffix));
      DEBUGF  ("  other suffixes =");
      if (FMT_INFO.alt_suffix) {
        const_string *alt;
        for (alt = FMT_INFO.alt_suffix; alt && *alt; alt++) {
          fprintf (stderr, " %s", *alt);
        }
        putc ('\n', stderr);
      } else {
        fputs (" (none)\n", stderr);
      }
      DEBUGF1 ("  search only with suffix = %d\n",FMT_INFO.suffix_search_only);
      DEBUGF1 ("  numeric format value = %d\n", format);
      DEBUGF1 ("  runtime generation program = %s\n", MAYBE (program));
      DEBUGF1 ("  extra program args = %s\n", MAYBE (program_args));
      DEBUGF1 ("  program enabled = %d\n", FMT_INFO.program_enabled_p);
      DEBUGF1 ("  program enable level = %d\n", FMT_INFO.program_enable_level);
    }
#endif /* KPSE_DEBUG */

  return FMT_INFO.path;
}

/* Look up a file NAME of type FORMAT, and the given MUST_EXIST.  This
   initializes the path spec for FORMAT if it's the first lookup of that
   type.  Return the filename found, or NULL.  This is the most likely
   thing for clients to call.  */
   
string
kpse_find_file P3C(const_string, name,  kpse_file_format_type, format,
                   boolean, must_exist)
{
  const_string suffix;
  const_string *alt;
  unsigned name_len = 0;
  boolean name_has_suffix_already = false;
  string ret = NULL;

  /* A null or empty NAME is surely a mistake.  */
  assert (name && *name);
  
  if (FMT_INFO.path == NULL)
    kpse_init_format (format);

  /* Does NAME already end in a possible suffix?  */
  suffix = FMT_INFO.suffix;
  if (suffix) {
    /* Avoid the strlen's entirely if no SUFFIX.  */
    unsigned suffix_len = strlen (suffix);
    name_len = strlen (name);
    name_has_suffix_already = (name_len > suffix_len
                      && FILESTRCASEEQ (suffix, name + name_len - suffix_len));
  }
  for (alt = FMT_INFO.alt_suffix; !name_has_suffix_already && alt && *alt;
       alt++) {
    unsigned suffix_len = strlen (*alt);
    if (!name_len) name_len = strlen (name);
    name_has_suffix_already = (name_len > suffix_len
                        && FILESTRCASEEQ (*alt, name + name_len - suffix_len));
  }
  
  /* Search #1: NAME has a suffix (any suffix). For example, foo.sty,
     foo.eps, foo.tex.  Since multiple suffixes (foo.bar.tex) are
     relatively rare, first try NAME.    (This loses
     if given `Times-Roman.tfm', but is ok given `Times-Roman'.)  */
  if (name_has_suffix_already || find_suffix (name)) {
    /* Search for original NAME, since it has a suffix.
    
       If we will search again with `suffix', avoid pounding the disk.
       Really should also avoid disk search until after we check the
       fontmaps, but ... lazy.  Sorry.  */
    boolean must = must_exist && FMT_INFO.suffix_search_only
                   && (!suffix || name_has_suffix_already);
    ret = kpse_path_search (FMT_INFO.path, name, must);
  }

  /* Search #2: If we have `foo' or `foo.bar', and the above search
     failed, try `foo.tex' or `foo.bar.tex' now (but don't try
     `foo.tex.tex'.  This is so we find the `foo.tex' before the
     executable `foo'.  But, if we might search yet again, still don't
     pound the disk.  */
  if (!ret && suffix && !name_has_suffix_already) {
    /* Append `.suffix' and search for it.  */
    string name_with_suffix = concat (name, suffix);
    ret = kpse_path_search (FMT_INFO.path, name_with_suffix,
                            FMT_INFO.suffix_search_only ? must_exist : false);
    free (name_with_suffix);
  }

  /* Search #3 (fonts only): Look for aliases for NAME.  I don't
     think this is necessary for VF files, since only names people might
     mention in TeX documents -- i.e., TFM's -- need be aliased.  */
  if (!ret && (format == kpse_tfm_format || format == kpse_gf_format
               || format == kpse_pk_format)) {
    string mapped_name;
    string *mapped_names;
    /* Include a suffix in what we pass to `kpse_fontmap_lookup'; otherwise,
       when we are searching for `circle10', for example, we'll just get
       back `lcircle10', and that basename won't match anything in ls-R.  */
    string name_with_suffix = name_has_suffix_already ? (string) name
                              : concat (name, suffix);
    mapped_names = kpse_fontmap_lookup (name_with_suffix);
    while (mapped_names && (mapped_name = *mapped_names++) && !ret) {
      ret = kpse_path_search (FMT_INFO.path, mapped_name, false);
    }
    if (!name_with_suffix)
      free (name_with_suffix);
  }

  /* Search #4 (sort of): Call MakeTeXTFM or whatever to create a
     missing file.  */
  if (!ret && must_exist) {
     ret = kpse_make_tex (format, name);
  }

  /* Search #5: If we have `foo', look for `foo' (if non-suffix searches
     are enabled for this FORMAT).  This is the last-chance search, so
     never disable the caller's MUST_EXIST.  */
  if (!ret && (name_has_suffix_already || !FMT_INFO.suffix_search_only)) {
    ret = kpse_path_search (FMT_INFO.path, name, must_exist);
  }

  return ret;
}

/* Open NAME along the search path for TYPE for reading and return the
   resulting file, or exit with an error message.  */

FILE *
kpse_open_file P2C(const_string, name,  kpse_file_format_type, type)
{
  string fullname = kpse_find_file (name, type, true);
  const_string mode = type == kpse_gf_format 
                      || type == kpse_ocp_format
                      || type == kpse_ofm_format
                      || type == kpse_ovf_format
                      || type == kpse_tfm_format
                      || type == kpse_vf_format
                      ? FOPEN_RBIN_MODE : FOPEN_R_MODE;
  FILE *f = fullname ? fopen (fullname, mode) : NULL;
  if (!f) {
    if (fullname) {
      perror (fullname);
      exit (1);
    } else {
      FATAL2 ("%s file `%s' not found", kpse_format_info[type].type, name);
    }
  }
  
  return f;
}
