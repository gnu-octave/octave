# paths.make -- installation directories.
#
# The compile-time paths are defined in kpathsea/paths.h, which is built
# from kpathsea/paths.h.in and these definitions.  See kpathsea/INSTALL
# for a description of how the various path-related files are used and
# created.

# Do not change prefix and exec_prefix in Makefile.in!
# configure doesn't propagate the change to the other Makefiles.
# Instead, give the -prefix/-exec-prefix options to configure.
# (See kpathsea/INSTALL for more details.) This is arguably
# a bug, but it's not likely to change soon.
prefix = @prefix@
exec_prefix = @exec_prefix@

# Architecture-dependent executables.
bindir = $(exec_prefix)/bin

# Architecture-independent executables.
scriptdir = $(bindir)

# Architecture-dependent files, such as lib*.a files.
libdir = $(exec_prefix)/lib

# Architecture-independent files.
datadir = $(prefix)/lib

# Header files.
includedir = $(prefix)/include

# GNU .info* files.
infodir = $(prefix)/info

# Unix man pages.
manext = 1
mandir = $(prefix)/man/man$(manext)

# TeX & MF-specific directories. Not all of the following are relevant
# for all programs, but it seems cleaner to collect everything in one place.

# The default paths are now in kpathsea/paths.h.in. Passing all the
# paths to sub-makes can make the arg list too long on system V.

# The root of the tree.
texmf = $(datadir)/texmf

# TeX and MF source files.
texinputdir = $(texmf)/tex
mfinputdir = $(texmf)/mf

# MakeTeXPK.site, texmf.cnf, etc.
web2cdir = $(texmf)/web2c

# The top-level font directory.
fontdir = $(texmf)/fonts

# Memory dumps (.fmt and .base).
fmtdir = $(texmf)/ini
basedir = $(fmtdir)

# Pool files.
texpooldir = $(texmf)/ini
mfpooldir = $(texpooldir)

# If install_fonts=true, the PostScript/LaserJet TFM and VF files for
# the builtin fonts get installed in subdirectories of this directory,
# named for the typeface families of these directories. If you don't
# have the default directory setup, you will want to set
# install_fonts=false.  Ditto for install_macros.
install_fonts = true
install_macros = true

# Where the .map files from fontname are installed.
fontnamedir = $(texmf)/fontname

# Where the dvips configuration files get installed, and where
# psfonts.map is.
dvipsdir = $(texmf)/dvips
psheaderdir = $(dvipsdir)

# MakeTeXPK will go here to create dc*.
dcfontdir = $(fontdir)/public/dc

# MakeTeXPK will go here if it exists to create nonstandard CM fonts,
# e.g., cmr11. See ftp.cs.umb.edu:pub/tex/sauter.tar.gz. The Sauter
# files must be in your regular MFINPUTS.
sauterdir = $(fontdir)/public/sauter

# If a font can't be found close enough to its stated size, we look for
# each of these sizes in the order given.  This colon-separated list is
# overridden by the envvar TEXSIZES, and by a program-specific variable
# (e.g., XDVISIZES), and perhaps by a config file (e.g., in dvips).
default_texsizes = 300:600

# End of paths.make.
