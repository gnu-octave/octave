## Variable will hold either WIN32 or UNIX source files
%canon_reldir%_libqterminal_la_MOC =

OCTAVE_GUI_QTERMINAL_MOC = \
  %reldir%/libqterminal/moc-QTerminal.cc

$(OCTAVE_GUI_QTERMINAL_MOC): | %reldir%/libqterminal/$(octave_dirstamp)

DIRSTAMP_FILES += %reldir%/libqterminal/$(octave_dirstamp)

%canon_reldir%_libqterminal_la_MOC += $(OCTAVE_GUI_QTERMINAL_MOC)

nodist_%canon_reldir%_libqterminal_la_SOURCES = $(%canon_reldir%_libqterminal_la_MOC)

%canon_reldir%_libqterminal_la_CPPFLAGS =

if AMCOND_WIN32_TERMINAL

%canon_reldir%_libqterminal_la_SOURCES = \
  %reldir%/libqterminal/win32/QTerminalColors.cpp \
  %reldir%/libqterminal/win32/QWinTerminalImpl.cpp \
  %reldir%/libqterminal/QTerminal.cc

LIBQTERMINAL_WIN32_MOC = \
  %reldir%/libqterminal/win32/moc-QWinTerminalImpl.cc

%canon_reldir%_libqterminal_la_MOC += $(LIBQTERMINAL_WIN32_MOC)

$(LIBQTERMINAL_WIN32_MOC): | %reldir%/libqterminal/win32/$(octave_dirstamp)

DIRSTAMP_FILES += %reldir%/libqterminal/win32/$(octave_dirstamp)

# This flag is required to let MOC know about Q_OS_WIN32.
MOC_CPPFLAGS += -DQ_OS_WIN32

%canon_reldir%_libqterminal_la_CPPFLAGS += -DUNICODE

else

%canon_reldir%_libqterminal_la_SOURCES = \
  %reldir%/libqterminal/unix/BlockArray.cpp \
  %reldir%/libqterminal/unix/Emulation.cpp \
  %reldir%/libqterminal/unix/Filter.cpp \
  %reldir%/libqterminal/unix/History.cpp \
  %reldir%/libqterminal/unix/KeyboardTranslator.cpp \
  %reldir%/libqterminal/unix/konsole_wcwidth.cpp \
  %reldir%/libqterminal/unix/kpty.cpp \
  %reldir%/libqterminal/unix/QUnixTerminalImpl.cpp \
  %reldir%/libqterminal/unix/Screen.cpp \
  %reldir%/libqterminal/unix/ScreenWindow.cpp \
  %reldir%/libqterminal/unix/TerminalCharacterDecoder.cpp \
  %reldir%/libqterminal/unix/Vt102Emulation.cpp \
  %reldir%/libqterminal/unix/SelfListener.cpp \
  %reldir%/libqterminal/unix/TerminalModel.cpp \
  %reldir%/libqterminal/unix/TerminalView.cpp \
  %reldir%/libqterminal/QTerminal.cc

LIBQTERMINAL_UNIX_MOC = \
  %reldir%/libqterminal/unix/moc-Emulation.cc \
  %reldir%/libqterminal/unix/moc-Filter.cc \
  %reldir%/libqterminal/unix/moc-QUnixTerminalImpl.cc \
  %reldir%/libqterminal/unix/moc-ScreenWindow.cc \
  %reldir%/libqterminal/unix/moc-SelfListener.cc \
  %reldir%/libqterminal/unix/moc-TerminalModel.cc \
  %reldir%/libqterminal/unix/moc-TerminalView.cc \
  %reldir%/libqterminal/unix/moc-Vt102Emulation.cc

%canon_reldir%_libqterminal_la_MOC += $(LIBQTERMINAL_UNIX_MOC)

%canon_reldir%_libqterminal_la_MOC_H := \
  $(%canon_reldir%_libqterminal_la_MOC:.cc=.h)

$(LIBQTERMINAL_UNIX_MOC): | %reldir%/libqterminal/unix/$(octave_dirstamp)

DIRSTAMP_FILES += %reldir%/libqterminal/unix/$(octave_dirstamp)

endif

noinst_LTLIBRARIES += %reldir%/libqterminal.la

%canon_reldir%_libqterminal_la_CPPFLAGS += \
  $(AM_CPPFLAGS) \
  -I$(srcdir)/%reldir%/libqterminal \
  -I$(srcdir)/liboctgui/src \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -Iliboctinterp -I$(srcdir)/liboctinterp \
  -Iliboctinterp/corefcn/util -I$(srcdir)/liboctinterp/corefcn/util \
  -Iliboctinterp/graphics -I$(srcdir)/liboctinterp/graphics \
  -Iliboctinterp/interp -I$(srcdir)/liboctinterp/interp \
  -I$(srcdir)/liboctinterp/load-save \
  -I$(srcdir)/liboctinterp/octave-value \
  -Iliboctinterp/parse-tree -I$(srcdir)/liboctinterp/parse-tree \
  -I$(srcdir)/liboctinterp/stream \
  -I$(srcdir)/liboctinterp/template-inst \
  @QT_CPPFLAGS@

%canon_reldir%_libqterminal_la_CFLAGS := ${CPICFLAG} ${XTRA_CFLAGS}

%canon_reldir%_libqterminal_la_CXXFLAGS := ${CXXPICFLAG} ${XTRA_CXXFLAGS}

liboctgui_liboctgui_la_LIBADD += %reldir%/libqterminal.la

noinst_HEADERS += \
  %reldir%/libqterminal/QTerminal.h \
  %reldir%/libqterminal/win32/QTerminalColors.h \
  %reldir%/libqterminal/win32/QWinTerminalImpl.h \
  %reldir%/libqterminal/unix/BlockArray.h \
  %reldir%/libqterminal/unix/Character.h \
  %reldir%/libqterminal/unix/CharacterColor.h \
  %reldir%/libqterminal/unix/Emulation.h \
  %reldir%/libqterminal/unix/ExtendedDefaultTranslator.h \
  %reldir%/libqterminal/unix/ExtendedDefaultTranslatorMac.h \
  %reldir%/libqterminal/unix/Filter.h \
  %reldir%/libqterminal/unix/History.h \
  %reldir%/libqterminal/unix/KeyboardTranslator.h \
  %reldir%/libqterminal/unix/konsole_wcwidth.h \
  %reldir%/libqterminal/unix/kpty.h \
  %reldir%/libqterminal/unix/kpty_p.h \
  %reldir%/libqterminal/unix/QUnixTerminalImpl.h \
  %reldir%/libqterminal/unix/Screen.h \
  %reldir%/libqterminal/unix/ScreenWindow.h \
  %reldir%/libqterminal/unix/TerminalCharacterDecoder.h \
  %reldir%/libqterminal/unix/Vt102Emulation.h \
  %reldir%/libqterminal/unix/SelfListener.h \
  %reldir%/libqterminal/unix/TerminalModel.h \
  %reldir%/libqterminal/unix/TerminalView.h

liboctgui_DISTCLEANFILES += \
  $(%canon_reldir%_libqterminal_la_MOC) \
  $(%canon_reldir%_libqterminal_la_MOC_H)
