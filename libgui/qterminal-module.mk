EXTRA_DIST += \
  libgui/qterminal-module.mk

noinst_HEADERS += \
  libgui/qterminal/libqterminal/QTerminal.h \
  libgui/qterminal/libqterminal/win32/QTerminalColors.h \
  libgui/qterminal/libqterminal/win32/QWinTerminalImpl.h \
  libgui/qterminal/libqterminal/unix/BlockArray.h \
  libgui/qterminal/libqterminal/unix/Character.h \
  libgui/qterminal/libqterminal/unix/CharacterColor.h \
  libgui/qterminal/libqterminal/unix/Emulation.h \
  libgui/qterminal/libqterminal/unix/ExtendedDefaultTranslator.h \
  libgui/qterminal/libqterminal/unix/ExtendedDefaultTranslatorMac.h \
  libgui/qterminal/libqterminal/unix/Filter.h \
  libgui/qterminal/libqterminal/unix/History.h \
  libgui/qterminal/libqterminal/unix/KeyboardTranslator.h \
  libgui/qterminal/libqterminal/unix/konsole_wcwidth.h \
  libgui/qterminal/libqterminal/unix/kpty.h \
  libgui/qterminal/libqterminal/unix/kpty_p.h \
  libgui/qterminal/libqterminal/unix/LineFont.h \
  libgui/qterminal/libqterminal/unix/QUnixTerminalImpl.h \
  libgui/qterminal/libqterminal/unix/Screen.h \
  libgui/qterminal/libqterminal/unix/ScreenWindow.h \
  libgui/qterminal/libqterminal/unix/TerminalCharacterDecoder.h \
  libgui/qterminal/libqterminal/unix/Vt102Emulation.h \
  libgui/qterminal/libqterminal/unix/SelfListener.h \
  libgui/qterminal/libqterminal/unix/TerminalModel.h \
  libgui/qterminal/libqterminal/unix/TerminalView.h

libgui_qterminal_libqterminal_la_MOC =

OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_MOC = \
  libgui/qterminal/libqterminal/moc-QTerminal.cc

$(OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_MOC): libgui/qterminal/libqterminal/$(octave_dirstamp)

DIRSTAMP_FILES += \
  libgui/qterminal/libqterminal/$(octave_dirstamp)

libgui_qterminal_libqterminal_la_MOC += \
  $(OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_MOC)

nodist_libgui_qterminal_libqterminal_la_SOURCES = $(libgui_qterminal_libqterminal_la_MOC)

libgui_qterminal_libqterminal_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  @QT_CPPFLAGS@ \
  -I$(srcdir)/libgui/qterminal/libqterminal \
  -I$(srcdir)/libgui/src

libgui_qterminal_libqterminal_la_CFLAGS = $(AM_CFLAGS)

libgui_qterminal_libqterminal_la_CXXFLAGS = $(AM_CXXFLAGS)

if WIN32_TERMINAL

libgui_qterminal_libqterminal_la_SOURCES = \
  libgui/qterminal/libqterminal/win32/QTerminalColors.cpp \
  libgui/qterminal/libqterminal/win32/QWinTerminalImpl.cpp \
  libgui/qterminal/libqterminal/QTerminal.cc

OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_WIN32_MOC = \
  libgui/qterminal/libqterminal/win32/moc-QWinTerminalImpl.cc

libgui_qterminal_libqterminal_la_MOC += \
  $(OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_WIN32_MOC)

$(OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_WIN32_MOC): libgui/qterminal/libqterminal/win32/$(octave_dirstamp)

DIRSTAMP_FILES += \
  libgui/qterminal/libqterminal/win32/$(octave_dirstamp)

libgui_qterminal_libqterminal_la_CPPFLAGS += -DUNICODE

# This flag is required to let MOC know about Q_OS_WIN32.
MOC_CPPFLAGS += -DQ_OS_WIN32

else

libgui_qterminal_libqterminal_la_SOURCES = \
  libgui/qterminal/libqterminal/unix/BlockArray.cpp \
  libgui/qterminal/libqterminal/unix/Emulation.cpp \
  libgui/qterminal/libqterminal/unix/Filter.cpp \
  libgui/qterminal/libqterminal/unix/History.cpp \
  libgui/qterminal/libqterminal/unix/KeyboardTranslator.cpp \
  libgui/qterminal/libqterminal/unix/konsole_wcwidth.cpp \
  libgui/qterminal/libqterminal/unix/kpty.cpp \
  libgui/qterminal/libqterminal/unix/QUnixTerminalImpl.cpp \
  libgui/qterminal/libqterminal/unix/Screen.cpp \
  libgui/qterminal/libqterminal/unix/ScreenWindow.cpp \
  libgui/qterminal/libqterminal/unix/TerminalCharacterDecoder.cpp \
  libgui/qterminal/libqterminal/unix/Vt102Emulation.cpp \
  libgui/qterminal/libqterminal/unix/SelfListener.cpp \
  libgui/qterminal/libqterminal/unix/TerminalModel.cpp \
  libgui/qterminal/libqterminal/unix/TerminalView.cpp \
  libgui/qterminal/libqterminal/QTerminal.cc

OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_UNIX_MOC = \
  libgui/qterminal/libqterminal/unix/moc-Emulation.cc \
  libgui/qterminal/libqterminal/unix/moc-Filter.cc \
  libgui/qterminal/libqterminal/unix/moc-QUnixTerminalImpl.cc \
  libgui/qterminal/libqterminal/unix/moc-ScreenWindow.cc \
  libgui/qterminal/libqterminal/unix/moc-SelfListener.cc \
  libgui/qterminal/libqterminal/unix/moc-TerminalModel.cc \
  libgui/qterminal/libqterminal/unix/moc-TerminalView.cc \
  libgui/qterminal/libqterminal/unix/moc-Vt102Emulation.cc

libgui_qterminal_libqterminal_la_MOC += \
  $(OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_UNIX_MOC)

$(OCTAVE_GUI_QTERMINAL_LIBQTERMINAL_UNIX_MOC): libgui/qterminal/libqterminal/unix/$(octave_dirstamp)

DIRSTAMP_FILES += \
  libgui/qterminal/libqterminal/unix/$(octave_dirstamp)

endif

noinst_LTLIBRARIES += libgui/qterminal/libqterminal.la

CLEANFILES += $(libgui_qterminal_libqterminal_la_MOC)
