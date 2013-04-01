/*

Copyright (C) 2011 Michael Goffioul.

This file is part of QConsole.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

QConsole is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

#include <QApplication>
#include <QColor>
#include <QFont>
#include <QHBoxLayout>
#include <QPaintEvent>
#include <QPainter>
#include <QResizeEvent>
#include <QScrollBar>
#include <QtDebug>
#include <QThread>
#include <QTimer>

#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <stdarg.h>
#define WIN32_LEAN_AND_MEAN
#define _WIN32_WINNT 0x0500 
#include <windows.h>
#include <cstring>

#include "QWinTerminalImpl.h"
#include "QTerminalColors.h"

// Uncomment to log activity to LOGFILENAME
// #define DEBUG_QCONSOLE
#define LOGFILENAME "QConsole.log"
// Uncomment to create hidden console window
#define HIDDEN_CONSOLE

#ifdef _MSC_VER
# pragma warning(disable : 4996)
#endif

//////////////////////////////////////////////////////////////////////////////

class QConsoleView : public QWidget
{
public:
  QConsoleView (QWinTerminalImpl* parent = 0) : QWidget (parent), q (parent) { }
  ~QConsoleView (void) { }

protected:
  void paintEvent (QPaintEvent* event) { q->viewPaintEvent (this, event); }
  void resizeEvent (QResizeEvent* event) { q->viewResizeEvent (this, event); }

private:
  QWinTerminalImpl* q;
};

//////////////////////////////////////////////////////////////////////////////

class QConsoleThread : public QThread
{
public:
  QConsoleThread (QWinTerminalImpl* console) : QThread (console), q (console) { }

protected:
  void run (void)
    { q->start (); }

private:
  QWinTerminalImpl* q;
};

//////////////////////////////////////////////////////////////////////////////

class QConsolePrivate
{
  friend class QWinTerminalImpl;

public:
  QConsolePrivate (QWinTerminalImpl* parent, const QString& cmd = QString ());
  ~QConsolePrivate (void);

  void updateConsoleSize (bool sync = false);
  void syncConsoleParameters (void);
  void grabConsoleBuffer (CHAR_INFO* buf = 0);
  void updateScrollBar (void);
  void setScrollValue (int value);
  void updateConsoleView (bool grab = true);
  void monitorConsole (void);
  void startCommand (void);
  void sendConsoleText (const QString& s);

  void log (const char* fmt, ...);

  void closeStandardIO (int fd, DWORD stdHandleId, const char* name);
  void setupStandardIO (DWORD stdHandleId, int fd, const char* name,
                        const char* devName);

private:
  QWinTerminalImpl* q;

private:
  QFont m_font;
  QColor m_backgroundColor;
  QString m_command;
  QConsoleColors m_colors;
  bool m_inWheelEvent;
  QString m_title;

  QSize m_charSize;
  QSize m_bufferSize;
  QRect m_consoleRect;
  QPoint m_cursorPos;

  HANDLE m_stdOut;
  HWND m_consoleWindow;
  CHAR_INFO* m_buffer;
  CHAR_INFO* m_tmpBuffer;
  HANDLE m_process;

  QConsoleView* m_consoleView;
  QScrollBar* m_scrollBar;
  QTimer* m_consoleWatcher;
  QConsoleThread *m_consoleThread;
};

//////////////////////////////////////////////////////////////////////////////

QConsolePrivate::QConsolePrivate (QWinTerminalImpl* parent, const QString& cmd)
    : q (parent), m_command (cmd), m_process (NULL), m_inWheelEvent (false)
{
  log (NULL);

  // Possibly detach from any existing console
  log ("Detaching from existing console (if any)...\n");
  FreeConsole ();
  log ("Closing standard IO...\n");
  closeStandardIO (0, STD_INPUT_HANDLE, "STDIN");
  closeStandardIO (1, STD_OUTPUT_HANDLE, "STDOUT");
  closeStandardIO (2, STD_ERROR_HANDLE, "STDERR");

#ifdef HIDDEN_CONSOLE
  HWINSTA hOrigSta, hNewSta;

  // Create new (hidden) console
  hOrigSta = GetProcessWindowStation ();
  hNewSta = CreateWindowStation (NULL, 0, GENERIC_ALL, NULL);
  log ("Current Windows station: %p.\nNew Windows station: %p.\n", hOrigSta,
       hNewSta);
  if (! SetProcessWindowStation (hNewSta))
    log ("Failed to switch to new Windows station.\n");
#endif
  if (! AllocConsole ())
    log ("Failed to create new console.\n");
#ifdef HIDDEN_CONSOLE
  if (! SetProcessWindowStation (hOrigSta))
    log ("Failed to restore original Windows station.\n");
  if (! CloseWindowStation (hNewSta))
    log ("Failed to close new Windows station.\n");
#endif

  log ("New (hidden) console created.\n");

  setupStandardIO (STD_INPUT_HANDLE,  0, "STDIN",  "CONIN$");
  setupStandardIO (STD_OUTPUT_HANDLE, 1, "STDOUT", "CONOUT$");
  setupStandardIO (STD_ERROR_HANDLE,  2, "STDERR", "CONOUT$");

  log ("Standard input/output/error set up.\n");

  *stdin = *(fdopen (0, "rb"));
  *stdout = *(fdopen (1, "wb"));
  *stderr = *(fdopen (2, "wb"));

  log ("POSIX standard streams created.\n");

  setvbuf (stdin, NULL, _IONBF, 0);
  setvbuf (stdout, NULL, _IONBF, 0);
  setvbuf (stderr, NULL, _IONBF, 0);

  log ("POSIX standard stream buffers adjusted.\n");

  HANDLE hStdOut = GetStdHandle (STD_OUTPUT_HANDLE);

  log ("Console allocated: hStdOut: %p\n", hStdOut);

  m_stdOut = hStdOut;
  m_consoleWindow = GetConsoleWindow ();

  // In case the console window hasn't been created hidden...
  ShowWindow (m_consoleWindow, SW_HIDE);

  CONSOLE_SCREEN_BUFFER_INFO sbi;

  GetConsoleScreenBufferInfo (hStdOut, &sbi);
  m_bufferSize = QSize (sbi.dwSize.X, qMax (sbi.dwSize.Y, (SHORT)500));
  m_consoleRect = QRect (sbi.srWindow.Left, sbi.srWindow.Top,
                         sbi.srWindow.Right - sbi.srWindow.Left + 1,
                         sbi.srWindow.Bottom - sbi.srWindow.Top + 1);
  m_cursorPos = QPoint (sbi.dwCursorPosition.X, sbi.dwCursorPosition.Y);

  log ("Initial console parameters:\n");
  log ("  buffer size: %d x %d\n", m_bufferSize.width (),
       m_bufferSize.height ());
  log ("  window: (%d, %d) -> (%d, %d) [%d x %d]\n",
       m_consoleRect.left (), m_consoleRect.top (),
       m_consoleRect.right (), m_consoleRect.bottom (),
       m_consoleRect.width (), m_consoleRect.height ());

  wchar_t titleBuf[260];
  GetConsoleTitleW (titleBuf, sizeof (titleBuf));
  q->setWindowTitle (QString::fromWCharArray (titleBuf));

  m_font.setFamily ("Lucida Console");
  m_font.setPointSize (9);
  m_font.setStyleHint (QFont::TypeWriter);
  m_backgroundColor = Qt::white;

  m_buffer = m_tmpBuffer = 0;

  m_consoleView = new QConsoleView (parent);
  m_scrollBar = new QScrollBar (Qt::Vertical, parent);

  QHBoxLayout* l = new QHBoxLayout (parent);
  l->setContentsMargins (0, 0, 0, 0);
  l->setSpacing (0);
  l->addWidget (m_consoleView, 1);
  l->addWidget (m_scrollBar, 0);

  m_consoleView->setPalette (QPalette (m_backgroundColor));
  m_consoleView->setAutoFillBackground (true);
  m_consoleView->setFont (m_font);
  parent->setFocusPolicy (Qt::StrongFocus);
  parent->winId ();

  updateScrollBar ();

  m_consoleWatcher = new QTimer (parent);
  m_consoleWatcher->setInterval (10);
  m_consoleWatcher->setSingleShot (false);
  
  QObject::connect (m_scrollBar, SIGNAL (valueChanged (int)),
                    q, SLOT (scrollValueChanged (int)));
  QObject::connect (m_consoleWatcher, SIGNAL (timeout (void)),
                    q, SLOT (monitorConsole (void)));

  m_consoleWatcher->start ();

  if (m_command.isEmpty ())
    m_consoleThread = 0;
  else
    {
      m_consoleThread = new QConsoleThread (q);
      QObject::connect (m_consoleThread, SIGNAL (finished (void)),
                        q, SIGNAL (terminated (void)));
      m_consoleThread->start ();
    }
}

//////////////////////////////////////////////////////////////////////////////

QConsolePrivate::~QConsolePrivate (void)
{
  if (m_consoleThread && m_consoleThread->isRunning () && m_process)
    {
      TerminateProcess (m_process, (UINT)-1);
      m_consoleThread->wait ();
    }
  if (m_buffer)
    delete [] m_buffer;
  if (m_tmpBuffer)
    delete [] m_tmpBuffer;
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::setupStandardIO (DWORD stdHandleId, int targetFd,
                                       const char* name, const char* devName)
{
  log ("Opening %s...\n", devName);

  int fd = open (devName, _O_RDWR | _O_BINARY);

  if (fd != -1)
    {
      if (fd != targetFd)
        {
          log ("Opened %s is not at target file descriptor %d, "
               "duplicating...\n", name, targetFd);
          if (dup2 (fd, targetFd) == -1)
            log ("Failed to duplicate file descriptor: errno=%d.\n", errno);
          if (close (fd) == -1)
            log ("Failed to close original file descriptor: errno=%d.\n",
                 errno);
        }
      else
        log ("%s opened and assigned to file descriptor %d.\n", devName, fd);
      if (! SetStdHandle (stdHandleId, (HANDLE) _get_osfhandle (targetFd)))
        log ("Failed to re-assign %s: error=%08x.\n", name, GetLastError ());
    }
  else
    log ("Failed to open %s: errno=%d.\n", devName, errno);
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::closeStandardIO (int fd, DWORD stdHandleId,
                                       const char* name)
{
  if (close (fd) == -1)
    log ("Failed to close file descriptor %d: errno=%d.\n", fd, errno);
  if (! CloseHandle (GetStdHandle (stdHandleId)))
    log ("Failed to close Win32 %s: error=%08x.\n", name, GetLastError ());
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::log (const char* fmt, ...)
{
#ifdef DEBUG_QCONSOLE
  if (fmt)
    {
      va_list l;
      FILE* flog = fopen (LOGFILENAME, "ab");

      va_start (l, fmt);
      vfprintf (flog, fmt, l);
      va_end (l);
      fclose (flog);
    }
  else
    {
      // Special case to re-initialize the log file
      FILE* flog = fopen (LOGFILENAME, "w");
      fclose (flog);
    }
#else
  Q_UNUSED (fmt);
#endif
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::updateConsoleSize (bool sync)
{
  QFontMetrics fm (m_font);
  QSize winSize = m_consoleView->size ();

  m_charSize.rwidth () = fm.maxWidth ();
  m_charSize.rheight () = fm.lineSpacing ();

  m_consoleRect.setWidth (winSize.width () / fm.maxWidth ());
  m_consoleRect.setHeight (winSize.height () / fm.lineSpacing ());

  m_bufferSize.rwidth () = m_consoleRect.width ();
  m_bufferSize.rheight () = qMax (m_bufferSize.height (),
                                  m_consoleRect.height ());

  m_consoleRect.moveLeft (0);
  if (m_consoleRect.bottom () >= m_bufferSize.height ())
    m_consoleRect.moveTop (m_bufferSize.height () - m_consoleRect.height ());

  log ("Console resized:\n");
  log ("  widget size: %d x %d\n", winSize.width (), winSize.height ());
  log ("  buffer size: %d x %d\n", m_bufferSize.width (),
       m_bufferSize.height ());
  log ("  window: (%d, %d) -> (%d, %d) [%d x %d]\n",
       m_consoleRect.left (), m_consoleRect.top (),
       m_consoleRect.right (), m_consoleRect.bottom (),
       m_consoleRect.width (), m_consoleRect.height ());

  if (sync)
    syncConsoleParameters ();

  updateScrollBar ();
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::syncConsoleParameters (void)
{
  CONSOLE_SCREEN_BUFFER_INFO sbi;
  HANDLE hStdOut = m_stdOut;

  GetConsoleScreenBufferInfo (hStdOut, &sbi);

  COORD bs;
  SMALL_RECT sr;

  bs.X = sbi.dwSize.X;
  bs.Y = m_bufferSize.height ();
  sr.Left   = sbi.srWindow.Left;
  sr.Right  = sbi.srWindow.Right;
  sr.Top    = m_consoleRect.top ();
  sr.Bottom = m_consoleRect.bottom ();

  if (bs.Y > sbi.dwSize.Y)
    {
      SetConsoleScreenBufferSize (hStdOut, bs);
      SetConsoleWindowInfo (hStdOut, TRUE, &sr);
    }
  else
    {
      SetConsoleWindowInfo (hStdOut, TRUE, &sr);
      SetConsoleScreenBufferSize (hStdOut, bs);
    }

  bs.X = m_bufferSize.width ();
  sr.Left  = m_consoleRect.left ();
  sr.Right = m_consoleRect.right ();

  if (bs.X > sbi.dwSize.X)
    {
      SetConsoleScreenBufferSize (hStdOut, bs);
      SetConsoleWindowInfo (hStdOut, TRUE, &sr);
    }
  else
    {
      SetConsoleWindowInfo (hStdOut, TRUE, &sr);
      SetConsoleScreenBufferSize (hStdOut, bs);
    }

  log ("Sync'ing console parameters:\n");
  log ("  buffer size: %d x %d\n", bs.X, bs.Y);
  log ("  window: (%d, %d) -> (%d, %d)\n",
       sr.Left, sr.Top, sr.Right, sr.Bottom);

  if (m_buffer)
    delete [] m_buffer;
  if (m_tmpBuffer)
    delete [] m_tmpBuffer;

  int bufSize = m_consoleRect.width () * m_consoleRect.height ();

  m_buffer = new CHAR_INFO[bufSize];
  m_tmpBuffer = new CHAR_INFO[bufSize];
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::grabConsoleBuffer (CHAR_INFO* buf)
{
  COORD bs, bc;
  SMALL_RECT r;

  bs.X = m_consoleRect.width ();
  bs.Y = m_consoleRect.height ();
  bc.X = 0;
  bc.Y = 0;

  r.Left   = m_consoleRect.left ();
  r.Top    = m_consoleRect.top ();
  r.Right  = m_consoleRect.right ();
  r.Bottom = m_consoleRect.bottom ();

  if (! ReadConsoleOutput (m_stdOut, (buf ? buf : m_buffer), bs, bc, &r))
    qCritical ("cannot read console output");
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::updateScrollBar (void)
{
  m_scrollBar->setMinimum (0);
  if (m_bufferSize.height () > m_consoleRect.height ())
    m_scrollBar->setMaximum (m_bufferSize.height () - m_consoleRect.height ());
  else
    m_scrollBar->setMaximum (0);
  m_scrollBar->setSingleStep (1);
  m_scrollBar->setPageStep (m_consoleRect.height ());
  m_scrollBar->setValue (m_consoleRect.top ());

  log ("Scrollbar parameters updated: %d/%d/%d/%d\n",
       m_scrollBar->minimum (), m_scrollBar->maximum (),
       m_scrollBar->singleStep (), m_scrollBar->pageStep ());
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::setScrollValue (int value)
{
  if (value == m_consoleRect.top ())
    return;

  SMALL_RECT r;
  HANDLE hStdOut = m_stdOut;

  if (value + m_consoleRect.height () > m_bufferSize.height ())
    value = m_bufferSize.height () - m_consoleRect.height ();

  r.Left = m_consoleRect.left ();
  r.Top = value;
  r.Right = m_consoleRect.right ();
  r.Bottom = value + m_consoleRect.height () - 1;

  log ("Scrolling window: (%d, %d) -> (%d, %d) [%d x %d]\n",
       r.Left, r.Top, r.Right, r.Bottom, 
       r.Right - r.Left + 1, r.Bottom - r.Top + 1);

  if (SetConsoleWindowInfo (hStdOut, TRUE, &r))
    {
      m_consoleRect.moveTop (value);
      updateConsoleView ();
    }
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::updateConsoleView (bool grab)
{
  if (grab)
    grabConsoleBuffer ();
  m_consoleView->update ();
  m_consoleWatcher->start ();
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::monitorConsole (void)
{
  CONSOLE_SCREEN_BUFFER_INFO sbi;
  HANDLE hStdOut = GetStdHandle (STD_OUTPUT_HANDLE);

  static wchar_t titleBuf[260];

  GetConsoleTitleW (titleBuf, sizeof (titleBuf));
  QString title = QString::fromWCharArray (titleBuf);

  if (title != m_title)
    {
      q->setWindowTitle (title);
      emit q->titleChanged (title);
    }

  if (GetConsoleScreenBufferInfo (hStdOut, &sbi))
    {
      if (m_bufferSize.width () != sbi.dwSize.X
          || m_bufferSize.height () != sbi.dwSize.Y)
        {
          // Buffer size changed
          m_bufferSize.rwidth () = sbi.dwSize.X;
          m_bufferSize.rheight () = sbi.dwSize.Y;
          updateScrollBar ();
        }

      if (m_cursorPos.x () != sbi.dwCursorPosition.X
          || m_cursorPos.y () != sbi.dwCursorPosition.Y)
        {
          // Cursor position changed
          m_consoleView->update
            ((m_cursorPos.x () - sbi.srWindow.Left) * m_charSize.width (),
             (m_cursorPos.y () - sbi.srWindow.Top) * m_charSize.height (),
             m_charSize.width (), m_charSize.height ());
          m_cursorPos.rx () = sbi.dwCursorPosition.X;
          m_cursorPos.ry () = sbi.dwCursorPosition.Y;
          m_consoleView->update
            ((m_cursorPos.x () - sbi.srWindow.Left) * m_charSize.width (),
             (m_cursorPos.y () - sbi.srWindow.Top) * m_charSize.height (),
             m_charSize.width (), m_charSize.height ());
        }

      if (m_consoleRect.left () != sbi.srWindow.Left
          || m_consoleRect.right () != sbi.srWindow.Right
          || m_consoleRect.top () != sbi.srWindow.Top
          || m_consoleRect.bottom () != sbi.srWindow.Bottom)
        {
          // Console window changed
          m_consoleRect = QRect (sbi.srWindow.Left, sbi.srWindow.Top,
                                 sbi.srWindow.Right - sbi.srWindow.Left + 1,
                                 sbi.srWindow.Bottom - sbi.srWindow.Top + 1);
          updateScrollBar ();
          updateConsoleView ();
          return;
        }

      if (m_tmpBuffer && m_buffer)
        {
          grabConsoleBuffer (m_tmpBuffer);
          if (memcmp (m_tmpBuffer, m_buffer,
                      sizeof (CHAR_INFO) * m_consoleRect.width () *
                      m_consoleRect.height ()))
            {
              // FIXME: compute the area to update based on the
              // difference between the 2 buffers.
              qSwap (m_buffer, m_tmpBuffer);
              updateConsoleView (false);
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::startCommand (void)
{
  QString cmd = m_command;

  if (cmd.isEmpty ())
    cmd = qgetenv ("COMSPEC").constData ();

  if (! cmd.isEmpty ())
    {
      STARTUPINFO si;
      PROCESS_INFORMATION pi;

      ZeroMemory (&si, sizeof (si));
      si.cb = sizeof (si);
      ZeroMemory (&pi, sizeof (pi));

      if (CreateProcessW (NULL,
                          (LPWSTR)cmd.unicode (),
                          NULL,
                          NULL,
                          TRUE,
                          0,
                          NULL,
                          NULL,
                          &si,
                          &pi))
        {
          CloseHandle (pi.hThread);
          m_process = pi.hProcess;
          WaitForSingleObject (m_process, INFINITE);
          CloseHandle (m_process);
          m_process = NULL;
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

void QConsolePrivate::sendConsoleText (const QString& s)
{
  // Send the string in chunks of 512 characters. Each character is
  // translated into an equivalent keypress event.

#define TEXT_CHUNK_SIZE 512

  int len = s.length ();
  INPUT_RECORD events[TEXT_CHUNK_SIZE];
  DWORD nEvents = 0, written;
  HANDLE hStdIn = GetStdHandle (STD_INPUT_HANDLE);

  ZeroMemory (events, sizeof (events));

  for (int i = 0; i < len; i++)
    {
      QChar c = s.at (i);

      if (c == L'\r' || c == L'\n')
        {
          if (c == L'\r' && i < (len - 1) && s.at (i+1) == L'\n')
            i++;
          if (nEvents)
            {
              WriteConsoleInput (hStdIn, events, nEvents, &written);
              nEvents = 0;
              ZeroMemory (events, sizeof (events));
            }
          PostMessage (m_consoleWindow, WM_KEYDOWN, VK_RETURN, 0x001C0001);
          PostMessage (m_consoleWindow, WM_KEYDOWN, VK_RETURN, 0xC01C0001);
        }
      else
        {
          events[nEvents].EventType                        = KEY_EVENT;
          events[nEvents].Event.KeyEvent.bKeyDown          = TRUE;
          events[nEvents].Event.KeyEvent.wRepeatCount      = 1;
          events[nEvents].Event.KeyEvent.wVirtualKeyCode   =
            LOBYTE (VkKeyScan (c.unicode ()));
          events[nEvents].Event.KeyEvent.wVirtualScanCode  = 0;
          events[nEvents].Event.KeyEvent.uChar.UnicodeChar = c.unicode ();
          events[nEvents].Event.KeyEvent.dwControlKeyState = 0;
          nEvents++;
        }

      if (nEvents == TEXT_CHUNK_SIZE
          || (nEvents > 0 && i == (len - 1)))
        {
          WriteConsoleInput (hStdIn, events, nEvents, &written);
          nEvents = 0;
          ZeroMemory (events, sizeof (events));
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

QWinTerminalImpl::QWinTerminalImpl (QWidget* parent)
    : QTerminalInterface (parent), d (new QConsolePrivate (this))
{
}

//////////////////////////////////////////////////////////////////////////////

QWinTerminalImpl::QWinTerminalImpl (const QString& cmd, QWidget* parent)
    : QTerminalInterface (parent), d (new QConsolePrivate (this, cmd))
{
}

//////////////////////////////////////////////////////////////////////////////

QWinTerminalImpl::~QWinTerminalImpl (void)
{
  delete d;
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::viewResizeEvent (QConsoleView*, QResizeEvent*)
{
  d->updateConsoleSize (true);
  d->grabConsoleBuffer ();
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::viewPaintEvent (QConsoleView* w, QPaintEvent* event)
{
  QPainter p (w);
  int cw = d->m_charSize.width (), ch = d->m_charSize.height ();
  int ascent, stride, cx1, cy1, cx2, cy2, x, y;
  WORD attr = 0;
  QString s;
  bool hasChar = false;

  QRect updateRect = event->rect ();

  cx1 = updateRect.left () / cw;
  cy1 = updateRect.top () / ch;
  cx2 = qMin (d->m_consoleRect.width () - 1, updateRect.right () / cw);
  cy2 = qMin (d->m_consoleRect.height () - 1, updateRect.bottom () / ch);

  if (cx1 > d->m_consoleRect.width () - 1
      || cy1 > d->m_consoleRect.height () - 1)
    return;

  p.setFont (d->m_font);
  p.setPen (Qt::black);

  ascent = p.fontMetrics ().ascent ();
  stride = d->m_consoleRect.width ();

  s.reserve (cx2 - cx1 + 1);
  y = ascent + cy1 * ch;;

  for (int j = cy1; j <= cy2; j++, y += ch)
    {
      // Reset string buffer and starting X coordinate
      s.clear ();
      hasChar = false;
      x = cx1 * cw;

      for (int i = cx1; i <= cx2; i++)
        {
          CHAR_INFO* ci = &(d->m_buffer[stride*j+i]);

          if ((ci->Attributes & 0x00ff) != attr)
            {
              // Character attributes changed
              if (! s.isEmpty ())
                {
                  // String buffer not empty -> draw it
                  if (hasChar || (attr & 0x00f0))
                    {
                      if (attr & 0x00f0)
                        p.fillRect (x, y-ascent, s.length () * cw, ch,
                                    p.brush ());
                      p.drawText (x, y, s);
                    }
                  x += (s.length () * cw);
                  s.clear ();
                  hasChar = false;
                }
              // Update current pen and store current attributes
              // FIXME: what about background?
              attr = (ci->Attributes & 0x00ff);
              p.setPen (Qt::black);
              p.setBrush (Qt::black);
            }

          // Append current character to the string buffer
          s.append (ci->Char.UnicodeChar);
          if (ci->Char.UnicodeChar != L' ')
            hasChar = true;
        }

      if (! s.isEmpty () && (hasChar || (attr & 0x00f0)))
        {
          // Line end reached, but string buffer not empty -> draw it
          // No need to update s or x, they will be reset on the next
          // for-loop iteration
          if (attr & 0x00f0)
            p.fillRect (x, y-ascent, s.length () * cw, ch, p.brush ());
          p.drawText (x, y, s);
        }
    }

  // Draw cursor
  p.setCompositionMode (QPainter::RasterOp_SourceXorDestination);
  p.fillRect ((d->m_cursorPos.x () - d->m_consoleRect.x ()) * cw,
              (d->m_cursorPos.y () - d->m_consoleRect.y ()) * ch,
              cw, ch, d->m_colors[7]);
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::wheelEvent (QWheelEvent* event)
{
  if (! d->m_inWheelEvent)
    {
      // Forward to the scrollbar (avoid recursion)
      d->m_inWheelEvent = true;
      QApplication::sendEvent (d->m_scrollBar, event);
      d->m_inWheelEvent = false;
    }
}

//////////////////////////////////////////////////////////////////////////////

bool QWinTerminalImpl::winEvent (MSG* msg, long* result)
{
  switch (msg->message)
    {
    case WM_KEYDOWN:
    case WM_KEYUP:
    //case WM_CHAR:
      // Forward Win32 message to the console window
      PostMessage (d->m_consoleWindow,
                   msg->message,
                   msg->wParam,
                   msg->lParam);
      result = 0;
      return true;
    default:
      return false;
    }
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::scrollValueChanged (int value)
{
  d->setScrollValue (value);
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::monitorConsole (void)
{
  d->monitorConsole ();
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::focusInEvent (QFocusEvent* event)
{
  QWidget::focusInEvent (event);
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::start (void)
{
  d->startCommand ();
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::sendText (const QString& s)
{
  d->sendConsoleText (s);
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::setTerminalFont (const QFont& f)
{
  d->m_font = f;
  d->m_consoleView->setFont (f);
  d->updateConsoleSize (true);
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::setSize (int columns, int lines)
{
  Q_UNUSED (columns);
  Q_UNUSED (lines);
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::copyClipboard (void)
{
}

//////////////////////////////////////////////////////////////////////////////

void QWinTerminalImpl::pasteClipboard (void)
{
}
