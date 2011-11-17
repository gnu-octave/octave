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
#include <windows.h>
#include <cstring>

#include "QConsole.h"
#include "QConsoleColors.h"

// Uncomment to log activity to LOGFILENAME
// #define DEBUG_QCONSOLE
#define LOGFILENAME "QConsole.log"
// Uncomment to create hidden console window
#define HIDDEN_CONSOLE

//////////////////////////////////////////////////////////////////////////////

class QConsoleView : public QWidget
{
public:
  QConsoleView (QConsole* parent = 0) : QWidget (parent), q (parent) { }
  ~QConsoleView (void) { }

protected:
  void paintEvent (QPaintEvent* event) { q->viewPaintEvent (this, event); }
  void resizeEvent (QResizeEvent* event) { q->viewResizeEvent (this, event); }

private:
  QConsole* q;
};

//////////////////////////////////////////////////////////////////////////////

class QConsoleThread : public QThread
{
public:
  QConsoleThread (QConsole* console) : QThread (console), q (console) { }

protected:
  void run (void)
    { q->start (); }

private:
  QConsole* q;
};

//////////////////////////////////////////////////////////////////////////////

class QConsolePrivate
{
  friend class QConsole;

public:
  QConsolePrivate (QConsole* parent, const QString& cmd = QString ());
  ~QConsolePrivate (void);

  void updateConsoleSize (bool sync = false);
  void syncConsoleParameters (void);
  void grabConsoleBuffer (CHAR_INFO* buf = 0);
  void updateScrollBar (void);
  void setScrollValue (int value);
  void updateConsoleView (bool grab = true);
  void monitorConsole (void);
  void startCommand (void);

  void log (const char* fmt, ...);

private:
  QConsole* q;

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

QConsolePrivate::QConsolePrivate (QConsole* parent, const QString& cmd)
    : q (parent), m_command (cmd), m_process (NULL), m_inWheelEvent (false)
{
  log (NULL);

  HANDLE hStdOut = GetStdHandle (STD_OUTPUT_HANDLE);
  if (hStdOut)
    {
      // FIXME: should we detach from the existing console and create
      //        a new one?
      log ("STDIN: %p, STDOUT: %p, STDERR: %p.\n",
           GetStdHandle (STD_INPUT_HANDLE), GetStdHandle (STD_OUTPUT_HANDLE),
           GetStdHandle (STD_ERROR_HANDLE));
      log ("Console existing, detaching...\n", hStdOut);
      FreeConsole ();
      log ("STDIN: %p, STDOUT: %p, STDERR: %p.\n",
           GetStdHandle (STD_INPUT_HANDLE), GetStdHandle (STD_OUTPUT_HANDLE),
           GetStdHandle (STD_ERROR_HANDLE));
      close (0);
      close (1);
      close (2);
    }

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

  log ("Hidden console created.\n");
  log ("STDIN: %p, STDOUT: %p, STDERR: %p.\n",
       GetStdHandle (STD_INPUT_HANDLE), GetStdHandle (STD_OUTPUT_HANDLE),
       GetStdHandle (STD_ERROR_HANDLE));

  // Setup stdin/stdout/stderr
  int fd_in = _open_osfhandle ((intptr_t) GetStdHandle (STD_INPUT_HANDLE),
                               _O_RDONLY | _O_BINARY);
  int fd_out = _open_osfhandle ((intptr_t) GetStdHandle (STD_OUTPUT_HANDLE),
                                _O_WRONLY | _O_BINARY);
  int fd_err = _open_osfhandle ((intptr_t) GetStdHandle (STD_ERROR_HANDLE),
                                _O_WRONLY | _O_BINARY);

  log ("Win32 standard handles opened: fd_in=%d, fd_out=%d, fd_err=%d\n",
       fd_in, fd_out, fd_err);

  if (fd_in == -1)
    {
      log ("Invalid STDIN, trying to open CONIN$ instead...\n");
      fd_in = open ("CONIN$", _O_RDWR | _O_BINARY);
      if (fd_in != -1)
        {
          log ("CONIN$ opened, assigning as STDIN...\n");
          SetStdHandle (STD_INPUT_HANDLE, (HANDLE) _get_osfhandle (fd_in));
        }
    }
  if (fd_in != -1 && fd_in != 0)
    {
      log ("Duplicating standard input in 0 file descriptor\n");
      dup2 (fd_in, 0);
      close (fd_in);
      SetStdHandle (STD_INPUT_HANDLE, (HANDLE) _get_osfhandle (0));
    }
  if (fd_out == -1)
    {
      log ("Invalid STDOUT, trying to open CONOUT$ instead...\n");
      fd_out = open ("CONOUT$", _O_RDWR | _O_BINARY);
      if (fd_out != -1)
        {
          log ("CONOUT$ opened, assigning as STDOUT...\n");
          SetStdHandle (STD_OUTPUT_HANDLE, (HANDLE) _get_osfhandle (fd_out));
        }
    }
  if (fd_out != -1 && fd_out != 1)
    {
      log ("Duplicating standard output in 1 file descriptor\n");
      dup2 (fd_out, 1);
      close (fd_out);
      SetStdHandle (STD_OUTPUT_HANDLE, (HANDLE) _get_osfhandle (1));
    }
  if (fd_err == -1)
    {
      log ("Invalid STDERR, trying to open CONOUT$ instead...\n");
      fd_err = open ("CONOUT$", _O_RDWR | _O_BINARY);
      if (fd_err != -1)
        {
          log ("CONOUT$ opened, assigning as STDERR...\n");
          SetStdHandle (STD_ERROR_HANDLE, (HANDLE) _get_osfhandle (fd_err));
        }
    }
  if (fd_err != -1 && fd_err != 2)
    {
      log ("Duplicating standard error in 2 file descriptor\n");
      dup2 (fd_err, 2);
      close (fd_err);
      SetStdHandle (STD_ERROR_HANDLE, (HANDLE) _get_osfhandle (2));
    }

  log ("Win32 standard handles fixed/duplicated: "
       "fd_in=%d, fd_out=%d, fd_err=%d\n", fd_in, fd_out, fd_err);

  *stdin = *(fdopen (0, "rb"));
  *stdout = *(fdopen (1, "wb"));
  *stderr = *(fdopen (2, "wb"));

  log ("POSIX standard streams created.\n");

  setvbuf (stdin, NULL, _IONBF, 0);
  setvbuf (stdout, NULL, _IONBF, 0);
  setvbuf (stderr, NULL, _IONBF, 0);

  log ("POSIX standard stream buffers adjusted.\n");

  hStdOut = GetStdHandle (STD_OUTPUT_HANDLE);

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
  q->setWindowTitle (QString::fromUtf16 (titleBuf));

  m_font.setFamily ("Lucida Console");
  m_font.setPointSize (9);
  m_font.setStyleHint (QFont::TypeWriter);
  m_backgroundColor = Qt::black;

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
  QString title = QString::fromUtf16 (titleBuf);

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
  if (! m_command.isEmpty ())
    {
      STARTUPINFO si;
      PROCESS_INFORMATION pi;

      ZeroMemory (&si, sizeof (si));
      si.cb = sizeof (si);
      ZeroMemory (&pi, sizeof (pi));

      if (CreateProcessW (NULL,
                          (LPWSTR)m_command.unicode (),
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

QConsole::QConsole (QWidget* parent)
    : d (new QConsolePrivate (this))
{
}

//////////////////////////////////////////////////////////////////////////////

QConsole::QConsole (const QString& cmd, QWidget* parent)
    : d (new QConsolePrivate (this, cmd))
{
}

//////////////////////////////////////////////////////////////////////////////

QConsole::~QConsole (void)
{
  delete d;
}

//////////////////////////////////////////////////////////////////////////////

void QConsole::viewResizeEvent (QConsoleView*, QResizeEvent*)
{
  d->updateConsoleSize (true);
  d->grabConsoleBuffer ();
}

//////////////////////////////////////////////////////////////////////////////

void QConsole::viewPaintEvent (QConsoleView* w, QPaintEvent* event)
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
              p.setPen (d->m_colors[attr & 0x000f]);
              p.setBrush (d->m_colors[(attr >> 4) & 0x000f]);
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

void QConsole::wheelEvent (QWheelEvent* event)
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

bool QConsole::winEvent (MSG* msg, long* result)
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

void QConsole::scrollValueChanged (int value)
{
  d->setScrollValue (value);
}

//////////////////////////////////////////////////////////////////////////////

void QConsole::monitorConsole (void)
{
  d->monitorConsole ();
}

//////////////////////////////////////////////////////////////////////////////

void QConsole::focusInEvent (QFocusEvent* event)
{
  QWidget::focusInEvent (event);
}

//////////////////////////////////////////////////////////////////////////////

void QConsole::start (void)
{
  d->startCommand ();
}
