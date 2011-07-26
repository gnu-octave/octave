#ifndef RESOURCEMANAGER_H
#define RESOURCEMANAGER_H

#include <QSettings>
#include <QDesktopServices>

class ResourceManager
{
public:
  ~ResourceManager ();

  static ResourceManager *
  instance ()
  {
    return &m_singleton;
  }

  QSettings *settings ();
  QString homePath ();
private:
  ResourceManager ();

  QSettings *m_settings;
  QString m_homePath;
  static ResourceManager m_singleton;
};

#endif // RESOURCEMANAGER_H
