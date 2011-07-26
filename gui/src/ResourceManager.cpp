#include "ResourceManager.h"

ResourceManager ResourceManager::m_singleton;

ResourceManager::ResourceManager ()
{
  QDesktopServices desktopServices;
  m_homePath = desktopServices.storageLocation (QDesktopServices::HomeLocation);
  m_settings = new QSettings (m_homePath + "/.octave-gui", QSettings::IniFormat);
}

ResourceManager::~ResourceManager ()
{
  delete m_settings;
}

QSettings *
ResourceManager::settings ()
{
  return m_settings;
}

QString
ResourceManager::homePath ()
{
  return m_homePath;
}
