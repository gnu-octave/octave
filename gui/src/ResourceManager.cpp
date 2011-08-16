/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "ResourceManager.h"
#include <QFile>
#include <QNetworkProxy>

ResourceManager ResourceManager::m_singleton;

ResourceManager::ResourceManager ()
{
  m_settings = 0;
  QDesktopServices desktopServices;
  m_homePath = desktopServices.storageLocation (QDesktopServices::HomeLocation);
  setSettings(m_homePath + "/.octave-gui");
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

void
ResourceManager::setSettings (QString file)
{
  delete m_settings;
  if (!QFile::exists (file))
    {
      QFile::copy("../default-settings/.octave-gui", file);
    }
  m_settings = new QSettings (file, QSettings::IniFormat);
}

QString
ResourceManager::findTranslatorFile (QString language)
{
  // TODO: Quick hack to be able to test language files.
  return QString("../languages/%1.qm").arg(language);
}

void
ResourceManager::updateNetworkSettings ()
{
  QNetworkProxy::ProxyType proxyType = QNetworkProxy::NoProxy;
  if (m_settings->value ("useProxyServer").toBool ())
    {
      QString proxyTypeString = m_settings->value ("proxyType").toString ();
      if (proxyTypeString == "Socks5Proxy")
        {
          proxyType = QNetworkProxy::Socks5Proxy;
        }
      else if (proxyTypeString == "HttpProxy")
        {
          proxyType = QNetworkProxy::HttpProxy;
        }
    }

  QNetworkProxy proxy;
  proxy.setType (proxyType);
  proxy.setHostName (m_settings->value ("proxyHostName").toString ());
  proxy.setPort (m_settings->value ("proxyPort").toInt ());
  proxy.setUser (m_settings->value ("proxyUserName").toString ());
  proxy.setPassword (m_settings->value ("proxyPassword").toString ());
  QNetworkProxy::setApplicationProxy (proxy);
}
