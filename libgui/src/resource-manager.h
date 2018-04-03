/*

Copyright (C) 2011-2018 Jacob Dawid

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_resource_manager_h)
#define octave_resource_manager_h 1

#include <QDesktopServices>
#include <QIcon>
#include <QComboBox>
#include <QMap>
#include <QSettings>
#include <QTranslator>

namespace octave
{
  class resource_manager : public QObject
  {
    Q_OBJECT

  protected:

  public:

    resource_manager (void);

    // No copying!

    resource_manager (const resource_manager&) = delete;

    resource_manager& operator = (const resource_manager&) = delete;

    ~resource_manager ();

    static QSettings * get_settings (void)
    {
      return instance_ok () ? instance->do_get_settings () : nullptr;
    }

    static QIcon icon (const QString& icon_name, bool fallback = true)
    {
      if (instance_ok ())
        return instance->do_icon (icon_name, fallback);

      return QIcon ();
    }

    static QSettings * get_default_settings (void)
    {
      return instance_ok () ? instance->do_get_default_settings () : nullptr;
    }

    static QString get_settings_file (void)
    {
      return instance_ok () ? instance->do_get_settings_file () : QString ();
    }

    static void reload_settings (void)
    {
      if (instance_ok ())
        instance->do_reload_settings ();
    }

    static void set_settings (const QString& file)
    {
      if (instance_ok ())
        instance->do_set_settings (file);
    }

    static bool update_settings_key (const QString& new_key,
                                     const QString& old_key)
    {
      return (instance_ok ()
              ? instance->do_update_settings_key (new_key, old_key)
              : false);
    }

    static void combo_encoding (QComboBox *combo, QString current = QString ())
    {
      if (instance_ok ())
        instance->do_combo_encoding (combo, current);
    }

    static QString get_gui_translation_dir (void);

    static void config_translators (QTranslator*, QTranslator*, QTranslator*);

    static void update_network_settings (void)
    {
      if (instance_ok ())
        instance->do_update_network_settings ();
    }

    static bool is_first_run (void)
    {
      return instance_ok () ? instance->do_is_first_run () : true;
    }

    static QString storage_class_chars (void) { return "afghip"; }
    static QStringList storage_class_names (void);
    static QList<QColor> storage_class_default_colors (void);

    static QString terminal_color_chars (void) { return "fbsc"; }
    static QStringList terminal_color_names (void);
    static QList<QColor> terminal_default_colors (void);

    static resource_manager *instance;

  public slots:

    static void cleanup_instance (void) { delete instance; instance = nullptr; }

    static QString varedit_color_chars (void) {return "fbsha"; }
    static QStringList varedit_color_names (void);
    static QList<QColor> varedit_default_colors (void);

  private:

    static bool instance_ok (void);

    QSettings * do_get_settings (void) const;

    QSettings * do_get_default_settings (void) const;

    QString do_get_settings_directory (void);

    QString do_get_settings_file (void);

    void do_reload_settings (void);

    void do_set_settings (const QString& file);

    bool do_update_settings_key (const QString& new_key, const QString& old_key);

    bool do_is_first_run (void) const;

    void do_update_network_settings (void);

    QIcon do_icon (const QString& icon, bool fallback);

    void do_combo_encoding (QComboBox *combo, QString current);

    QString m_settings_directory;

    QString m_settings_file;

    QSettings *m_settings;

    QSettings *m_default_settings;
  };
}

// FIXME: This is temporary and should be removed when all classes that
// use the resource_manager class are also inside the octave namespace.
using octave::resource_manager;

#endif
