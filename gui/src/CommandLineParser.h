/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef COMMANDLINEPARSER_H
#define COMMANDLINEPARSER_H

#include <QList>
#include <QString>

class CommandLineParser
{
public:
  struct CommandLineOption
  {
    QString longOption;
    QString shortOption;
    QString description;
    bool withArgument;

    bool operator== (CommandLineOption other)
    {
        return longOption == other.longOption
            || shortOption == other.shortOption;
    }
  };

  CommandLineParser ();
  void registerOption (CommandLineOption commandLineOption);
  void registerOption (QString longOption, QString shortOption, QString description, bool withArgument);
  void parse (int argc, char** argv);

private:
  QList<CommandLineOption> m_registeredCommandLineOptions;
};

#endif // COMMANDLINEPARSER_H
