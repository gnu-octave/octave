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

#include "CommandLineParser.h"

CommandLineParser::CommandLineParser ()
{
}

void
CommandLineParser::registerOption (CommandLineOption commandLineOption)
{
  if (m_registeredCommandLineOptions.contains(commandLineOption))
    m_registeredCommandLineOptions.append(commandLineOption);
}

void
CommandLineParser::registerOption (QString longOption, QString shortOption, QString description, bool withArgument)
{
  CommandLineOption commandLineOption;
  commandLineOption.longOption = longOption;
  commandLineOption.shortOption = shortOption;
  commandLineOption.description = description;
  commandLineOption.withArgument = withArgument;
  registerOption (commandLineOption);
}

void
CommandLineParser::parse (int argc, char** argv)
{
  Q_UNUSED(argc);
  Q_UNUSED(argv);
}
