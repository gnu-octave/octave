/* Quint - A graphical user interface for Octave
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

#ifndef OCTAVETERMINAL_H
#define OCTAVETERMINAL_H
#include "QTerminalWidget.h"

class OctaveTerminal : public QTerminalWidget {
    Q_OBJECT
public:
    OctaveTerminal(QWidget *parent = 0);
    ~OctaveTerminal();

private:
    void construct();
};
#endif // OCTAVETERMINAL_H
