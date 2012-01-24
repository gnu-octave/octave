#!/bin/bash

# OctaveGUI - A graphical user interface for Octave
# Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

qterminal_dir="qterminal";
echo "Updating qterminal..";

# Check whether we have already cloned the repository:
if [ -d $qterminal_dir ]; then
	# Yes, so just pull any changes and rebuild.
	cd qterminal
	git pull
	qmake-qt4 qterminal.pro
	cd ..
else
	# No, clone the repository, checkout the stable branch
	# and build it.
	git clone https://code.google.com/p/qterminal/
	git checkout master
	cd qterminal
	qmake-qt4 qterminal.pro
	make
	cd ..
fi
