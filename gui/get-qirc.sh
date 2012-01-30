#!/bin/bash

# OctaveGUI - A graphical user interface for Octave
# Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

qirc_dir="qirc";
echo "Updating qirc..";

# Check whether we have already cloned the repository:
if [ -d $qirc_dir ]; then
	# Yes, so just pull any changes and rebuild.
	cd qirc
	git pull
	qmake-qt4 qirc.pro
	cd ..
else
	# No, clone the repository, checkout the stable branch
	# and build it.
	git clone https://code.google.com/p/qirc/
	git checkout master
	cd qirc
	qmake-qt4 qirc.pro
	make
	cd ..
fi
