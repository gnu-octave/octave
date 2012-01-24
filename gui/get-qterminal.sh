#!/bin/bash

qterminal_dir="qterminal";
echo "Updating qterminal..";

# Check whether we have already cloned the repository:
if [ -d $qterminal_dir ]; then
	# Yes, so just pull any changes and rebuild.
	cd qterminal
	git pull
	qmake-qt4
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
