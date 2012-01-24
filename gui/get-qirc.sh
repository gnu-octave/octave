#!/bin/bash

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
