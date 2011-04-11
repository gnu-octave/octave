
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
  
        QCodeEdit, copyright (c) 2006-2009 Luc Bruant aka fullmetalcoder,
                  is a free and open source software
  
  QCodeEdit sources are part of Edyuk and are thus available under GNU General Public
  License version 3 (GPL v3), as published by the Free Software Foundation.

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

QCodeEdit is a project aiming at the creation of a flexible and powerful text
editing framework for Qt4. It has started as a sub-project of Edyuk, copyright (c)
Luc Bruant, a free and open source IDE. As it proved successful it is also
released as a standalone library to make it easy for everyone to use such
a framework within other apps without having to reinvent the wheel.

Writing closed source applications with QCodeEdit is possible after buying a
proper license. For more informations about pricing and licensing conditions
please contact the author directly <non.deterministic.finite.organism@gmail.com>
Note that you will still need a Qt commercial license for that or, starting with
Qt 4.5, a LGPL one.

QCodeEdit depends on Qt 4.3 or newer, copyright (c) Nokia Corporation, which can
be downloaded at : ftp://ftp.trolltech.com/qt/sources

More information about Qt and Qt Software (formerly Trolltech) :
http://www.qtsoftware.com

Hoping you'll like it.


The author would like to thank all the people who contributed to QCodeEdit in various ways :

 * testing, reporting bugs, making suggestions :
	Jeremy Sonander, from Saros Inc
	Phil Martinot
	Benito van der Zander
	Ulrich Van Den Hekke
	Boris Barbulovski
	
 * contributing patches :
	Jerome Vizcaino
	Benito van der Zander
	Ulrich Van Den Hekke
	Boris Barbulovski

 * funding (by buying commercial licenses) :
	Saros Inc
	Movimento SE

 * spreading the word :
	Johan Thelin (posted a blog post that appeared on PlanetKDE)

(If you have been forgotten send an email to the author and the list will be updated)


IMPORTANT : If you encounter any sort of troubles trying to build or run QCodeEdit
please send a bug report with as many details as possible (including but not limited
to : OS, Qt version, compiler, QCodeEdit version, compile log, backtrace, ...) to the
team using either of these :
	* staff@qcodeedit.edyuk.org
	* Edyuk task tracker on Sf.net : http://sf.net/projects/edyuk
	* Edyuk webissues server (needs a WebIssues client) : http://edyuk.org/webissues/
		- login		: anonymous
		- password	: anonymous
	* QtCentre dedicated thread in Qt Software section : http://www.qtcentre.org/forum


In case you don't understand, blocks of text enclosed between lines of minus signs are
shell commands. The dollar signs just stand for command prompt.


 >>> Building :

------------------------------------------------------------------------------------------
$ qmake
$ make
------------------------------------------------------------------------------------------

This will build the library and the example. You may want to alter the build mode to force
either debug or release. If so just pass the mode you want to make, e.g. :

------------------------------------------------------------------------------------------
$ make release
------------------------------------------------------------------------------------------
or
------------------------------------------------------------------------------------------
$ make debug
------------------------------------------------------------------------------------------

Finally, with admins rights/root privilegdes, you can install QCodeEdit so as to be able
to use it simply :

------------------------------------------------------------------------------------------
$ qmake
$ make install
------------------------------------------------------------------------------------------

NB : the extra "qmake" is NEEDED to ensure that binaries will be located and copied properly.
NB 2 : Only the "make install" command requires root priviledges, "qmake" can and should
always be run as normal user.
NB 3 : Apart from libs and headers, QCodeEdit also provides a .prf file which makes it
easier to use the lib in another project. Under non-Unix platforms it is recommended to
copy the files manually (and possibly edit them to fit your needs).


 >>> Using within an app :

To have one of your app building with QCodeEdit just add the following line to your project
file (this won't work if you have not installed QCodeEdit as described above....) :

CONFIG += qcodeedit

If you did not install QCodeEdit as described above you will have to either update the file
qcodeedit.prf or inspect it to determine what project variables need to be adjusted and how.

Then, add proper headers in your sources and start coding. :D


 >>> Testing :

A very basic example is provided which open a list of files passed as parameters
and try to highlight them according to their file extension. Only a few language
definitions are provided :
  * C++ (with Doxygen support)
  * PHP
  * XML
  * Doxygen alone (for .dox files)
  * QMake project files
  * Python
  * QtScript/JavaScript
  * C# (WIP)
  * Lua (WIP)
  * LaTex (WIP)
  * BibTex (WIP)

If you write a new one for your own use (or modify an existing one to suit you needs)
please consider contributing it back to the project.

------------------------------------------------------------------------------------------
$ example/example [file]
------------------------------------------------------------------------------------------

Note : on Unix system it is recommended to use the script due to the need of setting
the LD_LIBRARY_PATH variable properly :

------------------------------------------------------------------------------------------
$ ./example.sh [file]
------------------------------------------------------------------------------------------

NB : [file] stands for a filename. If omitted a minimal string will be loaded and
considered as C++ source code


 >>> Generating documentation [needs Doxygen : http://www.doxygen.org] :

------------------------------------------------------------------------------------------
$ doxygen
------------------------------------------------------------------------------------------

NB :
	* This will create the documentation in the doc folder. Just open doc/html/index.html


 >>> Fetching bleeding edge sources [needs Subversion : http://subversion.tigris.org] :

------------------------------------------------------------------------------------------
$ svn co http://edyuk.svn.sf.net/svnroot/edyuk/trunk/3rdparty/qcodeedit2
------------------------------------------------------------------------------------------

NB : Using a graphical client this command extends to a "checkout" action using the above
repository URL.
