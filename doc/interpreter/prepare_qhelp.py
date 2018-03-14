## Copyright (C) 2018 Pantxo Diribarne
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## Usage: python prepare_qhelp FNAME HTMLDIR
##
## Create a help project file FNAME.qhp and a help collection project
## file FNAME.qhcp after parsing index.html and Function-Index.html files
## in directory HTMLDIR. The latter must be relative to the parent
## directory of FNAME.

import sys, os
import lxml.html as lh
import lxml.etree as etree

if len(sys.argv) < 3:
    sys.exit('Usage: %s fname htmldir' % sys.argv[0])

# Parameters
basedir = os.path.abspath (os.path.dirname (sys.argv[1]))
fname = os.path.join (basedir, os.path.basename (sys.argv[1]))
htmldir =  os.path.basename (sys.argv[2])

title = 'GNU Octave Manual'
version = '1.0'
namespace = 'org.octave.interpreter-1.0'


# Utility functions
def get_toc (node):
    toc = list ()
    for li in node.iterchildren('li'):
        item = dict ()
        for a in li.iter('a'):
            item['name'] = a.text_content ()
            item['link'] = a.attrib['href']
            break

        for ul in li.iter('ul'):
            item['children'] = get_toc (ul)
            break

        toc.append(item)
    return toc

def populate_toc (node, toc, hdir):
    for item in toc:
        selt = etree.SubElement (node, 'section')
        selt.attrib['title'] = item['name']
        selt.attrib['ref'] = os.path.join (hdir, item['link'])
        if (item.has_key('children')):
            populate_toc (selt, item['children'], hdir)

def get_keywords (node):
    out = list ()
    keys = list ()
    for code in node.iter('code'):
        item = dict ()
        a = code.getparent ()
        key = code.text_content ()
        # Only retain the first entry for a given keyword
        if (key not in keys):
            keys.append (key)
            item['name'] = key
            item['link'] = a.attrib['href']
            out.append(item)
    return out

def populate_keywords (node, ref, hdir):
    for item in ref:
        selt = etree.SubElement (node, 'keyword')
        selt.attrib['name'] = item['name']
        selt.attrib['id'] = item['name']
        selt.attrib['ref'] = os.path.join (hdir, item['link'])

# Parse index.html to retrieve the table of contents
url = os.path.join (basedir, htmldir, 'index.html')
node = lh.parse(url).getroot ()
for elt in node.iter('div'):
    if (elt.attrib.has_key('class') and elt.attrib['class'] == 'contents'):
        node = elt
        for elt in node.iter('ul'):
            if elt.attrib['class'] == 'no-bullet':
                node = elt
                break
        break

toc = get_toc (node);

# Parse Function-Index.html to retrieve the function reference
url = os.path.join (basedir, htmldir, 'Function-Index.html')
node = lh.parse (url).getroot ()
for elt in node.iter('table'):
    if (elt.attrib.has_key('class') and elt.attrib['class'] == 'index-fn'):
        node = elt
        break

ref = get_keywords (node)

# Prepare Qt Help Project document
root = etree.Element("QtHelpProject")
root.attrib['version'] = version

elt = etree.SubElement (root, 'namespace')
elt.text = namespace

elt = etree.SubElement (root, 'virtualFolder')
elt.text = 'doc'

elt = etree.SubElement (root, 'customFilter')
elt.attrib['name'] = 'Octave Manual'
selt = etree.SubElement (elt, 'filterAttribute')
selt.text = 'core'
selt = etree.SubElement (elt, 'filterAttribute')
selt.text = 'manual'

elt = etree.SubElement (root, 'customFilter')
elt.attrib['name'] = 'Octave C++ API'
selt = etree.SubElement (elt, 'filterAttribute')
selt.text = 'core'
selt = etree.SubElement (elt, 'filterAttribute')
selt.text = 'cpp'

elt = etree.SubElement (root, 'filterSection')
selt = etree.SubElement (elt, 'filterAttribute')
selt.text = 'core'
selt = etree.SubElement (elt, 'filterAttribute')
selt.text = 'manual'
selt = etree.SubElement (elt, 'toc')
sselt = etree.SubElement (selt, 'section')
sselt.attrib['title'] = title
sselt.attrib['ref'] = os.path.join (htmldir, 'index.html')
populate_toc (sselt, toc, htmldir)

selt = etree.SubElement (elt, 'keywords')
populate_keywords (selt, ref, htmldir)

selt = etree.SubElement (elt, 'files')
sselt = etree.SubElement (selt, 'file')
sselt.text = os.path.join (htmldir, '*.html')
sselt = etree.SubElement (selt, 'file')
sselt.text = os.path.join (htmldir, '*.png')
sselt = etree.SubElement (selt, 'file')
sselt.text = os.path.join (htmldir, '*.css')

fid = open (fname + '.qhp', 'w+')
fid.write ('<?xml version="1.0" encoding="UTF-8"?>\n')
fid.write ('<!--DO NOT EDIT!  Generated automatically by prepare_qhelp.py-->\n')
fid.write (etree.tostring (root, pretty_print = True))
fid.close ()

# Prepare Qt Help Collection Project document
root = etree.Element("QHelpCollectionProject")
root.attrib['version'] = version
elt = etree.SubElement (root, 'docFiles')

selt = etree.SubElement (elt, 'generate')
sselt = etree.SubElement (selt, 'file')
ssselt = etree.SubElement (sselt, 'input')
tmp = fname + '.qhp'
ssselt.text = tmp
ssselt = etree.SubElement (sselt, 'output')
ssselt.text = fname + '.qch'

selt = etree.SubElement (elt, 'register')
sselt = etree.SubElement (selt, 'file')
sselt.text = fname + '.qch'

fid = open (fname + '.qhcp', 'w+')
fid.write ('<?xml version="1.0" encoding="UTF-8"?>\n')
fid.write ('<!--DO NOT EDIT! Generated automatically by prepare_qhelp.py-->\n')
fid.write (etree.tostring (root, pretty_print = True))
fid.close ()
