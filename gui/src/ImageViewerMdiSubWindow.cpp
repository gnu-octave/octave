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

#include "ImageViewerMdiSubWindow.h"
#include <QLabel>
#include <QPixmap>
#include <QScrollArea>

ImageViewerMdiSubWindow::ImageViewerMdiSubWindow(QPixmap pixmap, QWidget *parent)
    : QMdiSubWindow(parent),
      m_pixmap(pixmap) {
    construct();
}

void ImageViewerMdiSubWindow::construct() {
    QLabel *label = new QLabel();
    label->setBackgroundRole(QPalette::Base);
    label->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
    label->setScaledContents(true);
    label->setPixmap(m_pixmap);

    QScrollArea *scrollArea = new QScrollArea(this);
    scrollArea->setBackgroundRole(QPalette::Dark);
    scrollArea->setWidget(label);
    setWidget(scrollArea);
}
