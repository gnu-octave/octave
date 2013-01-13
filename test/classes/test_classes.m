## Copyright (C) 2009-2012 Robert T. Short
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%%  Test script for legacy OOP.
%%  Requires the path to contain the test classes.
%%
%%  Note: This script and all classes are also intended to run
%%        in MATLAB to test compatibility.  Don't break that!
%%
%%  To Do:  This script tests to ensure that things done correctly work
%%          corrrectly.  It should also check that things done incorrectly
%%          error properly.
%%
%%  The classes used for the tests reside in the test directory.
%%
%%  The classes provide the ability to test most of the major features
%%  of the legacy OOP facilities.  There are a number of classes, mostly
%%  kind of the same, that create a hierarchy.

%%  Test the Snork class.  The Snork class has a number of the standard
%%  methods that the others don't so we can test indexing and other
%%  features.
%!shared snk, snk1, snk2
%!test snk = Snork();
%!  assert(gick(snk),1)
%!  assert(snk.gick,1)
%!  assert(snk(2),1);
%!  assert(snk{end},3);
%!test snk = gick(snk,2);
%!  assert(gick(snk),2)
%!test snk = set(snk,'gick',7);
%!  assert(get(snk, 'gick'), 7)
%!test snk.gick = 4;
%! assert(gick(snk),4)
%!   snk(1) = 3;
%!test snk{end} = 9;
%!  assert(cack(snk),[3 1 2 9])
%!  assert(getStash(snk),1)             % Check private functions.
%!  assert(isobject(snk))
%!  assert(class(snk),'Snork')
%!  assert(isa(snk,'Snork'))
%!  assert(~isa(snk,'Sneetch'))
%!  assert(ismethod(snk,'gick'))
%!  assert(~ismethod(snk,'bletch'))
%!  assert(exist('snk'))
%!  assert(~exist('blink'))
%!test snk1 = Snork(snk);
%!  assert(class(snk1),'Snork')
%!  assert(gick(snk1),4)
%!test snk2 = Snork(-3);
%!  assert(class(snk2),'Snork')
%!  assert(gick(snk2),-3)
%!test x=[1 2 3 4];
%!  assert(x(snk),1);
%% x=methods('Snork');                   % Need to test the methods function.
%% save temp snk;
%% load temp                             % This load causes a segment fault.
%% assert(cack(snk),[-1 -2 -3 -4]);      % This is a major bug!

%% The Spork class is a near clone of Snork but without as many standard
%% methods.  We are testing no new octave features, but this is makes
%% sure that we haven't bollixed up the Spork class if we should make
%% changes.  We use Spork in the class hierarchy.
%!shared sprk
%!test sprk = Spork();
%!  assert(geek(sprk),1)
%!test sprk = geek(sprk,3);
%!  assert(geek(sprk),3)
%!test sprk = set(sprk,'geek',7);
%!  assert(get(sprk, 'geek'), 7)
%!  assert(class(sprk),'Spork');
%!  assert(isa(sprk,'Spork'))

%%  The Blork class is a near clone of Snork but without as many standard
%%  methods.  We are testing no new octave features, but this is makes
%%  sure that we haven't bollixed up the Blork class if we should make
%%  changes.  We use Blork in the class hierarchy.
%!shared blrk
%!test blrk = Blork();
%!  assert(bleek(blrk),1)
%!test blrk = bleek(blrk,3);
%!  assert(bleek(blrk),3)
%!test blrk = set(blrk,'bleek',13);
%!  assert(get(blrk, 'bleek'), 13)
%!  assert(class(blrk),'Blork');
%!  assert(isa(blrk,'Blork'))

%%  The Cork class is a near clone of Snork but without as many standard
%%  methods.  We are testing no new octave features, but this is makes
%%  sure that we haven't bollixed up the Cork class if we should make
%%  changes.  We use Cork in the class hierarchy.
%!shared crk
%!test crk = Cork(23);
%!  assert(click(crk),23)
%!test crk = click(crk,3);
%!  assert(click(crk),3)
%!test crk = set(crk,'click',13);
%!  assert(get(crk, 'click'), 13)
%!  assert(class(crk),'Cork');
%!  assert(isa(crk,'Cork'))

%%  The Dork class tests single inheritance.
%!shared drk
%!test drk = Dork();
%!  assert(gack(drk),0)
%!test drk = gack(drk,-2);
%!  assert(gack(drk),-2)
%!test drk = gick(drk,2);
%!  assert(gick(drk),2);
%!test drk = set(drk, 'gick',3, 'gack',-3);
%!  assert(get(drk, 'gick'), 3)
%!  assert(get(drk, 'gack'), -3)
%!  assert(class(drk),'Dork')
%!  assert(isa(drk,'Dork'))
%!  assert(isa(drk,'Snork'))
%!  assert(getStash(drk),2)
%!test drk1 = Dork(drk);
%!  assert(class(drk1),'Dork')
%!  assert(isa(drk1,'Snork'))
%!  assert(gick(drk1),3)
%!  assert(gack(drk1),-3)
%!test drk2 = Dork(-4,4);
%!  assert(class(drk2),'Dork')
%!  assert(isa(drk2,'Snork'))
%!  assert(gick(drk2),-4)
%!  assert(gack(drk2),4)

%%  The Pork class is essentially a clone of Dork.  It is used as part
%%  of the multiple inheritance test.
%!shared prk, drk
%!test prk = Pork();
%!  assert(geek(prk),1)
%!  assert(gurk(prk),0)
%!test prk = gurk(prk,-3);
%!  assert(gurk(prk),-3)
%!test prk = geek(prk,9);
%!  assert(geek(prk),9)
%!  assert(class(prk),'Pork')
%!  assert(isa(prk,'Pork'))
%!  assert(isa(prk,'Spork'))
%!test drk = Dork();                   % Precedence.
%!  assert(bling(drk,prk),2)
%!  assert(bling(prk,drk),2)
  
%%  The Gork class tests aggregation and multiple inheritance.
%!shared grk
%!test grk = Gork();
%!  assert(gick(grk),1)
%!  assert(geek(grk),1)
%!  assert(gack(grk),0)
%!  assert(gurk(grk),0)
%!  assert(bleek(grk),1)
%!  assert(gark(grk),-2)
%!  assert(click(cork(grk)),17)
%!  assert(class(cork(grk)),'Cork')
%!test grk = gick(grk,3);
%!test grk = geek(grk,4);
%!test grk = gack(grk,-9);
%!test grk = gurk(grk,-8);
%!test grk = bleek(grk,-7);
%!test grk = gark(grk,-6);
%!test grk = cork(grk,click(cork(grk),23));
%!  assert(gick(grk),3)
%!  assert(geek(grk),4)
%!  assert(gack(grk),-9)
%!  assert(gurk(grk),-8)
%!  assert(bleek(grk),-7)
%!  assert(gark(grk),-6)
%!  assert(click(cork(grk)),23)
%!test
%!    cork1 = Cork(13);
%!    grk = set(grk, 'gick',-5, 'gack',-6, 'gark',-7, 'cork',cork1);
%!  assert(get(grk,'gick'),-5)
%!  assert(get(grk,'gack'),-6)
%!  assert(get(grk,'gark'),-7)
%!  assert(click(get(grk, 'cork')),13);
%!test grk = set(grk, 'cork',12);
%!  assert(click(get(grk, 'cork')),12);
%!  assert(class(cork(grk)),'Cork')
%!  assert(class(grk),'Gork')
%!  assert(isa(grk,'Gork'))
%!  assert(isa(grk,'Dork'))
%!  assert(isa(grk,'Pork'))
%!  assert(isa(grk,'Blork'))
%!  assert(isa(grk,'Snork'))
%!  assert(isa(grk,'Spork'))

%%  Basic classdef tests
%!shared p, i, amt
%! p = foo_payment (4, 4*12, 50e3);
%! i = p.rate / (12 * 100);
%! amt = (p.principle * i) / (1 - (1 + i)^(-p.term));
%!assert (class (p), "foo_payment");
%!assert (p.term, 48);
%!assert (p.rate, 4.0);
%!assert (p.principle, 50e3);
%!assert (p.amount, amt, eps ())
%!xtest
%! assert (amount (p), amt, eps ())
%!xtest
%! xassert (properties (p), {'rate'; 'term'; 'principle'})
%!xtest
%! xassert (methods (p), {'amount'; 'foo_payment'})
