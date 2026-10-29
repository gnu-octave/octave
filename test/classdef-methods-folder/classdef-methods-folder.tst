########################################################################
##
## Copyright (C) 2013-2026 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
##
########################################################################

## clear classdef definitions cached from other test files
%!shared
%! clear classes

# valid calls graph:
# ------------------
# n,hn                       ==> [m, m2, s, so, s2, s2o, l, p; hm, hmo, hmvo, hm2, hm2o, hm2vo, hs, hsv, hso, hsvo, hs2, hs2v, hs2o, hs2vo, hl, hp]
# m,hm,hmo,hmvo              ==> [m2, s, so, s2, s2o, l, p; hm2, hm2o, hm2vo, hs, hsv, hso, hsvo, hs2, hs2v, hs2o, hs2vo, hl, hp]
# m2,hm2,hm2o,hm2vo          ==> [m, s, so, s2, s2o, p; hmo, hmvo, hs, hsv, hso, hsvo, hs2, hs2v, hs2o, hs2vo, hp]
# s,so,hs,hsv,hso,hsvo       ==> [s2, l, p; hs2, hs2v, hl, hp]
# s2,s2o,hs2,hs2v,hs2o,hs2vo ==> [s, p; hs, hsv, hp]
# l,hl                       ==> [s, s2, p; hs, hsv, hs2, hs2v, hp]
# p,hp                       ==> [s, s2; hs, hsv, hs2, hs2v]
#
# note: to avoid repetition, we skip cycles e.g: ...->{s,so,s2,s2o}->*

# direct calls
# m, m2, s, so, s2, s2o
%!test
%! obj = testclass ();
%! assert (obj.method_function (), testclass.m);
%! assert (obj.method_external_function (), testclass.m2);
%! assert (testclass.static_function (), testclass.s);
%! assert (obj.static_function (), testclass.s);
%! assert (testclass.static_external_function (), testclass.s2);
%! assert (obj.static_external_function (), testclass.s2);
%! err = false; try local_function (); catch, err = true; end_try_catch, assert (err);
%! err = false; try private_function (); catch, err = true; end_try_catch, assert (err);

# dispatch calls

# n, n->m, n->m->m2, n->m->s, n->m->so, n->m->s2, n->m->s2o,
# n->m->l, n->m->l->s, n->m->l->s2, n->m->l->p, n->m->l->p->s, n->m->l->p->s2,
# n->m->p, n->m->p->s, n->m->p->s2,
# n->m2, n->m2->m, n->m2->s, n->m2->so, n->m2->s2, n->m2->s2o,
# n->m2->p, n->m2->p->s, n->m2->p->s2,
# n->s, n->so, n->s2, n->s2o,
# n->l, n->l->s, n->l->s2, n->l->p, n->l->p->s, n->l->p->s2,
# n->p, n->p->s, n->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.n), testclass.n);
%! assert (obj.call (testclass.n, testclass.m), testclass.m);
%! assert (obj.call (testclass.n, testclass.m, testclass.m2), testclass.m2);
%! assert (obj.call (testclass.n, testclass.m, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.so), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.m, testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.n, testclass.m, testclass.l), testclass.l);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.m, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.m2), testclass.m2);
%! assert (obj.call (testclass.n, testclass.m2, testclass.m), testclass.m);
%! assert (obj.call (testclass.n, testclass.m2, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m2, testclass.so), testclass.s);
%! assert (obj.call (testclass.n, testclass.m2, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.m2, testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.n, testclass.m2, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.m2, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m2, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.so), testclass.s);
%! assert (obj.call (testclass.n, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.n, testclass.l), testclass.l);
%! assert (obj.call (testclass.n, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.l, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.n, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.p, testclass.s2), testclass.s2);

# m, m->m2, m->m2->s, m->m2->so, m->m2->s2, m->m2->s2o,
# m->m2->p, m->m2->p->s, m->m2->p->s2
# m->s, m->so, m->s2, m->s2o,
# m->l, m->l->s, m->l->s2, m->l->p, m->l->p->s, m->l->p->s2,
# m->p, m->p->s, m->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.m), testclass.m);
%! assert (obj.call (testclass.m, testclass.m2), testclass.m2);
%! assert (obj.call (testclass.m, testclass.m2, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.m2, testclass.so), testclass.s);
%! assert (obj.call (testclass.m, testclass.m2, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m, testclass.m2, testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.m, testclass.m2, testclass.p), testclass.p);
%! assert (obj.call (testclass.m, testclass.m2, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.m2, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.so), testclass.s);
%! assert (obj.call (testclass.m, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m, testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.m, testclass.l), testclass.l);
%! assert (obj.call (testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.l, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m, testclass.p), testclass.p);
%! assert (obj.call (testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.p, testclass.s2), testclass.s2);

# m2, m2->m, m2->m->s, m2->m->so, m2->m->s2, m2->m->s2o,
# m2->m->l, m2->m->l->s, m2->m->l->s2,
# m2->m->l->p, m2->m->l->p->s, m2->m->l->p->s2
# m2->m->p, m2->m->p->s, m2->m->p->s2
# m2->s, m2->so, m2->s2, m2->s2o,
# m2->p, m2->p->s, m2->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.m2), testclass.m2);
%! assert (obj.call (testclass.m2, testclass.m), testclass.m);
%! assert (obj.call (testclass.m2, testclass.m, testclass.s), testclass.s);
%! assert (obj.call (testclass.m2, testclass.m, testclass.so), testclass.s);
%! assert (obj.call (testclass.m2, testclass.m, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m2, testclass.m, testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.m2, testclass.m, testclass.l), testclass.l);
%! assert (obj.call (testclass.m2, testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.m2, testclass.m, testclass.l, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m2, testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.m2, testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.m2, testclass.m, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m2, testclass.m, testclass.p), testclass.p);
%! assert (obj.call (testclass.m2, testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.m2, testclass.m, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m2, testclass.s), testclass.s);
%! assert (obj.call (testclass.m2, testclass.so), testclass.s);
%! assert (obj.call (testclass.m2, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.m2, testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.m2, testclass.p), testclass.p);
%! assert (obj.call (testclass.m2, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.m2, testclass.p, testclass.s2), testclass.s2);

# s, s->s2,
# s->l, s->l->s, s->l->s2, s->l->p, s->l->p->s, s->l->p->s2,
# s->p, s->p->s, s->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.s), testclass.s);
%! assert (obj.call (testclass.s, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.s, testclass.l), testclass.l);
%! assert (obj.call (testclass.s, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.s, testclass.l, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.s, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.s, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.s, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.s, testclass.p), testclass.p);
%! assert (obj.call (testclass.s, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.s, testclass.p, testclass.s2), testclass.s2);

# so, so->s2,
# so->l, so->l->s, so->l->s2, so->l->p, so->l->p->s, so->l->p->s2,
# so->p, so->p->s, so->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.so), testclass.s);
%! assert (obj.call (testclass.so, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.so, testclass.l), testclass.l);
%! assert (obj.call (testclass.so, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.so, testclass.l, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.so, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.so, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.so, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.so, testclass.p), testclass.p);
%! assert (obj.call (testclass.so, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.so, testclass.p, testclass.s2), testclass.s2);

# s2, s2->s, s2->p, s2->p->s, s2->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.s2), testclass.s2);
%! assert (obj.call (testclass.s2, testclass.s), testclass.s);
%! assert (obj.call (testclass.s2, testclass.p), testclass.p);
%! assert (obj.call (testclass.s2, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.s2, testclass.p, testclass.s2), testclass.s2);

# s2o, s2o->s, s2o->p, s2o->p->s, s2o->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.s2o), testclass.s2);
%! assert (obj.call (testclass.s2o, testclass.s), testclass.s);
%! assert (obj.call (testclass.s2o, testclass.p), testclass.p);
%! assert (obj.call (testclass.s2o, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.s2o, testclass.p, testclass.s2), testclass.s2);

# l, l->s, l->s2, l->p, l->p->s, l->p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.l), testclass.l);
%! assert (obj.call (testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.l, testclass.s2), testclass.s2);
%! assert (obj.call (testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.l, testclass.p, testclass.s2), testclass.s2);

# p, p->s, p->s->l, p->s2
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.p), testclass.p);
%! assert (obj.call (testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.p, testclass.s2), testclass.s2);
%! err = false; try obj.call (testclass.p, testclass.l); catch, err = true; end_try_catch, assert (err);
%! assert (obj.call (testclass.p, testclass.s, testclass.l), testclass.l);

# valid calls graph:
# ------------------
# n,hn                       ==> [m, m2, s, so, s2, s2o, l, p; hm, hmo, hmvo, hm2, hm2o, hm2vo, hs, hsv, hso, hsvo, hs2, hs2v, hs2o, hs2vo, hl, hp]
# m,hm,hmo,hmvo              ==> [m2, s, so, s2, s2o, l, p; hm2, hm2o, hm2vo, hs, hsv, hso, hsvo, hs2, hs2v, hs2o, hs2vo, hl, hp]
# m2,hm2,hm2o,hm2vo          ==> [m, s, so, s2, s2o, p; hmo, hmvo, hs, hsv, hso, hsvo, hs2, hs2v, hs2o, hs2vo, hp]
# s,so,hs,hsv,hso,hsvo       ==> [s2, l, p; hs2, hs2v, hl, hp]
# s2,s2o,hs2,hs2v,hs2o,hs2vo ==> [s, p; hs, hsv, hp]
# l,hl                       ==> [s, s2, p; hs, hsv, hs2, hs2v, hp]
# p,hp                       ==> [s, s2; hs, hsv, hs2, hs2v]
#
# note: to avoid repetition, we skip cycles e.g: ...->{s,so,s2,s2o}->*

#TODO add tests for error cases: {m2,s2,p}->l
# (*->{l,hl} are only accessible for functions defined in same file)
#TODO another interesting case: m2->hm not allowed, while m->hm2 is allowed

## dispatch calls using function handles

# hn, hn->m, hn->m->m2, hn->m->s, hn->m->so, hn->m->s2, hn->m->s2o,
# hn->m->l, hn->m->l->s, hn->m->l->s2, hn->m->l->p, hn->m->l->p->s, hn->m->l->p->s2,
# hn->m->p, hn->m->p->s, hn->m->p->s2,
# hn->m2, hn->m2->m, hn->m2->s, hn->m2->so, hn->m2->s2, hn->m2->s2o,
# hn->m2->p, hn->m2->p->s, hn->m2->p->s2,
# hn->s, hn->so, hn->s2, hn->s2o,
# hn->l, hn->l->s, hn->l->s2, hn->l->p, hn->l->p->s, hn->l->p->s2,
# hn->p, hn->p->s, hn->p->s2
%!test
%! obj = testclass ();
%! hn = obj.call (testclass.hn);
%! assert (hn (), testclass.n);
%! assert (hn (testclass.m), testclass.m);
%! assert (hn (testclass.m, testclass.m2), testclass.m2);
%! assert (hn (testclass.m, testclass.s), testclass.s);
%! assert (hn (testclass.m, testclass.so), testclass.s);
%! assert (hn (testclass.m, testclass.s2), testclass.s2);
%! assert (hn (testclass.m, testclass.s2o), testclass.s2);
%! assert (hn (testclass.m, testclass.l), testclass.l);
%! assert (hn (testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (hn (testclass.m, testclass.l, testclass.s2), testclass.s2);
%! assert (hn (testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (hn (testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.m, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hn (testclass.m, testclass.p), testclass.p);
%! assert (hn (testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.m, testclass.p, testclass.s2), testclass.s2);
%! assert (hn (testclass.m2), testclass.m2);
%! assert (hn (testclass.m2, testclass.m), testclass.m);
%! assert (hn (testclass.m2, testclass.s), testclass.s);
%! assert (hn (testclass.m2, testclass.so), testclass.s);
%! assert (hn (testclass.m2, testclass.s2), testclass.s2);
%! assert (hn (testclass.m2, testclass.s2o), testclass.s2);
%! assert (hn (testclass.m2, testclass.p), testclass.p);
%! assert (hn (testclass.m2, testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.m2, testclass.p, testclass.s2), testclass.s2);
%! assert (hn (testclass.s), testclass.s);
%! assert (hn (testclass.so), testclass.s);
%! assert (hn (testclass.s2), testclass.s2);
%! assert (hn (testclass.s2o), testclass.s2);
%! assert (hn (testclass.l), testclass.l);
%! assert (hn (testclass.l, testclass.s), testclass.s);
%! assert (hn (testclass.l, testclass.s2), testclass.s2);
%! assert (hn (testclass.l, testclass.p), testclass.p);
%! assert (hn (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hn (testclass.p), testclass.p);
%! assert (hn (testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.p, testclass.s2), testclass.s2);

# hm, hm->m2, hm->m2->s, hm->m2->so, hm->m2->s2, hm->m2->s2o,
# hm->m2->p, hm->m2->p->s, hm->m2->p->s2
# hm->s, hm->so, hm->s2, hm->s2o,
# hm->l, hm->l->s, hm->l->s2, hm->l->p, hm->l->p->s, hm->l->p->s2,
# hm->p, hm->p->s, hm->p->s2
%!test
%! obj = testclass ();
%! hm = obj.call (testclass.hm);
%! assert (hm (obj), testclass.m);
%! assert (hm (obj, testclass.m2), testclass.m2);
%! assert (hm (obj, testclass.m2, testclass.s), testclass.s);
%! assert (hm (obj, testclass.m2, testclass.so), testclass.s);
%! assert (hm (obj, testclass.m2, testclass.s2), testclass.s2);
%! assert (hm (obj, testclass.m2, testclass.s2o), testclass.s2);
%! assert (hm (obj, testclass.m2, testclass.p), testclass.p);
%! assert (hm (obj, testclass.m2, testclass.p, testclass.s), testclass.s);
%! assert (hm (obj, testclass.m2, testclass.p, testclass.s2), testclass.s2);
%! assert (hm (obj, testclass.s), testclass.s);
%! assert (hm (obj, testclass.so), testclass.s);
%! assert (hm (obj, testclass.s2), testclass.s2);
%! assert (hm (obj, testclass.s2o), testclass.s2);
%! assert (hm (obj, testclass.l), testclass.l);
%! assert (hm (obj, testclass.l, testclass.s), testclass.s);
%! assert (hm (obj, testclass.l, testclass.s2), testclass.s2);
%! assert (hm (obj, testclass.l, testclass.p), testclass.p);
%! assert (hm (obj, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hm (obj, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hm (obj, testclass.p), testclass.p);
%! assert (hm (obj, testclass.p, testclass.s), testclass.s);
%! assert (hm (obj, testclass.p, testclass.s2), testclass.s2);

# hmo, hmo->m2, hmo->m2->s, hmo->m2->so, hmo->m2->s2, hmo->m2->s2o,
# hmo->m2->p, hmo->m2->p->s, hmo->m2->p->s2
# hmo->s, hmo->so, hmo->s2, hmo->s2o,
# hmo->l, hmo->l->s, hmo->l->s2, hmo->l->p, hmo->l->p->s, hmo->l->p->s2,
# hmo->p, hmo->p->s, hmo->p->s2
%!test
%! obj = testclass ();
%! hmo = obj.call (testclass.hmo);
%! assert (hmo (), testclass.m);
%! assert (hmo (testclass.m2), testclass.m2);
%! assert (hmo (testclass.m2, testclass.s), testclass.s);
%! assert (hmo (testclass.m2, testclass.so), testclass.s);
%! assert (hmo (testclass.m2, testclass.s2), testclass.s2);
%! assert (hmo (testclass.m2, testclass.s2o), testclass.s2);
%! assert (hmo (testclass.m2, testclass.p), testclass.p);
%! assert (hmo (testclass.m2, testclass.p, testclass.s), testclass.s);
%! assert (hmo (testclass.m2, testclass.p, testclass.s2), testclass.s2);
%! assert (hmo (testclass.s), testclass.s);
%! assert (hmo (testclass.so), testclass.s);
%! assert (hmo (testclass.s2), testclass.s2);
%! assert (hmo (testclass.s2o), testclass.s2);
%! assert (hmo (testclass.l), testclass.l);
%! assert (hmo (testclass.l, testclass.s), testclass.s);
%! assert (hmo (testclass.l, testclass.s2), testclass.s2);
%! assert (hmo (testclass.l, testclass.p), testclass.p);
%! assert (hmo (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hmo (testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hmo (testclass.p), testclass.p);
%! assert (hmo (testclass.p, testclass.s), testclass.s);
%! assert (hmo (testclass.p, testclass.s2), testclass.s2);

# hmvo, hmvo->m2, hmvo->m2->s, hmvo->m2->so, hmvo->m2->s2, hmvo->m2->s2o,
# hmvo->m2->p, hmvo->m2->p->s, hmvo->m2->p->s2
# hmvo->s, hmvo->so, hmvo->s2, hmvo->s2o,
# hmvo->l, hmvo->l->s, hmvo->l->s2, hmvo->l->p, hmvo->l->p->s, hmvo->l->p->s2,
# hmvo->p, hmvo->p->s, hmvo->p->s2
%!test
%! obj = testclass ();
%! hmvo = obj.call (testclass.hmvo);
%! assert (hmvo (), testclass.m);
%! assert (hmvo (testclass.m2), testclass.m2);
%! assert (hmvo (testclass.m2, testclass.s), testclass.s);
%! assert (hmvo (testclass.m2, testclass.so), testclass.s);
%! assert (hmvo (testclass.m2, testclass.s2), testclass.s2);
%! assert (hmvo (testclass.m2, testclass.s2o), testclass.s2);
%! assert (hmvo (testclass.m2, testclass.p), testclass.p);
%! assert (hmvo (testclass.m2, testclass.p, testclass.s), testclass.s);
%! assert (hmvo (testclass.m2, testclass.p, testclass.s2), testclass.s2);
%! assert (hmvo (testclass.s), testclass.s);
%! assert (hmvo (testclass.so), testclass.s);
%! assert (hmvo (testclass.s2), testclass.s2);
%! assert (hmvo (testclass.s2o), testclass.s2);
%! assert (hmvo (testclass.l), testclass.l);
%! assert (hmvo (testclass.l, testclass.s), testclass.s);
%! assert (hmvo (testclass.l, testclass.s2), testclass.s2);
%! assert (hmvo (testclass.l, testclass.p), testclass.p);
%! assert (hmvo (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hmvo (testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hmvo (testclass.p), testclass.p);
%! assert (hmvo (testclass.p, testclass.s), testclass.s);
%! assert (hmvo (testclass.p, testclass.s2), testclass.s2);

# hm2, hm2->m, hm2->m->s, hm2->m->so, hm2->m->s2, hm2->m->s2o,
# hm2->m->l, hm2->m->l->s, hm2->m->l->s2,
# hm2->m->l->p, hm2->m->l->p->s, hm2->m->l->p->s2
# hm2->m->p, hm2->m->p->s, hm2->m->p->s2
# hm2->s, hm2->so, hm2->s2, hm2->s2o,
# hm2->p, hm2->p->s, hm2->p->s2
%!test
%! obj = testclass ();
%! hm2 = obj.call (testclass.hm2);
%! assert (hm2 (obj), testclass.m2);
%! assert (hm2 (obj, testclass.m), testclass.m);
%! assert (hm2 (obj, testclass.m, testclass.s), testclass.s);
%! assert (hm2 (obj, testclass.m, testclass.so), testclass.s);
%! assert (hm2 (obj, testclass.m, testclass.s2), testclass.s2);
%! assert (hm2 (obj, testclass.m, testclass.s2o), testclass.s2);
%! assert (hm2 (obj, testclass.m, testclass.l), testclass.l);
%! assert (hm2 (obj, testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (hm2 (obj, testclass.m, testclass.l, testclass.s2), testclass.s2);
%! assert (hm2 (obj, testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (hm2 (obj, testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hm2 (obj, testclass.m, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hm2 (obj, testclass.m, testclass.p), testclass.p);
%! assert (hm2 (obj, testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (hm2 (obj, testclass.m, testclass.p, testclass.s2), testclass.s2);
%! assert (hm2 (obj, testclass.s), testclass.s);
%! assert (hm2 (obj, testclass.so), testclass.s);
%! assert (hm2 (obj, testclass.s2), testclass.s2);
%! assert (hm2 (obj, testclass.s2o), testclass.s2);
%! assert (hm2 (obj, testclass.p), testclass.p);
%! assert (hm2 (obj, testclass.p, testclass.s), testclass.s);
%! assert (hm2 (obj, testclass.p, testclass.s2), testclass.s2);

# hm2o, hm2o->m, hm2o->m->s, hm2o->m->so, hm2o->m->s2, hm2o->m->s2o,
# hm2o->m->l, hm2o->m->l->s, hm2o->m->l->s2,
# hm2o->m->l->p, hm2o->m->l->p->s, hm2o->m->l->p->s2
# hm2o->m->p, hm2o->m->p->s, hm2o->m->p->s2
# hm2o->s, hm2o->so, hm2o->s2, hm2o->s2o,
# hm2o->p, hm2o->p->s, hm2o->p->s2
%!test
%! obj = testclass ();
%! hm2o = obj.call (testclass.hm2o);
%! assert (hm2o (), testclass.m2);
%! assert (hm2o (testclass.m), testclass.m);
%! assert (hm2o (testclass.m, testclass.s), testclass.s);
%! assert (hm2o (testclass.m, testclass.so), testclass.s);
%! assert (hm2o (testclass.m, testclass.s2), testclass.s2);
%! assert (hm2o (testclass.m, testclass.s2o), testclass.s2);
%! assert (hm2o (testclass.m, testclass.l), testclass.l);
%! assert (hm2o (testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (hm2o (testclass.m, testclass.l, testclass.s2), testclass.s2);
%! assert (hm2o (testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (hm2o (testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hm2o (testclass.m, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hm2o (testclass.m, testclass.p), testclass.p);
%! assert (hm2o (testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (hm2o (testclass.m, testclass.p, testclass.s2), testclass.s2);
%! assert (hm2o (testclass.s), testclass.s);
%! assert (hm2o (testclass.so), testclass.s);
%! assert (hm2o (testclass.s2), testclass.s2);
%! assert (hm2o (testclass.s2o), testclass.s2);
%! assert (hm2o (testclass.p), testclass.p);
%! assert (hm2o (testclass.p, testclass.s), testclass.s);
%! assert (hm2o (testclass.p, testclass.s2), testclass.s2);

# hm2vo, hm2vo->m, hm2vo->m->s, hm2vo->m->so, hm2vo->m->s2, hm2vo->m->s2o,
# hm2vo->m->l, hm2vo->m->l->s, hm2vo->m->l->s2,
# hm2vo->m->l->p, hm2vo->m->l->p->s, hm2vo->m->l->p->s2
# hm2vo->m->p, hm2vo->m->p->s, hm2vo->m->p->s2
# hm2vo->s, hm2vo->so, hm2vo->s2, hm2vo->s2o,
# hm2vo->p, hm2vo->p->s, hm2vo->p->s2
%!test
%! obj = testclass ();
%! hm2vo = obj.call (testclass.hm2vo);
%! assert (hm2vo (), testclass.m2);
%! assert (hm2vo (testclass.m), testclass.m);
%! assert (hm2vo (testclass.m, testclass.s), testclass.s);
%! assert (hm2vo (testclass.m, testclass.so), testclass.s);
%! assert (hm2vo (testclass.m, testclass.s2), testclass.s2);
%! assert (hm2vo (testclass.m, testclass.s2o), testclass.s2);
%! assert (hm2vo (testclass.m, testclass.l), testclass.l);
%! assert (hm2vo (testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (hm2vo (testclass.m, testclass.l, testclass.s2), testclass.s2);
%! assert (hm2vo (testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (hm2vo (testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hm2vo (testclass.m, testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hm2vo (testclass.m, testclass.p), testclass.p);
%! assert (hm2vo (testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (hm2vo (testclass.m, testclass.p, testclass.s2), testclass.s2);
%! assert (hm2vo (testclass.s), testclass.s);
%! assert (hm2vo (testclass.so), testclass.s);
%! assert (hm2vo (testclass.s2), testclass.s2);
%! assert (hm2vo (testclass.s2o), testclass.s2);
%! assert (hm2vo (testclass.p), testclass.p);
%! assert (hm2vo (testclass.p, testclass.s), testclass.s);
%! assert (hm2vo (testclass.p, testclass.s2), testclass.s2);

# hs, hs->s2,
# hs->l, hs->l->s, hs->l->s2, hs->l->p, hs->l->p->s, hs->l->p->s2,
# hs->p, hs->p->s, hs->p->s2
%!test
%! obj = testclass ();
%! hs = obj.call (testclass.hs);
%! assert (hs (), testclass.s);
%! assert (hs (testclass.s2), testclass.s2);
%! assert (hs (testclass.l), testclass.l);
%! assert (hs (testclass.l, testclass.s), testclass.s);
%! assert (hs (testclass.l, testclass.s2), testclass.s2);
%! assert (hs (testclass.l, testclass.p), testclass.p);
%! assert (hs (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hs (testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hs (testclass.p), testclass.p);
%! assert (hs (testclass.p, testclass.s), testclass.s);
%! assert (hs (testclass.p, testclass.s2), testclass.s2);

# hsv, hsv->s2,
# hsv->l, hsv->l->s, hsv->l->s2, hsv->l->p, hsv->l->p->s, hsv->l->p->s2,
# hsv->p, hsv->p->s, hsv->p->s2
%!test
%! obj = testclass ();
%! hsv = obj.call (testclass.hsv);
%! assert (hsv (), testclass.s);
%! assert (hsv (testclass.s2), testclass.s2);
%! assert (hsv (testclass.l), testclass.l);
%! assert (hsv (testclass.l, testclass.s), testclass.s);
%! assert (hsv (testclass.l, testclass.s2), testclass.s2);
%! assert (hsv (testclass.l, testclass.p), testclass.p);
%! assert (hsv (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hsv (testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hsv (testclass.p), testclass.p);
%! assert (hsv (testclass.p, testclass.s), testclass.s);
%! assert (hsv (testclass.p, testclass.s2), testclass.s2);

# hso, hso->s2,
# hso->l, hso->l->s, hso->l->s2, hso->l->p, hso->l->p->s, hso->l->p->s2,
# hso->p, hso->p->s, hso->p->s2
%!test
%! obj = testclass ();
%! hso = obj.call (testclass.hso);
%! assert (hso (), testclass.s);
%! assert (hso (testclass.s2), testclass.s2);
%! assert (hso (testclass.l), testclass.l);
%! assert (hso (testclass.l, testclass.s), testclass.s);
%! assert (hso (testclass.l, testclass.s2), testclass.s2);
%! assert (hso (testclass.l, testclass.p), testclass.p);
%! assert (hso (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hso (testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hso (testclass.p), testclass.p);
%! assert (hso (testclass.p, testclass.s), testclass.s);
%! assert (hso (testclass.p, testclass.s2), testclass.s2);

# hsvo, hsvo->s2,
# hsvo->l, hsvo->l->s, hsvo->l->s2, hsvo->l->p, hsvo->l->p->s, hsvo->l->p->s2,
# hsvo->p, hsvo->p->s, hsvo->p->s2
%!test
%! obj = testclass ();
%! hsvo = obj.call (testclass.hsvo);
%! assert (hsvo (), testclass.s);
%! assert (hsvo (testclass.s2), testclass.s2);
%! assert (hsvo (testclass.l), testclass.l);
%! assert (hsvo (testclass.l, testclass.s), testclass.s);
%! assert (hsvo (testclass.l, testclass.s2), testclass.s2);
%! assert (hsvo (testclass.l, testclass.p), testclass.p);
%! assert (hsvo (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hsvo (testclass.l, testclass.p, testclass.s2), testclass.s2);
%! assert (hsvo (testclass.p), testclass.p);
%! assert (hsvo (testclass.p, testclass.s), testclass.s);
%! assert (hsvo (testclass.p, testclass.s2), testclass.s2);

# hs2, hs2->s, hs2->p, hs2->p->s, hs2->p->s2
%!test
%! obj = testclass ();
%! hs2 = obj.call (testclass.hs2);
%! assert (hs2 (), testclass.s2);
%! assert (hs2 (testclass.s), testclass.s);
%! assert (hs2 (testclass.p), testclass.p);
%! assert (hs2 (testclass.p, testclass.s), testclass.s);
%! assert (hs2 (testclass.p, testclass.s2), testclass.s2);

# hs2v, hs2v->s, hs2v->p, hs2v->p->s, hs2v->p->s2
%!test
%! obj = testclass ();
%! hs2v = obj.call (testclass.hs2v);
%! assert (hs2v (), testclass.s2);
%! assert (hs2v (testclass.s), testclass.s);
%! assert (hs2v (testclass.p), testclass.p);
%! assert (hs2v (testclass.p, testclass.s), testclass.s);
%! assert (hs2v (testclass.p, testclass.s2), testclass.s2);

# hs2o, hs2o->s, hs2o->p, hs2o->p->s, hs2o->p->s2
%!test
%! obj = testclass ();
%! hs2o = obj.call (testclass.hs2o);
%! assert (hs2o (), testclass.s2);
%! assert (hs2o (testclass.s), testclass.s);
%! assert (hs2o (testclass.p), testclass.p);
%! assert (hs2o (testclass.p, testclass.s), testclass.s);
%! assert (hs2o (testclass.p, testclass.s2), testclass.s2);

# hs2vo, hs2vo->s, hs2vo->p, hs2vo->p->s, hs2vo->p->s2
%!test
%! obj = testclass ();
%! hs2vo = obj.call (testclass.hs2vo);
%! assert (hs2vo (), testclass.s2);
%! assert (hs2vo (testclass.s), testclass.s);
%! assert (hs2vo (testclass.p), testclass.p);
%! assert (hs2vo (testclass.p, testclass.s), testclass.s);
%! assert (hs2vo (testclass.p, testclass.s2), testclass.s2);

# hl, hl->s, hl->s2, hl->p, hl->p->s, hl->p->s2
%!test
%! obj = testclass ();
%! hl = obj.call (testclass.hl);
%! assert (hl (), testclass.l);
%! assert (hl (testclass.s), testclass.s);
%! assert (hl (testclass.s2), testclass.s2);
%! assert (hl (testclass.p), testclass.p);
%! assert (hl (testclass.p, testclass.s), testclass.s);
%! assert (hl (testclass.p, testclass.s2), testclass.s2);

# hp, hp->s, hp->s->l, hp->s2
# [assert throws]: hp->l
%!test
%! obj = testclass ();
%! hp = obj.call (testclass.hp);
%! assert (hp (), testclass.p);
%! assert (hp (testclass.s), testclass.s);
%! assert (hp (testclass.s2), testclass.s2);
%! err = false; try hp (testclass.l); catch, err = true; end_try_catch, assert (err);
%! assert (hp (testclass.s, testclass.l), testclass.l);
