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
# n,hn                 ==> [m, s, so, l, p; hm, hmo, hmvo, hs, hsv, hso, hsvo, hl, hp]
# m,hm,hmo,hmvo        ==> [s, so, l, p; hs, hsv, hso, hsvo, hl, hp]
# s,so,hs,hsv,hso,hsvo ==> [l, p; hl, hp]
# l,hl                 ==> [s, p; hs, hsv, hp]
# p,hp                 ==> [s; hs, hsv]
#
# note: to avoid repetition, we skip cycles e.g: ...->{s,so}->*

# direct calls

# m, s, so
# [assert throws]: l, p
%!test
%! obj = testpkg.testclass ();
%! assert (obj.method_function (), testpkg.testclass.m);
%! assert (testpkg.testclass.static_function (), testpkg.testclass.s);
%! assert (obj.static_function (), testpkg.testclass.s);
%! err = false; try local_function (); catch, err = true; end_try_catch, assert (err);
%! err = false; try private_function (); catch, err = true; end_try_catch, assert (err);

# dispatch calls

# n, n->m, n->m->s, n->m->so,
# n->m->l, n->m->l->s, n->m->l->p, n->m->l->p->s,
# n->m->p, n->m->p->s,
# n->s, n->so,
# n->l, n->l->s, n->l->p, n->l->p->s,
# n->p, n->p->s
%!xtest
%! obj = testpkg.testclass ();
%! assert (obj.call (testpkg.testclass.n), testpkg.testclass.n);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m), testpkg.testclass.m);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.so), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.l), testpkg.testclass.l);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.m, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.so), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.l), testpkg.testclass.l);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.n, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# m,
# m->s, m->so,
# m->l, m->l->s, m->l->p, m->l->p->s,
# m->p, m->p->s,
%!xtest
%! obj = testpkg.testclass ();
%! assert (obj.call (testpkg.testclass.m), testpkg.testclass.m);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.so), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.l), testpkg.testclass.l);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.m, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# s,
# s->l, s->l->s, s->l->p, s->l->p->s,
# s->p, s->p->s
%!xtest
%! obj = testpkg.testclass ();
%! assert (obj.call (testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.s, testpkg.testclass.l), testpkg.testclass.l);
%! assert (obj.call (testpkg.testclass.s, testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.s, testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.s, testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.s, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.s, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# so,
# so->l, so->l->s, so->l->p, so->l->p->s,
# so->p, so->p->s
%!xtest
%! obj = testpkg.testclass ();
%! assert (obj.call (testpkg.testclass.so), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.so, testpkg.testclass.l), testpkg.testclass.l);
%! assert (obj.call (testpkg.testclass.so, testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.so, testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.so, testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.so, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.so, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# l, l->s, l->p, l->p->s
%!xtest
%! obj = testpkg.testclass ();
%! assert (obj.call (testpkg.testclass.l), testpkg.testclass.l);
%! assert (obj.call (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (obj.call (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# p, p->s, p->s->l
# [assert throws]: p->l
%!xtest
%! obj = testpkg.testclass ();
%! assert (obj.call (testpkg.testclass.p), testpkg.testclass.p);
%! assert (obj.call (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! err = false; try obj.call (testpkg.testclass.p, testpkg.testclass.l); catch, err = true; end_try_catch, assert (err);
%! assert (obj.call (testpkg.testclass.p, testpkg.testclass.s, testpkg.testclass.l), testpkg.testclass.l);

# valid calls graph:
# ------------------
# n,hn                 ==> [m, s, so, l, p; hm, hmo, hmvo, hs, hsv, hso, hsvo, hl, hp]
# m,hm,hmo,hmvo        ==> [s, so, l, p; hs, hsv, hso, hsvo, hl, hp]
# s,so,hs,hsv,hso,hsvo ==> [l, p; hl, hp]
# l,hl                 ==> [s, p; hs, hsv, hp]
# p,hp                 ==> [s; hs, hsv]
#
# note: to avoid repetition, we skip cycles e.g: ...->{s,so}->*

#% dispatch calls using function handles

# hn, hn->m, hn->m->s, hn->m->so,
# hn->m->l, hn->m->l->s, hn->m->l->p, hn->m->l->p->s,
# hn->m->p, hn->m->p->s,
# hn->s, hn->so,
# hn->l, hn->l->s, hn->l->p, hn->l->p->s,
# hn->p, hn->p->s
%!test
%! obj = testpkg.testclass ();
%! hn = obj.call (testpkg.testclass.hn);
%! assert (hn (), testpkg.testclass.n);
%! assert (hn (testpkg.testclass.m), testpkg.testclass.m);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.so), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.l), testpkg.testclass.l);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hn (testpkg.testclass.m, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.s), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.so), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.l), testpkg.testclass.l);
%! assert (hn (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hn (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hn (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hn (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hm,
# hm->s, hm->so,
# hm->l, hm->l->s,hm->l->p, hm->l->p->s,
# hm->p, hm->p->s,
%!test
%! obj = testpkg.testclass ();
%! hm = obj.call (testpkg.testclass.hm);
%! assert (hm (obj), testpkg.testclass.m);
%! assert (hm (obj, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hm (obj, testpkg.testclass.so), testpkg.testclass.s);
%! assert (hm (obj, testpkg.testclass.l), testpkg.testclass.l);
%! assert (hm (obj, testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hm (obj, testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hm (obj, testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hm (obj, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hm (obj, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hmo,
# hmo->s, hmo->so,
# hmo->l, hmo->l->s, hmo->l->p, hmo->l->p->s,
# hmo->p, hmo->p->s
%!test
%! obj = testpkg.testclass ();
%! hmo = obj.call (testpkg.testclass.hmo);
%! assert (hmo (), testpkg.testclass.m);
%! assert (hmo (testpkg.testclass.s), testpkg.testclass.s);
%! assert (hmo (testpkg.testclass.so), testpkg.testclass.s);
%! assert (hmo (testpkg.testclass.l), testpkg.testclass.l);
%! assert (hmo (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hmo (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hmo (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hmo (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hmo (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hmvo,
# hmvo->s, hmvo->so,
# hmvo->l, hmvo->l->s, hmvo->l->p, hmvo->l->p->s,
# hmvo->p, hmvo->p->s
%!test
%! obj = testpkg.testclass ();
%! hmvo = obj.call (testpkg.testclass.hmvo);
%! assert (hmvo (), testpkg.testclass.m);
%! assert (hmvo (testpkg.testclass.s), testpkg.testclass.s);
%! assert (hmvo (testpkg.testclass.so), testpkg.testclass.s);
%! assert (hmvo (testpkg.testclass.l), testpkg.testclass.l);
%! assert (hmvo (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hmvo (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hmvo (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hmvo (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hmvo (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hs,
# hs->l, hs->l->s, hs->l->p, hs->l->p->s,
# hs->p, hs->p->s
%!test
%! obj = testpkg.testclass ();
%! hs = obj.call (testpkg.testclass.hs);
%! assert (hs (), testpkg.testclass.s);
%! assert (hs (testpkg.testclass.l), testpkg.testclass.l);
%! assert (hs (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hs (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hs (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hs (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hs (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hsv,
# hsv->l, hsv->l->s,, hsv->l->p, hsv->l->p->s,
# hsv->p, hsv->p->s,
%!test
%! obj = testpkg.testclass ();
%! hsv = obj.call (testpkg.testclass.hsv);
%! assert (hsv (), testpkg.testclass.s);
%! assert (hsv (testpkg.testclass.l), testpkg.testclass.l);
%! assert (hsv (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hsv (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hsv (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hsv (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hsv (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hso,
# hso->l, hso->l->s, hso->l->p, hso->l->p->s,
# hso->p, hso->p->s,
%!test
%! obj = testpkg.testclass ();
%! hso = obj.call (testpkg.testclass.hso);
%! assert (hso (), testpkg.testclass.s);
%! assert (hso (testpkg.testclass.l), testpkg.testclass.l);
%! assert (hso (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hso (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hso (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hso (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hso (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hsvo,
# hsvo->l, hsvo->l->s,, hsvo->l->p, hsvo->l->p->s,
# hsvo->p, hsvo->p->s,
%!test
%! obj = testpkg.testclass ();
%! hsvo = obj.call (testpkg.testclass.hsvo);
%! assert (hsvo (), testpkg.testclass.s);
%! assert (hsvo (testpkg.testclass.l), testpkg.testclass.l);
%! assert (hsvo (testpkg.testclass.l, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hsvo (testpkg.testclass.l, testpkg.testclass.p), testpkg.testclass.p);
%! assert (hsvo (testpkg.testclass.l, testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);
%! assert (hsvo (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hsvo (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hl, hl->s, hl->p, hl->p->s
%!test
%! obj = testpkg.testclass ();
%! hl = obj.call (testpkg.testclass.hl);
%! assert (hl (), testpkg.testclass.l);
%! assert (hl (testpkg.testclass.s), testpkg.testclass.s);
%! assert (hl (testpkg.testclass.p), testpkg.testclass.p);
%! assert (hl (testpkg.testclass.p, testpkg.testclass.s), testpkg.testclass.s);

# hp, hp->s, hp->s->l
# [assert throws]: hp->l
%!test
%! obj = testpkg.testclass ();
%! hp = obj.call (testpkg.testclass.hp);
%! assert (hp (), testpkg.testclass.p);
%! assert (hp (testpkg.testclass.s), testpkg.testclass.s);
%! err = false; try hp (testpkg.testclass.l); catch, err = true; end_try_catch, assert (err);
%! assert (hp (testpkg.testclass.s, testpkg.testclass.l), testpkg.testclass.l);
