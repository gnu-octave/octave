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
%! obj = testpkg.testsubpkg.testclass ();
%! assert (obj.method_function (), testpkg.testsubpkg.testclass.m);
%! assert (testpkg.testsubpkg.testclass.static_function (), testpkg.testsubpkg.testclass.s);
%! assert (obj.static_function (), testpkg.testsubpkg.testclass.s);
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
%! obj = testpkg.testsubpkg.testclass ();
%! assert (obj.call (testpkg.testsubpkg.testclass.n), testpkg.testsubpkg.testclass.n);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m), testpkg.testsubpkg.testclass.m);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.n, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# m,
# m->s, m->so,
# m->l, m->l->s, m->l->p, m->l->p->s,
# m->p, m->p->s,
%!xtest
%! obj = testpkg.testsubpkg.testclass ();
%! assert (obj.call (testpkg.testsubpkg.testclass.m), testpkg.testsubpkg.testclass.m);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# s,
# s->l, s->l->s, s->l->p, s->l->p->s,
# s->p, s->p->s
%!xtest
%! obj = testpkg.testsubpkg.testclass ();
%! assert (obj.call (testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (obj.call (testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# so,
# so->l, so->l->s, so->l->p, so->l->p->s,
# so->p, so->p->s
%!xtest
%! obj = testpkg.testsubpkg.testclass ();
%! assert (obj.call (testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.so, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (obj.call (testpkg.testsubpkg.testclass.so, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.so, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.so, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.so, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.so, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# l, l->s, l->p, l->p->s
%!xtest
%! obj = testpkg.testsubpkg.testclass ();
%! assert (obj.call (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (obj.call (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (obj.call (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# p, p->s, p->s->l
# [assert throws]: p->l
%!xtest
%! obj = testpkg.testsubpkg.testclass ();
%! assert (obj.call (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (obj.call (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! err = false; try obj.call (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.l); catch, err = true; end_try_catch, assert (err);
%! assert (obj.call (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);

# valid calls graph:
# ------------------
# n,hn                 ==> [m, s, so, l, p; hm, hmo, hmvo, hs, hsv, hso, hsvo, hl, hp]
# m,hm,hmo,hmvo        ==> [s, so, l, p; hs, hsv, hso, hsvo, hl, hp]
# s,so,hs,hsv,hso,hsvo ==> [l, p; hl, hp]
# l,hl                 ==> [s, p; hs, hsv, hp]
# p,hp                 ==> [s; hs, hsv]
#
# note: to avoid repetition, we skip cycles e.g: ...->{s,so}->*

# dispatch calls using function handles

# hn, hn->m, hn->m->s, hn->m->so,
# hn->m->l, hn->m->l->s, hn->m->l->p, hn->m->l->p->s,
# hn->m->p, hn->m->p->s,
# hn->s, hn->so,
# hn->l, hn->l->s, hn->l->p, hn->l->p->s,
# hn->p, hn->p->s
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hn = obj.call (testpkg.testsubpkg.testclass.hn);
%! assert (hn (), testpkg.testsubpkg.testclass.n);
%! assert (hn (testpkg.testsubpkg.testclass.m), testpkg.testsubpkg.testclass.m);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hn (testpkg.testsubpkg.testclass.m, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hn (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hn (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hn (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hn (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hm,
# hm->s, hm->so,
# hm->l, hm->l->s,hm->l->p, hm->l->p->s,
# hm->p, hm->p->s,
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hm = obj.call (testpkg.testsubpkg.testclass.hm);
%! assert (hm (obj), testpkg.testsubpkg.testclass.m);
%! assert (hm (obj, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hm (obj, testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (hm (obj, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hm (obj, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hm (obj, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hm (obj, testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hm (obj, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hm (obj, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hmo,
# hmo->s, hmo->so,
# hmo->l, hmo->l->s, hmo->l->p, hmo->l->p->s,
# hmo->p, hmo->p->s
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hmo = obj.call (testpkg.testsubpkg.testclass.hmo);
%! assert (hmo (), testpkg.testsubpkg.testclass.m);
%! assert (hmo (testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hmo (testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (hmo (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hmo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hmo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hmo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hmo (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hmo (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hmvo,
# hmvo->s, hmvo->so,
# hmvo->l, hmvo->l->s, hmvo->l->p, hmvo->l->p->s,
# hmvo->p, hmvo->p->s
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hmvo = obj.call (testpkg.testsubpkg.testclass.hmvo);
%! assert (hmvo (), testpkg.testsubpkg.testclass.m);
%! assert (hmvo (testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hmvo (testpkg.testsubpkg.testclass.so), testpkg.testsubpkg.testclass.s);
%! assert (hmvo (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hmvo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hmvo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hmvo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hmvo (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hmvo (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hs,
# hs->l, hs->l->s, hs->l->p, hs->l->p->s,
# hs->p, hs->p->s
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hs = obj.call (testpkg.testsubpkg.testclass.hs);
%! assert (hs (), testpkg.testsubpkg.testclass.s);
%! assert (hs (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hs (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hs (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hs (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hs (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hs (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hsv,
# hsv->l, hsv->l->s,, hsv->l->p, hsv->l->p->s,
# hsv->p, hsv->p->s,
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hsv = obj.call (testpkg.testsubpkg.testclass.hsv);
%! assert (hsv (), testpkg.testsubpkg.testclass.s);
%! assert (hsv (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hsv (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hsv (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hsv (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hsv (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hsv (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hso,
# hso->l, hso->l->s, hso->l->p, hso->l->p->s,
# hso->p, hso->p->s,
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hso = obj.call (testpkg.testsubpkg.testclass.hso);
%! assert (hso (), testpkg.testsubpkg.testclass.s);
%! assert (hso (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hso (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hso (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hso (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hso (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hso (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hsvo,
# hsvo->l, hsvo->l->s,, hsvo->l->p, hsvo->l->p->s,
# hsvo->p, hsvo->p->s,
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hsvo = obj.call (testpkg.testsubpkg.testclass.hsvo);
%! assert (hsvo (), testpkg.testsubpkg.testclass.s);
%! assert (hsvo (testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
%! assert (hsvo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hsvo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hsvo (testpkg.testsubpkg.testclass.l, testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hsvo (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hsvo (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hl, hl->s, hl->p, hl->p->s
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hl = obj.call (testpkg.testsubpkg.testclass.hl);
%! assert (hl (), testpkg.testsubpkg.testclass.l);
%! assert (hl (testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! assert (hl (testpkg.testsubpkg.testclass.p), testpkg.testsubpkg.testclass.p);
%! assert (hl (testpkg.testsubpkg.testclass.p, testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);

# hp, hp->s, hp->s->l
# [assert throws]: hp->l
%!test
%! obj = testpkg.testsubpkg.testclass ();
%! hp = obj.call (testpkg.testsubpkg.testclass.hp);
%! assert (hp (), testpkg.testsubpkg.testclass.p);
%! assert (hp (testpkg.testsubpkg.testclass.s), testpkg.testsubpkg.testclass.s);
%! err = false; try hp (testpkg.testsubpkg.testclass.l); catch, err = true; end_try_catch, assert (err);
%! assert (hp (testpkg.testsubpkg.testclass.s, testpkg.testsubpkg.testclass.l), testpkg.testsubpkg.testclass.l);
