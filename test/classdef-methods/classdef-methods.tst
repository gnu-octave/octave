%!shared
%! clear classes

# Dispatch calls

# Direct calls: m, s, so, p
%!test
%! obj = testclass ();
%! assert (obj.method_function (), testclass.m);
%! assert (testclass.static_function (), testclass.s);
%! assert (obj.static_function (), testclass.s);

# n, n->m, n->m->s, n->m->so,
# n->m->l, n->m->l->s, n->m->l->p, n->m->l->p->s,
# n->m->p, n->m->p->s,
# n->s, n->so,
# n->l, n->l->s, n->l->p, n->l->p->s,
# n->p, n->p->s
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.n), testclass.n);
%! assert (obj.call (testclass.n, testclass.m), testclass.m);
%! assert (obj.call (testclass.n, testclass.m, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.so), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.l), testclass.l);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.m, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.so), testclass.s);
%! assert (obj.call (testclass.n, testclass.l), testclass.l);
%! assert (obj.call (testclass.n, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.n, testclass.p), testclass.p);
%! assert (obj.call (testclass.n, testclass.p, testclass.s), testclass.s);

# m,
# m->s, m->so,
# m->l, m->l->s, m->l->p, m->l->p->s,
# m->p, m->p->s
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.m), testclass.m);
%! assert (obj.call (testclass.m, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.so), testclass.s);
%! assert (obj.call (testclass.m, testclass.l), testclass.l);
%! assert (obj.call (testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.m, testclass.p), testclass.p);
%! assert (obj.call (testclass.m, testclass.p, testclass.s), testclass.s);

# s,
# s->l, s->l->s, s->l->p, s->l->p->s,
# s->p, s->p->s
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.s), testclass.s);
%! assert (obj.call (testclass.s, testclass.l), testclass.l);
%! assert (obj.call (testclass.s, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.s, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.s, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.s, testclass.p), testclass.p);
%! assert (obj.call (testclass.s, testclass.p, testclass.s), testclass.s);

# so,
# so->l, so->l->s, so->l->p, so->l->p->s,
# so->p, so->p->s
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.so), testclass.s);
%! assert (obj.call (testclass.so, testclass.l), testclass.l);
%! assert (obj.call (testclass.so, testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.so, testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.so, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.so, testclass.p), testclass.p);
%! assert (obj.call (testclass.so, testclass.p, testclass.s), testclass.s);

# l, l->s, l->p, l->p->s
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.l), testclass.l);
%! assert (obj.call (testclass.l, testclass.s), testclass.s);
%! assert (obj.call (testclass.l, testclass.p), testclass.p);
%! assert (obj.call (testclass.l, testclass.p, testclass.s), testclass.s);

# p, p->s, p->s->l
%!test
%! obj = testclass ();
%! assert (obj.call (testclass.p), testclass.p);
%! assert (obj.call (testclass.p, testclass.s), testclass.s);
%! assert (obj.call (testclass.p, testclass.s, testclass.l), testclass.l);

# Dispatch calls using function handles

# hn, hn->m, hn->m->s, hn->m->so,
# hn->m->l, hn->m->l->s, hn->m->l->p, hn->m->l->p->s,
# hn->m->p, hn->m->p->s,
# hn->s, hn->so,
# hn->l, hn->l->s, hn->l->p, hn->l->p->s,
# hn->p, hn->p->s
%!test
%! obj = testclass ();
%! hn = obj.call (testclass.hn);
%! assert (hn (), testclass.n);
%! assert (hn (testclass.m), testclass.m);
%! assert (hn (testclass.m, testclass.s), testclass.s);
%! assert (hn (testclass.m, testclass.so), testclass.s);
%! assert (hn (testclass.m, testclass.l), testclass.l);
%! assert (hn (testclass.m, testclass.l, testclass.s), testclass.s);
%! assert (hn (testclass.m, testclass.l, testclass.p), testclass.p);
%! assert (hn (testclass.m, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.m, testclass.p), testclass.p);
%! assert (hn (testclass.m, testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.s), testclass.s);
%! assert (hn (testclass.so), testclass.s);
%! assert (hn (testclass.l), testclass.l);
%! assert (hn (testclass.l, testclass.s), testclass.s);
%! assert (hn (testclass.l, testclass.p), testclass.p);
%! assert (hn (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hn (testclass.p), testclass.p);
%! assert (hn (testclass.p, testclass.s), testclass.s);

# hm,
# hm->s, hm->so,
# hm->l, hm->l->s,hm->l->p, hm->l->p->s,
# hm->p, hm->p->s,
%!test
%! obj = testclass ();
%! hm = obj.call (testclass.hm);
%! assert (hm (obj), testclass.m);
%! assert (hm (obj, testclass.s), testclass.s);
%! assert (hm (obj, testclass.so), testclass.s);
%! assert (hm (obj, testclass.l), testclass.l);
%! assert (hm (obj, testclass.l, testclass.s), testclass.s);
%! assert (hm (obj, testclass.l, testclass.p), testclass.p);
%! assert (hm (obj, testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hm (obj, testclass.p), testclass.p);
%! assert (hm (obj, testclass.p, testclass.s), testclass.s);

# hmo,
# hmo->s, hmo->so,
# hmo->l, hmo->l->s, hmo->l->p, hmo->l->p->s,
# hmo->p, hmo->p->s
%!test
%! obj = testclass ();
%! hmo = obj.call (testclass.hmo);
%! assert (hmo (), testclass.m);
%! assert (hmo (testclass.s), testclass.s);
%! assert (hmo (testclass.so), testclass.s);
%! assert (hmo (testclass.l), testclass.l);
%! assert (hmo (testclass.l, testclass.s), testclass.s);
%! assert (hmo (testclass.l, testclass.p), testclass.p);
%! assert (hmo (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hmo (testclass.p), testclass.p);
%! assert (hmo (testclass.p, testclass.s), testclass.s);

# hmvo,
# hmvo->s, hmvo->so,
# hmvo->l, hmvo->l->s, hmvo->l->p, hmvo->l->p->s,
# hmvo->p, hmvo->p->s
%!test
%! obj = testclass ();
%! hmvo = obj.call (testclass.hmvo);
%! assert (hmvo (), testclass.m);
%! assert (hmvo (testclass.s), testclass.s);
%! assert (hmvo (testclass.so), testclass.s);
%! assert (hmvo (testclass.l), testclass.l);
%! assert (hmvo (testclass.l, testclass.s), testclass.s);
%! assert (hmvo (testclass.l, testclass.p), testclass.p);
%! assert (hmvo (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hmvo (testclass.p), testclass.p);
%! assert (hmvo (testclass.p, testclass.s), testclass.s);

# hs,
# hs->l, hs->l->s, hs->l->p, hs->l->p->s,
# hs->p, hs->p->s
%!test
%! obj = testclass ();
%! hs = obj.call (testclass.hs);
%! assert (hs (), testclass.s);
%! assert (hs (testclass.l), testclass.l);
%! assert (hs (testclass.l, testclass.s), testclass.s);
%! assert (hs (testclass.l, testclass.p), testclass.p);
%! assert (hs (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hs (testclass.p), testclass.p);
%! assert (hs (testclass.p, testclass.s), testclass.s);

# hsv,
# hsv->l, hsv->l->s,, hsv->l->p, hsv->l->p->s,
# hsv->p, hsv->p->s,
%!test
%! obj = testclass ();
%! hsv = obj.call (testclass.hsv);
%! assert (hsv (), testclass.s);
%! assert (hsv (testclass.l), testclass.l);
%! assert (hsv (testclass.l, testclass.s), testclass.s);
%! assert (hsv (testclass.l, testclass.p), testclass.p);
%! assert (hsv (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hsv (testclass.p), testclass.p);
%! assert (hsv (testclass.p, testclass.s), testclass.s);

# hso,
# hso->l, hso->l->s, hso->l->p, hso->l->p->s,
# hso->p, hso->p->s,
%!test
%! obj = testclass ();
%! hso = obj.call (testclass.hso);
%! assert (hso (), testclass.s);
%! assert (hso (testclass.l), testclass.l);
%! assert (hso (testclass.l, testclass.s), testclass.s);
%! assert (hso (testclass.l, testclass.p), testclass.p);
%! assert (hso (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hso (testclass.p), testclass.p);
%! assert (hso (testclass.p, testclass.s), testclass.s);

# hsvo,
# hsvo->l, hsvo->l->s,, hsvo->l->p, hsvo->l->p->s,
# hsvo->p, hsvo->p->s,
%!test
%! obj = testclass ();
%! hsvo = obj.call (testclass.hsvo);
%! assert (hsvo (), testclass.s);
%! assert (hsvo (testclass.l), testclass.l);
%! assert (hsvo (testclass.l, testclass.s), testclass.s);
%! assert (hsvo (testclass.l, testclass.p), testclass.p);
%! assert (hsvo (testclass.l, testclass.p, testclass.s), testclass.s);
%! assert (hsvo (testclass.p), testclass.p);
%! assert (hsvo (testclass.p, testclass.s), testclass.s);

# hl, hl->s, hl->p, hl->p->s
%!test
%! obj = testclass ();
%! hl = obj.call (testclass.hl);
%! assert (hl (), testclass.l);
%! assert (hl (testclass.s), testclass.s);
%! assert (hl (testclass.p), testclass.p);
%! assert (hl (testclass.p, testclass.s), testclass.s);

# hp, hp->s, hp->s->l
%!test
%! obj = testclass ();
%! hp = obj.call (testclass.hp);
%! assert (hp (), testclass.p);
%! assert (hp (testclass.s), testclass.s);
%! assert (hp (testclass.s, testclass.l), testclass.l);
# [throws]: hp->l
%!error <'local_function' undefined>
%! obj = testclass ();
%! hp = obj.call (testclass.hp);
%! hp (testclass.l);
