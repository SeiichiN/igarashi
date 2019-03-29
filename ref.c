/*  ref.c */
/* OCaml の 参照を C で表現する */

int p = 5;
int q = 2;

int *refp = &p;
int *refq = &q;

*refq = *refp;

printf("%d %d", p, q);

