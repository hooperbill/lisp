/* lisp definitions - modernized Stage 0 */

#ifndef LISP_H
#define LISP_H

#include <stdio.h>

#define MAXIN	256	/* maximum characters during input */
#define MAXCHR	128	/* maximum characters for a property name */
#define GMIN	100	/* minimum cells left before collection happens */

#define MARK	0x80
#define UNMARK	0x7F

#define LIST	0x00
#define ATOM	0x10

#define APVAL	0x11	/* atom property */
#define NPVAL	0x12	/* numeric property */

#define EXPR	0x14 	/* used as an internal marker, use lambda (x) */
#define FEXPR	0x15	/* not used, use nlambda (x) */
#define LEXPR	0x16	/* not used, use lambda x */

#define SUBR	0x17
#define FSUBR	0x18
#define LSUBR	0x19

#define LAMBDA	0x1A
#define NLAMBDA	0x1B
#define MACRO	0x1C	/* the user uses macro (x) */

#define LPAREN	'('
#define RPAREN	')'
#define BRKT	']'
#define END		'\0'
#define DOT		'.'
#define QUOTE	'\''
#define DQUOTE	'"'
#define COMMENT	';'
#define NULLCHAR 0x00

#define PROG	'p'
#define RETURN	'r'
#define GOTO	'g'

#define STRING  0x01       /* indicates whether the property name */
			   /* should be enclosed in quotes */

struct pnrec {     /* property name record */
    struct pnrec *fptr, *bptr;
    int flg;
    char name[2];
};

struct lnode		/* lisp node */
{   int nodeid;
    union
    {   struct		/* cons node */
        {   struct lnode *car;
            struct lnode *cdr;
        } cn;
        struct		/* atom node */
        {   int flg;
            struct pnrec *name;
        } an;
        struct		/* numeric atom node */
        {   int dummy;
            int ival;
        } nn;
        struct		/* built-in function atom node */
        {   struct lnode *(*func)();
            int args;
        } fn;
    } u;
};

/* Core */
struct lnode *fnode(void);
struct lnode *garbage(struct lnode *c, struct lnode *xlist);
struct lnode *readcon(struct lnode *ptr);
struct lnode *readline(struct lnode *ptr);
struct lnode *create(void);
struct lnode *apnode(char *astrng);
struct lnode *npnode(int ival);
struct lnode *init(void);

/* Print / I/O */
struct lnode *prin1(struct lnode *nptr);
struct lnode *princ(struct lnode *ptr);
struct lnode *print(struct lnode *ptr);
struct lnode *terpri(struct lnode *ptr);
struct lnode *iprint(struct lnode *ptr);
struct lnode *save(struct lnode *ptr, struct lnode *fptr);
struct lnode *load(struct lnode *ptr);
struct lnode *open(struct lnode *sptr, struct lnode *mptr);
struct lnode *close(struct lnode *ptr);

/* List / structure */
struct lnode *car(struct lnode *ptr);
struct lnode *cdr(struct lnode *ptr);
struct lnode *cadr(struct lnode *ptr);
struct lnode *caddr(struct lnode *ptr);
struct lnode *null(struct lnode *ptr);
struct lnode *atom(struct lnode *ptr);
struct lnode *listp(struct lnode *ptr);
struct lnode *stringp(struct lnode *ptr);
struct lnode *list(struct lnode *ptr);
struct lnode *cons(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *xcons(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *ncons(struct lnode *ptr1);
struct lnode *conz(struct lnode *x, struct lnode *y, struct lnode *z);
struct lnode *rplaca(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *rplacd(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *equal(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *eqq(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *reverse(struct lnode *ptr);
struct lnode *nreverse(struct lnode *ptr);
struct lnode *append(struct lnode *p1, struct lnode *p2);
struct lnode *nconc(struct lnode *p1, struct lnode *p2);
struct lnode *last(struct lnode *ptr);
struct lnode *lastcar(struct lnode *ptr);
struct lnode *delete(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *subst(struct lnode *new, struct lnode *old, struct lnode *form);
struct lnode *member(struct lnode *ptr, struct lnode *lptr);
struct lnode *length(struct lnode *ptr);
struct lnode *nthcdr(struct lnode *nval, struct lnode *ptr);
struct lnode *nth(struct lnode *nval, struct lnode *ptr);

/* Control */
struct lnode *prog(struct lnode *ptr, struct lnode *xlist);
struct lnode *iprog(struct lnode *ptr, struct lnode *xlist);
struct lnode *preturn(struct lnode *ptr);
struct lnode *pgoto(struct lnode *ptr, struct lnode *xlist);
struct lnode *prog1(struct lnode *ptr);
struct lnode *progn(struct lnode *ptr);
struct lnode *let(struct lnode *ptr, struct lnode *xlist);
struct lnode *do_(struct lnode *ptr, struct lnode *xlist);
struct lnode *evcon(struct lnode *ptr, struct lnode *xlist);
struct lnode *quote(struct lnode *ptr, struct lnode *xlist);
struct lnode *and(struct lnode *ptr, struct lnode *xlist);
struct lnode *or(struct lnode *ptr, struct lnode *xlist);
struct lnode *not(struct lnode *ptr1);

/* Association / env */
struct lnode *assoc(struct lnode *kptr, struct lnode *xlist);
struct lnode *iassoc(struct lnode *p, struct lnode *l);
struct lnode *set(struct lnode *ptr, struct lnode *xlist);
struct lnode *setq(struct lnode *ptr, struct lnode *xlist);
struct lnode *push(struct lnode *ptr, struct lnode *xlist);
struct lnode *pop(struct lnode *ptr, struct lnode *xlist);
struct lnode *define(struct lnode *ptr, struct lnode *xlist);
struct lnode *iset(struct lnode *ptr1, struct lnode *ptr2, struct lnode *xlist);
struct lnode *arg(struct lnode *nptr);
struct lnode *setarg(struct lnode *nptr, struct lnode *aptr);
struct lnode *listify(struct lnode *nptr);
struct lnode *sublist(int i, int j);
struct lnode *pairlis(struct lnode *name, struct lnode *value, struct lnode *xlist);
struct lnode *pairnam(struct lnode *name, struct lnode *value, struct lnode *xlist);
struct lnode *pairnil(struct lnode *name, struct lnode *xlist);

/* Properties */
struct lnode *putprop(struct lnode *psym, struct lnode *pval, struct lnode *pid);
struct lnode *setplist(struct lnode *psym, struct lnode *lst);
struct lnode *defprop(struct lnode *ptr, struct lnode *xlist);
struct lnode *get(struct lnode *psym, struct lnode *pid);
struct lnode *prplst(struct lnode *psym);
struct lnode *remprop(struct lnode *psym, struct lnode *pid);
struct lnode *getd(struct lnode *ptr);

/* Arithmetic */
struct lnode *numberp(struct lnode *ptr);
struct lnode *plus(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *add1(struct lnode *ptr);
struct lnode *diff(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *sub1(struct lnode *ptr);
struct lnode *minus(struct lnode *ptr);
struct lnode *times(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *divide(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *mod(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *lisp_abs(struct lnode *ptr);
struct lnode *lrandom(struct lnode *ptr);
struct lnode *greatp(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *lessp(struct lnode *ptr1, struct lnode *ptr2);
struct lnode *plusp(struct lnode *ptr);
struct lnode *zerop(struct lnode *ptr);
struct lnode *minusp(struct lnode *ptr);
struct lnode *oddp(struct lnode *ptr);

/* Map / eval / apply */
struct lnode *maplist(struct lnode *lst, struct lnode *xlist);
struct lnode *mapcar(struct lnode *lst, struct lnode *xlist);
struct lnode *eval(struct lnode *ptr, struct lnode *xlist);
struct lnode *ieval(struct lnode *ptr, struct lnode *xlist);
struct lnode *evalis(struct lnode *ptr, struct lnode *xlist);
struct lnode *apply(struct lnode *lst, struct lnode *xlist);
struct lnode *iapply(struct lnode *fn, struct lnode *arglst, struct lnode *xlist);
struct lnode *trace(void);
struct lnode *untrace(void);
struct lnode *syscall(struct lnode *ptr);
_Noreturn struct lnode *quit(void);

struct pnrec *findpn(char *pnstrng);
struct pnrec *insrtpn(char *pnstrng);
char *itoa(int n);
_Noreturn void error(int errnum, struct lnode *errptr);
FILE *lopen(char *name, char *mode);
void sig_handler(int sig);
void csubr(char *pname, struct lnode *(*subr)(), char stype, int nargs);

#endif /* LISP_H */
