/* lisp definitions */

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

char *valid_token = "()].\'\";\0\n ";

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

struct lnode *fnode(), *garbage(), *readcon(), *create(), *apnode(),
             *npnode(), *prin1(), *princ(), *print(), *terpri(), *iprint(),
             *save(), *load(), *car(),*cdr(), *cadr(), *caddr(), *null(),
             *atom(), *listp(), *list(), *prog(), *iprog(), *preturn(),
             *pgoto(), *prog1(), *progn(), *let(), *do_(),
             *assoc(), *iassoc(), *rplaca(), *rplacd(), *equal(), *eqq(),
             *reverse(), *nrevers(), *append(), *nconc(), *last(),
             *lastcar(), *delete(), *subst(), *cons(), *xcons(),
             *ncons(), *conz(), *set(), *setq(), *push(), *pop(),
             *define(), *iset(), *arg(), *setarg(), *listify(),
             *sublist(), *pairlis(), *pairnam(), *pairnil(), *member(),
             *putprop(), *setplis(), *defprop(), *get(), *prplst(),
             *remprop(), *and(), *or(), *not(), *numberp(),
             *plus(), *add1(), *diff(), *sub1(), *minus(), *times(),
             *divide(), *mod(), *lisp_abs(), *lrandom(), *greatp(), *lessp(),
             *plusp(), *zerop(), *minusp(), *oddp(), *maplist(),
             *mapcar(), *evcon(), *getd(), *length(), *nthcdr(), *nth(),
             *quote(), *trace(), *untrace(), *eval(), *ieval(),
             *evalis(), *apply(), *iapply(), *init(), *stringp(),
			 *open(), *close();

struct pnrec *findpn(), *insrtpn();
char *itoa();
void error();
FILE *lopen();
void sig_handler();
