/* -*-C-*-
*******************************************************************************
*
* File:         lisp.c
* RCS:          $Header: lisp.c,v 1.9 92/03/24 13:53:41 wgh Exp $
* Description:  A tiny lisp interpreter
* Author:       Bill Hooper, SIL
* Created:      June 1990
* Modified:
* Language:     C
* Package:      N/A
* Status:       Experimental (Do Not Distribute)
*
* (c) Copyright 1990, all rights reserved.
*
* A tiny lisp interpreter based on Maclisp from MIT. It was written as
* a tool for learning lisp and the inner workings of a lisp interpreter.
*
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include "lisp.h"

#define TRUE !NULL

FILE *lispin, *lispout;

char inputLine[MAXIN], sym, peeksym, token[MAXCHR], *inptr, progmode;
char *pnend, *pnstrt;
char *errmsg[] = 
{   
	"atom or left paren expected", 
		"atom, left paren, or right paren expected", 
		"lambda and macro are not functions", 
		"right paren expected", 
		"argument cannot be an atom", 
		"argument must be an atom", 
		"undefined function", 
		"unseen go", 
		"lambda or macro expected", 
		"out of cells", 
		"cannot open file", 
		"illegal atom", 
		"missing double quote", 
		"numeric property expected", 
		"cannot redefine builtin functions", 
		"unexpected eof", 
		"property name out of memory",
		"property name to large",
		"string property expected"
};

int peeked, itoken;
	jmp_buf warmadr;
unsigned tflg, pflg,     /* trace flag, print flag */
	gcnt, gflg;           /* collected cell count, collection flag */
struct lnode *toh, *boh; /* top of heap, bottom of heap */

/* for those who really think they have to know */
/* atom_list - top level atom list */
/* function_list - function list, all functions are stored here */
/* property_list - property list, for put, get and rem properties */
/* garbage_list - garbage list, car is used to link this list */
/* expression_list - expression list, used with LEXPR and LSUBR functions */
/* current_eval_list - current evaluation, pointer to current list being evaluated */
/* property_name_list - property name list, not really an atom list */

struct lnode *atom_list, *function_list, *property_list, *garbage_list, *expression_list, *current_eval_list,
*truenode, *nilnode, *eofnode;
struct pnrec *property_name_list;

void sig_handler(sig, num)
int sig, num;
{
    if (lispin != stdin) {
        fclose(lispin);
		lispin = stdin;
    }
	
    if (lispout != stdout) {
        fclose(lispout);
		lispout = stdout;
    }
	
    garbage(nilnode, atom_list);
	
    longjmp(warmadr, -1);
}

/* fetch a node from the free list */

struct lnode *fnode()
{ struct lnode *tmp;

if ((tmp = garbage_list) == NULL)
	error(9, nilnode);

garbage_list = garbage_list->u.cn.car;

if (--gcnt < GMIN)
	gflg = TRUE;

return(tmp);
}

/* many thanks to Knuth for the garbage collection algorithim */

void tagnode(p)
struct lnode *p;
{
    struct lnode *t, *q;
	
    t = NULL;
	
e2:
    p->nodeid |= MARK;
	
    if (p->nodeid & ATOM) goto e6;
	
    if (((q = p->u.cn.car) != NULL) && (q->nodeid & MARK) == 0) {
		p->nodeid |= ATOM;
        p->u.cn.car = t;
        t = p;
        p = q;
        goto e2;
    }
	
e5:
    if (((q = p->u.cn.cdr) != NULL) && ((q->nodeid & MARK) == 0)) {
		p->u.cn.cdr = t;
		t = p;
		p = q;
		goto e2;
    }
	
e6:
    if (t == NULL) return;
    q = t;
    if (q->nodeid & ATOM) {
		q->nodeid ^= ATOM;
        t = q->u.cn.car;
        q->u.cn.car = p;
        p = q;
        goto e5;
    } else {
		t = q->u.cn.cdr;
        q->u.cn.cdr = p;
        p = q;
        goto e6;
    }
}

struct lnode *garbage(c, xlist)
struct lnode *c, *xlist;
{
    struct lnode *t;
    struct pnrec *pn, *free_pnrec;
	
	/* initialize global garbage list */
    
    gflg = gcnt = 0;
    garbage_list = (struct lnode *)NULL;
    
    /* clear the mark on all property names in the list */
	
	pn = property_name_list;
    while (pn != NULL) {
		pn->flg &= UNMARK;
		pn = pn->fptr;
    }
	
    /* tag all valid nodes in each list */
	
    tagnode(xlist);
    tagnode(function_list);
    tagnode(property_list);
	/*    tagnode(current_eval_list); */
    tagnode(expression_list);
	
    /* tag the constants */
	
    truenode->nodeid |= MARK;
    nilnode->nodeid |= MARK;
    eofnode->nodeid |= MARK;
	
    /* now travel through all the nodes and create a new garbage list */
	
    for (t = boh; t < toh; t++)
		if (t->nodeid & MARK) {	/* if marked then keep it */
			t->nodeid &= UNMARK;
			if (t->nodeid & ATOM && t->nodeid != NPVAL) /* mark the property name as used */
				t->u.an.name->flg |= MARK;
		} else {		/* if not marked then put on garbage list */
			t->u.cn.car = garbage_list;
			garbage_list = t;
			gcnt++;
		}
		
	/* now travel through the property names and free any unmarked ones */
		
	pn = property_name_list;
	while (pn != NULL) {
		if (pn->flg & MARK) {	/* if marked then keep it */
			pn->flg &= UNMARK;
			pn = pn->fptr;
		} else {
			free_pnrec = pn;	/* free this propery name when we're done removing it from the list */
			pn = pn->fptr;
			if (free_pnrec->bptr == NULL) {
				property_name_list = free_pnrec->fptr;
			} else {
				free_pnrec->bptr->fptr = free_pnrec->fptr;
			}
			if (free_pnrec->fptr != NULL)
				free_pnrec->fptr->bptr = free_pnrec->bptr;
			free(free_pnrec);
		}
	}
	
	return(npnode(gcnt));
}

/* check for number property, used by numeric functions */

int chknp(ptr)
struct lnode *ptr;
{   
    if (ptr->nodeid == NPVAL)
		return(ptr->u.nn.ival);
    else
		error(13, ptr);
}

/* check to see if we have a valid token */

int isvalid(c)
char c;
{
    return(c != LPAREN && c != RPAREN && c != BRKT && c != DOT &&
		c != QUOTE && c != DQUOTE && c != '\0' && c != '\n' && c != ' ');
}

/* get a token from the input string */

void gettoken()
{
    int tkncnt;
	
    if (peeked) {
		sym = peeksym;
        peeked = (int) NULL;
        return;
    }
    sym = APVAL;
	
    while (isspace(*inptr) || *inptr == '\n') inptr++;
	
    switch (*inptr) {
    case NULLCHAR:
    case LPAREN: 
    case RPAREN:
    case BRKT:
    case DOT: 
    case QUOTE:
		sym = *inptr++;
		return;
    case COMMENT:
		sym = END;
		return;
    case EOF:
		error(15, eofnode);
    case DQUOTE:
		inptr++;		/* skip past the double quote */
		tkncnt = 0;
		/* everything between taken as is */
		while (*inptr != DQUOTE) {
			if (*inptr == '\0' || *inptr == '\n') {
				if (lispin != stdin) {
					inptr = fgets(inputLine, MAXIN, lispin);
					if (inptr == NULL)
						error(15, eofnode);
				}
				else
					error(12, nilnode);
			}
			if (tkncnt >= MAXCHR)
				error(17, nilnode);
			else
				token[tkncnt++] = *inptr++;
		}
		token[0] |= MARK;	/* mark high bit to indicate a string */
		token[tkncnt] = '\0';
		inptr++;
		return;
    default:
		tkncnt = 0;
		while (isvalid(*inptr)) {
			if (tkncnt >= MAXCHR)
				error(17, nilnode);
			else
				token[tkncnt++] = *inptr++;
			if ((*inptr == '\0' || *inptr == '\n') && lispin != stdin) {
				inptr = fgets(inputLine, MAXIN, lispin);
				if (inptr == NULL)
					error(15, eofnode);
			}
		}
		token[tkncnt] = '\0';
		if (isdigit(token[0]) || token[0] == '+' || token[0] == '-') {
			itoken = atoi(token);
			sym = NPVAL;
		}
		return;
    }
}

/* read a line of code and construct a list */

struct lnode *readcon(ptr)
struct lnode *ptr;
{
	FILE *fio;
	struct lnode *tmp;
	int i;
	
	if (chknp(ptr) == 1) {
		tmp = arg(ptr);
		i = sscanf(tmp->u.an.name->name, "#%X", &fio);
		if (i == EOF || i != 1)
			error(11, ptr);
		lispin = fio;
	}
	
	peeked = (int) NULL;
	inptr = fgets(inputLine, MAXIN, lispin);
	if (inptr == NULL) {
		lispin = stdin;
		return(eofnode);
	}
	gettoken();
	tmp = create();
	
	lispin = stdin;
	return(tmp);
}

/* read a line and construct a string */

struct lnode *readline(ptr)
struct lnode *ptr;
{
	FILE *fio;
	struct lnode *tmp;
	int i;
	
	if (chknp(ptr) == 1) {
		tmp = arg(ptr);
		i = sscanf(tmp->u.an.name->name, "#%X", &fio);
		if (i == EOF || i != 1)
			error(11, ptr);
		lispin = fio;
	}
	
	peeked = (int)NULL;
	
	inptr = fgets(inputLine, MAXIN, lispin);
	lispin = stdin;
	if (inptr == NULL)
		return(eofnode);
	
	inptr[strlen(inptr) - 1] = '\0';
	*inptr |= MARK;	/* mark high bit to indicate a string */
	return(apnode(inptr));
}

struct lnode *create()
{
    struct lnode *newptr;
	
    switch(sym) {
    case RPAREN:
    case BRKT:
    case DOT:
		error(0, nilnode);
    case END:
		return(nilnode);
    case QUOTE:
		gettoken();
		return(cons(apnode("quote"), ncons(create())));
    case APVAL:
		return(!strcmp(token, "NIL") ? nilnode : 
	       !strcmp(token, "T") ? truenode : apnode(token));
    case NPVAL:
		return(npnode(itoken));
    case LPAREN:
		gettoken();
		if (sym == END) {
			if (lispin == stdin) fputc('?', lispout);
			inptr = fgets(inputLine, MAXIN, lispin);
			if (inptr == NULL) error(15, eofnode);
			gettoken();
		}
		switch (sym) {
		case DOT:
			error(1, nilnode);
		case RPAREN:
			return(nilnode);
		case BRKT:
			peeked = TRUE;
			peeksym = BRKT;
			return(nilnode);
		}
		newptr = fnode();
		newptr->nodeid = LIST;
		newptr->u.cn.car = create();
		gettoken();
		if (sym == DOT) {	/* (<s-exp> . <s-exp>) */
			gettoken();
			newptr->u.cn.cdr = create();
			gettoken();
			if (sym != RPAREN && sym != BRKT)
				error(3, nilnode);
		} else {		/* (<s-exp> <s-exp> ... <s-exp>) */
			peeked = TRUE;
			peeksym = sym;
			sym = LPAREN;
			newptr->u.cn.cdr = create();
		}
		return(newptr);
    }
    return(nilnode);
}

/* create an atom property node */

struct lnode *apnode(astrng)
char *astrng;
{ 
    struct lnode *tmp;
	
    tmp = fnode();
    tmp->nodeid = APVAL;
    if (*astrng & MARK) {
		*astrng &= UNMARK;
		tmp->u.an.flg = STRING;
    } else
		tmp->u.an.flg = (int) NULL;
    tmp->u.an.name = findpn(astrng);
    return(tmp);
}

/* create a numeric property node */

struct lnode *npnode(ival)
int ival;
{
    struct lnode *tmp;
	
    tmp = fnode();
    tmp->nodeid = NPVAL;
    tmp->u.nn.ival = ival;
    return(tmp);
}

/* find a property name in the property name list */
/* if a match is not found create a new property name */

struct pnrec *findpn(pnstrng)
char *pnstrng;
{
    struct pnrec *pn = property_name_list;
	
    while (pn != NULL) {
		if (strcmp(pn->name, pnstrng) == 0) {
            return(pn);
		}
		pn = pn->fptr;
    }
    return(insrtpn(pnstrng));
}

/* insert a property name into the property name list */

struct pnrec *insrtpn(pnstrng)
char *pnstrng;
{
    struct pnrec *pn;
    int len = strlen(pnstrng);
    
    pn = (struct pnrec *)malloc(sizeof(struct pnrec) + len);
	
    if (pn == NULL)	/* did we run out of memory? */
		error(16, nilnode);
	
    pn->flg = (int) NULL;
    strcpy(pn->name, pnstrng);
    if (property_name_list)
		property_name_list->bptr = pn;
    pn->fptr = property_name_list;
    pn->bptr = NULL;
    property_name_list = pn;
	
    return(pn);
}

/* display the offending list, print an ASCII error message */
/* if the file I/O is not to the console, close it */
/* perform garbage collection */

void error(errnum, errptr)
int errnum;
struct lnode *errptr;
{
    if (lispin != stdin) {
        fclose(lispin);
		lispin = stdin;
    }
	
    if (lispout != stdout) {
        fclose(lispout);
		lispout = stdout;
    }
	
    if (errptr) {
		iprint(errptr);
		fputc('\n', lispout);
    }
    fprintf(lispout, "%s\n", errmsg[errnum]);
	
    garbage(nilnode, atom_list);
	
    longjmp(warmadr, errnum);
}

/* internal print routine */

struct lnode *iprint(ptr)
struct lnode *ptr;
{
	struct lnode *tmp;
	
	tmp = ptr;
	if (ptr->nodeid) {		/* an atom */
		if (ptr->nodeid == NPVAL) {
			fprintf(lispout, "%d ", ptr->u.nn.ival);
		} else {
			if ((ptr->u.an.flg & STRING) && !pflg) {
				fprintf(lispout,"\"%s\" ", ptr->u.an.name->name);
			} else
				fprintf(lispout,"%s ", ptr->u.an.name->name);
		}
	} else {			/* a list */
		fputc('(', lispout);
		do {
			iprint(ptr->u.cn.car);
			if (ptr->u.cn.cdr == nilnode)
				break;
			else
				if (ptr->u.cn.cdr->nodeid) {
					fputc('.', lispout);
					iprint(ptr->u.cn.car);
					break;
				} else
					ptr = ptr->u.cn.cdr;
		} while (TRUE);
		fputc(')', lispout);
	}
	return(tmp);
}

struct lnode *prin1(nptr)
struct lnode *nptr;
{
	FILE *fio;
	struct lnode *tmp;
	int i;
	
	if (chknp(nptr) == 2) {
		tmp = arg(nptr);
		i = sscanf(tmp->u.an.name->name, "#%X", &fio);
		if (i == EOF || i != 1)
			error(11, nptr);
		lispout = fio;
	}
	
	tmp = iprint(arg(npnode(1)));
	
	if (chknp(nptr) == 2)
		lispout = stdout;
	
	return(tmp);
}

struct lnode *princ(ptr)
struct lnode *ptr;
{
    struct lnode *tmp;
	
    pflg = TRUE;
    tmp = prin1(ptr);
    pflg = (int) NULL;
    return(tmp);
}

struct lnode *print(ptr)
struct lnode *ptr;
{
	struct lnode *tmp;
	
	tmp = prin1(ptr);
	
	if (chknp(ptr) == 2) {
		expression_list = cons(ncons(arg(ptr)), expression_list);	/* push LSUBR list */
		terpri(npnode(1));
		expression_list = expression_list->u.cn.cdr;			/* pop LSUBR list */
	} else
		terpri(npnode(0));
	
	return(tmp);
}

struct lnode *terpri(ptr)
struct lnode *ptr;
{
	FILE *fio;
	struct lnode *tmp;
	int i;
	
	if (chknp(ptr) == 1) {
		tmp = arg(ptr);
		i = sscanf(tmp->u.an.name->name, "#%X", &fio);
		if (i == EOF || i != 1)
			error(11, ptr);
		lispout = fio;
	}
	
    fputc('\n', lispout);
    lispout = stdout;
	
    return(nilnode);
}

struct lnode *save(ptr, fptr)
struct lnode *ptr, *fptr;
{
	FILE *fio;
	struct lnode *tmp, *file;
	int i;
	
	inptr = strcpy(inputLine, "w");
	*inptr |= MARK;	/* mark high bit to indicate a string */
	file = open(fptr, apnode(inptr));
	i = sscanf(file->u.an.name->name, "#%X", &fio);
	if (i == EOF || i != 1)
		error(11, ptr);
	lispout = fio;
	
	iprint(ptr);
	fputc('\n', lispout);
	while (ptr != nilnode) {
		if ((tmp = getd(ptr->u.cn.car)) != nilnode) {
			iprint(cons(apnode("define"), cons(ptr->u.cn.car, ncons(tmp))));
			fputc('\n', lispout);
		}
		else
			if ((tmp = iassoc(ptr->u.cn.car, atom_list)) != nilnode) {
				iprint(cons(apnode("setq"),
					cons(ptr->u.cn.car, 
					ncons(cons(apnode("quote"),
					ncons(tmp->u.cn.cdr))))));
				fputc('\n', lispout);
			}
			else
				if ((tmp = prplst(ptr->u.cn.car)) != nilnode) {
					iprint(cons(apnode("setplist"),
						cons(ptr->u.cn.car, 
						ncons(cons(apnode("quote"),
						ncons(tmp->u.cn.cdr))))));
					fputc('\n', lispout);
				}
				else {
					iprint(ptr->u.cn.car);
					fputc('\n', lispout);
				}
				ptr = ptr->u.cn.cdr;
	}
	close(file);
	lispout = stdout;
	return(ptr);
}

struct lnode *load(ptr)
struct lnode *ptr;
{
	struct lnode *tmp, *rtmp, *file;
	
	inptr = strcpy(inputLine, "r");
	*inptr |= MARK;	/* mark high bit to indicate a string */
	file = open(ptr, apnode(inptr));
	
	expression_list = cons(cons(file, nilnode), expression_list);	/* push LSUBR list */
	rtmp = readcon(npnode(1));
	while ((tmp = readcon(npnode(1))) != eofnode)
		ieval(tmp, atom_list); 
	expression_list = expression_list->u.cn.cdr;			/* pop LSUBR list */
	
	close(file);
	lispin = stdin;
	return(rtmp);
}

struct lnode *open(sptr, mptr)
struct lnode *sptr, *mptr;
{
	
	FILE *fio;
	char str[16];
	
	if (listp(sptr) == truenode)
		error(5, sptr);
	if (stringp(sptr) == nilnode)
		error(18,sptr);
	if (listp(mptr) == truenode)
		error(5, mptr);
	if (stringp(mptr) == nilnode)
		error(18,mptr);
	if (!(fio = fopen(sptr->u.an.name->name, mptr->u.an.name->name)))
		error(10, sptr);
	sprintf(str, "#%X", fio);
	return(apnode(str));
	
}

struct lnode *close(ptr)
struct lnode *ptr;
{
	FILE *fio;
	int i;
	
	i = sscanf(ptr->u.an.name->name, "#%X", &fio);
	if (i == EOF || i != 1)
		error(11, ptr);
	return((fclose(fio) == 0) ? truenode : nilnode);
}

struct lnode *car(ptr)
struct lnode *ptr;
{
    if (ptr->nodeid)
		error(4, ptr);
    else
		return(ptr->u.cn.car);
}

struct lnode *cdr(ptr)
struct lnode *ptr;
{
    if (ptr->nodeid)
		error(4, ptr);
    else
		return(ptr->u.cn.cdr);
}

struct lnode *cadr(ptr)
struct lnode *ptr;
{
    return(car(cdr(ptr)));
}

struct lnode *caddr(ptr)
struct lnode *ptr;
{
    return(car(cdr(cdr(ptr))));
}

struct lnode *null(ptr)
struct lnode *ptr;
{
	return((ptr == nilnode) ? truenode : nilnode);
}

struct lnode *atom(ptr)
struct lnode *ptr;
{
    return((ptr->nodeid) ? truenode : nilnode); 
}

struct lnode *listp(ptr)
struct lnode *ptr;
{
    return((!ptr->nodeid || ptr == nilnode) ? truenode : nilnode); 
}

struct lnode *stringp(ptr)
struct lnode *ptr;
{
    return((ptr->nodeid && ptr->u.an.flg & STRING) ? truenode : nilnode); 
}

struct lnode *list(ptr)
struct lnode *ptr;
{
    return(expression_list->u.cn.car); 
}

struct lnode *prog(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *tmp;
	
    tmp = iprog(cdr(ptr), pairnil(car(ptr), xlist));
    progmode = (char) NULL;
    return(tmp);
}

struct lnode *iprog(ptr, xlist) /* internal prog for prog and do */
struct lnode *ptr, *xlist;
{
    struct lnode *tptr;
    struct lnode *t;
	
    tptr = ptr;
    progmode = PROG;
    while (tptr != nilnode) {
		if (!tptr->u.cn.car->nodeid) {
			t = ieval(tptr->u.cn.car, xlist);
            switch (progmode) {
			case RETURN:
				return(t);
			case GOTO:
				if ((tptr = member(t, ptr)) == nilnode)
					error(7, t);
			case NULLCHAR:
				progmode = PROG;
            }
        }
		tptr = tptr->u.cn.cdr;
    }
    progmode = (char) NULL;
    return(nilnode);
}

struct lnode *preturn(ptr)
struct lnode *ptr;
{
    if (progmode == PROG)
		progmode = RETURN;
    return(ptr);
}

struct lnode *pgoto(ptr, xlist)
struct lnode *ptr, *xlist;
{
    if (progmode == PROG) {
		progmode = GOTO;
        return(car(ptr));
    }
    else
        error(7, car(ptr));
}

struct lnode *prog1(ptr)
struct lnode *ptr;
{
    return(car(expression_list->u.cn.car)); 
}

struct lnode *progn(ptr)
struct lnode *ptr;
{
    return(lastcar(expression_list->u.cn.car));
}

struct lnode *let(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *tmp, *tcar, *i, *v;
	
    i = v = nilnode;
    tmp = car(ptr);
    while (tmp != nilnode) {	/* pull out vars and initial values */
		v = cons(car(tcar = tmp->u.cn.car), v);
        i = cons(((tcar->u.cn.cdr == nilnode) ? nilnode : cadr(tcar)), i);
        tmp = tmp->u.cn.cdr;
    }
    return(lastcar(evalis(cdr(ptr), pairlis(v, evalis(i, xlist), xlist))));
}

struct lnode *do_(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *v, *r, *tx, *t, *tcar, *i;
	
    tx = xlist;			/* temporary xlist, before bindings */
    r = i = v = nilnode;
    t = car(ptr);
    while (t != nilnode) {	/* pull out vars, initial values and repeats */
		v = cons(car(tcar = t->u.cn.car), v);
        i = cons(((tcar->u.cn.cdr == nilnode) ? nilnode : cadr(tcar)), i);
        r = cons(((tcar->u.cn.cdr->u.cn.cdr == nilnode) ? nilnode : caddr(tcar)), r);
        t = t->u.cn.cdr;
    }
    xlist = pairlis(v, evalis(i, xlist), tx); /* eval the init and bind */
    ptr = ptr->u.cn.cdr;
	
    tcar = car(ptr);		/* pointer to end test */
    i = ptr->u.cn.cdr;	/* pointer to body */
    while (TRUE) {
		if (ieval(car(tcar), xlist) != nilnode) /* end test */
            return(lastcar(evalis(tcar->u.cn.cdr, xlist)));
		
		t = iprog(i, xlist);	/* prog the body */
		if (progmode == RETURN) {
			progmode = (char) NULL;
			return(t);
		}
		
		/* eval the repeat and bind */
		xlist = pairlis(v, evalis(r, xlist), tx);
    }
}

/* this assoc the user gets, args passed aren't guaranteed */

struct lnode *assoc(kptr, xlist)
struct lnode *kptr, *xlist;
{
    while (xlist != nilnode) {
		if (eqq(kptr, car(car(xlist))) == truenode)
            return(xlist->u.cn.car);
        xlist = xlist->u.cn.cdr;
    }
    return(nilnode);
}

/* internal assoc for looking at the atom_list, property_list and function_list */
/* arguments are guaranteed */

struct lnode *iassoc(p, l)
struct lnode *p, *l;
{
    struct lnode *t;
    struct pnrec *pn;
    
    pn = p->u.an.name;	/* get the pointer to the print name */
    while (l != nilnode) {
		t = l->u.cn.car;
        if (pn == t->u.cn.car->u.an.name)
            return(t);
        l = l->u.cn.cdr;
    }
    return(nilnode);
}

struct lnode *rplaca(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    if (ptr1->nodeid)
        error(4, ptr1);
    ptr1->u.cn.car = ptr2;
    return(ptr1);
}

struct lnode *rplacd(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    if (ptr1->nodeid)
        error(4, ptr1);
    ptr1->u.cn.cdr = ptr2;
    return(ptr1);
}

struct lnode *equal(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    if (eqq(ptr1, ptr2) == truenode)
        return(truenode);
    else
        if (ptr1->nodeid || ptr2->nodeid)
            return(nilnode);
		if (equal(ptr1->u.cn.car, ptr2->u.cn.car) == nilnode ||
			equal(ptr1->u.cn.cdr, ptr2->u.cn.cdr) == nilnode)
			return(nilnode);
		return(truenode);
}

struct lnode *eqq(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    if (ptr1->nodeid && ptr2->nodeid)
        return((ptr1->u.an.name == ptr2->u.an.name) ? truenode :
	nilnode);
    return((ptr1 == ptr2) ? truenode : nilnode);
}

struct lnode *reverse(ptr)
struct lnode *ptr;
{
    struct lnode *t;
	
    t = nilnode;
    while(ptr != nilnode) {
		t = cons(car(ptr), t);
        ptr = ptr->u.cn.cdr;
    }
    return(t);
}

struct lnode *nreverse(ptr)
struct lnode *ptr;
{
    struct lnode *tcar, *top = ptr, *lcar = lastcar(ptr);
    
    if (listp(ptr) == nilnode)
		error(5, ptr);
    
    while (top->u.cn.car != lcar){
		ptr = top;
		while (ptr->u.cn.cdr != nilnode) {
			tcar = ptr->u.cn.car;
			ptr->u.cn.car = ptr->u.cn.cdr->u.cn.car;
			ptr->u.cn.cdr->u.cn.car = tcar;
			if (ptr->u.cn.car == lcar)
				break;
			ptr = ptr->u.cn.cdr;
		}
    }
    return(ptr);
}

struct lnode *append(p1, p2)
struct lnode *p1, *p2;
{
    return((p1 == nilnode) ? p2 : cons(car(p1), append(cdr(p1), p2))); 
}

struct lnode *nconc(p1, p2)
struct lnode *p1, *p2;
{
    rplacd(last(p1), p2);
    return(p1);
}

struct lnode *last(ptr)
struct lnode *ptr;
{
    while(cdr(ptr) != nilnode)
		ptr = ptr->u.cn.cdr;
    return(ptr);
}

struct lnode *lastcar(ptr)
struct lnode *ptr;
{
    while(cdr(ptr) != nilnode)
		ptr = ptr->u.cn.cdr;
    return(ptr->u.cn.car);
}

struct lnode *delete(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return((ptr2 == nilnode) ? nilnode :
	   (eqq(ptr1, car(ptr2)) != nilnode) ?
		   delete(ptr1, ptr2->u.cn.cdr) :
	   cons(ptr2->u.cn.car, delete(ptr1, ptr2->u.cn.cdr)));
}

struct lnode *subst(new, old, form)
struct lnode *new, *old, *form;
{
    if (equal(form, old) != nilnode)
		return(new);
    if (form->nodeid)
		return(form);
    return(cons(subst(new, old, car(form)), subst(new, old, cdr(form))));
}

struct lnode *cons(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    struct lnode *ctmp;
	
    ctmp = fnode();
    ctmp->nodeid = LIST;
    ctmp->u.cn.car = ptr1;
    ctmp->u.cn.cdr = ptr2;
    return(ctmp);
}

struct lnode *xcons(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return(cons(ptr2, ptr1)); 
}

struct lnode *ncons(ptr1)
struct lnode *ptr1;
{
    return(cons(ptr1, nilnode)); 
}

struct lnode *conz(x, y, z)
struct lnode *x, *y, *z;
{
    return(rplaca(rplacd(x, z), y)); 
}

struct lnode *set(ptr, xlist)
struct lnode *ptr, *xlist;
{
    ptr = evalis(ptr, xlist);
    return(iset(car(ptr), cadr(ptr), xlist));
}

struct lnode *setq(ptr, xlist)
struct lnode *ptr, *xlist;
{
    return(iset(car(ptr), ieval(cadr(ptr), xlist), xlist)); 
}

struct lnode *push(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *tmp;
	
    tmp = evalis(ptr, xlist);
    return(iset(cadr(ptr), 
		cons(tmp->u.cn.car, tmp->u.cn.cdr->u.cn.car), xlist));
}

struct lnode *pop(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *tptr;
	
    tptr = car(evalis(ptr, xlist));
    if (tptr == nilnode)
		return(nilnode);
    iset(car(ptr), cdr(tptr), xlist);
    return(car(tptr));
}

struct lnode *define(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *t;
	
    if ((t = iassoc(car(ptr), function_list)) == nilnode)
		function_list = cons(t = cons(ptr->u.cn.car, ptr->u.cn.cdr->u.cn.car), function_list);
    else
		if (t->u.cn.car->nodeid == EXPR)
			t->u.cn.cdr = ptr->u.cn.cdr->u.cn.car;
		else
			error(14, ptr);
		t->u.cn.car->nodeid = EXPR;
		return(ptr->u.cn.car);
}

struct lnode *iset(ptr1, ptr2, xlist)
struct lnode *ptr1, *ptr2, *xlist;
{
    struct lnode *t;
	
    if (!ptr1->nodeid) error(5, ptr1);
    t = xlist;
    while (t->u.cn.cdr != nilnode) {
		if (xlist->u.cn.car->u.cn.car->u.an.name == 
			ptr1->u.an.name)
            return(xlist->u.cn.car->u.cn.cdr = ptr2);
		t = xlist;
		xlist = xlist->u.cn.cdr;
    }
    t->u.cn.cdr = cons(cons(ptr1, ptr2), nilnode);
    return(ptr2);
}

struct lnode *arg(nptr)
struct lnode *nptr;
{
    struct lnode *t;
    int n;
	
    t = length(expression_list->u.cn.car);
    if (nptr == nilnode)
        return(t);
    n = chknp(nptr) - 1;
    if (n >= 0 && n < chknp(t))
        return(nth(npnode(n), expression_list->u.cn.car));
    else
        error(11, nptr);
}

struct lnode *setarg(nptr, aptr)
struct lnode *nptr, *aptr;
{
    rplaca(nthcdr(sub1(nptr), expression_list->u.cn.car), aptr);
    return(aptr);
}

struct lnode *listify(nptr)
struct lnode *nptr;
{
    int ival, sval;
	
    if ((ival = chknp(nptr)) < 0) {
		sval = chknp(arg(nilnode));
        return(sublist(sval, sval + ival + 1));
    }
    else
        return(sublist(ival, 1));
}

struct lnode *sublist(i, j) /* internal routine for listify */
int i, j;
{
    struct lnode *lst;
	
    lst = nilnode;
    for (; i >= j; i--)
        lst = cons(arg(npnode(i)), lst);
    return(lst);
}

struct lnode *pairlis(name, value, xlist)
struct lnode *name, *value, *xlist;
{
    if (name == nilnode) return(xlist);
	
    while (name != nilnode) {	/* EXPR */
		xlist = cons(cons(car(name), car(value)), xlist);
		name = name->u.cn.cdr;
		value = value->u.cn.cdr;
    }
    
    return(xlist);
}

struct lnode *pairnam(name, value, xlist)
struct lnode *name, *value, *xlist;
{
    return((name == nilnode) ? xlist :
	   cons(cons(car(name), value), xlist));
}

struct lnode *pairnil(name, xlist)
struct lnode *name, *xlist;
{
    while (name != nilnode) {
		xlist = cons(ncons(car(name)), xlist);
        name = name->u.cn.cdr;
    }
    return(xlist);
}

struct lnode *member(ptr, lptr)
struct lnode *ptr, *lptr;
{
    while (lptr != nilnode)
        if (eqq(ptr, car(lptr)) == truenode)
            break;
        else
            lptr = lptr->u.cn.cdr;
		return(lptr);
}

struct lnode *putprop(psym, pval, pid)
struct lnode *psym, *pval, *pid;
{ 
    struct lnode *tmp;
	
    if (listp(psym) == truenode)
        error(5, psym);
	
    /* look for symbol on property_list */
    if ((tmp = iassoc(psym, property_list)) == nilnode) {
		tmp = ncons(psym);
        property_list = cons(tmp, property_list);
    }
	
    tmp->u.cn.cdr = cons(pid, cons(pval, tmp->u.cn.cdr));
    return(pval);
}

struct lnode *setplist(psym, lst)
struct lnode *psym, *lst;
{
    struct lnode *t;
	
    if ((t = iassoc(psym, property_list)) == nilnode)
        property_list = cons(cons(psym, lst), property_list);
    else
        t->u.cn.cdr = lst;
    return(lst);
}

struct lnode *defprop(ptr, xlist)
struct lnode *ptr, *xlist;
{
    return(putprop(car(ptr), cadr(ptr), caddr(ptr))); 
}

struct lnode *get(psym, pid)
struct lnode *psym, *pid;
{
    struct lnode *tmp;
	
    if ((tmp = iassoc(psym, property_list)) == nilnode)
        return(nilnode);
    tmp = tmp->u.cn.cdr;
    while (tmp != nilnode)
        if (eqq(pid, tmp->u.cn.car) == truenode)
            return(tmp->u.cn.cdr->u.cn.car);
        else
            tmp = tmp->u.cn.cdr->u.cn.cdr;
		return(nilnode);
}

struct lnode *prplst(psym) /* return property list, Lisp property_list function */
struct lnode *psym;
{
    struct lnode *tmp;
	
    return(((tmp = iassoc(psym, property_list)) == nilnode) ? nilnode : 
	   tmp->u.cn.cdr);
}

struct lnode *remprop(psym, pid)
struct lnode *psym, *pid;
{
    struct lnode *tmp, *pval;
	
    if ((tmp = iassoc(psym, property_list)) == nilnode)
        return(nilnode);
    while(tmp->u.cn.cdr != nilnode)
        if (eqq(pid, tmp->u.cn.cdr->u.cn.car) == truenode) {
			pval = tmp->u.cn.cdr->u.cn.cdr->u.cn.car;
            tmp->u.cn.cdr = tmp->u.cn.cdr->u.cn.cdr->u.cn.cdr;
            return(pval);
        }
        else
            tmp = tmp->u.cn.cdr->u.cn.cdr;
		return(nilnode);
}

struct lnode *and(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *tmp;
	
    tmp = truenode;
    while (ptr != nilnode) {
		if ((tmp = ieval(car(ptr), xlist)) == nilnode)
            break;
        ptr = ptr->u.cn.cdr;
    }
    return(tmp);
}

struct lnode *or(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *tmp;
	
    tmp = nilnode;
    while (ptr != nilnode) {
		if ((tmp = ieval(car(ptr), xlist)) != nilnode)
            break;
        ptr = ptr->u.cn.cdr;
    }
    return(tmp);
}

struct lnode *not(ptr1)
struct lnode *ptr1;
{
    return((ptr1 == nilnode) ? truenode : nilnode); 
}

struct lnode *numberp(ptr)
struct lnode *ptr;
{
    return((ptr->nodeid == NPVAL) ? truenode : nilnode); 
}

struct lnode *plus(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return(npnode(chknp(ptr1) + chknp(ptr2))); 
}

struct lnode *add1(ptr)
struct lnode *ptr;
{
    if (ptr->nodeid == NPVAL)
		return(npnode(ptr->u.nn.ival + 1));
    else
		error(13, ptr); 
}

struct lnode *diff(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return(npnode(chknp(ptr1) - chknp(ptr2)));
}

struct lnode *sub1(ptr)
struct lnode *ptr;
{
    if (ptr->nodeid == NPVAL)
		return(npnode(ptr->u.nn.ival - 1));
    else
		error(13, ptr);
}

struct lnode *minus(ptr)
struct lnode *ptr;
{
    return(npnode(-chknp(ptr))); 
}

struct lnode *times(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return(npnode(chknp(ptr1) * chknp(ptr2))); 
}

struct lnode *divide(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return(npnode(chknp(ptr1) / chknp(ptr2))); 
}

struct lnode *mod(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return(npnode(chknp(ptr1) % chknp(ptr2))); 
}

struct lnode *lisp_abs(ptr)
struct lnode *ptr;
{
    int t;
	
    return(((t = chknp(ptr)) < 0) ? npnode(-t) : npnode(t));
}

struct lnode *lrandom(ptr)
struct lnode *ptr;
{
    int s;
	
    if ((s = chknp(ptr)) != 0)
        srand(s);
    return(npnode(rand()));
}

struct lnode *greatp(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return((chknp(ptr1) > chknp(ptr2)) ? truenode : nilnode); 
}

struct lnode *lessp(ptr1, ptr2)
struct lnode *ptr1, *ptr2;
{
    return((chknp(ptr1) < chknp(ptr2)) ? truenode : nilnode); 
}

struct lnode *plusp(ptr)
struct lnode *ptr;
{
    return((chknp(ptr) >= 0) ? truenode : nilnode); 
}

struct lnode *zerop(ptr)
struct lnode *ptr;
{
    return((chknp(ptr) == 0) ? truenode : nilnode); 
}

struct lnode *minusp(ptr)
struct lnode *ptr;
{
    return((chknp(ptr) < 0) ? truenode : nilnode); 
}

struct lnode *oddp(ptr)
struct lnode *ptr;
{
    return((chknp(ptr) & 0x01) ? truenode : nilnode); 
}

struct lnode *maplist(lst, xlist)
struct lnode *lst, *xlist;
{
    struct lnode *t, *c, *l, *fn;
    char dflg;
	
    fn = car(lst = evalis(lst, xlist));
    lst = lst->u.cn.cdr;
    l = nilnode;
    dflg = (char) NULL;
    while (!dflg) {
		c = nilnode;
        t = lst;
        while (t != nilnode) {
			c = cons(car(t), c);
            t->u.cn.car = t->u.cn.car->u.cn.cdr;
            if (t->u.cn.car == nilnode) dflg = TRUE;
            t = t->u.cn.cdr;
        }
        l = cons(iapply(fn, nreverse(c), xlist), l);
    }
    return(nreverse(l));
}

struct lnode *mapcar(lst, xlist)
struct lnode *lst, *xlist;
{
    struct lnode *t, *c, *l, *fn;
    char dflg;
	
    fn = car(lst = evalis(lst, xlist));
    lst = lst->u.cn.cdr;
    l = nilnode;
    dflg = (char) NULL;
    while (!dflg) {
		c = nilnode;
        t = lst;
        while (t != nilnode) {
			c = cons(car(car(t)), c);
            t->u.cn.car = t->u.cn.car->u.cn.cdr;
            if (t->u.cn.car == nilnode)
                dflg = TRUE;
            t = t->u.cn.cdr;
        }
        l = cons(iapply(fn, nreverse(c), xlist), l);
    }
    return(nreverse(l));
}

struct lnode *evcon(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *t, *r;
	
    while (ptr != nilnode)
        if ((r = ieval(car(t = ptr->u.cn.car), xlist)) != nilnode)
            return((t->u.cn.cdr == nilnode) ? r : 
		lastcar(evalis(t->u.cn.cdr, xlist)));
        else
            ptr = ptr->u.cn.cdr;
		return(nilnode);
}

quit()
{
    exit(0);
}

struct lnode *getd(ptr)
struct lnode *ptr;
{
    struct lnode *tmp;
	
    return(((tmp = iassoc(ptr, function_list)) == nilnode) ? nilnode : 
	   tmp->u.cn.cdr);
}

struct lnode *length(ptr)
struct lnode *ptr;
{
    int lcnt;
	
    lcnt = 0;
    /*    if (ptr->nodeid)
	error(4, ptr); */
    while (ptr != nilnode) {
        lcnt++;
		ptr = ptr->u.cn.cdr;
    }
    return(npnode(lcnt));
}

struct lnode *nthcdr(nval, ptr)
struct lnode *nval, *ptr;
{
    int lcnt;
	
    if ((lcnt = chknp(nval)) < 0)
        error(11, nval);
    while (lcnt)
    {   lcnt--;
	if ((ptr = ptr->u.cn.cdr) == nilnode)
		break;
    }
    return(ptr);
}

struct lnode *nth(nval, ptr)
struct lnode *nval, *ptr;
{
    struct lnode *t;
	
    return(((t = nthcdr(nval, ptr)) == nilnode) ? t : t->u.cn.car);
}

struct lnode *quote(ptr, xlist)
struct lnode *ptr, *xlist;
{
    return(car(ptr));
}

struct lnode *trace()
{
    tflg = TRUE;
    return(truenode);
}

struct lnode *untrace()
{
    tflg = (unsigned) NULL;
    return(nilnode);
}


struct lnode *syscall(ptr)
struct lnode *ptr;
{
	int sysreturn;
	
	if (listp(ptr) == truenode)
        error(5,ptr);
    sysreturn = system(ptr->u.an.name->name);
    return(npnode(sysreturn));
}

/* this is the eval the user gets */

struct lnode *eval(ptr, xlist)
struct lnode *ptr, *xlist;
{
    return(ieval(ieval(ptr->u.cn.car, xlist), xlist)); 
}

/* internal evaluation */

struct lnode *ieval(ptr, xlist)
struct lnode *ptr, *xlist;
{
    struct lnode *a;
    struct lnode *ecar, *ecdr, *t;
	
    if (gflg)
        garbage(nilnode, xlist); /* getting close to running out */
	
    if (tflg) {
		fputc('>', lispout);
		iprint(ptr);
		fputc('\n', lispout);
    }
	
    if (ptr->nodeid)		/* ptr is an ATOM, return value */
        a = (ptr->nodeid == NPVAL) ? ptr :
	((t = iassoc(ptr, xlist)) == nilnode) ? nilnode : t->u.cn.cdr;
    else {
		ecar = ptr->u.cn.car; /* ptr is a LIST, look at the car for func */
        ecdr = ptr->u.cn.cdr;
        if (ecar->nodeid) {	/* ecar is an ATOM */
			if ((t = iassoc(ecar, function_list)) == nilnode)
                if ((t = iassoc(ecar, xlist)) == nilnode)
                    error(6, ecar);
                else
                    return(ieval(cons(t->u.cn.cdr, ecdr), xlist));
		} else			/* ecar is a LIST */
            if ((t = iassoc(ecar->u.cn.car, function_list)) == nilnode) 
                error(6, ecar->u.cn.car);
			
			switch(t->u.cn.car->nodeid) {
			case EXPR:
				a = ieval(cons(t->u.cn.cdr, ecdr), xlist);
				break;
			case LAMBDA:
			case SUBR:
			case LSUBR:
				ecdr = evalis(ecdr, xlist);
			case NLAMBDA:
			case FSUBR:
			case MACRO: 
			default:
				a = iapply(ptr->u.cn.car, ecdr, xlist);
			}
    }
	
    if (tflg) {
		fputc('<', lispout);
		iprint(a);
		fputc('\n', lispout);
    }
	
    return(a);
}

struct lnode *evalis(ptr, xlist)
struct lnode *ptr, *xlist;
{
	struct lnode *lst;
	
	if (ptr == nilnode)
		return(nilnode);
	lst = nilnode;
	while (ptr != nilnode) {
		lst = cons(ieval(car(ptr), xlist), lst);
		ptr = ptr->u.cn.cdr;
	}
	return(nreverse(lst));
}

/* external apply, the one the user gets */

struct lnode *apply(lst, xlist)
struct lnode *lst, *xlist;
{
    return(iapply(ieval(car(lst), xlist), ieval(cadr(lst), xlist),
		xlist)); 
}

/* internal apply, system use only */

struct lnode *iapply(fn, arglst, xlist)
struct lnode *fn, *arglst, *xlist;
{
    struct lnode *tcar, *tcdr, *tmp, *arg1, *arg2;
    struct lnode *(*func)();
	
    if (fn->nodeid) {
		if ((tmp = iassoc(fn, function_list)) == nilnode)
            error(6, fn);
		tcdr = tmp->u.cn.cdr;
		func = tcdr->u.fn.func;
		switch(tmp->u.cn.car->nodeid) {
		case EXPR:
			return(iapply(tcdr, arglst, xlist));
		case FSUBR:
			return((*func)(arglst, xlist));
		case SUBR:
			switch(tcdr->u.fn.args) { /* number of arguments */
			case 3:
				return((*func)(car(arglst), cadr(arglst),
					caddr(arglst)));
			case 2:
				return((*func)(car(arglst), cadr(arglst)));
			case 1:
				return((*func)(car(arglst)));
			case 0:
				return((*func)());
			}
			case LSUBR:
				expression_list = cons(arglst, expression_list);	/* push LSUBR list */
				tmp = (*func)(length(arglst));
				expression_list = expression_list->u.cn.cdr;		/* pop LSUBR list */
				return(tmp);
			default:
				error(2, fn);
		}
    } else {
		tcar = fn->u.cn.car;
        if (tcar->nodeid && ((tmp = iassoc(tcar, function_list)) != nilnode)) {
			arg1 = cadr(fn);
            arg2 = caddr(fn);
            switch (tmp->u.cn.car->nodeid) {
			case LAMBDA:
				if (arg1->nodeid && arg1 != nilnode) { /* LEXPR */
					if (arg1->nodeid == NPVAL)
						error(11, arg1);
					else {
						expression_list = cons(arglst, expression_list); /* push LEXPR list */
						tmp = ieval(arg2,
							cons(cons(arg1, length(arglst)), xlist));
						expression_list = expression_list->u.cn.cdr; /* pop LEXPR list */
					}
				} else		/* EXPR */
					tmp = ieval(arg2, pairlis(arg1, arglst, xlist));
				return(tmp);
			case NLAMBDA:
				return(ieval(arg2, pairnam(arg1, arglst, xlist)));
			case MACRO:
				tmp = ieval(arg2, pairnam(arg1, cons(fn, arglst), xlist));
				return(ieval(tmp, xlist));
			default:
				error(8, tmp);
			}
		} else
			return(ieval(cons(ieval(tcar, xlist), arglst), xlist));
    }
}

/* create the property list */

void csubr(pname, subr, stype, nargs)
char *pname, stype;
struct lnode *(*subr)();
int nargs;
{
    struct lnode *arg1, *arg2;
	
    arg1 = fnode();
    arg1->nodeid = stype;
    arg1->u.an.flg = (int) NULL;
    arg1->u.an.name = insrtpn(pname);
	
    arg2 = fnode();
    arg2->nodeid = NPVAL;
    arg2->u.fn.func = subr;
    arg2->u.fn.args = nargs;
	
    function_list = cons(cons(arg1, arg2), function_list);
}

struct lnode *init()
{
    property_name_list = NULL;		/* nothing in the property name list yet */
    
    garbage_list = boh = (struct lnode *)malloc(sizeof(struct lnode) * 5000);
    toh = boh + 5000;		/* top of heap */
    gcnt = 0;
    while (garbage_list < toh) {
		garbage_list->nodeid = 0;
		garbage_list->u.cn.car = garbage_list + 1;
		garbage_list++;
		gcnt++;
    }
    garbage_list = boh;
    gflg = tflg = pflg = (unsigned) NULL;
	
    truenode = apnode("T");
    nilnode = apnode("NIL");
    eofnode = apnode("*eof*");
	
    atom_list = function_list = property_list = nilnode;
    atom_list = cons(cons(nilnode, nilnode), atom_list);
    atom_list = cons(cons(truenode, truenode), atom_list);
	
    csubr("system", syscall, SUBR, 1);
    csubr("clear", init, SUBR, 0);
    csubr("gc", garbage, FSUBR, 2);
    csubr("save", save, SUBR, 2);
    csubr("load", load, SUBR, 1);
    csubr("quit", quit, SUBR, 0);
	
    csubr("trace", trace, SUBR, 0);
    csubr("untrace", untrace, SUBR, 0);
	
    csubr("eval", eval, FSUBR, 2);
    csubr("assoc", assoc, SUBR, 2);
    csubr("member", member, SUBR, 2);
    csubr("apply", apply, FSUBR, 2);
	
    csubr("lambda", NULL, LAMBDA, 0);
    csubr("nlambda", NULL, NLAMBDA, 0);
    csubr("macro", NULL, MACRO, 0);
	
    csubr("define", define, FSUBR, 2);
	
    csubr("putprop", putprop, SUBR, 3);
    csubr("get", get, SUBR, 2);
    csubr("getd", getd, SUBR, 1);
    csubr("property_list", prplst, SUBR, 1);
    csubr("remprop", remprop, SUBR, 2);
    csubr("defprop", defprop, FSUBR, 2);
    csubr("setplist", setplist, SUBR, 2);
	
    csubr("and", and, FSUBR, 2);
    csubr("or", or, FSUBR, 2);
    csubr("not", not, SUBR, 1);
	
    csubr("append", append, SUBR, 2);
    csubr("nconc", nconc, SUBR, 2);
    csubr("reverse", reverse, SUBR, 1);
    csubr("nreverse", nreverse, SUBR, 1);
    csubr("xcons", xcons, SUBR, 2);
    csubr("ncons", ncons, SUBR, 1);
    csubr("delete", delete, SUBR, 2);
    csubr("equal", equal, SUBR, 2);
    csubr("subst", subst, SUBR, 3);
    csubr("length", length, SUBR, 1);
	
    csubr("nthcdr", nthcdr, SUBR, 2);
    csubr("nth", nth, SUBR, 2);
	
    csubr("maplist", maplist, FSUBR, 2);
    csubr("mapcar", mapcar, FSUBR, 2);
	
    csubr("random", lrandom, SUBR, 1);
    csubr("abs", labs, SUBR, 1);
    csubr("oddp", oddp, SUBR, 1);
    csubr("mod", mod, SUBR, 2);
    csubr("divide", divide, SUBR, 2);
    csubr("times", times, SUBR, 2);
    csubr("minus", minus, SUBR, 1);
    csubr("difference", diff, SUBR, 2);
    csubr("diff", diff, SUBR, 2);
    csubr("plus", plus, SUBR, 2);
    csubr("sub1", sub1, SUBR, 1);
    csubr("add1", add1, SUBR, 1);
    csubr("lessp", lessp, SUBR, 2);
    csubr("greaterp", greatp, SUBR, 2);
    csubr("minusp", minusp, SUBR, 1);
    csubr("zerop", zerop, SUBR, 1);
    csubr("plusp", plusp, SUBR, 1);
    csubr("numberp", numberp, SUBR, 1);
	
    csubr("null", null, SUBR, 1);
    csubr("atom", atom, SUBR, 1);
    csubr("listp", listp, SUBR, 1);
    csubr("stringp", stringp, SUBR, 1);
	
    csubr("prog", prog, FSUBR, 2);
    csubr("return", preturn, SUBR, 1);
    csubr("go", pgoto, FSUBR, 2);
    csubr("progn", progn, LSUBR, 1);
    csubr("prog1", prog1, LSUBR, 1);
    csubr("let", let, FSUBR, 2);
    csubr("do", do_, FSUBR, 2);
	
    csubr("arg", arg, SUBR, 1);
    csubr("listify", listify, SUBR, 1);
    csubr("setarg", setarg, SUBR, 2);
	
    csubr("readline", readline, LSUBR, 1);
    csubr("read", readcon, LSUBR, 1);
    csubr("terpri", terpri, LSUBR, 1);
    csubr("princ", princ, LSUBR, 1);
    csubr("prin1", prin1, LSUBR, 1);
    csubr("print", print, LSUBR, 1);
	
    csubr("push", push, FSUBR, 2);
    csubr("pop", pop, FSUBR, 2);
	
    csubr("conz", conz, SUBR, 3);
    csubr("set", set, FSUBR, 2);
    csubr("last", last, SUBR, 1);
    csubr("rplaca", rplaca, SUBR, 2);
    csubr("rplacd", rplacd, SUBR, 2);
    csubr("cons", cons, SUBR, 2);
    csubr("list", list, LSUBR, 1);
    csubr("eq", eqq, SUBR, 2);
    csubr("cond", evcon, FSUBR, 2);
    csubr("caddr", caddr, SUBR, 1);
    csubr("cadr", cadr, SUBR, 1);
    csubr("cdr", cdr, SUBR, 1);
    csubr("car", car, SUBR, 1);
    csubr("setq", setq, FSUBR, 2);
    csubr("quote", quote, FSUBR, 2);
    csubr("open", open, SUBR, 2);
    csubr("close", close, SUBR, 1);
    return(nilnode);
}

/* finally, main */

main()
{
	int jmpflg;
	
	if( signal(SIGINT, sig_handler) == SIG_ERR )
	{
		fprintf( stderr, "Couldn't set SIGINT\n" );
		abort();   
	}
	
    lispin = stdin;
    lispout = stdout;

    fputs("Tiny Lisp v2.0\n", lispout);
	init();
	
	jmpflg = setjmp(warmadr);
	if (jmpflg == -1) {
		if( signal(SIGINT, sig_handler) == SIG_ERR )
		{	
			fprintf( stderr, "Couldn't set SIGINT\n" );
			abort();   
		}
	}
	terpri(npnode(0));
	
	while (TRUE) {  
		expression_list = current_eval_list = nilnode;
		fputc(':', lispout);
		iprint(ieval(current_eval_list = readcon(npnode(0)), atom_list));
		fputc('\n', lispout);
	}
} 

