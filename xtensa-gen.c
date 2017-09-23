/* 
 * Copyright (C) 2017 <>< C. Lohr
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#ifdef TARGET_DEFS_ONLY

#define RC_INT     0x0001 /* generic integer register */
#define RC_FLOAT   0x0002 /* generic float register [NOT IMPLEMENTED] */
#define RC_A0      0x0004
#define RC_A1      0x0008 /* Stack pointer */
#define RC_A2      0x0010 /* Return register */
#define RC_A3      0x0020
#define RC_A4      0x0040
#define RC_A5      0x0080
#define RC_A6      0x0100
#define RC_A7      0x0200
#define RC_A8      0x0400
#define RC_A9      0x0800

#define NB_REGS         10

#include "tcc.h"

//XXX Is this correct?
#define LDOUBLE_SIZE 8
#define PTR_SIZE 8


enum {
    TREG_A0 = 0,
    TREG_A1,
    TREG_A2,
    TREG_A3,
    TREG_A4,
    TREG_A5,
    TREG_A6,
    TREG_A7,
    TREG_A8,
    TREG_A9,
};

#define REG_IRET   TREG_A2
#define REG_LRET   TREG_A3
#define REG_FRET   0x8000

#define RC_IRET    RC_A2
#define RC_LRET    RC_A3
#define RC_FRET    0x8000

#define RC_IRET    RC_C67_A4	/* function return: integer register */
#define RC_LRET    RC_C67_A5	/* function return: second integer register */
#define RC_FRET    RC_C67_A4	/* function return: float register */


#else


ST_DATA const int reg_classes[NB_REGS] = {
	RC_INT | RC_A0,
	RC_INT | RC_A1,
	RC_INT | RC_A2,
	RC_INT | RC_A3,
	RC_INT | RC_A4,
	RC_INT | RC_A5,
	RC_INT | RC_A6,
	RC_INT | RC_A7,
	RC_INT | RC_A8,
	RC_INT | RC_A9,
};


/* output a symbol and patch all calls to it */
ST_FUNC void gsym_addr(int t, int a)
{
	printf( "gsym_addr( %d, %d );\n", t, a );
}

ST_FUNC void gsym(int t)
{
	printf( "TODO gsym(%d)\n", t );
}

ST_FUNC void g(int c)
{
	printf( "g(%d)\n", c );
}

ST_FUNC void o(unsigned int c)
{
    while (c) {
        g(c);
        c = c >> 8;
    }
}

/* store register 'r' in lvalue 'v' */
ST_FUNC void store(int r, SValue *v)
{
	printf( "Store( %d -> %d %d %d TYPE %d)\n", r, v->type.t, v->c.i, v->r, v->type.t & VT_BTYPE );
}

ST_FUNC void load(int r, SValue *sv)
{
}

ST_FUNC int gjmp(int t)
{
	printf( "gjmp( %d )\n", t );
//    return gjmp2(0xe9, t);
}

/* generate a jump to a fixed address */
ST_FUNC void gjmp_addr(int a)
{
	printf( "gjmp_addr( %d )\n", a );
/*
    int r;
    r = a - ind - 2;
    if (r == (char)r) {
        g(0xeb);
        g(r);
    } else {
        oad(0xe9, a - ind - 5);
    } */
}



ST_FUNC void gfunc_call(int nb_args)
{
	printf( "gfunc_call( nb_args = %d )\n", nb_args );
}


ST_FUNC void gtst_addr(int inv, int a)
{
	printf( "[fixme] gtst_addr( %d %d )\n", inv, a );
	return 0;
}

/* generate a test. set 'inv' to invert test. Stack entry is popped */
ST_FUNC int gtst(int inv, int t)
{
	printf( "[fixme] gtst: %d %d\n", inv, t );
	return 0;
}









///////////////////////////////////////////////////////////////////////////////
////////////////Unimplemented features - these are just not done///////////////
///////////////////////////////////////////////////////////////////////////////


/* convert from one floating point type to another */
void gen_cvt_ftof(int t)
{
	tcc_error( "gen_cvt_ftof( %d ) - but floating point is unimplemented on xtensa.\n", t );
}

/* convert fp to int 't' type */
void gen_cvt_ftoi(int t)
{
	tcc_error( "gen_cvt_ftoi( %d ) - but floating point is unimplemented on xtensa.\n", t );
}

void gen_cvt_itof(int t)
{
	tcc_error( "gen_cvt_itof( %d ) - but floating point is unimplemented on xtensa.\n", t );
}

void gen_opf(int op)
{
	tcc_error( "gen_opf( %d ) - but floating point is unimplemented on xtensa.\n", op );
}


/* bound check support functions */
#ifdef CONFIG_TCC_BCHECK


/* generate a bounded pointer addition */
ST_FUNC void gen_bounded_ptr_add(void)
{
#if 0
    /* prepare fast i386 function call (args in eax and edx) */
    gv2(RC_EAX, RC_EDX);
    /* save all temporary registers */
    vtop -= 2;
    save_regs(0);
    /* do a fast function call */
    gen_static_call(TOK___bound_ptr_add);
    /* returned pointer is in eax */
    vtop++;
    vtop->r = TREG_EAX | VT_BOUNDED;
    /* address of bounding function call point */
    vtop->c.i = (cur_text_section->reloc->data_offset - sizeof(Elf32_Rel));
#endif
}
/* patch pointer addition in vtop so that pointer dereferencing is
   also tested */
ST_FUNC void gen_bounded_ptr_deref(void)
{
	printf( "gen_bounded_ptr_deref()\n" );
#if 0
    addr_t func;
    int  size, align;
    Elf32_Rel *rel;
    Sym *sym;

    size = 0;
    /* XXX: put that code in generic part of tcc */
    if (!is_float(vtop->type.t)) {
        if (vtop->r & VT_LVAL_BYTE)
            size = 1;
        else if (vtop->r & VT_LVAL_SHORT)
            size = 2;
    }
    if (!size)
        size = type_size(&vtop->type, &align);
    switch(size) {
    case  1: func = TOK___bound_ptr_indir1; break;
    case  2: func = TOK___bound_ptr_indir2; break;
    case  4: func = TOK___bound_ptr_indir4; break;
    case  8: func = TOK___bound_ptr_indir8; break;
    case 12: func = TOK___bound_ptr_indir12; break;
    case 16: func = TOK___bound_ptr_indir16; break;
    default:
        tcc_error("unhandled size when dereferencing bounded pointer");
        func = 0;
        break;
    }

    /* patch relocation */
    /* XXX: find a better solution ? */
    rel = (Elf32_Rel *)(cur_text_section->reloc->data + vtop->c.i);
    sym = external_global_sym(func, &func_old_type, 0);
    if (!sym->c)
        put_extern_sym(sym, NULL, 0, 0);
    rel->r_info = ELF32_R_INFO(sym->c, ELF32_R_TYPE(rel->r_info));
#endif
}
#endif


#endif
