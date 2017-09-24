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

#define LDOUBLE_ALIGN 4
#define LDOUBLE_SIZE 8   //XXX Is this correct? What does this do?
#define PTR_SIZE 4
#define MAX_ALIGN     8


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

#define RC_IRET    RC_A2	/* function return: integer register */
#define RC_LRET    RC_A3	/* function return: second integer register */
#define RC_FRET    0x8000	/* function return: float register (not currently in use) */


#else

#include "tcc.h"

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
	int ind1;
	if (nocode_wanted)
		return;
	ind1 = ind + 1;
	if (ind1 > cur_text_section->data_allocated)
		section_realloc(cur_text_section, ind1);
	printf( "g(%x)\n", c );
	cur_text_section->data[ind] = c;
	ind = ind1;
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
	printf( "store( %d -> %d %lu %d TYPE %d)\n", r, v->type.t, v->c.i, v->r, v->type.t & VT_BTYPE );
}

ST_FUNC void load(int r, SValue *sv)
{
	printf( "load( %d -> %d %lu %d TYPE %d)\n", r, sv->type.t, sv->c.i, sv->r, sv->type.t & VT_BTYPE );
}

ST_FUNC int gjmp(int t)
{
	printf( "gjmp( %d )\n", t );
//    return gjmp2(0xe9, t);
	return 0;
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


/* generate function prolog of type 't' */
ST_FUNC void gfunc_prolog(CType *func_type)
{
  Sym *sym,*sym2;
  int n, nf, size, align, rs, struct_ret = 0;
  int addr, pn, sn; /* pn=core, sn=stack */
  CType ret_type;
  printf( "gfunc_prolog(%d %p)\n", func_type->t, func_type->ref );

#ifdef TCC_ARM_EABI
  struct avail_regs avregs = AVAIL_REGS_INITIALIZER;
#endif

  sym = func_type->ref;
  func_vt = sym->type;
  func_var = (func_type->ref->f.func_type == FUNC_ELLIPSIS);

  n = nf = 0;
  if ((func_vt.t & VT_BTYPE) == VT_STRUCT &&
      !gfunc_sret(&func_vt, func_var, &ret_type, &align, &rs))
  {
    n++;
    struct_ret = 1;
    func_vc = 12; /* Offset from fp of the place to store the result */
  }
  for(sym2 = sym->next; sym2 && (n < 4 || nf < 16); sym2 = sym2->next) {
    size = type_size(&sym2->type, &align);
#ifdef TCC_ARM_EABI
    if (float_abi == ARM_HARD_FLOAT && !func_var &&
        (is_float(sym2->type.t) || is_hgen_float_aggr(&sym2->type))) {
      int tmpnf = assign_vfpreg(&avregs, align, size);
      tmpnf += (size + 3) / 4;
      nf = (tmpnf > nf) ? tmpnf : nf;
    } else
#endif
    if (n < 4)
      n += (size + 3) / 4;
  }
  o( 0xabcdabcd );
//  o(0xE1A0C00D); /* mov ip,sp */
  if (func_var)
    n=4;
  if (n) {
    if(n>4)
      n=4;
#ifdef TCC_ARM_EABI
    n=(n+1)&-2;
#endif
    //o(0xE92D0000|((1<<n)-1)); /* save r0-r4 on stack if needed */
	o(0xdeadbeef );
  }
#if 0
  if (nf) {
    if (nf>16)
      nf=16;
    nf=(nf+1)&-2; /* nf => HARDFLOAT => EABI */
    o(0xED2D0A00|nf); /* save s0-s15 on stack if needed */
  }
  //o(0xE92D5800); /* save fp, ip, lr */
  //o(0xE1A0B00D); /* mov fp, sp */
  //func_sub_sp_offset = ind;
  //o(0xE1A00000); /* nop, leave space for stack adjustment in epilog */
#endif
#ifdef TCC_ARM_EABI
  if (float_abi == ARM_HARD_FLOAT) {
    func_vc += nf * 4;
    avregs = AVAIL_REGS_INITIALIZER;
  }
#endif


  pn = struct_ret, sn = 0;
  while ((sym = sym->next)) {
    CType *type;
    type = &sym->type;
    size = type_size(type, &align);
    size = (size + 3) >> 2;
    align = (align + 3) & ~3;
    printf( " Param: %d %d %d\n", type, size, align );
#ifdef TCC_ARM_EABI
    if (float_abi == ARM_HARD_FLOAT && !func_var && (is_float(sym->type.t)
        || is_hgen_float_aggr(&sym->type))) {
      int fpn = assign_vfpreg(&avregs, align, size << 2);
      if (fpn >= 0)
        addr = fpn * 4;
      else
        goto from_stack;
    } else
#endif
    if (pn < 4) {
#ifdef TCC_ARM_EABI
        pn = (pn + (align-1)/4) & -(align/4);
#endif
      addr = (nf + pn) * 4;
      pn += size;
      if (!sn && pn > 4)
        sn = (pn - 4);
    } else {
#ifdef TCC_ARM_EABI
from_stack:
        sn = (sn + (align-1)/4) & -(align/4);
#endif
      addr = (n + nf + sn) * 4;
      sn += size;
    }
    sym_push(sym->v & ~SYM_FIELD, type, VT_LOCAL | lvalue_type(type->t),
             addr + 12);
  }
  //last_itod_magic=0;
  //leaffunc = 1;
  loc = 0;


}

/* generate function epilog */
ST_FUNC void gfunc_epilog(void)
{
	printf( "gfunc_epilog()\n" );
}


/* generate a test. set 'inv' to invert test. Stack entry is popped */
ST_FUNC int gtst(int inv, int t)
{
	printf( "[fixme] gtst: %d %d\n", inv, t );
	return 0;
}



/* generate an integer binary operation */
void gen_opi(int op)
{
	printf( "gen_opi( %d )\n", op );
}


void ggoto(void)
{
	printf( "ggoto()\n" );
}

/* Return the number of registers needed to return the struct, or 0 if
   returning via struct pointer. */
ST_FUNC int gfunc_sret(CType *vt, int variadic, CType *ret, int *ret_align, int *regsize)
{
	*ret_align = 1; //Probably don't have to re-align values?
	return 0;
}



/* Restore the SP from a location on the stack */
ST_FUNC void gen_vla_sp_restore(int addr) {
	printf( "gen_vla_sp_restore( %d )\n", addr );
//    gen_modrm64(0x8b, TREG_RSP, VT_LOCAL, NULL, addr);
}



/* Save the stack pointer onto the stack and return the location of its address */
ST_FUNC void gen_vla_sp_save(int addr) {
	printf( "gen_vla_sp_save( %d )\n", addr );
    /* mov %rsp,addr(%rbp)*/
    //gen_modrm64(0x89, TREG_RSP, VT_LOCAL, NULL, addr);
}



/* Subtract from the stack pointer, and push the resulting value onto the stack */
ST_FUNC void gen_vla_alloc(CType *type, int align) {
	printf( "gen_vla_alloc( %d );\n", align );
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
	tcc_error( "gen_bounded_ptr_add() is not implemented\n" );
}

/* patch pointer addition in vtop so that pointer dereferencing is
   also tested */
ST_FUNC void gen_bounded_ptr_deref(void)
{
	tcc_error( "gen_bounded_ptr_deref() is not implemented\n" );
}
#endif


#endif
