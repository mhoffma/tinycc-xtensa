/* 
 * Copyright (C) 2017 <>< Charles Lohr
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

#define RC_INT     0x000001 /* generic integer register */
#define RC_FLOAT   0x000002 /* generic float register [NOT IMPLEMENTED] */
#define RC_A0      0x000010
#define RC_A1      0x000020 /* Stack pointer */
#define RC_A2      0x000040 /* Function call registers */
#define RC_A3      0x000080 
#define RC_A4      0x000100
#define RC_A5      0x000200
#define RC_A6      0x000400
#define RC_A7      0x000800 
#define RC_A8      0x001000 /* Work registers */
#define RC_A9      0x002000
#define RC_A10     0x004000
#define RC_A11     0x008000
#define RC_A12     0x010000
#define RC_A13     0x020000
#define RC_A14     0x040000
#define RC_A15     0x080000
#define NB_REGS         16

#include "tcc.h"

#define REGSIZE 4 		//Size of register (in bytes)
#define NR_CALLREGS  6		//You have A2 to A7 to pass variables into functions.


#define LDOUBLE_ALIGN 4
#define LDOUBLE_SIZE 8   //XXX Is this correct? What does this do?
#define PTR_SIZE 4
#define MAX_ALIGN     8

#define dbginfo(x...) printf(x)

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
	TREG_A10,
	TREG_A11,
	TREG_A12,
	TREG_A13,
	TREG_A14,
	TREG_A15,
};

#define REG_IRET   TREG_A2
#define REG_LRET   TREG_A3
#define REG_FRET   0x80000000

#define REG_WORK1 TREG_A8


#define RC_IRET    RC_A2	/* function return: integer register */
#define RC_LRET    RC_A3	/* function return: second integer register */
#define RC_FRET    0x8000	/* function return: float register (not currently in use) */

//static uint8_t fastcall_regs[NR_CALLREGS] = { TREG_A2, TREG_A3, TREG_A4, TREG_A5, TREG_A6, TREG_A7 };
#else

#include "tcc.h"

ST_DATA const int reg_classes[NB_REGS] = {
	RC_FLOAT | RC_INT | RC_A0,
	RC_FLOAT | RC_INT | RC_A1,
	RC_FLOAT | RC_INT | RC_A2,
	RC_FLOAT | RC_INT | RC_A3,
	RC_FLOAT | RC_INT | RC_A4,
	RC_FLOAT | RC_INT | RC_A5,
	RC_FLOAT | RC_INT | RC_A6,
	RC_FLOAT | RC_INT | RC_A7,
	RC_FLOAT | RC_INT | RC_A8,
	RC_FLOAT | RC_INT | RC_A9,
	RC_FLOAT | RC_INT | RC_A10,
	RC_FLOAT | RC_INT | RC_A11,
	RC_FLOAT | RC_INT | RC_A12,
	RC_FLOAT | RC_INT | RC_A13,
	RC_FLOAT | RC_INT | RC_A14,
	RC_FLOAT | RC_INT | RC_A15,
};

ST_DATA unsigned long func_sub_sp_offset;


/* output a symbol and patch all calls to it */
ST_FUNC void gsym_addr(int t, int a)
{
	dbginfo( "gsym_addr( %d, %d );\n", t, a );
}

ST_FUNC void gsym(int t)
{
	dbginfo( "TODO gsym(%d)\n", t );
}

#if 0

ST_FUNC void g(int c)
{
	int ind1;
	if (nocode_wanted)
		return;
	ind1 = ind + 1;
	if (ind1 > cur_text_section->data_allocated)
		section_realloc(cur_text_section, ind1);
	dbginfo( "g(%x)\n", c );
	cur_text_section->data[ind] = c;
	ind = ind1;
}

#endif

#if 0
ST_FUNC void o(unsigned int c)
{
    while (c) {
        g(c);
        c = c >> 8;
    }
}
#endif
#if 0

/* type definition */
typedef struct CType {
    int t;
    struct Sym *ref;
} CType;


/* constant value */
typedef union CValue {
    long double ld;
    double d;
    float f;
    uint64_t i;
    struct {
        int size;
        const void *data;
    } str;
    int tab[LDOUBLE_SIZE/4];
} CValue;


typedef struct SValue {
    CType type;      /* type */
    unsigned short r;      /* register + flags */
    unsigned short r2;     /* second register, used for 'long long'
                              type. If not used, set to VT_CONST */
    CValue c;              /* constant, if VT_CONST */
    struct Sym *sym;       /* symbol, if (VT_SYM | VT_CONST), or if
    			      result of unary() for an identifier. */
} SValue;
#endif

/* store register/short term op 'r' in lvalue 'v' 
 This is usually after a mathematical operation. 

 The value used here in v->c.i indicates the last value passed into sym_push.
 
*/
ST_FUNC void store(int r, SValue *v)
{
	dbginfo( "store( %d -> %d %ld reg %d TYPE %d)\n", r, v->type.t, v->c.i, v->r, v->type.t & VT_BTYPE );
}

/* */
ST_FUNC void load(int r, SValue *sv)
{
	//XXX XXX Something is wrong. sv->c.i points to stack variables on stack OR stack-parameters.  Problem is right now, they can collide!!!
	dbginfo( "load ( %d -> %d %ld reg %d TYPE %d)\n", r, sv->type.t, sv->c.i, sv->r, sv->type.t & VT_BTYPE );
}

ST_FUNC int gjmp(int t)
{
	dbginfo( "gjmp( %d )\n", t );
//    return gjmp2(0xe9, t);
	return 0;
}

/* generate a jump to a fixed address */
ST_FUNC void gjmp_addr(int a)
{
	dbginfo( "gjmp_addr( %d )\n", a );

}



ST_FUNC void gfunc_call(int nb_args)
{
	dbginfo( "gfunc_call( nb_args = %d )\n", nb_args );
}


ST_FUNC void gfunc_prolog(CType *func_type)
{
	Sym *sym,*sym2;
//	int n, nf, size, align, rs, struct_ret = 0;
	int align;
	int rs;
	int n = 0;
	int param_reg = 4;

	CType ret_type;

	func_sub_sp_offset = ind;

	sym = func_type->ref;
	func_vt = sym->type;
	func_var = (func_type->ref->f.func_type == FUNC_ELLIPSIS);

	if( func_var )
	{
		tcc_error( "error: javascript port does not support ellipsis yet.\n" );
		return;
	}

	dbginfo( "gfunc_prolog(%d %p  %p %d %d)\n", func_type->t, func_type->ref, sym, func_vt.t, func_var );

	n = 0;

	if ((func_vt.t & VT_BTYPE) == VT_STRUCT)
	{
		if( gfunc_sret(&func_vt, func_var, &ret_type, &align, &rs) )
		{
			dbginfo( "Returning a struct... or is it an ellipisis Gonna need to figure this stuff out.  Wait.  how big is it??\n" );
			n++;
			//struct_ret = 1;
			//func_vc = 12; /* Offset from fp of the place to store the result */
		}
	}

	//Step through all parameters to this function.
	for(sym2 = sym->next; sym2; sym2 = sym2->next)
	{
		CType *type = &sym2->type;
		int size = type_size(type, &align);
		int param_addr;
		//Need to be careful here:  We will need to select each value on up, unless it doesn't fit in the provided register space.
		dbginfo( "symming: %p -> %d %d\n", sym2, size, n );

		size = (size+3) & (~3);
		if( (n + size)/REGSIZE > NR_CALLREGS )
		{
			loc -= size;
			param_addr = loc; //A stack place.
			//param_stack_addr += size;
		}
		else
		{
			param_addr = param_reg; //A register
			param_reg += size;
		}

		dbginfo( "Now n is %d/%d pa: %d\n", n/REGSIZE, NR_CALLREGS, param_addr );

		sym_push(sym2->v & ~SYM_FIELD, type, VT_LOCAL | lvalue_type(type->t), param_addr );
		n += size;
	}

	//Fix up the stack.  If parameters were passed in on the stack, that will make us need more room.
	//loc = -param_stack_addr-4;  //XXX THIS IS WRONG WRONG WRONG!!!

	//XXX TODO WORK ON STACK MANAGEMENT

}

/* generate function epilog */
ST_FUNC void gfunc_epilog(void)
{
	ind = func_sub_sp_offset;
	ind -= 4;
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

	//Only supports binomial operators.
	/* Other interesting things to look at:
		gv(RC_INT);
		vswap();
		c=intr(gv(RC_INT));
	*/

        gv2(RC_INT, RC_INT);  //Expecting operands in integer registers, only.
	int r = vtop[-1].r;
	int fr = vtop[0].r;
	vtop--;
	printf( "gen_opi( %d('%c') ... %d %d )\n", op, op, r, fr ); //Note r,fr == order that operands will work in?  Or are they registers corresponding to the "r" in load/store?
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
