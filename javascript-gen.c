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

/*
  DISCLAIMER: THIS IS NOT INTENDED AS A PERFORMANT C TO JAVASCRIPT COMPILER.
  Just an educational target for TCC.  You can use this to understand how to
  write your own target in TCC.

  This is a pretend processor.  Really geared for a bare-minimum implementation
  of a TCC target.  It's inbetween a processor and straight javascript. It was
  so I could figure out how to write target architecutres for TCC.  Hopefully
  it also can serve as a basis for other people to write their own targets for
  TCC.  Specifically, I was trying to port to an xtensa processor, so you will
  see a lot of stuff here geared specficially so I could head in that
  direction.

  The design of this "javascript" processor is to have a fixed memory size,
  and the stack + heap + globals live within it.  

*/

#ifdef TARGET_DEFS_ONLY

#define R_JS_CODE_ABS32 0x33
#define R_JS_DATA_ABS32 0x34

/* Let's pretend we have 4 registers that could be floating point OR intege. */

#define RC_INT     0x000001 /* generic integer register */
#define RC_FLOAT   0x000002 /* generic float register [NOT IMPLEMENTED] */
#define RC_A0      0x000010
#define RC_A1      0x000020
#define RC_A2      0x000040
#define RC_A3      0x000080
#define RC_A4      0x000100
#define RC_LR      0x000200
#define NB_REGS         6

#include "tcc.h"

#define REGSIZE 	4	/* Size of register or basic unit in bytes */

#define LDOUBLE_ALIGN	4
#define LDOUBLE_SIZE	8
#define PTR_SIZE	4
#define MAX_ALIGN	8

#define dbginfo(x...) printf(x)

enum {
	TREG_A0 = 0,
	TREG_A1,
	TREG_A2,
	TREG_A3,
	TREG_A4,
	TREG_LR,
};

#define REG_IRET   TREG_A0
#define REG_LRET   TREG_A1
#define REG_FRET   TREG_A3

#define RC_IRET    RC_A0	/* function return: integer register */
#define RC_LRET    RC_A1	/* function return: second integer register */
#define RC_FRET    RC_A3	/* function return: float register */

#else

#include "tcc.h"
#include <stdarg.h>

ST_DATA CType func_ret_type;
ST_DATA unsigned long func_sub_sp_offset;
ST_DATA int func_align;


/* This reg_classes array is used by tccgen.c for a allocating registers for
   the load/store functions. */
ST_DATA const int reg_classes[NB_REGS] = {
	RC_INT | RC_A0,
	RC_INT | RC_A1,
	RC_INT | RC_A2,
	RC_FLOAT | RC_A3,
	RC_FLOAT | RC_A4,
	RC_INT | RC_LR,
};

/* USEFUL STUFF:

"goto" and JavaScript:
    TODO: Write this section.

Patching, etc.

	t = usually the symbol used for "hey, TCC, remember this address to patch
		when you get done and come back around.

	ind = current output locaiton, usually used to patch variables.

	patching variables is done by seeking back to the last thing needing
	patching, and looking at its value.  If 0, then keep patching up the
	chain.



*/


/* The g / gprintf functions are for emitting code.  This is normally done for
   assembly opcodes, however, we can abuse it and output plain text into ELF 
   sections. g updates 'ind' which is a pointer to where we are in the section
   using this, we can update positions. */
ST_FUNC void g(char c)
{
	int ind1;


	if (nocode_wanted)
		return;
	ind1 = ind + 1;
	if (ind1 > cur_text_section->data_allocated)
	{
		section_realloc(cur_text_section, ind1);
	}
	cur_text_section->data[ind] = c;
	ind = ind1;
}


/* This is a convenience function that mimics printf, but lets us output
   directly to the data section. */
ST_FUNC void gprintf( const char * format, ... )
{
	char buffer[1024];
	char * buffptr = &buffer[0];
	char c;
	int len;
	va_list v;
	va_start( v, format );
	len = vsnprintf( buffer, sizeof( buffer ) - 1, format, v );
	if( len < 0 )
	{
		tcc_error( "produced line is too long for outputting." );
	}
	buffer[len] = 0;

	while( ( c = (*(buffptr++)) ) )
	{
		g( c );
		putchar( c );
	}
}

/* output a symbol and patch all calls to it */
ST_FUNC void gsym_addr(int t, int a)
{
	/* This looks a little weird but handles patching.  This is so you can say
	   "I'm going to update a symbol, somewhere in the future.  You don't have
	   to know where you'll be jumping exactly.  Once the target is generated,
	   this code will go back and update refrences to it.  Also, it's treated
	   kind of like a linked list.  If you go back and patch a value, but it's
	   nonzero, then it's actually pointing FURTHER back, so you'll have to
	   patch it's target as well.
	*/

	while (t) {
		unsigned char *ptr = cur_text_section->data + t;
		uint32_t n;  /* next value */
		char buff[11];
		int r;

		/* We look back and see if the value we're patching points back even
		   further.  We can see this because we use an eight-digit hex number
		   to represent states. */
		r = sscanf(ptr, "%08x", &n);

		if( r < 1 )
			tcc_error( "Can't patch %d pointer at bad location: %d: %s", ind, 
				t, ptr ); 

		/* Overwrite the text where it was with the new target location. 
		   normally, you can just overwrite the raw data, but JS is human-
		   readable. */
		sprintf( buff, "%08x", a );
		memcpy( ptr, buff, 8 );
		t = n;
	}

	/* vvv what does greloc do?  I gotta figure out how this works. */
	/* 
	 greloc(cur_text_section, sym, t, R_JS_CODE_ABS32); //S, Sym, Offset, Type
	*/

	if( !t )
	{
		gprintf( "	case 0x%08x:\n", ind );
	}
	else
	{
		gprintf( "	case 0x%08x: //Patch %08x\n", ind, t );
	}

	/* XXX TODO: Also, patch the sym in the GOT/PLT table? */
}

ST_FUNC void gsym(int t)
{
	gsym_addr(t, ind);
}

/* store register/short term op 'r' in lvalue 'v' 
 This is usually after a mathematical operation. 

 The value used here in v->c.i indicates the last value passed into sym_push.
*/
ST_FUNC void store(int r, SValue *sv)
{
#if 0
	CType *type = &sv->type;
	//int t = type->t;
	int size = type_size(type, &func_align);
	int is_mem = 0;
	int is_stack = 0;
	int memaddy = 0;
	int valtype = sv->r & VT_VALMASK; //or should this use 	//int t = type->t;?

	size = ( size+ (REGSIZE-1) ) & (~ (REGSIZE-1) ); //This handles rounding up to the closest next unit.

	if( valtype == VT_LOCAL || valtype == VT_LLOCAL ) { is_mem = 1; is_stack = 1; memaddy = sv->c.i; }

	if( is_mem )
	{
		//XXX TODO: check for t == VT_FLOAT, t == VT_DOUBLE
		if( size == 4 )
		{
			gprintf( "	tcc_write( %s%d, %d, cpua%d );\n", is_stack?"cpusp":"", memaddy, size, r );
		}
		else if( size == 8 )
		{
			gprintf( "	tcc_write( %s%d, %d, cpua%d );\n", is_stack?"cpusp":"", memaddy, size, r );
			gprintf( "	tcc_write( %s%d, %d, cpua%d );\n", is_stack?"cpusp":"", memaddy+4, size, sv->r2 );
		}
		else
		{
			tcc_error( "Variable type (%d) not supported.", size );
		}
		return;
	}
	else if( valtype == VT_CONST )
	{
		tcc_error( "Cannot store constants. [%x] [%d]", sv->r, sv->c.i );
	}
	else
	{
		tcc_error( "not mem store %d (r=%d) is such a thing even possible?\n", sv->r, r );
	}

	//dbginfo( "store( %d -> %d %ld reg %d TYPE %d)\n", r, v->type.t, v->c.i, v->r, v->type.t & VT_BTYPE );

void store(int r, SValue *sv)
{
#endif
	SValue v1;
	int v, ft, fc, fr, sign;

	fr = sv->r;
	ft = sv->type.t;
	fc = sv->c.i;

	if(fc>=0)
		sign=0;
	else {
		sign=1;
		fc=-fc;
	}

	v = fr & VT_VALMASK;
	if (fr & VT_LVAL || fr == VT_LOCAL) {
		uint32_t base = 0x0; /* fp */
		if(v < VT_CONST) {
			base=v;
			v=VT_LOCAL;
			fc=sign=0;
		} else if(v == VT_CONST) {
			v1.type.t = ft;
			v1.r = fr&~VT_LVAL;
			v1.c.i = sv->c.i;
			v1.sym=sv->sym;
			load(TREG_LR, &v1);
			base = TREG_LR; /* lr */
			fc=sign=0;
			v=VT_LOCAL;
		}
		if(v == VT_LOCAL) {
			if(is_float(ft)) {
				//calcaddr(&base,&fc,&sign,1020,2);
				tcc_error( "Store float [r:%d fc:%d base:%d sign%d]\n", r, fc, base, sign );
			} else if((ft & (VT_BTYPE|VT_UNSIGNED)) == VT_BYTE || (ft & VT_BTYPE) == VT_SHORT) {
				//calcaddr(&base,&fc,&sign,255,0);
				tcc_error( "Store byte or short [r:%d fc:%d base:%d sign%d]\n", r, fc, base, sign );
			} else {
				gprintf( "	heap32[(%s%c%d)>>2] = cpua%d|0;\n", base?"cpua5":"cpusp", (sign)?'-':'+', fc, r );
			}
			return;
		}
	}
	tcc_error("store unimplemented");
}

/* */
ST_FUNC void load(int r, SValue *sv)
{
#if 0
	CType *type = &sv->type;
	int size = type_size(type, &func_align);
	int is_mem = 0;
	int is_stack = 0;
	int memaddy = 0;
	int valtype = sv->r & VT_VALMASK; //or should this use 	//int t = type->t;?

	//dbginfo( "load ( %d -> %d %ld reg %d/%d TYPE %d)\n", r, sv->type.t, sv->c.i, sv->r, sv->r2, sv->type.t & VT_BTYPE );



	size = ( size+ (REGSIZE-1) ) & (~ (REGSIZE-1) ); //This handles rounding up to the closest next unit.

	if( valtype == VT_LOCAL || valtype == VT_LLOCAL  ) { is_mem = 1; is_stack = 1; memaddy = sv->c.i; }

	//printf( "LTYPE: %d\n", type->t );
	if( is_mem )
	{
		//XXX TODO: check for t == VT_FLOAT, t == VT_DOUBLE
		if( size == 4 )
		{
			gprintf( "	cpua%d = tcc_readreg( %s%d );\n", r, is_stack?"cpusp":"", memaddy );
		}
		else if( size == 8 )
		{
			gprintf( "	cpua%d = tcc_readreg( %s%d );\n", r, is_stack?"cpusp":"", memaddy );
			gprintf( "	cpua%d = tcc_readreg( %s%d );\n", sv->r2, is_stack?"cpusp":"", memaddy+4 );
		}
		else
		{
			tcc_error( "Error: variable type (%d) not supported for register loading.", size );
		}
		return;
	}
	else if( ( sv->r & VT_VALMASK) == VT_CONST )
	{
		/* Got a constant.  Note that this is straight assigning a constant to
		   a register, unlike when an immediate const is places after an
		   operator. */
		if( sv->r & VT_LVAL_UNSIGNED )
			gprintf( "	cpua%d = %u;\n", r, sv->c.i );
		else
			gprintf( "	cpua%d = %d;\n", r, sv->c.i );
	}
	else if( ( sv->r & VT_VALMASK ) == VT_JMP || ( sv->r & VT_VALMASK ) == VT_JMP )
	{
		gprintf( "	//JMP Placeholder (no code needed)\n" );
	}
	else if( sv->r & VT_SYM )
	{
		gprintf( "VTSYM  load %04x %04x  %p %d   %p\n", sv->r, sv->r2, type->ref, size, sv->sym );
		gprintf( "	cpu%d = tcc_readreg( 0x" );
	    greloc(cur_text_section, sv->sym, ind, R_JS_DATA_ABS32);	// rem the inst need to be patched
		gprintf( "00000000);\n", r );
	}
	else
	{
		gprintf( "Unknown load mechanism %04x %04x  %p %d   %p\n", sv->r, sv->r2, type->ref, size, sv->sym );
	}
#endif
	int v, ft, fc, fr, sign;
	SValue v1;
	ft = sv->type.t;
	int size = type_size(&sv->type, &func_align);

	fr = sv->r;
	fc = sv->c.i;

	if(fc>=0)
		sign=0;
	else {
		sign=1;
		fc=-fc;
	}

	v = fr & VT_VALMASK;
	if (fr & VT_LVAL) {
		uint32_t base = 0; // fp
		if(v == VT_LLOCAL) {
			v1.type.t = VT_PTR;
			v1.r = VT_LOCAL | VT_LVAL;
			v1.c.i = sv->c.i;
			gprintf( "		//Doing a pre-load for LLOCAL.\n" );
			load(TREG_LR, &v1);
			gprintf( "		//Pre-load done.\n" );
			base = TREG_LR; /* lr */
			fc=sign=0;
			v=VT_LOCAL;
		} else if(v == VT_CONST) {
			v1.type.t = VT_PTR;
			v1.r = fr&~VT_LVAL;
			v1.c.i = sv->c.i;
			v1.sym=sv->sym;
			gprintf( "		//Doing a pre-load for VT_CONST.\n" );
			load(TREG_LR, &v1);
			gprintf( "		//Pre-load done.\n" );
			base = TREG_LR; /* lr */
			fc=sign=0;
			v=VT_LOCAL;
		} else if(v < VT_CONST) {
			base=v;
			fc=sign=0;
			v=VT_LOCAL;
		}

		if(v == VT_LOCAL) {
			if(is_float(ft)) {
				//calcaddr(&base,&fc,&sign,1020,2);
				tcc_error( "Load float [r:%d fc:%d base:%d sign%d]\n", r, fc, base, sign );
			} else if((ft & (VT_BTYPE|VT_UNSIGNED)) == VT_BYTE || (ft & VT_BTYPE) == VT_SHORT) {
				//calcaddr(&base,&fc,&sign,255,0);
				gprintf( "	cpua%d = (heap32[(%s%c%d)>>2]>>(((%s%c%d)&3)*8))&0xff;\n", r, base?"cpua5":"cpusp", (sign)?'-':'+', fc, base?"cpua5":"cpusp", (sign)?'-':'+', fc );
				//tcc_error( "Load byte or short [r:%d fc:%d base:%d sign%d]\n", r, fc, base, sign );
			} else {
				gprintf( "	cpua%d = heap32[(%s%c%d)>>2]|0;\n", r, base?"cpua5":"cpusp", (sign)?'-':'+', fc );
			}
			return;
		}
	} else {
		if (v == VT_CONST) {
			if (fr & VT_SYM )
			{
				int backupind = 0;
				gprintf( "	cpua%d = /*read indirect from*/ ( 0x", r );
				if(fr & VT_SYM)
				{
					backupind = ind;
					greloc(cur_text_section, sv->sym, ind, R_JS_DATA_ABS32);
				}
				gprintf( "%08x ); /* greloc at: %x (fc=%d, sign=%d) */\n", sv->c.i, backupind, fc, sign );
			}
			else
			{
				//gprintf( "Direct constant load [r:%d fc:%d sv->c.i:%d sign:%d]\n", r, fc, sv->c.i, sign );
				gprintf( "	cpua%d = %c%d|0;\n", r, sign?'-':' ', fc );
			}
			return;
		} else if (v == VT_LOCAL) {
			gprintf( "Load LOCAL [r:%d fc:%d sv->c.i:%d sign:%d]\n", r, fc, sv->c.i, sign );
			if (fr & VT_SYM ) {
				greloc(cur_text_section, sv->sym, ind, R_JS_DATA_ABS32);
			}
			return;
		} else if(v == VT_CMP) {
			gprintf( "Load VT_CMP [r:%d fc:%d sv->c.i:%d sign:%d]\n", r, fc, sv->c.i, sign );
			return;
		} else if (v == VT_JMP || v == VT_JMPI) {
			gprintf( "Load VT_JMP/JMPI[r:%d fc:%d sv->c.i:%d sign:%d]\n", r, fc, sv->c.i, sign );
			gsym(sv->c.i);
			return;
		} else if (v < VT_CONST) {
			if(is_float(ft))
			{
				gprintf( "Load [last] float [r:%d fc:%d sv->c.i:%d sign:%d]\n", r, fc, sv->c.i, sign );
			}
			else
			{
				//gprintf( "	cpua%d = tcc_readreg( cpua%d ); //XXX TODO: Check parameters. [%d %d %d %d %d   size: %d]\n", r, fc, r, fc, sv->c.i, sign, v, size );
				gprintf( "	cpua%d = cpua%d;\n", r, fc );
				//gprintf( "Load [last] not float [r:%d fc:%d sv->c.i:%d sign:%d %d  V: %d]\n", r, fc, sv->c.i, sign, v );
			}
			return;
		}
	}
}


ST_FUNC int gjmp(int t)
{
	int ret = ind+11;		//Set patch address to current location + the offset to the "00000000" in code_.
	gprintf( "	state = 0x%08x|0; break; //gjmp(%d) r: %x t: %x vtop->c.i: %x %x\n", t, t, ind, t, vtop->c.i, vtop->r );
	return ret;
}

/* generate a jump to a fixed address */
ST_FUNC void gjmp_addr(int a)
{
	gjmp(a);
}


/* Generate a function call.  Once we're here, vtop contains a ton of info
   about our current state, i.e. every argument that needs to be passed in as
   well as the function call, itself! */
ST_FUNC void gfunc_call(int nb_args)
{
	int i;
	//greloc(cur_text_section, vtop->sym, ind, R_JS_CODE_ABS32);

	SValue * functop = &vtop[-nb_args];

	gprintf( "	%s( ", get_tok_str(functop->sym->v, NULL) );

	functop++;

	for( i = nb_args-1; i>=0; i-- )
	{
		SValue * vt = &vtop[-i];
		gprintf( "/*%d*/", i );
		int vtype = vt->r & VT_VALMASK;
		if( vtype == VT_CONST )
		{
			if( vt->r & VT_LVAL_UNSIGNED )
				gprintf( "0x%lx", vt->c.i );
			else
				gprintf( "0x%x", vt->c.i );
		}
		else if( vtype == VT_LOCAL )
		{
			int btype = ((vt->type.t)&VT_BTYPE);
			if( btype == VT_FLOAT )
			{
				tcc_error( "Float function param not implemented." );
			}
			else if( btype == VT_DOUBLE )
			{
				tcc_error( "Do func param not implemented." );
			}
			else
			{
				int reg = gv(RC_INT);
				gprintf( "cpua%d /* debug: %d */", reg, vt->r & ~VT_VALMASK );
			}
		}
		else
		{
			tcc_error( "Unkown parameter type %d\n", vtop->r  );
		}
		gprintf( "%c", (i != nb_args - 1)?',':' ');
	}

	vtop = functop - 1; /* Fully pop function call off stack */

	gprintf( ");\n" );
}


ST_FUNC void gfunc_prolog(CType *func_type)
{
	Sym *sym,*sym2;
//	int n, nf, size, align, rs, struct_ret = 0;
	int rs;
	int n = 0;

	sym = func_type->ref;

	//Tell ELF about this function call, so we get a symbol generated for us.
	put_elf_reloc(symtab_section, cur_text_section, ind, R_JS_CODE_ABS32, 0);

	gprintf( "\nfunction %s(", funcname );

	func_vt = sym->type;
	func_var = (sym->f.func_type == FUNC_ELLIPSIS);

	if( func_var )
	{
		tcc_error( "error: javascript port does not support ellipsis yet.\n" );
		return;
	}

	{
	        char * ptr = section_ptr_add(data_section, 6);
        	memcpy(ptr, "_start", 6);
	}


	//dbginfo( "gfunc_prolog(%d %p  %p %d %d)\n", func_type->t, func_type->ref, sym, func_vt.t, func_var );

	func_sub_sp_offset = loc;
	loc = 0;

	if ((func_vt.t & VT_BTYPE) == VT_STRUCT)
	{
		if( gfunc_sret(&func_vt, func_var, &func_ret_type, &func_align, &rs) )
		{
			dbginfo( "TODO: FIXME Returning a struct... or is it an ellipisis Gonna need to figure this stuff out.  Wait.  how big is it??\n" );
			n++;
			//struct_ret = 1;
			//func_vc = 12; /* Offset from fp of the place to store the result */
		}
	}
	else
	{
		func_ret_type = func_vt;
	}

	//Step through all parameters to this function.
	for(sym2 = sym->next; sym2; sym2 = sym2->next)
	{
		CType *type = &sym2->type;
		int size = type_size(type, &func_align); //Size, in bytes.  This does not handle rounding up.
		size = ( size+ (REGSIZE-1) ) & (~ (REGSIZE-1) ); //This handles rounding up to the closest next unit.

		//Need to be careful here:  We will need to select each value on up, unless it doesn't fit in the provided register space.
		//dbginfo( "symming: %p -> %d %d\n", sym2, size, n );

		gprintf( " %s%c", get_tok_str( sym2->v & ~SYM_FIELD, 0 ), (sym2->next)?',':' ' );

		loc -= size;

		sym_push(sym2->v & ~SYM_FIELD, type, VT_LOCAL | lvalue_type(type->t), loc );
	}
	gprintf( ") //Loc in: %d, vtop: %p\n{\n	tccprolog();\n", loc, vtop );

	//First, load these values onto the stack.
	for(sym2 = sym->next; sym2; sym2 = sym2->next)
	{
		//This is particularly awful.  But! We want to do two things:
		// (1) Support the javascript ABI so you can straight up call C functions.
		// (2) Use the stack stuff TCC does, so we can test what it would really be like.
		CType *type = &sym2->type;
		int size = type_size(type, &func_align);
		size = ( size+ (REGSIZE-1) ) & (~ (REGSIZE-1) ); //This handles rounding up to the closest next unit.
		if( type->t == VT_FLOAT )
		{
			//gprintf( "\ttcc_push( %d, float32touint32( %s ), %d );\n", 4, get_tok_str( sym2->v & ~SYM_FIELD, 0 )  );
			gprintf( "	heap32[(cpuspl-=4)>>2] = float32touint32( %s );\n", get_tok_str( sym2->v & ~SYM_FIELD, 0 ) );
		}
		else if( type->t == VT_DOUBLE )
		{
			//gprintf( "\ttcc_push( %d, float64touint64( %s ), %d );\n", 8, get_tok_str( sym2->v & ~SYM_FIELD, 0 ) );
			gprintf( "	var stage64 = float64touint64( %s );\n", get_tok_str( sym2->v & ~SYM_FIELD, 0 ) );
			gprintf( "	heap32[(cpuspl-=4)>>2] = stage64 & 0xffffffff;\n" );
			gprintf( "	heap32[(cpuspl-=4)>>2] = stage64 >> 32;\n" );
		}
		else  //Uint8array or actual number.
		{
			gprintf( "	heap32[(cpuspl-=4)>>2] = %s;\n", get_tok_str( sym2->v & ~SYM_FIELD, 0 ) );
			//gprintf( "\ttcc_push( %d, %s );\n", size, get_tok_str( sym2->v & ~SYM_FIELD, 0 ) );
		}
	}

	gprintf( "	//Current loc: %d\n", loc );

	gprintf( "	var state = 0|0;\n" );
	gprintf( "	looptop: do { switch( state ) {\n" );
	gprintf( "	case 0x00000000:	//Default function state\n" );

}

/* generate function epilog */
ST_FUNC void gfunc_epilog(void)
{
	//Use func_ret_type, too!!!
	//ind -= 4;

	gprintf( "	break looptop; } } while( true );\n" );

	//XXX TODO: How do we return structures?
	//XXX TODO: Right now this doesn't actually return anything.
	if( (func_ret_type.t&VT_BTYPE) == VT_VOID )
	{
		//Do nothing
	}
	else if( (func_ret_type.t&VT_BTYPE) == VT_FLOAT )
	{
		gprintf( "	return cpua0;\n" ); //TODO
	}
	else if( (func_ret_type.t&VT_BTYPE) == VT_DOUBLE )
	{
		gprintf( "	return cpua0;\n" ); //TODO
	}
	else
	{
		gv(RC_INT);
		gprintf( "	return cpua%d; //Type: %d\n", vtop[0].r, func_ret_type.t );
	}
	gprintf( "} //stack loc: %d original stack: %d vtop: %p\n", loc, func_sub_sp_offset, vtop );

	loc = func_sub_sp_offset;
}

static const char * mapcc( int cc )
{
	switch( cc )
	{
		case TOK_ULT:	return "	cpua%d = (cpua%d %s< %s%d)?1:0;\n";
		case TOK_UGE:	return "	cpua%d = (cpua%d %s>= %s%d)?1:0;\n";
		case TOK_EQ:	return "	cpua%d = (cpua%d %s== %s%d)?1:0;\n";
		case TOK_NE:	return "	cpua%d = (cpua%d %s!= %s%d)?1:0;\n";
		case TOK_ULE:	return "	cpua%d = (cpua%d %s<= %s%d)?1:0;\n";
		case TOK_UGT:	return "	cpua%d = (cpua%d %s> %s%d)?1:0;\n";
		case TOK_LT:	return "	cpua%d = (utos32(cpua%d) %s< utos32(%s%d))?1:0;\n";
		case TOK_GE:	return "	cpua%d = (utos32(cpua%d) %s>= utos32(%s%d))?1:0;\n";
		case TOK_LE:	return "	cpua%d = (utos32(cpua%d) %s<= utos32(%s%d))?1:0;\n";
		case TOK_GT:	return "	cpua%d = (utos32(cpua%d) %s> utos32(%s%d))?1:0;\n";
	}
	tcc_error( "Error: mapcc operator not implemneted (%02x)\n", cc );
	return "";
}

#if 0
//XXX TODO: This function should be used when 'inv' is sent.
static int negcc(int cc)
{
  switch(cc)
  {
	case TOK_ULT:	return TOK_UGE;
	case TOK_UGE:	return TOK_ULT;
	case TOK_EQ:	return TOK_NE;
	case TOK_NE:	return TOK_EQ;
	case TOK_ULE:	return TOK_UGT;
	case TOK_UGT:	return TOK_ULE;
	case TOK_Nset:	return TOK_Nclear;
	case TOK_Nclear:	return TOK_Nset;
	case TOK_LT:	return TOK_GE;
	case TOK_GE:	return TOK_LT;
	case TOK_LE:	return TOK_GT;
	case TOK_GT:	return TOK_LE;
  }
  tcc_error("unexpected condition code");
  return TOK_NE;
}
#endif

/* generate a test. set 'inv' to invert test. Stack entry is popped */
int gtst(int inv, int t)
{
	int v = vtop->r & VT_VALMASK;
	int r = ind;

//	gprintf( "in T: %d\n", t );
/*  int v, r;
	int p;
          p = vtop->c.i;

  v = vtop->r & VT_VALMASK;
  r=ind;
printf( "gtst(%d, %d, %d, %d  %d %d)\n",v, inv, t, ind, vtop->r, nocode_wanted );
   */
	if (nocode_wanted)
	{
		;
	}
	else if (v == VT_CMP)
	{
		//const char * op=mapcc(inv?negcc(vtop->c.i):vtop->c.i);
		//printf( "Encode branch CMP: %d %d <<%s>>   %d\n", r, t, op, vtop->c.i );
		gprintf( "	if( cpua0 %c= 0 )   state = 0x00000000|0; break; //r: %x t: %x vtop->c.i: %x %x\n", inv?'=':'!', r, t, vtop->c.i, vtop->r );
		t = r+30;
		gprintf( "	//Setting t = %d\n", t );

	}
	else if (v == VT_JMP || v == VT_JMPI)
	{
		if ((v & 1) == inv)
		{
			if(!vtop->c.i)
				vtop->c.i=t;
			else
			{
				uint32_t *x;
				int p;
				if(t) {
					p = vtop->c.i;
					/*do
					{
						p = decbranch(lp=p);
					} while(p);
					x = (uint32_t *)(cur_text_section->data + lp);
					*x &= 0xff000000;
					*x |= encbranch(lp,t,1);*/
					tcc_error( "TODO: Encode branch JMP: %p %d %d\n", x, p, t );
				}
				t = vtop->c.i;
				gprintf( "	//Setting t = %d\n", t );
			}
		}
		else
		{
			t = gjmp(t);
			gprintf( "	//Setting t = %d\n", t );
			gsym(vtop->c.i);
		}
	}
	gprintf( "	//gtst(%d, %d, %d  vtop->r=%02x %d ++ %d ++ %lu) (probably no code needed?)\n", inv, t, ind, vtop->r, nocode_wanted,     vtop->type.t, vtop->c.i );
	vtop--;
	return t;
}

/* generate an integer binary operation */
void gen_opi(int op)
{
	//int r, fr, opc;
	//Only supports binomial operators.
	/* Other interesting things to look at:
		gv(RC_INT);
		vswap();
		c=intr(gv(RC_INT));
	*/
#if 1
	int c;
	uint32_t r, fr;
	unsigned short retreg = REG_IRET;
	const char * ops = "";

	//printf( "GOT OP: %d %c\n", op, op );
	const char * outputenc = "	cpua%d = cpua%d %s %s%d;\n";

	c=0;
	switch(op) {
	case '+': 	ops = "+"; 	c=1;	break;
	case TOK_ADDC1: ops = "addc";	c=1;	break;
	case '-':	ops = "-";	c=1;	break;
	case TOK_SUBC1: /* sub with carry generation */	ops = "subc";	c=1;	break;
	case TOK_ADDC2: /* add with carry use */	ops = "addcu";	c=1;	break;
	case TOK_SUBC2: /* sub with carry use */	ops = "subcu";	c=1;	break;
	case '&':	ops = "&";	c=1;	break;
	case '^':	ops = "^";	c=1;	break;
	case '|':	ops = "|";	c=1;	break;
	case '*':	ops = "*";	c=1;	break;
	case TOK_SHL:	ops = "<<";	c=2;	break;
	case TOK_SHR:	ops = ">>";	c=2;	break;
	case TOK_SAR:	ops = "<<<";	c=2;	break;
	case '/':
	case TOK_PDIV:  ops = "/";	c=3;	break;
	case TOK_UDIV:	ops = "udiv";	c=3;	break;
	case '%':	ops = "%";	c=3;	break;
	case TOK_UMOD:	ops = "umod";	c=3;	break;
	case TOK_UMULL: ops = "umul";	c=3;	break;
	default:	
		outputenc = mapcc(op);
		ops = "";
		c=1;
		break;
	}

	switch(c) {
	case 1:
		if ((vtop->r & VT_VALMASK) == VT_CMP || (vtop->r & (VT_VALMASK & ~1)) == VT_JMP)
		{
			printf( "GVMASK\n" );
			gv(RC_INT);
		}
//		printf( "vtop R: %x\n", vtop->r );
//		printf( "vtop R: %x\n", vtop->r );
		vswap();
		c=gv(RC_INT);
		vswap();
		if((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
			r = vtop[-1].r;
			gprintf( outputenc, retreg, r, ops, "", vtop->c.i );
			goto done;
		}

//		printf( "Got V: %d\n", c );
//		printf( "vtop R: %x\n", vtop->r );
		fr=gv(RC_INT);
		r=vtop[-1].r;
		gprintf( "		//---%d / %d- %s--\n", c, fr, ops );
		gprintf( outputenc, retreg, r, ops, "cpua", fr );
		
done:
		vtop--;
		if (op >= TOK_ULT && op <= TOK_GT) {
			vtop->r = VT_CMP;
			vtop->c.i = op;
		}
		break;
	case 2:
		if ((vtop->r & VT_VALMASK) == VT_CMP ||
			(vtop->r & (VT_VALMASK & ~1)) == VT_JMP)
			gv(RC_INT);
		vswap();
		r=gv(RC_INT);
		vswap();
		//opc|=r;
		if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
			fr=vtop[-1].r;
			c = vtop->c.i & 0x1f;
		} else {
			fr=gv(RC_INT);
			c=vtop[-1].r;
		}

		//XXX TODO: Look into how intr,get_reg_ex works, and how it re-assigns registers.
		gprintf( "-_-%d / %d- %s--\n", c, fr, ops );
		vtop--;
		break;
	case 3:
		/*
		This is how you would operate a function call.
			vpush_global_sym(&func_old_type, func);
			vrott(3);
			gfunc_call(2);
			vpushi(0);
			vtop->r = retreg;
		*/

		break;
	default:
		tcc_error("gen_opi %i unimplemented!",op);
	}
#endif

#if 0
	int r, fr;
        gv2(RC_INT, RC_INT);  //Expecting operands in integer registers, only.

	r = vtop[-1].r;
	fr = vtop[0].r;

	//printf( "gen_opi( %d('%c') ... %d %d )\n", op, op, r, fr ); //Note r,fr == order that operands will work in?  Or are they registers corresponding to the "r" in load/store?
	switch( op )
	{
	case '+':
	case '-': case '/': case '%': case '*': case '&': case '|':
	case '^':
		gprintf( "	cpua0 = cpua%d %c cpua%d;\n", r, op, fr );
		break;
	case TOK_ULT:	case TOK_UGE:	case TOK_EQ:	case TOK_NE:
	case TOK_ULE:	case TOK_UGT:	case TOK_LT:	case TOK_GE:
	case TOK_LE:	case TOK_GT:
		gprintf( mapcc(op), r, fr );
		break;

	default: tcc_error( "Token 0x%02x not implemented on javascript.\n", op ); break;
	}

	vtop--;

	//I do not understand the following code.
        if (op >= TOK_ULT && op <= TOK_GT) {
		gprintf( "TOK: %d >= %d <= %d\n", op, TOK_ULT, TOK_GT );
            vtop->r = VT_CMP;
            vtop->c.i = op;
        }
#endif

}


void ggoto(void)
{
	tcc_error( "goto not implemented in javascript" );
}

/* Return the number of registers needed to return the struct, or 0 if
   returning via struct pointer. */
ST_FUNC int gfunc_sret(CType *vt, int variadic, CType *ret, int *ret_align, int *regsize)
{
	printf( "gfunc_sret\n" );
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
