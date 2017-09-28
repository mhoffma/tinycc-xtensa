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

  This is a pretend processor.  Really geared for a bare-minimum implementation
  of a TCC target.  It's a totally made up architecture, but exists mostly so
  I could figure out how to write target architecutres for TCC.  Hopefully it
  also can serve as a basis for other people to write their own targets for
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
#define NB_REGS         5

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

ST_DATA FILE * f_js;
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
};


/* The g / gprintf functions are for emitting code.  This is normally done for
   assembly opcodes, however, we can abuse it and output plain text into ELF 
   sections. */
ST_FUNC void g(char c)
{
	int ind1;

	if( !f_js )
	{
		FILE * f = fopen( "a.out.html", "w" );
		fprintf( f, "<HTML><HEAD><SCRIPT src=a.out.js></HEAD><BODY></BODY></HTML>" );
		fclose( f );
		f_js = fopen( "a.out.js", "w" );
		fprintf( f_js, 
"var cpusp = 1048576;\n"
"var cpua0 = 0|0;\n"
"var cpua1 = 0|0;\n"
"var cpua2 = 0.0;\n"
"var cpua3 = 0.0;\n"
"var memarray = new Uint8Array(stackpointer);\n"
"function tcc_write( location, size, value ) {\n"
"	if( typeof value === 'number' )\n"
"	{\n"
"		for( var i = 0; i < ((size&~3)+4)|0; i+=4 )\n"
"		{\n"
"			memarray[location + i] = value;\n"
"			value>>=32;\n"
"		}\n"
"	} else {\n"
"		for( var i = 0; i < ((size&~3)+4)|0; i++ )\n"
"		{\n"
"			var v = 0;\n"
"			for( var j = 0; j < 4; j++ )\n"
"			{\n"
"				v |= value[j+i*4]<<(j*8);\n"
"			}\n"
"			memarray[location + i] = v;\n"
"		}\n"
"	}\n"
"}\n"
"function tcc_writereg( location, value ) { tcc_write( location, 4 value ); }"
"function tcc_read( location, size, type )\n"
"{\n"
"	if( type === 'number' )\n"
"	{\n"
"		var ret = 0;\n"
"		for( var i = 0; i < ((size&~3)+4)|0); i+=4 )\n"
"		{\n"
"			ret <<= 32;\n"
"			ret |= memarray[location + i] = value;\n"
"		}\n"
"		return ret;\n"
"	} else {\n"
"		var ret = new Uint8Array( size );\n"
"		for( var i = 0; i < ((size&~3)+4)|0; i++ )\n"
"		{\n"
"			var v = memarray[location + i];\n"
"			for( var j = 0; j < 4; j++ )\n"
"			{\n"
"				v |= value[j+i*4]<<(j*8);\n"
"			}\n"
"		}\n"
"		return ret;\n"
"	}\n"
"}\n"
"function tcc_readreg( location ) { tcc_write( location, 4, value ); }"
"function tcc_pop( size, type )\n"
"{\n"
"	var ret = tcc_read( stackpointer, size, type );\n"
"	stackpointer += size;\n"
"	return ret;\n"
"}\n"
"function tcc_push( size, val )\n"
"{\n"
"	stackpointer -= size;\n"
"	var ret = tcc_write( stackpointer, size, val );\n"
"	return ret;\n"
"}\n"
"function float32touint32( f )\n"
"{\n"
"	var farr = new Float32Array(1);\n"
"	farr[0] = f;\n"
"	var iarr = new Uint32Array(farr.buffer);\n"
"	return iarr[0];\n"
"}\n"
"function uint32tofloat32( f )\n"
"{\n"
"	var iarr = new Uint32Array(1);\n"
"	iarr[0] = f;\n"
"	var farr = new Float32Array(iarr.buffer);\n"
"	return farr[0];\n"
"}\n"
"function float64touint64( f )\n"
"{\n"
"	var farr = new Float64Array(1);\n"
"	farr[0] = f;\n"
"	var iarr = new Uint64Array(farr.buffer);\n"
"	return iarr[0];\n"
"}\n"
"function uint64tofloat64( f )\n"
"{\n"
"	var iarr = new Uint64Array(1);\n"
"	iarr[0] = f;\n"
"	var farr = new Float64Array(iarr.buffer);\n"
"	return farr[0];\n"
"}\n"
"function utos32( f )\n"
"{\n"
"	return (new int32Array([f]))[0];\n"
"}\n"
 );
	}

	fputc( c, f_js );

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

#if 0
    while (t) {
        unsigned char *ptr = cur_text_section->data + t;
        uint32_t n = read32le(ptr); /* next value */
        write32le(ptr, a - t - 4);
        t = n;
    }
#endif

	//This looks a little weird but handles patching.
	while (t) {
		unsigned char *ptr = cur_text_section->data + t;
		uint32_t n;  /* next value */
		char buff[11];
		int r;
//		printf( "PTR TO PATCH: %p\n", ptr );
		r = sscanf(ptr, "%08x", &n);
		if( r < 1 )
			tcc_error( "Can't patch pointer at bad location: %s", ptr ); 
		//write32le(ptr, a - t - 4);
		//printf( "Patch: %s %d %d %d\n", ptr, a, t, n );
		sprintf( buff, "%08x", a );
		memcpy( ptr, buff, 8 );
		//printf( "::%s:%s::\n", ptr, buff );
		t = n;
	}

	{
		Sym *sym = get_sym_ref(&char_pointer_type, cur_text_section, a, 0);
		printf( "SYM: %p = %p, a: %p SYM:%p  T: %p  PLACE TO PATCH: %p\n", &char_pointer_type, cur_text_section, a, sym, t, ind );
	}
	//greloc(cur_text_section, sym, t, R_JS_CODE_ABS32); //S, Sym, Offset, Type

	if( !t )
	{
		gprintf( "code_%08x:\n", ind );
	}
	else
	{
		gprintf( "code_%08x: //Patch %08x\n", ind, t );

//	sprintf( &cur_text_section->data[offset], "%08x", newtarget );

		//gpatchptr( t, ind );
	}
	//Also, patch the sym.
	
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
	CType *type = &sv->type;
	//int t = type->t;
	int size = type_size(type, &func_align);
	int is_mem = 0;
	int is_stack = 0;
	int memaddy = 0;
	int valtype = sv->r & VT_VALMASK; //or should this use 	//int t = type->t;?

	//dbginfo( "load ( %d -> %d %ld reg %d/%d TYPE %d)\n", r, sv->type.t, sv->c.i, sv->r, sv->r2, sv->type.t & VT_BTYPE );


	size = ( size+ (REGSIZE-1) ) & (~ (REGSIZE-1) ); //This handles rounding up to the closest next unit.

	if( valtype == VT_LOCAL || valtype == VT_LLOCAL ) { is_mem = 1; is_stack = 1; memaddy = sv->c.i; }

	if( is_mem )
	{
		//XXX TODO: check for t == VT_FLOAT, t == VT_DOUBLE
		if( size == 4 )
		{
			gprintf( "	tcc_write( %s%d, %d, cpua%d );\n", is_stack?"stackpointer":"", memaddy, size, r );
		}
		else if( size == 8 )
		{
			gprintf( "	tcc_write( %s%d, %d, cpua%d );\n", is_stack?"stackpointer":"", memaddy, size, r );
			gprintf( "	tcc_write( %s%d, %d, cpua%d );\n", is_stack?"stackpointer":"", memaddy+4, size, sv->r2 );
		}
		else
		{
			tcc_error( "Variable type (%d) not supported.", size );
		}
		return;
	}
	else if( valtype == VT_CONST )
	{
		tcc_error( "Cannot store constants." );
	}
	else
	{
		tcc_error( "not mem store %d (r=%d) is such a thing even possible?\n", sv->r, r );
	}


	//dbginfo( "store( %d -> %d %ld reg %d TYPE %d)\n", r, v->type.t, v->c.i, v->r, v->type.t & VT_BTYPE );
}

/* */
ST_FUNC void load(int r, SValue *sv)
{
	CType *type = &sv->type;
	int size = type_size(type, &func_align);
	int is_mem = 0;
	int is_stack = 0;
	int memaddy = 0;
	int valtype = sv->r & VT_VALMASK; //or should this use 	//int t = type->t;?

	//dbginfo( "load ( %d -> %d %ld reg %d/%d TYPE %d)\n", r, sv->type.t, sv->c.i, sv->r, sv->r2, sv->type.t & VT_BTYPE );

	size = ( size+ (REGSIZE-1) ) & (~ (REGSIZE-1) ); //This handles rounding up to the closest next unit.

	if( valtype == VT_LOCAL || valtype == VT_LLOCAL  ) { is_mem = 1; is_stack = 1; memaddy = sv->c.i; }

	printf( "LTYPE: %d\n", type->t );
	if( is_mem )
	{
		//XXX TODO: check for t == VT_FLOAT, t == VT_DOUBLE
		if( size == 4 )
		{
			gprintf( "	cpua%d = tcc_readreg( %s%d );\n", r, is_stack?"stackpointer":"", memaddy );
		}
		else if( size == 8 )
		{
			gprintf( "	cpua%d = tcc_readreg( %s%d );\n", r, is_stack?"stackpointer":"", memaddy );
			gprintf( "	cpua%d = tcc_readreg( %s%d );\n", sv->r2, is_stack?"stackpointer":"", memaddy+4 );
		}
		else
		{
			//tcc_error( "Error: variable type (%d) not supported.", size );
			gprintf( "	//XXX TODO: FIXME Variable type %d\n", size );
		}
		return;
	}
	else if( ( sv->r & VT_VALMASK) == VT_CONST )
	{
		gprintf( "	cpua%d = %d;\n", r, sv->c.i );
	}
	else if( ( sv->r & VT_VALMASK ) == VT_JMP || ( sv->r & VT_VALMASK ) == VT_JMP )
	{	gprintf( "	//JMP Placeholder (no code needed)\n" );
	}
	else
	{
		tcc_error( "NOT MEM load %04x %04x  %p %d\n", sv->r, sv->r2, type->ref, size );
	}

}


ST_FUNC int gjmp(int t)
{
	gprintf( "gjmp( %d ) TODO\n", t );
//    return gjmp2(0xe9, t);
	return 0;
}

/* generate a jump to a fixed address */
ST_FUNC void gjmp_addr(int a)
{
	gprintf( "gjmp_addr( %d ) TODO\n", a );

}



ST_FUNC void gfunc_call(int nb_args)
{
	int i;
	printf( "PP: %p\n", vtop->sym );
	//greloc(cur_text_section, vtop->sym, ind, R_JS_CODE_ABS32);
	vtop--;
	for( i = 0 ; i < nb_args; i ++ )
	{
		vtop--;
	}
	gprintf( "gfunc_call( nb_args = %d )\n", nb_args );
}


ST_FUNC void gfunc_prolog(CType *func_type)
{
	Sym *sym,*sym2;
//	int n, nf, size, align, rs, struct_ret = 0;
	int rs;
	int n = 0;

	sym = func_type->ref;

	gprintf( "function %s(", funcname );

	func_vt = sym->type;
	func_var = (sym->f.func_type == FUNC_ELLIPSIS);

	if( func_var )
	{
		tcc_error( "error: javascript port does not support ellipsis yet.\n" );
		return;
	}

	{
	        const char * ptr = section_ptr_add(data_section, 6);
        	memcpy(ptr, "_start", 6);
	}


	//dbginfo( "gfunc_prolog(%d %p  %p %d %d)\n", func_type->t, func_type->ref, sym, func_vt.t, func_var );

	func_sub_sp_offset = loc;
	loc = -4;

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
	gprintf( ") //Loc in: %d, vtop: %p\n{\n", loc, vtop );

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
			gprintf( "\ttcpush( float32touint32( %s ), %d );\n",  get_tok_str( sym2->v & ~SYM_FIELD, 0 ), 4 );
		}
		else if( type->t == VT_DOUBLE )
		{
			gprintf( "\ttcpush( float64touint64( %s ), %d );\n",  get_tok_str( sym2->v & ~SYM_FIELD, 0 ), 8 );
		}
		else  //Uint8array or actual number.
		{
			gprintf( "\ttcpush( %s, %d );\n",  get_tok_str( sym2->v & ~SYM_FIELD, 0 ), size );
		}
	}

	gprintf( "	//Current loc: %d\n", loc );
}

/* generate function epilog */
ST_FUNC void gfunc_epilog(void)
{
	//Use func_ret_type, too!!!
	//ind -= 4;

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
		case TOK_ULT:	return "	cpua0 = (cpua%d < cpu%d)?1:0;\n";
		case TOK_UGE:	return "	cpua0 = (cpua%d >= cpu%d)?1:0;\n";
		case TOK_EQ:	return "	cpua0 = (cpua%d == cpu%d)?1:0;\n";
		case TOK_NE:	return "	cpua0 = (cpua%d != cpu%d)?1:0;\n";
		case TOK_ULE:	return "	cpua0 = (cpua%d <= cpu%d)?1:0;\n";
		case TOK_UGT:	return "	cpua0 = (cpua%d > cpu%d)?1:0;\n";
		case TOK_LT:	return "	cpua0 = (utos32(cpua%d) < utos32(cpu%d))?1:0;\n";
		case TOK_GE:	return "	cpua0 = (utos32(cpua%d) >= utos32(cpu%d))?1:0;\n";
		case TOK_LE:	return "	cpua0 = (utos32(cpua%d) <= utos32(cpu%d))?1:0;\n";
		case TOK_GT:	return "	cpua0 = (utos32(cpua%d) > utos32(cpu%d))?1:0;\n";
	}
	tcc_error( "Error: mapcc operator not implemneted (%02x)\n", cc );
	return "";
}

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

/* generate a test. set 'inv' to invert test. Stack entry is popped */
int gtst(int inv, int t)
{
	int v = vtop->r & VT_VALMASK;
	int r = ind;
	int p = vtop->c.i;
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
		gprintf( "	if( cpua0 != 0 )   goto code_00000000; //r: %x t: %x vtop->c.i: %x %x\n", r, t, vtop->c.i, vtop->r );
		t = r+30;
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
					printf( "Encode branch JMP: %p %d %d\n", x, p, t );
				}
				t = vtop->c.i;
			}
		}
		else
		{
			t = gjmp(t);
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
	int c, func = 0;
	uint32_t opc = 0, r, fr;
	unsigned short retreg = REG_IRET;
	const char * ops = "";

	printf( "GOT OP: %d %c\n", op, op );
	

	c=0;
	switch(op) {
	case '+': 	opc = '+'; 	c=1;	break;
	case TOK_ADDC1: ops = "addc";	c=1;	break;
	case '-':	opc = '-';	c=1;	break;
	case TOK_SUBC1: /* sub with carry generation */	ops = "subc";	c=1;	break;
	case TOK_ADDC2: /* add with carry use */	ops = "addcu";	c=1;	break;
	case TOK_SUBC2: /* sub with carry use */	ops = "subcu";	c=1;	break;
	case '&':	opc = '&';	c=1;	break;
	case '^':	opc = '^';	c=1;	break;
	case '|':	opc = '|';	c=1;	break;
	case '*':	opc = '*';	c=1;	break;
	case TOK_SHL:	ops = "<<";	c=2;	break;
	case TOK_SHR:	ops = ">>";	c=2;	break;
	case TOK_SAR:	ops = "<<<";	c=2;	break;
	case '/':
	case TOK_PDIV:  opc = '/';	c=3;	break;
	case TOK_UDIV:	ops = "udiv";	c=3;	break;
	case '%':	opc = '%';	c=3;	break;
	case TOK_UMOD:	ops = "umod";	c=3;	break;
	case TOK_UMULL: ops = "umul";	c=3;	break;
	default:	opc = '@';	c=1;	break;
	}

	switch(c) {
	case 1:
		if ((vtop->r & VT_VALMASK) == VT_CMP || (vtop->r & (VT_VALMASK & ~1)) == VT_JMP)
		{
			printf( "GVMASK\n" );
			gv(RC_INT);
		}
		printf( "vtop R: %x\n", vtop->r );
		vswap();
		printf( "vtop R: %x\n", vtop->r );
		c=gv(RC_INT);
		printf( "Got V\n" );
		vswap();
		printf( "vtop R: %x\n", vtop->r );
		if((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
			uint8_t cons = vtop->c.i;
			r=vtop[-1].r;
			gprintf( "__-%d / %d / %s [%d%c] %c // CONSTANT: %d \n", r, cons, ops, op,op, opc, vtop->c.i );
			goto done;
		}
		fr=gv(RC_INT);
		r=vtop[-1].r;
		gprintf( "---%d / %d- %c(0x%02x) %s--\n", c, fr, opc, opc, ops );
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
		opc|=r;
		if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
			fr=vtop[-1].r;
			c = vtop->c.i & 0x1f;
		} else {
			fr=gv(RC_INT);
			c=vtop[-1].r;
		}

		//XXX TODO: Look into how intr,get_reg_ex works, and how it re-assigns registers.
		gprintf( "-_-%d / %d- %c %s--\n", c, fr, opc, ops );
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
	printf( "ggoto()\n" );
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
