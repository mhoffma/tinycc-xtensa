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

#define RC_INT     0x0001 /* generic integer register */
#define RC_FLOAT   0x0002 /* generic float register */
#define RC_A0      0x0004
#define RC_A1      0x0008
#define RC_A2      0x0010
#define RC_A3      0x0020
#define RC_A4      0x0040
#define RC_A5      0x0080
#define RC_A6      0x0100
#define RC_A7      0x0200
#define RC_A8      0x0400
#define RC_A9      0x0800

#define NB_REGS         10

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
#if 0
    while (t) {
        unsigned char *ptr = cur_text_section->data + t;
        uint32_t n = read32le(ptr); /* next value */
        write32le(ptr, a - t - 4);
        t = n;
    }
#endif
	printf( "gsym_addr( %d, %d );\n", t, a );
}

ST_FUNC void gsym(int t)
{
    gsym_addr(t, ind);
}

ST_FUNC void g(int c)
{
/*    int ind1;
    if (nocode_wanted)
        return;
    ind1 = ind + 1;
    if (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    cur_text_section->data[ind] = c;
    ind = ind1;*/
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
#if 0
    int fr, bt, ft, fc;

#ifdef TCC_TARGET_PE
    SValue v2;
    v = pe_getimport(v, &v2);
#endif

    ft = v->type.t;
    fc = v->c.i;
    fr = v->r & VT_VALMASK;
    ft &= ~(VT_VOLATILE | VT_CONSTANT);
    bt = ft & VT_BTYPE;
    /* XXX: incorrect if float reg to reg */
    if (bt == VT_FLOAT) {
        o(0xd9); /* fsts */
        r = 2;
    } else if (bt == VT_DOUBLE) {
        o(0xdd); /* fstpl */
        r = 2;
    } else if (bt == VT_LDOUBLE) {
        o(0xc0d9); /* fld %st(0) */
        o(0xdb); /* fstpt */
        r = 7;
    } else {
        if (bt == VT_SHORT)
            o(0x66);
        if (bt == VT_BYTE || bt == VT_BOOL)
            o(0x88);
        else
            o(0x89);
    }
    if (fr == VT_CONST ||
        fr == VT_LOCAL ||
        (v->r & VT_LVAL)) {
        gen_modrm(r, v->r, v->sym, fc);
    } else if (fr != r) {
        o(0xc0 + fr + r * 8); /* mov r, fr */
    }
#endif
}

ST_FUNC void load(int r, SValue *sv)
{
#if 0
    int v, t, ft, fc, fr;
    SValue v1;

#ifdef TCC_TARGET_PE
    SValue v2;
    sv = pe_getimport(sv, &v2);
#endif

    fr = sv->r;
    ft = sv->type.t & ~VT_DEFSIGN;
    fc = sv->c.i;

    ft &= ~(VT_VOLATILE | VT_CONSTANT);

    v = fr & VT_VALMASK;
    if (fr & VT_LVAL) {
        if (v == VT_LLOCAL) {
            v1.type.t = VT_INT;
            v1.r = VT_LOCAL | VT_LVAL;
            v1.c.i = fc;
            fr = r;
            if (!(reg_classes[fr] & RC_INT))
                fr = get_reg(RC_INT);
            load(fr, &v1);
        }
        if ((ft & VT_BTYPE) == VT_FLOAT) {
            o(0xd9); /* flds */
            r = 0;
        } else if ((ft & VT_BTYPE) == VT_DOUBLE) {
            o(0xdd); /* fldl */
            r = 0;
        } else if ((ft & VT_BTYPE) == VT_LDOUBLE) {
            o(0xdb); /* fldt */
            r = 5;
        } else if ((ft & VT_TYPE) == VT_BYTE || (ft & VT_TYPE) == VT_BOOL) {
            o(0xbe0f);   /* movsbl */
        } else if ((ft & VT_TYPE) == (VT_BYTE | VT_UNSIGNED)) {
            o(0xb60f);   /* movzbl */
        } else if ((ft & VT_TYPE) == VT_SHORT) {
            o(0xbf0f);   /* movswl */
        } else if ((ft & VT_TYPE) == (VT_SHORT | VT_UNSIGNED)) {
            o(0xb70f);   /* movzwl */
        } else {
            o(0x8b);     /* movl */
        }
        gen_modrm(r, fr, sv->sym, fc);
    } else {
        if (v == VT_CONST) {
            o(0xb8 + r); /* mov $xx, r */
            gen_addr32(fr, sv->sym, fc);
        } else if (v == VT_LOCAL) {
            if (fc) {
                o(0x8d); /* lea xxx(%ebp), r */
                gen_modrm(r, VT_LOCAL, sv->sym, fc);
            } else {
                o(0x89);
                o(0xe8 + r); /* mov %ebp, r */
            }
        } else if (v == VT_CMP) {
            oad(0xb8 + r, 0); /* mov $0, r */
            o(0x0f); /* setxx %br */
            o(fc);
            o(0xc0 + r);
        } else if (v == VT_JMP || v == VT_JMPI) {
            t = v & 1;
            oad(0xb8 + r, t); /* mov $1, r */
            o(0x05eb); /* jmp after */
            gsym(fc);
            oad(0xb8 + r, t ^ 1); /* mov $0, r */
        } else if (v != r) {
            o(0x89);
            o(0xc0 + r + v * 8); /* mov v, r */
        }
    }
#endif
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
#if 0
    int size, align, r, args_size, i, func_call;
    Sym *func_sym;
    
    args_size = 0;
    for(i = 0;i < nb_args; i++) {
        if ((vtop->type.t & VT_BTYPE) == VT_STRUCT) {
            size = type_size(&vtop->type, &align);
            /* align to stack align size */
            size = (size + 3) & ~3;
            /* allocate the necessary size on stack */
            oad(0xec81, size); /* sub $xxx, %esp */
            /* generate structure store */
            r = get_reg(RC_INT);
            o(0x89); /* mov %esp, r */
            o(0xe0 + r);
            vset(&vtop->type, r | VT_LVAL, 0);
            vswap();
            vstore();
            args_size += size;
        } else if (is_float(vtop->type.t)) {
            gv(RC_FLOAT); /* only one float register */
            if ((vtop->type.t & VT_BTYPE) == VT_FLOAT)
                size = 4;
            else if ((vtop->type.t & VT_BTYPE) == VT_DOUBLE)
                size = 8;
            else
                size = 12;
            oad(0xec81, size); /* sub $xxx, %esp */
            if (size == 12)
                o(0x7cdb);
            else
                o(0x5cd9 + size - 4); /* fstp[s|l] 0(%esp) */
            g(0x24);
            g(0x00);
            args_size += size;
        } else {
            /* simple type (currently always same size) */
            /* XXX: implicit cast ? */
            r = gv(RC_INT);
            if ((vtop->type.t & VT_BTYPE) == VT_LLONG) {
                size = 8;
                o(0x50 + vtop->r2); /* push r */
            } else {
                size = 4;
            }
            o(0x50 + r); /* push r */
            args_size += size;
        }
        vtop--;
    }
    save_regs(0); /* save used temporary registers */
    func_sym = vtop->type.ref;
    func_call = func_sym->f.func_call;
    /* fast call case */
    if ((func_call >= FUNC_FASTCALL1 && func_call <= FUNC_FASTCALL3) ||
        func_call == FUNC_FASTCALLW) {
        int fastcall_nb_regs;
        uint8_t *fastcall_regs_ptr;
        if (func_call == FUNC_FASTCALLW) {
            fastcall_regs_ptr = fastcallw_regs;
            fastcall_nb_regs = 2;
        } else {
            fastcall_regs_ptr = fastcall_regs;
            fastcall_nb_regs = func_call - FUNC_FASTCALL1 + 1;
        }
        for(i = 0;i < fastcall_nb_regs; i++) {
            if (args_size <= 0)
                break;
            o(0x58 + fastcall_regs_ptr[i]); /* pop r */
            /* XXX: incorrect for struct/floats */
            args_size -= 4;
        }
    }
#ifndef TCC_TARGET_PE
    else if ((vtop->type.ref->type.t & VT_BTYPE) == VT_STRUCT)
        args_size -= 4;
#endif
    gcall_or_jmp(0);

    if (args_size && func_call != FUNC_STDCALL && func_call != FUNC_FASTCALLW)
        gadd_sp(args_size);
    vtop--;
#endif
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








/* convert fp to int 't' type */
ST_FUNC void gen_cvt_ftoi(int t)
{
	printf( "gen_cvt_ftoi( %d )\n", t );
#if 0    int bt = vtop->type.t & VT_BTYPE;
    if (bt == VT_FLOAT)
        vpush_global_sym(&func_old_type, TOK___fixsfdi);
    else if (bt == VT_LDOUBLE)
        vpush_global_sym(&func_old_type, TOK___fixxfdi);
    else
        vpush_global_sym(&func_old_type, TOK___fixdfdi);
    vswap();
    gfunc_call(1);
    vpushi(0);
    vtop->r = REG_IRET;
    vtop->r2 = REG_LRET;
#endif
}

/* convert from one floating point type to another */
ST_FUNC void gen_cvt_ftof(int t)
{
	printf( "gen_cvt_ftof( %d )\n", t );
    /* all we have to do on i386 is to put the float in a register */
    //gv(RC_FLOAT);
}


