#!/bin/bash
make tcc
./tcc mytest.c -o a.out -shared -nostdlib -nostdinc
objcopy a.out -I elf32-little -j .text -O binary a.out.js

