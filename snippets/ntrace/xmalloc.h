#ifndef __XMALLOC_H__
#define __XMALLOC_H__ 1
/* 
   elmo - ELectronic Mail Operator

   Copyright (C) 2002, 2003 rzyjontko

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; version 2.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

   ----------------------------------------------------------------------

   1. What is this?

   These are wrappers for malloc-related functions.  They always check if
   corespondent function did not return NULL in which case they just finish
   program with error message.  They can also find memory leaks in your
   program.

   Wrappers require that you have defined a function:
       void error_critical (int status, int errno, const char *msg);

   This function should report error, and exit with given status code.


   2. How can I find my memory leaks?

   It is very simple.  Compile this module with -DXMALLOC_DEBUG option, then
   define environment variable XMALLOC_TRACE, so that it is a valid name of
   file which can be overwritten.  Run your program, and after it finishes
   use mtrace script (which is part of GNU libc).

        $ export XMALLOC_TRACE=xmalloc.log
        $ ./my_prog
        $ mtrace my_prog xmalloc.log
        No memory leaks.


   3. Deeper tracing.

   The mtrace script has one disadvantage, that is not disabled with
   xmalloc wrappers.  If you have many objects used in many different
   places in your program you will get confused very fast with messages
   that you get from mtrace.  This is because mtrace can only show you
   one stack frame.  I worked this around by writing ntrace utility.

   You need to compile your program with -DXMALLOC_CNT=4 (or even more
   if you wish).  Now:

        $ export XMALLOC_TRACE=xmalloc.log
        $ ./my_prog
        $ ntrace.exe my_prog xmalloc.log

        Memory not freed:
        -----------------
          Address       Size    Caller
        0x810a830       65      /home/rzyj/projekty/elmo/src/str.c:76
                                /home/rzyj/projekty/elmo/src/str.c:86
                                /home/rzyj/projekty/elmo/src/debug.c:266
                                /home/rzyj/projekty/elmo/src/debug.c:345
        Total memory not freed: 65

   Ntrace is also much faster than mtrace.

*/
/*****************************************************************************
 *    INTERFACE REQUIRED HEADERS
 ****************************************************************************/

#include <stdlib.h>

/*****************************************************************************
 *    INTERFACE DEFINITIONS / ENUMERATIONS / SIMPLE TYPEDEFS
 ****************************************************************************/
/****************************************************************************
 *    INTERFACE CLASS PROTOTYPES / EXTERNAL CLASS REFERENCES
 ****************************************************************************/
/****************************************************************************
 *    INTERFACE STRUCTURES / UTILITY CLASSES
 ****************************************************************************/
/****************************************************************************
 *    INTERFACE DATA DECLARATIONS
 ****************************************************************************/
/****************************************************************************
 *    INTERFACE FUNCTION PROTOTYPES
 ****************************************************************************/

extern void *xmalloc (size_t n);
extern void *xcalloc (size_t n, size_t s);
extern void *xrealloc (void *p, size_t n);
extern char *xstrdup (const char *str);
extern void  xfree (void *ptr);

/****************************************************************************
 *    INTERFACE OBJECT CLASS DEFINITIONS
 ****************************************************************************/
/****************************************************************************
 *    INTERFACE TRAILING HEADERS
 ****************************************************************************/
/****************************************************************************
 *
 *    END HEADER xmalloc.h
 *
 ****************************************************************************/
#endif
