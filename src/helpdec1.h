/*
helpdeco -- utility program to dissect Windows help files
Copyright (C) 1997 Manfred Winterhoff

This file is part of helpdeco; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA, 02111-1307, USA or visit:
http://www.gnu.org
*/


#ifndef helpdec_h
#define helpdec_h
extern void *helpdeco_malloc(long bytes);             /* save malloc function */
extern void *helpdeco_realloc(void *ptr, long bytes); /* save realloc function */
extern char *helpdeco_strdup(const char *ptr);        /* save strdup function */
extern size_t helpdeco_fread(void *ptr, long bytes,
                       FILE *f); /* save fread function */
extern size_t
helpdeco_gets(char *ptr, size_t size,
        FILE *f); /* read nul terminated string from regular file */
extern void helpdeco_fclose(FILE *f); /* checks if disk is full */
extern FILE *helpdeco_fopen(const char *filename,
                      const char *mode);  /* save fopen function */
extern uint16_t helpdeco_getw(FILE *f);         /* get 16 bit quantity */
extern uint32_t helpdeco_getdw(FILE *f);           /* get legacy_long */
extern void helpdeco_putw(uint16_t w, FILE *f); /* write 16 bit quantity */
extern void helpdeco_putdw(uint32_t x, FILE *f);   /* write legacy_long to file */
extern void helpdeco_putcdw(uint32_t x,
                   FILE *f); /* write compressed legacy_long to file */
extern void helpdeco_putcw(unsigned_legacy_int x,
                  FILE *f);             /* write compressed word to file */

#endif /* helpdec_h */
