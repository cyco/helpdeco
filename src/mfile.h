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

#ifndef helpdeco_mfile_h
#define helpdeco_mfile_h

#include "types.h"
#include <stdio.h>

/* HELPDECO sometimes has to work off the help file, sometimes needs to do
// the same with (decompressed) information stored in memory. MFILE and the
// memory mapped file functions allow to write the same code for both, but
// this approach needs some declarations first... */

typedef struct mfile /* a class would be more appropriate */
{
    FILE* f;
    char* ptr;
    char* end;
    int (*get)(struct mfile*);
    int (*put)(struct mfile*, char);
    size_t (*read)(struct mfile*, void*, long);
    long (*tell)(struct mfile*);
    void (*seek)(struct mfile*, long);
} MFILE;

extern int mfile_memory_put(MFILE* f, char c); /* put char to memory mapped file */
extern int mfile_file_put(MFILE* f, char c); /* put char to regular file */
extern int mfile_memory_get(MFILE* f); /* get char from memory mapped file */
extern int mfile_file_get(MFILE* f); /* get char from regular file */
extern size_t mfile_memory_read(MFILE* f, void* ptr,
    long bytes); /* read function for memory mapped file */
extern size_t mfile_file_read(MFILE* f, void* ptr,
    long bytes); /* read function for regular file */
extern long mfile_memory_tell(MFILE* f); /* tell for memory mapped file */
extern long mfile_file_tell(MFILE* f); /* tell for regular file */
extern void mfile_memory_seek(MFILE* f, long offset); /* seek in memory mapped file */
extern void mfile_file_seek(MFILE* f, long offset); /* seek in regular file */
extern MFILE* mfile_create_map(char* ptr,
    size_t size); /* assign a memory mapped file */
extern MFILE* mfile_create_virtual(FILE* f); /* assign a real file */
extern void mfile_close(MFILE* f); /* close a MFILE */
extern legacy_int mfile_get_word(
    MFILE* f); /* read 16 bit value from memory mapped file or regular file */
extern uint16_t mfile_get_cword(
    MFILE* f); /* get compressed word from memory mapped file or regular file */
extern uint32_t mfile_get_cdword(MFILE* f); /* get compressed legacy_long from memory
                                        mapped file or regular file */
extern uint32_t mfile_get_dword(
    MFILE* f); /* get legacy_long from memory mapped file or regular file */
extern size_t mfile_read_string(char* ptr, size_t size,
    MFILE* f); /* read nul terminated string from memory
                                       mapped or regular file */
extern legacy_long mfile_copy_bytes(MFILE* f, legacy_long bytes, FILE* out);
extern legacy_long mfile_decompress(legacy_int method, MFILE* f, legacy_long bytes,
    MFILE* fTarget);
extern legacy_long mfile_decompress_into_buffer(legacy_int method, FILE* HelpFile,
    legacy_long bytes, void* ptr,
    legacy_long size);
extern legacy_long mfile_decompress_into_file(legacy_int method, MFILE* f,
    legacy_long bytes, FILE* fTarget);

#endif /* helpdeco_mfile_h */
