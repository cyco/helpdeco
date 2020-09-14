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

#include "mfile.h"

#include "helpdec1.h"
#include "string.h"

extern void error(const char* format, ...);

int mfile_memory_put(MFILE* f, char c) /* put char to memory mapped file */
{
    if (f->ptr >= f->end)
        return 0;
    *f->ptr++ = c;
    return 1;
}

int mfile_file_put(MFILE* f, char c) /* put char to regular file */
{
    if (putc(c, f->f) == -1)
        return 0;
    return 1;
}

int mfile_memory_get(MFILE* f) /* get char from memory mapped file */
{
    if (f->ptr >= f->end)
        return -1;
    return *(unsigned char*)f->ptr++;
}

int mfile_file_get(MFILE* f) /* get char from regular file */
{
    return getc(f->f);
}

size_t mfile_memory_read(MFILE* f, void* ptr,
    long bytes) /* read function for memory mapped file */
{
    if (bytes < 0 || bytes > f->end - f->ptr) {
        error("read(%ld) failed", bytes);
        bytes = f->end - f->ptr;
    }
    memcpy(ptr, f->ptr, bytes);
    f->ptr += bytes;
    return bytes;
}

size_t mfile_file_read(MFILE* f, void* ptr,
    long bytes) /* read function for regular file */
{
    return helpdeco_fread(ptr, bytes, f->f);
}

long mfile_memory_tell(MFILE* f) /* tell for memory mapped file */
{
    return (legacy_long)f->ptr;
}

long mfile_file_tell(MFILE* f) /* tell for regular file */
{
    return ftell(f->f);
}

void mfile_memory_seek(MFILE* f, long offset) /* seek in memory mapped file */
{
    f->ptr = (char*)offset;
}

void mfile_file_seek(MFILE* f, long offset) /* seek in regular file */
{
    fseek(f->f, offset, SEEK_SET);
}

MFILE* mfile_create_map(char* ptr, size_t size) /* assign a memory mapped file */
{
    MFILE* f;

    f = helpdeco_malloc(sizeof(MFILE));
    f->f = NULL;
    f->ptr = ptr;
    f->end = ptr + size;
    f->get = mfile_memory_get;
    f->put = mfile_memory_put;
    f->read = mfile_memory_read;
    f->tell = mfile_memory_tell;
    f->seek = mfile_memory_seek;
    return f;
}

MFILE* mfile_create_virtual(FILE* f) /* assign a real file */
{
    MFILE* mf;

    mf = helpdeco_malloc(sizeof(MFILE));
    mf->f = f;
    mf->ptr = mf->end = NULL;
    mf->get = mfile_file_get;
    mf->put = mfile_file_put;
    mf->read = mfile_file_read;
    mf->tell = mfile_file_tell;
    mf->seek = mfile_file_seek;
    return mf;
}

void mfile_close(MFILE* f) /* close a MFILE */
{
    if (f)
        free(f);
}

legacy_int mfile_get_word(
    MFILE* f) /* read 16 bit value from memory mapped file or regular file */
{
    unsigned char b;

    b = f->get(f);
    return ((uint16_t)(f->get(f)) << 8) | (uint16_t)b;
}

uint16_t mfile_get_cword(
    MFILE* f) /* get compressed word from memory mapped file or regular file */
{
    unsigned char b;

    b = f->get(f);
    if (b & 1)
        return (((uint16_t)(f->get(f)) << 8) | (uint16_t)b) >> 1;
    return ((uint16_t)b >> 1);
}

uint32_t mfile_get_cdword(
    MFILE* f) /* get compressed long from memory mapped file or regular file */
{
    uint16_t w;

    w = mfile_get_word(f);
    if (w & 1)
        return (((uint32_t)mfile_get_word(f) << 16) | (uint32_t)w) >> 1;
    return ((uint32_t)w >> 1);
}

uint32_t
mfile_get_dword(MFILE* f) /* get long from memory mapped file or regular file */
{
    uint16_t w;

    w = mfile_get_word(f);
    return ((uint32_t)mfile_get_word(f) << 16) | (uint32_t)w;
}

size_t mfile_read_string(char* ptr, size_t size,
    MFILE* f) /* read nul terminated string from memory mapped or
                               regular file */
{
    size_t i;
    legacy_int c;

    i = 0;
    while ((c = f->get(f)) > 0) {
        if (i >= size - 1) {
            fputs("String length exceeds decompiler limit.\n", stderr);
            exit(1);
        }
        ptr[i++] = c;
    }
    ptr[i] = '\0';
    return i;
}

legacy_long mfile_copy_bytes(MFILE* f, legacy_long bytes, FILE* out)
{
    legacy_long length;
    legacy_int size;
    static char buffer[512];

    for (length = 0; length < bytes; length += size) {
        size = (legacy_int)(bytes - length > sizeof(buffer) ? sizeof(buffer)
                                                            : bytes - length);
        f->read(f, buffer, size);
        fwrite(buffer, size, 1, out);
    }
    return length;
}

signed char count; /* for run len decompression */

int mfile_derun(MFILE* f, char c) /* expand runlen compressed data */
{
    legacy_int i;

    if (count & 0x7F) {
        if (count & 0x80) {
            f->put(f, c);
            count--;
            return 1;
        }
        for (i = 0; i < count; i++) {
            f->put(f, c);
        }
        count = 0;
        return i;
    }
    count = (signed char)c;
    return 0;
}

/* copies bytes from (memory mapped or regular file) f to (memory mapped or
// regular file) fTarget, decompressed using method
// 0: copy (no decompression)
// 1: runlen decompression
// 2: LZ77 decompression
// 3: runlen and LZ77 decompression
// returns number of bytes copied to fTarget. Doesn't complain if fTarget
// is a memory mapped file and buffer is full, just stops writing */
legacy_long mfile_decompress(legacy_int method, MFILE* f, legacy_long bytes,
    MFILE* fTarget)
{
    static unsigned char lzbuffer[0x1000];
    int (*Emit)(MFILE * f, char c);
    unsigned char bits = 0, mask;
    legacy_int pos, len, back;
    legacy_long n;

    n = 0;
    if (method & 1) {
        Emit = mfile_derun;
        count = 0;
    } else {
        Emit = fTarget->put;
    }
    if (method & 2) {
        mask = 0;
        pos = 0;
        while (bytes-- > 0) {
            if (!mask) {
                bits = f->get(f);
                mask = 1;
            } else {
                if (bits & mask) {
                    if (bytes-- == 0)
                        break;
                    back = mfile_get_word(f);
                    len = ((back >> 12) & 15) + 3;
                    back = pos - (back & 0xFFF) - 1;
                    while (len-- > 0) {
                        n += Emit(fTarget,
                            lzbuffer[pos++ & 0xFFF] = lzbuffer[back++ & 0xFFF]);
                    }
                } else {
                    n += Emit(fTarget, lzbuffer[pos++ & 0xFFF] = f->get(f));
                }
                mask <<= 1;
            }
        }
    } else {
        while (bytes-- > 0)
            n += Emit(fTarget, f->get(f));
    }
    return n;
}

legacy_long mfile_decompress_into_buffer(legacy_int method, FILE* HelpFile,
    legacy_long bytes, void* ptr,
    legacy_long size)
{
    MFILE* f;
    MFILE* mf;

    f = mfile_create_map(ptr, size);
    mf = mfile_create_virtual(HelpFile);
    bytes = mfile_decompress(method, mf, bytes, f);
    mfile_close(mf);
    mfile_close(f);
    return bytes;
}

legacy_long mfile_decompress_into_file(legacy_int method, MFILE* f, legacy_long bytes,
    FILE* fTarget)
{
    MFILE* mf;

    mf = mfile_create_virtual(fTarget);
    bytes = mfile_decompress(method, f, bytes, mf);
    mfile_close(mf);
    return bytes;
}
