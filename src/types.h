/*
helpdeco -- utility program to dissect Windows help files
Copyright (C) 1997 Manfred Winterhoff
Copyright (C) 2020 Christoph Leimbrock

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

#ifndef helpdeco_types_h
#define helpdeco_types_h
#include <stdlib.h>

#if UINTPTR_MAX == 0xffffffff
typedef long legacy_long;
typedef unsigned long unsigned_legacy_long;
typedef int legacy_int;
typedef unsigned int unsigned_legacy_int;
#elif UINTPTR_MAX == 0xffffffffffffffff
typedef int legacy_long;
typedef unsigned int unsigned_legacy_long;
typedef short legacy_int;
typedef unsigned short unsigned_legacy_int;
#else
#error Unknown platform
#endif

typedef enum { FALSE,
    TRUE } BOOL;

#endif /* helpdeco_types_h */
