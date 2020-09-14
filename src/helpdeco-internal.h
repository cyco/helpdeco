/*
helpdeco -- utility program to dissect Windows help files
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

#ifndef helpdeco_internal_h
#define helpdeco_internal_h

char *phrase_print(unsigned_legacy_int PhraseNum, char *out, FILE *f);
char *phrase_expand(unsigned char *str, legacy_long len, char *out);

BOOL html_dump(FILE *HelpFile, FILE *__html_output);
void html_change_font(FILE *rtf, unsigned_legacy_int i, BOOL ul, BOOL uldb);
const char *html_font_name(HELPDECO_CTX *ctx, legacy_int id);
BOOL html_define_fonts(FILE *HelpFile, FILE *rtf);
#endif /* helpdeco_internal_h */
