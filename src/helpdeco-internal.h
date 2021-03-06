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

legacy_int bitmap_extract_all(char* szFilename, MFILE* f);
char* bitmap_export_name(unsigned_legacy_int n);
legacy_int bitmap_extract(char* szFilename, MFILE* f);

void hpj_list_bitmaps(FILE* hpj);
void hpj_list_macros(FILE* HelpFile, FILE* hpj);
void hpj_list_map(FILE* HelpFile, FILE* hpj);
void hpj_list_aliases(FILE* hpj);
void hpj_dump_system(FILE* HelpFile, FILE* hpj, char* IconFileName);

char* phrase_print(unsigned_legacy_int PhraseNum, char* out, FILE* f);
char* phrase_expand(unsigned char* str, legacy_long len, char* out);

void rtf_puts(FILE* rtf, const char* str);
void rtf_load_font(FILE* HelpFile, FILE* rtf, FILE* hpj);
void rtf_change_font(FILE* rtf, unsigned_legacy_int i, BOOL ul, BOOL uldb);
void rtf_build_filename(char* buffer, legacy_int topic);
void rtf_annotate(legacy_long pos, FILE* rtf);
void rtf_list_keywords(FILE* HelpFile, FILE* rtf, legacy_long TopicOffset);
void rtf_add_footnotes(FILE* rtf, legacy_long TopicNum, uint32_t BrowseNum);
FILE* rtf_dump(FILE* HelpFile, FILE* rtf, FILE* hpj, BOOL makertf);

BOOL html_dump(FILE* HelpFile, FILE* __html_output, BOOL inline_bitmaps);
void html_change_font(FILE* rtf, unsigned_legacy_int i, BOOL ul, BOOL uldb, BOOL in_span);
const char* html_font_name(HELPDECO_CTX* ctx, legacy_int id);
BOOL html_define_fonts(FILE* HelpFile, FILE* rtf);
void html_dump_bitmap(FILE* HelpFile, FILE* html, uint16_t bitmap);
#endif /* helpdeco_internal_h */
