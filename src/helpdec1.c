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

#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NONSTDC_NO_WARNINGS

/* HELPDEC1.C - HELPDECO supporting functions */
#include "helpdeco.h"

void error(const char *format, ...) {
  va_list arg;

  fputs("\n", stderr);
  va_start(arg, format);
  vfprintf(stderr, format, arg);
  va_end(arg);

  if (ctx->opt_interactive) {
    fputs("\nPress CR to continue at your own risk, any other key to exit.\n",
          stderr);
    if (getch() != '\r')
      exit(1);
  } else {
    exit(1);
  }
}

#ifndef HAVE_STRNCPY
#ifndef HAVE_STRLCPY

size_t strlcpy(char *dest, const char *src,
               size_t len) /* limited string copy */
{
  size_t i;

  if (!dest)
    return 0;
  for (i = 0; i < len - 1 && src && src[i]; i++)
    dest[i] = src[i];
  dest[i] = '\0';
  return i;
}

#endif /* !defined(HAVE_STRLCPY) */
#endif /* !defined(HAVE_STRNCPY) */

void *helpdeco_malloc(long bytes) /* save malloc function */
{
  void *ptr;

  if (bytes < 1L || ((size_t)bytes != bytes) ||
      (ptr = malloc((size_t)bytes)) == NULL) {
    fprintf(stderr, "Allocation of %ld bytes failed. File too big.\n", bytes);
    exit(1);
  }
  return ptr;
}

void *helpdeco_realloc(void *ptr, long bytes) /* save realloc function */
{
  if (!ptr)
    return helpdeco_malloc(bytes);
  if (bytes < 1L || bytes != (size_t)bytes ||
      (ptr = realloc(ptr, (size_t)bytes)) == NULL) {
    fprintf(stderr, "Reallocation to %ld bytes failed. File too big.\n", bytes);
    exit(1);
  }
  return ptr;
}

char *helpdeco_strdup(const char *ptr) /* save strdup function */
{
  size_t len;
  char *dup;

  if (!ptr)
    return NULL;
  len = strlen(ptr);
  dup = helpdeco_malloc(len + 1);
  strcpy(dup, ptr);
  return dup;
}

size_t helpdeco_fread(void *ptr, long bytes, FILE *f) /* save fread function */
{
  size_t result = 0;

  if (bytes == 0)
    return 0;
  if (bytes < 0 || bytes != (size_t)bytes ||
      (result = fread(ptr, 1, (size_t)bytes, f)) != bytes) {
    error("helpdeco_fread(%ld) at %ld failed", bytes, ftell(f));
  }
  return result;
}

size_t helpdeco_gets(char *ptr, size_t size,
                     FILE *f) /* read nul terminated string from regular file */
{
  size_t i;
  int c;

  i = 0;
  while ((c = getc(f)) > 0) {
    if (i >= size - 1) {
      fputs("String length exceeds decompiler limit.\n", stderr);
      exit(1);
    }
    ptr[i++] = c;
  }
  ptr[i] = '\0';
  return i;
}

void helpdeco_fclose(FILE *f) /* checks if disk is full */
{
  if (ferror(f) != 0) {
    fputs("File write error. Program aborted.\n", stderr);
    exit(2);
  }
  fclose(f);
}

FILE *helpdeco_fopen(const char *filename, const char *mode) /* save fopen function */
{
  FILE *f;
  char ch;

  if (!ctx->opt_overwrite) {
    f = fopen(filename, "rb");
    if (f) {
      fclose(f);
      fprintf(stderr, "File %s already exists. Overwrite (Y/N/All/Quit) ? Y\b",
              filename);
      do {
        ch = toupper(getch());
        if (ch == '\r')
          ch = 'Y';
        else if (ch == '\x1B')
          ch = 'N';
      } while (ch != 'Q' && ch != 'A' && ch != 'Y' && ch != 'N');
      printf("%c\n", ch);
      if (ch == 'Q')
        exit(0);
      if (ch == 'A')
        ctx->opt_overwrite = TRUE;
      if (ch == 'N')
        return NULL;
    }
  }
  f = fopen(filename, mode);
  if (!f) {
    error("Can not create '%s'.", filename);
  } else {
    fprintf(stderr, "Creating %s...\n", filename);
  }
  return f;
}

uint16_t helpdeco_getw(FILE *f) /* get 16 bit quantity */
{
  int ch;

  ch = getc(f);
  return ch | (getc(f) << 8);
}

uint32_t helpdeco_getdw(FILE *f) /* get long */
{
  uint16_t w;

  w = helpdeco_getw(f);
  return ((uint32_t)helpdeco_getw(f) << 16) | (uint32_t)w;
}

void helpdeco_putw(uint16_t w, FILE *f) /* write 16 bit quantity */
{
  putc((w & 0xFF), f);
  putc((w >> 8), f);
}

void helpdeco_putdw(uint32_t x, FILE *f) /* write long to file */
{
  fwrite(&x, 4, 1, f);
}

void helpdeco_putcdw(uint32_t x, FILE *f) /* write compressed long to file */
{
  if (x > 32767L) {
    helpdeco_putw((unsigned int)(x << 1) + 1, f);
    helpdeco_putw(x >> 15, f);
  } else {
    helpdeco_putw(x << 1, f);
  }
}

void helpdeco_putcw(unsigned_legacy_int x, FILE *f) /* write compressed word to file */
{
  if (x > 127) {
    helpdeco_putw((x << 1) + 1, f);
  } else {
    putc(x << 1, f);
  }
}

legacy_long copy(FILE *f, legacy_long bytes, FILE *out) {
  legacy_long length;
  legacy_int size;
  static char buffer[512];

  for (length = 0; length < bytes; length += size) {
    size = (legacy_int)(bytes - length > sizeof(buffer) ? sizeof(buffer)
                                                        : bytes - length);
    helpdeco_fread(buffer, size, f);
    fwrite(buffer, size, 1, out);
  }
  return length;
}

void HexDump(FILE *f, legacy_long FileLength, legacy_long offset) {
  unsigned char b[16];
  legacy_long l;
  legacy_int n, i;

  puts("[-Addr-] [--------------------Data---------------------] "
       "[-----Text-----]");
  fseek(f, offset, SEEK_CUR);
  for (l = offset; l < FileLength; l += 16) {
    printf("%08lX ", l);
    n = (legacy_int)(FileLength - l > 16 ? 16 : FileLength - l);
    for (i = 0; i < n; i++)
      printf("%02X ", b[i] = getc(f));
    while (i++ < 16)
      printf("	 ");
    for (i = 0; i < n; i++)
      putchar(isprint(b[i]) ? b[i] : '.');
    putchar('\n');
  }
}

void HexDumpMemory(void *bypMem, unsigned_legacy_int FileLength) {
  unsigned char b[16];
  unsigned_legacy_int l;
  legacy_int n, i;

  puts("[-Addr-] [--------------------Data---------------------] "
       "[-----Text-----]");
  for (l = 0; l < FileLength; l += 16) {
    printf("%08X ", l);
    n = (legacy_int)(FileLength - l > 16 ? 16 : FileLength - l);
    for (i = 0; i < n; i++)
      printf("%02X ", b[i] = *(unsigned char *)bypMem++);
    while (i++ < 16)
      printf("	 ");
    for (i = 0; i < n; i++)
      putchar(isprint(b[i]) ? b[i] : '.');
    putchar('\n');
  }
}

/* write str to stdout, replacing nonprintable characters with hex codes,
// returning str+len. PrintString doesn't stop at NUL characters */
char *PrintString(char *str, unsigned_legacy_int len) {
  while (len-- > 0) {
    if (isprint((unsigned char)*str)) {
      putchar(*str);
    } else {
      printf("(%02x)", *(unsigned char *)str);
    }
    str++;
  }
  return str;
}

/* get next bit (lsb first) from 32 bit words in f, initialized if f = NULL */
/* important to read longs to stop at right position */
BOOL GetBit(FILE *f) {
  static uint32_t mask;
  static uint32_t value;

  if (f) {
    mask <<= 1;
    if (!mask) {
      value = helpdeco_getdw(f);
      mask = 1;
    }
  } else {
    mask = 0; /* initialize */
  }
  return (value & mask) != 0;
}

/* scan-functions for reading compressed values from LinkData1 */
int16_t scanint(char **ptr) /* scan a compressed short */
{
  int16_t ret;
  if (*(*ptr) & 1) {
    ret = (*(((uint16_t *)(*ptr))) >> 1) - 0x4000;
    *ptr = *ptr + sizeof(uint16_t);
  } else {
    ret = (*(((unsigned char *)(*ptr))) >> 1) - 0x40;
    *ptr = *ptr + sizeof(unsigned char);
  }
  return ret;
}

uint16_t scanword(char **ptr) /* scan a compressed unsiged short */
{
  uint16_t ret;
  if (*(*ptr) & 1) {
    ret = (*(((uint16_t *)(*ptr))) >> 1);
    *ptr = *ptr + sizeof(uint16_t);
  } else {
    ret = (*(((unsigned char *)(*ptr))) >> 1);
    *ptr = *ptr + sizeof(unsigned char);
  }
  return ret;
}

uint32_t scanlong(char **ptr) /* scan a compressed long */
{
  uint32_t ret;
  if (*(*ptr) & 1) {
    ret = (*(((uint32_t *)(*ptr))) >> 1) - 0x40000000;
    *ptr = *ptr + sizeof(uint32_t);
  } else {
    ret = (*(((uint16_t *)(*ptr))) >> 1) - 0x4000;
    *ptr = *ptr + sizeof(uint16_t);
  }
  return ret;
}

/* locates internal file FileName or internal directory if FileName is NULL
// reads FILEHEADER and returns TRUE with current position in HelpFile set
// to first byte of data of FileName or returns FALSE if not found. Stores
// UsedSpace (that's the file size) in FileLength if FileLength isn't NULL */
BOOL SearchFile(FILE *HelpFile, const char *FileName, legacy_long *FileLength) {
  HELPHEADER Header;
  FILEHEADER FileHdr;
  BTREEHEADER BtreeHdr;
  BTREENODEHEADER CurrNode;
  legacy_long offset;
  char TempFile[255];
  legacy_int i, n;

  fseek(HelpFile, 0, SEEK_SET);
  read_HELPHEADER(&Header, HelpFile);
  if (Header.Magic != 0x00035F3FL)
    return FALSE;
  fseek(HelpFile, Header.DirectoryStart, SEEK_SET);
  read_FILEHEADER(&FileHdr, HelpFile);
  if (!FileName) {
    if (FileLength)
      *FileLength = FileHdr.UsedSpace;
    return TRUE;
  }
  read_BTREEHEADER(&BtreeHdr, HelpFile);
  offset = ftell(HelpFile);
  fseek(HelpFile, offset + BtreeHdr.RootPage * (legacy_long)BtreeHdr.PageSize,
        SEEK_SET);
  for (n = 1; n < BtreeHdr.NLevels; n++) {
    read_BTREEINDEXHEADER_to_BTREENODEHEADER(&CurrNode, HelpFile);
    for (i = 0; i < CurrNode.NEntries; i++) {
      helpdeco_gets(TempFile, sizeof(TempFile), HelpFile);
      if (strcmp(FileName, TempFile) < 0)
        break;
      CurrNode.PreviousPage = helpdeco_getw(HelpFile);
    }
    fseek(HelpFile,
          offset + CurrNode.PreviousPage * (legacy_long)BtreeHdr.PageSize,
          SEEK_SET);
  }
  read_BTREENODEHEADER(&CurrNode, HelpFile);
  for (i = 0; i < CurrNode.NEntries; i++) {
    helpdeco_gets(TempFile, sizeof(TempFile), HelpFile);
    offset = helpdeco_getdw(HelpFile);
    if (strcmp(TempFile, FileName) == 0) {
      fseek(HelpFile, offset, SEEK_SET);
      read_FILEHEADER(&FileHdr, HelpFile);
      if (FileLength)
        *FileLength = FileHdr.UsedSpace;
      return TRUE;
    }
  }
  return FALSE;
}

/* read first (and next) page from B+ tree. HelpFile must be positioned
// at start of internal file prior calling GetFirstPage. It will be
// positioned at first B+ tree entry after return from GetFirstPage.
// Number of TotalBtreeEntries stored in TotalEntries if pointer is
// not NULL, NumberOfEntries of first B+ tree page returned.
// buf stores position, so GetNextPage will seek to position itself. */
int16_t GetFirstPage(FILE *HelpFile, BUFFER *buf, legacy_long *TotalEntries) {
  legacy_int CurrLevel;
  BTREEHEADER BTreeHdr;
  BTREENODEHEADER CurrNode;

  read_BTREEHEADER(&BTreeHdr, HelpFile);
  if (TotalEntries)
    *TotalEntries = BTreeHdr.TotalBtreeEntries;
  if (!BTreeHdr.TotalBtreeEntries)
    return 0;
  buf->FirstLeaf = ftell(HelpFile);
  buf->PageSize = BTreeHdr.PageSize;
  fseek(HelpFile,
        buf->FirstLeaf + BTreeHdr.RootPage * (legacy_long)BTreeHdr.PageSize,
        SEEK_SET);
  for (CurrLevel = 1; CurrLevel < BTreeHdr.NLevels; CurrLevel++) {
    read_BTREEINDEXHEADER_to_BTREENODEHEADER(&CurrNode, HelpFile);
    fseek(HelpFile,
          buf->FirstLeaf +
              CurrNode.PreviousPage * (legacy_long)BTreeHdr.PageSize,
          SEEK_SET);
  }
  read_BTREENODEHEADER(&CurrNode, HelpFile);
  buf->NextPage = CurrNode.NextPage;
  return CurrNode.NEntries;
}

int16_t GetNextPage(FILE *HelpFile, BUFFER *buf) /* walk Btree */
{
  BTREENODEHEADER CurrNode;

  if (buf->NextPage == -1)
    return 0;
  fseek(HelpFile, buf->FirstLeaf + buf->NextPage * (legacy_long)buf->PageSize,
        SEEK_SET);
  read_BTREENODEHEADER(&CurrNode, HelpFile);
  buf->NextPage = CurrNode.NextPage;
  return CurrNode.NEntries;
}

/* reads next record from |SYSTEM file, returns NULL if no more available
// Use last system record as parameter SysRec (saves filehandle and pos) */
SYSTEMRECORD *GetNextSystemRecord(SYSTEMRECORD *SysRec) {
  if (SysRec->Remaining < 4) {
    free(SysRec);
    return NULL;
  }
  fseek(SysRec->File, SysRec->SavePos, SEEK_SET);
  SysRec->RecordType = helpdeco_getw(SysRec->File);
  SysRec->DataSize = helpdeco_getw(SysRec->File);
  SysRec->Remaining -= 4;
  if (SysRec->Remaining < SysRec->DataSize) {
    free(SysRec);
    return NULL;
  }
  SysRec = helpdeco_realloc(SysRec, sizeof(SYSTEMRECORD) + SysRec->DataSize);
  helpdeco_fread(SysRec->Data, SysRec->DataSize, SysRec->File);
  SysRec->Data[SysRec->DataSize] = '\0';
  SysRec->Remaining -= SysRec->DataSize;
  SysRec->SavePos = ftell(SysRec->File);
  return SysRec;
}

/* reads first record from |SYSTEM file, returns NULL if none found */
SYSTEMRECORD *GetFirstSystemRecord(FILE *HelpFile) {
  SYSTEMHEADER SysHdr;
  SYSTEMRECORD *SysRec;
  legacy_long FileLength;

  if (!SearchFile(HelpFile, "|SYSTEM", &FileLength))
    return NULL;
  read_SYSTEMHEADER(&SysHdr, HelpFile);
  if (SysHdr.Major != 1 || SysHdr.Minor < 16)
    return NULL;
  SysRec = helpdeco_malloc(sizeof(SYSTEMRECORD));
  SysRec->File = HelpFile;
  SysRec->SavePos = ftell(HelpFile);
  SysRec->Remaining = FileLength - sizeof(SYSTEMHEADER);
  return GetNextSystemRecord(SysRec);
}

void ListFiles(FILE *HelpFile) /* display internal directory */
{
  BUFFER buf;
  char FileName[255];
  legacy_int j, i, n;

  puts("FileName		  FileOffset | FileName 	       "
       "FileOffset");
  puts("-----------------------------------+----------------------------------"
       "-");
  j = 0;
  for (n = GetFirstPage(HelpFile, &buf, NULL); n;
       n = GetNextPage(HelpFile, &buf)) {
    for (i = 0; i < n; i++) {
      helpdeco_gets(FileName, sizeof(FileName), HelpFile);
      printf("%-23s 0x%08lX", FileName, helpdeco_getdw(HelpFile));
      if (j++ & 1)
        putchar('\n');
      else
        printf(" | ");
    }
  }
  if (j & 1)
    putchar('\n');
}

void ListBaggage(FILE *HelpFile, FILE *hpj,
                 BOOL before31) /* writes out [BAGGAGE] section */
{
  BOOL headerwritten;
  char *leader;
  char FileName[255];
  legacy_long FileLength;
  BUFFER buf;
  legacy_int i, n;
  FILE *f;
  legacy_long savepos;

  headerwritten = FALSE;
  leader = &"|bm"[before31];
  SearchFile(HelpFile, NULL, NULL);
  for (n = GetFirstPage(HelpFile, &buf, NULL); n;
       n = GetNextPage(HelpFile, &buf)) {
    for (i = 0; i < n; i++) {
      helpdeco_gets(FileName, sizeof(FileName), HelpFile);
      helpdeco_getdw(HelpFile);
      if (FileName[0] != '|' && memcmp(FileName, leader, strlen(leader)) != 0 &&
          !strstr(FileName, ".GRP") && !strstr(FileName, ".tbl")) {
        savepos = ftell(HelpFile);
        if (SearchFile(HelpFile, FileName, &FileLength)) {
          if (!headerwritten) {
            fputs("[BAGGAGE]\n", hpj);
            headerwritten = TRUE;
          }
          fprintf(hpj, "%s\n", FileName);
          f = helpdeco_fopen(FileName, "wb");
          if (f) {
            copy(HelpFile, FileLength, f);
            helpdeco_fclose(f);
          }
        }
        fseek(HelpFile, savepos, SEEK_SET);
      }
    }
  }
  if (headerwritten)
    putc('\n', hpj);
}

void PrintWindow(FILE *hpj, SECWINDOW *SWin) {
  if (SWin->Flags & WSYSFLAG_NAME)
    fprintf(hpj, "%s", SWin->Name);
  putc('=', hpj);
  if (SWin->Flags & WSYSFLAG_CAPTION)
    fprintf(hpj, "\"%s\"", SWin->Caption);
  putc(',', hpj);
  if (SWin->Flags &
      (WSYSFLAG_X | WSYSFLAG_Y | WSYSFLAG_WIDTH | WSYSFLAG_HEIGHT)) {
    putc('(', hpj);
    if (SWin->Flags & WSYSFLAG_X)
      fprintf(hpj, "%d", SWin->X);
    putc(',', hpj);
    if (SWin->Flags & WSYSFLAG_Y)
      fprintf(hpj, "%d", SWin->Y);
    putc(',', hpj);
    if (SWin->Flags & WSYSFLAG_WIDTH)
      fprintf(hpj, "%d", SWin->Width);
    putc(',', hpj);
    if (SWin->Flags & WSYSFLAG_HEIGHT)
      fprintf(hpj, "%d", SWin->Height);
    putc(')', hpj);
  }
  putc(',', hpj);
  if (SWin->Maximize)
    fprintf(hpj, "%u", SWin->Maximize);
  putc(',', hpj);
  if (SWin->Flags & WSYSFLAG_RGB)
    fprintf(hpj, "(%d,%d,%d)", SWin->Rgb[0], SWin->Rgb[1], SWin->Rgb[2]);
  putc(',', hpj);
  if (SWin->Flags & WSYSFLAG_RGBNSR)
    fprintf(hpj, "(%d,%d,%d)", SWin->RgbNsr[0], SWin->RgbNsr[1],
            SWin->RgbNsr[2]);
  if (SWin->Flags & (WSYSFLAG_TOP | WSYSFLAG_AUTOSIZEHEIGHT)) {
    if (SWin->Flags & WSYSFLAG_AUTOSIZEHEIGHT) {
      if (SWin->Flags & WSYSFLAG_TOP) {
        fputs(",f3", hpj);
      } else {
        fputs(",f2", hpj);
      }
    } else
      fputs(",1", hpj);
  }
  putc('\n', hpj);
}

void PrintMVBWindow(FILE *hpj, MVBWINDOW *SWin) {
  fprintf(hpj, "%s", SWin->Name);
  putc('=', hpj);
  fprintf(hpj, "\"%s\"", SWin->Caption);
  putc(',', hpj);
  putc('(', hpj);
  fprintf(hpj, "%d", SWin->X);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Y);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Width);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Height);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Maximize);
  putc(')', hpj);
  putc(',', hpj);
  putc('(', hpj);
  fprintf(hpj, "%d", 0);
  putc(')', hpj);
  putc(',', hpj);
  fprintf(hpj, "(%d,%d,%d)", SWin->TopRgb[0], SWin->TopRgb[1], SWin->TopRgb[2]);
  putc(',', hpj);
  fprintf(hpj, "(%d,%d,%d)", SWin->RgbNsr[0], SWin->RgbNsr[1], SWin->RgbNsr[2]);
  putc(',', hpj);
  fprintf(hpj, "(%d,%d,%d)", SWin->Rgb[0], SWin->Rgb[1], SWin->Rgb[2]);
  putc(',', hpj);
  putc('(', hpj);
  fprintf(hpj, "%d", SWin->X2);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Y2);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Width2);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Height2);
  putc(',', hpj);
  fprintf(hpj, "%d", 0);
  putc(')', hpj);
  putc(',', hpj);
  putc('(', hpj);
  fprintf(hpj, "%d", 1);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->X3);
  putc(',', hpj);
  fprintf(hpj, "%d", SWin->Y3);
  putc(')', hpj);
  putc(',', hpj);
  putc('(', hpj);
  fprintf(hpj, "%d", 0);
  putc(')', hpj);
  putc(',', hpj);
  putc('(', hpj);
  fprintf(hpj, "%d", 1);
  putc(')', hpj);
  putc('\n', hpj);
}

void ToMapDump(FILE *HelpFile, legacy_long FileLength) {
  legacy_long i;

  for (i = 0; i * 4 < FileLength; i++) {
    printf("TopicNum: %-12ld TopicOffset: 0x%08lX\n", i, helpdeco_getdw(HelpFile));
  }
}

void GroupDump(FILE *HelpFile) {
  GROUPHEADER GroupHeader;
  char *ptr = NULL;
  unsigned_legacy_long i;

  read_GROUPHEADER(&GroupHeader, HelpFile);
  switch (GroupHeader.GroupType) {
  case 2:
    ptr = helpdeco_malloc(GroupHeader.BitmapSize);
    helpdeco_fread(ptr, GroupHeader.BitmapSize, HelpFile);
  case 1:
    for (i = GroupHeader.FirstTopic; i <= GroupHeader.LastTopic; i++) {
      if (GroupHeader.GroupType == 1 || ptr[i >> 3] & (1 << (i & 7)))
        printf("TopicNumber: %lu\n", i);
    }
    break;
  default:
    fprintf(stderr, "GroupHeader GroupType %ld unknown\n",
            GroupHeader.GroupType);
  }
}

void KWMapDump(FILE *HelpFile) {
  uint16_t n, i;
  KWMAPREC KeywordMap;

  n = helpdeco_getw(HelpFile);
  for (i = 0; i < n; i++) {
    read_KWMAPREC(&KeywordMap, HelpFile);
    printf("Keyword: %-12ld LeafPage: %u\n", KeywordMap.FirstRec,
           KeywordMap.PageNum);
  }
}

void KWDataDump(FILE *HelpFile, legacy_long FileLength) {
  legacy_long i;

  for (i = 0; i < FileLength; i += 4) {
    printf("KWDataAddress: 0x%08lx TopicOffset: 0x%08lX\n", i, helpdeco_getdw(HelpFile));
  }
}

void CatalogDump(FILE *HelpFile) {
  CATALOGHEADER catalog;
  legacy_long n;

  read_CATALOGHEADER(&catalog, HelpFile);
  for (n = 0; n < catalog.entries; n++) {
    printf("Topic: %-12ld TopicOffset: 0x%08lx\n", n + 1, helpdeco_getdw(HelpFile));
  }
}

void CTXOMAPDump(FILE *HelpFile) {
  CTXOMAPREC CTXORec;
  uint16_t n, i;

  n = helpdeco_getw(HelpFile);
  for (i = 0; i < n; i++) {
    read_CTXOMAPREC(&CTXORec, HelpFile);
    printf("MapId: %-12ld TopicOffset: 0x%08lX\n", CTXORec.MapID,
           CTXORec.TopicOffset);
  }
}

void LinkDump(FILE *HelpFile) {
  legacy_long data[3];
  legacy_int n, i;

  n = helpdeco_getw(HelpFile);
  for (i = 0; i < n; i++) {
    data[0] = helpdeco_getdw(HelpFile);
    data[1] = helpdeco_getdw(HelpFile);
    data[2] = helpdeco_getdw(HelpFile);
    printf("Annotation for topic 0x%08lx 0x%08lx 0x%08lx\n", data[0], data[1],
           data[2]);
  }
}

void AnnotationDump(FILE *HelpFile, legacy_long FileLength, const char *name) {
  legacy_long l;

  printf("Annotation %s for topic 0x%08lx:\n", name, atol(name));
  for (l = 0; l < FileLength; l++)
    putchar(getc(HelpFile));
  putchar('\n');
}

typedef uint8_t BYTE;
typedef uint16_t WORD;
typedef uint32_t DWORD;
typedef uint64_t QWORD;

extern WORD get_WORD(BYTE *b) { return b[0] | b[1] << 8; }

extern DWORD get_DWORD(BYTE *b) {
  return b[0] | b[1] << 8 | b[2] << 16 | b[3] << 24;
}

extern QWORD get_QWORD(BYTE *b) {
  return b[0] | b[1] << 8 | b[2] << 16 | b[3] << 24 | (QWORD)b[4] << 32 |
         (QWORD)b[5] << 40 | (QWORD)b[6] << 48 | (QWORD)b[7] << 56;
}

#define s(a)                                                                   \
  BOOL read_##a(a *obj, FILE *file) {                                          \
    BYTE buf[sizeof_##a];                                                      \
    if (helpdeco_fread(buf, sizeof_##a, file)) {                               \
      uint32_t i = 0;
#define s2(a, b)                                                               \
  BOOL read_##a##_to_##b(b *obj, FILE *file) {                                 \
    BYTE buf[sizeof_##a];                                                      \
    if (helpdeco_fread(buf, sizeof_##a, file)) {                               \
      uint32_t i = 0;
#define g(a)                                                                   \
  BOOL get_##a(a *obj, BYTE *buf) {                                            \
    if (1) {                                                                   \
      uint32_t i = 0;
#define a(a, b)                                                                \
  memcpy(&obj->a[0], buf + i, b);                                              \
  i += b;
#define b(a)                                                                   \
  obj->a = *(buf + i);                                                         \
  i++;
#define w(a)                                                                   \
  obj->a = get_WORD(buf + i);                                                  \
  i += 2;
#define d(a)                                                                   \
  obj->a = get_DWORD(buf + i);                                                 \
  i += 4;
#define q(a)                                                                   \
  obj->a = get_QWORD(buf + i);                                                 \
  i += 8;
#define e                                                                      \
  return TRUE;                                                                 \
  }                                                                            \
  else return FALSE;                                                           \
  }

s(HELPHEADER) d(Magic) d(DirectoryStart) d(FreeChainStart) d(EntireFileSize) e

    s(FILEHEADER) d(ReservedSpace) d(UsedSpace) b(FileFlags) e

    s(BTREEHEADER) w(Magic) w(Flags) w(PageSize) a(Structure, 0x10)
        w(MustBeZero) w(PageSplits) w(RootPage) w(MustBeNegOne) w(TotalPages)
            w(NLevels) d(TotalBtreeEntries) e

    s(BTREEINDEXHEADER) w(Unknown) w(NEntries) w(PreviousPage) e

    /* for reading index nodes into regular nodes, boink */
    s2(BTREEINDEXHEADER, BTREENODEHEADER) w(Unknown) w(NEntries)
        w(PreviousPage) obj->NextPage = 0;
e

    s(BTREENODEHEADER) w(Unknown) w(NEntries) w(PreviousPage) w(NextPage) e

    s(SYSTEMHEADER) w(Magic) w(Minor) w(Major) d(GenDate) w(Flags) e

    s(GROUPHEADER) d(Magic) d(BitmapSize) d(LastTopic) d(FirstTopic)
        d(TopicsUsed) d(TopicCount) d(GroupType) d(Unknown1) d(Unknown2)
            d(Unknown3) e

    s(KWMAPREC) d(FirstRec) w(PageNum) e

    s(CATALOGHEADER) w(magic) w(always8) w(always4) d(entries) a(zero, 30) e

    g(CTXOMAPREC) d(MapID) d(TopicOffset) e

    s(CTXOMAPREC) get_CTXOMAPREC(obj, buf + i);
i += sizeof_CTXOMAPREC;
e

    BOOL
    read_CTXOMAPRECs(CTXOMAPREC *objs, int n, FILE *file) {
  BYTE buf[sizeof_CTXOMAPREC];
  int i;
  for (i = 0; i < n; i++) {
    if (helpdeco_fread(buf, sizeof_CTXOMAPREC, file)) {
      objs[i].MapID = get_DWORD(buf);
      objs[i].TopicOffset = get_DWORD(buf + 4);
    } else
      return FALSE;
  }
  return TRUE;
}

s(STOPHEADER) uint32_t j = 0;
d(Magic) w(BytesUsed) while (j < 17){w(Unused[j++])} e

    s(PHRINDEXHDR) WORD bitfield = 0;
d(always4A01) d(entries) d(compressedsize) d(phrimagesize)
    d(phrimagecompressedsize) d(always0) bitfield = get_WORD(buf + i);
i += 2;
obj->bits = bitfield;
obj->unknown = bitfield >> 4;
w(always4A00) e

    s(CONTEXTREC) d(HashValue) d(TopicOffset) e

    s(FONTHEADER) w(NumFacenames) w(NumDescriptors) w(FacenamesOffset)
        w(DescriptorsOffset) w(NumFormats) w(FormatsOffset) w(NumCharmaps)
            w(CharmapsOffset) e

    s(CHARMAPHEADER) uint32_t j = 0;
w(Magic) w(Size) w(Unknown1) w(Unknown2) w(Entries) w(Ligatures)
    w(LigLen) while (j < 13){w(Unknown[j++])} e

    g(MVBFONT) w(FontName) w(expndtw) w(style) a(FGRGB, 3) a(BGRGB, 3) d(Height)
        a(mostlyzero, 12) w(Weight) b(unknown10) b(unknown11) b(Italic)
            b(Underline) b(StrikeOut) b(DoubleUnderline) b(SmallCaps)
                b(unknown17) b(unknown18) b(PitchAndFamily) b(unknown20) b(up) e

    s(MVBFONT) get_MVBFONT(obj, buf + i);
i += sizeof_MVBFONT;
e

    s(MVBSTYLE) w(StyleNum) w(BasedOn) get_MVBFONT(&obj->font, buf + i);
i += sizeof_MVBFONT;
a(unknown, 35) a(StyleName, 65) e

    g(NEWFONT) b(unknown1) w(FontName) a(FGRGB, 3) a(BGRGB, 3) b(unknown5)
        b(unknown6) b(unknown7) b(unknown8) b(unknown9) d(Height)
            a(mostlyzero, 12) w(Weight) b(unknown10) b(unknown11) b(Italic)
                b(Underline) b(StrikeOut) b(DoubleUnderline) b(SmallCaps)
                    b(unknown17) b(unknown18) b(PitchAndFamily) e

    s(NEWFONT) get_NEWFONT(obj, buf + i);
i += sizeof_NEWFONT;
e

    s(NEWSTYLE) w(StyleNum) w(BasedOn) get_NEWFONT(&obj->font, buf + i);
i += sizeof_NEWFONT;
a(unknown, 35) a(StyleName, 65) e

    s(OLDFONT) b(Attributes) b(HalfPoints) b(FontFamily) w(FontName) a(FGRGB, 3)
        a(BGRGB, 3) e

    s(TOPICBLOCKHEADER) d(LastTopicLink) d(FirstTopicLink) d(LastTopicHeader) e

    BOOL read_VIOLARECs(VIOLAREC *objs, int n, FILE *file) {
  BYTE buf[sizeof_VIOLAREC];
  int i;
  for (i = 0; i < n; i++) {
    if (helpdeco_fread(buf, sizeof_VIOLAREC, file)) {
      objs[i].TopicOffset = get_DWORD(buf);
      objs[i].WindowNumber = get_DWORD(buf + 4);
    } else
      return FALSE;
  }
  return TRUE;
}

BOOL read_CONTEXTRECs(CONTEXTREC *objs, int n, FILE *file) {
  BYTE buf[sizeof_CONTEXTREC];
  int i;
  for (i = 0; i < n; i++) {
    if (helpdeco_fread(buf, sizeof_CONTEXTREC, file)) {
      objs[i].HashValue = get_DWORD(buf);
      objs[i].TopicOffset = get_DWORD(buf + 4);
    } else
      return FALSE;
  }
  return TRUE;
}
