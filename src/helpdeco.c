/*
helpdeco -- utility program to dissect Windows help files
Copyright (C) 1997 Manfred Winterhoff
Copyright (C) 2001 Ben Collver

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

#include "helpdeco.h"
#include "helpdeco-internal.h"

static char topic_title[256];
static char scratch_buffer[4096];
static char keyword[512];

/* necessary compiler options for 16 bit version using Borland C/C++:
//   bcc -ml -K -Os -p helpdeco.c helpdec1.c
// Don't compile using Register Calling Convention in BC3.1: compiler bug.
// To compile 32 bit version using Microsoft VC4.0 create a new workspace
// for a WIN32 console application, insert HELPDECO.C and HELPDEC1.C into
// it and compile. Use /KNOWEAS 16 bit version as stub to create dual-mode
// application.
// Byte align ! Portable to little endian machines only.
*/
HELPDECO_CTX *helpdeco_make_ctx(void) {
  HELPDECO_CTX *ctx = calloc(1, sizeof(HELPDECO_CTX));
  if (!ctx) {
    helpdeco_errorf("Unable to allocate memory for helpdeco ctx");
  }

  size_t i;

  /* initialize hash value coding oldtable */
  memset(ctx->oldtable, 0, sizeof(ctx->oldtable));
  for (i = 0; i < 9; i++)
    ctx->oldtable['1' + i] = i + 1;
  ctx->oldtable['0'] = 10;
  ctx->oldtable['.'] = 12;
  ctx->oldtable['_'] = 13;
  for (i = 0; i < 26; i++)
    ctx->oldtable['A' + i] = ctx->oldtable['a' + i] = 17 + i;

  ctx->prefixhash[0] = 0;
  for (i = 1; ctx->prefix[i]; i++)
    ctx->prefixhash[i] = hash(ctx->prefix[i]);

  ctx->opt_extractmacros = TRUE;
  ctx->opt_guessing = TRUE;
  ctx->opt_resolvebrowse = TRUE;
  ctx->index_separators[0] = ',';
  ctx->index_separators[1] = ';';
  ctx->prefix[0] = "";
  ctx->prefix[1] = "idh_";
  ctx->prefix[2] = "helpid_";
  ctx->phrase_style = Old;

  return ctx;
}

void helpdeco_free_ctx(HELPDECO_CTX *ctx) {
  if (!ctx)
    return;

  if (ctx->phrases) {
    free(ctx->phrases);
    ctx->phrases = NULL;
  }

  if (ctx->external) {
    free(ctx->external);
    ctx->external = NULL;
  }

  if (ctx->alternative.entry) {
    free(ctx->alternative.entry);
    ctx->alternative.entry = NULL;
  }

  if (ctx->browse.entry) {
    free(ctx->browse.entry);
    ctx->browse.entry = NULL;
  }

  if (ctx->context_rec.entry) {
    free(ctx->context_rec.entry);
    ctx->context_rec.entry = NULL;
  }

  if (ctx->extension.entry) {
    free(ctx->extension.entry);
    ctx->extension.entry = NULL;
  }

  if (ctx->font.entry) {
    free(ctx->font.entry);
    ctx->font.entry = NULL;
  }

  if (ctx->font.entry) {
    free(ctx->font.entry);
    ctx->font.entry = NULL;
  }

  if (ctx->group.entry) {
    free(ctx->group.entry);
    ctx->group.entry = NULL;
  }

  if (ctx->hashrec.entry) {
    free(ctx->hashrec.entry);
    ctx->hashrec.entry = NULL;
  }

  if (ctx->phrase.offset) {
    free(ctx->phrase.offset);
    ctx->phrase.offset = NULL;
  }

  if (ctx->start.entry) {
    free(ctx->start.entry);
    ctx->start.entry = NULL;
  }

  if (ctx->topic.entry) {
    free(ctx->topic.entry);
    ctx->topic.entry = NULL;
  }

  if (ctx->windowname.entry) {
    free(ctx->windowname.entry);
    ctx->windowname.entry = NULL;
  }

  if (ctx->stopword_filename.entry) {
    free(ctx->stopword_filename.entry);
    ctx->stopword_filename.entry = NULL;
  }

  if (ctx->keyword_rec.entry) {
    free(ctx->keyword_rec.entry);
    ctx->keyword_rec.entry = NULL;
  }

  free(ctx);
}
HELPDECO_CTX *ctx;

#define MAXKEYWORDS                                                            \
  (64 << (sizeof(legacy_int) * 2)) /* 16 bit: 1024, 32 bit: 16348 */

/* index into bmpext: bit 0=multiresolution bit 1=bitmap, bit 2=metafile, bit
 * 3=hotspot data, bit 4=embedded, bit 5=transparent */
const char *const bmpext[] = {"???", "mrb", "bmp", "mrb", "wmf", "mrb",
                              "mrb", "mrb", "shg", "mrb", "shg", "mrb",
                              "shg", "mrb", "shg", "mrb"};

const unsigned char lookup[] = {0, 3, 1,
                                2, 4, 5}; /* to translate font styles */

const static signed char table[256] = {
    '\x00', '\xD1', '\xD2', '\xD3', '\xD4', '\xD5', '\xD6', '\xD7', '\xD8',
    '\xD9', '\xDA', '\xDB', '\xDC', '\xDD', '\xDE', '\xDF', '\xE0', '\xE1',
    '\xE2', '\xE3', '\xE4', '\xE5', '\xE6', '\xE7', '\xE8', '\xE9', '\xEA',
    '\xEB', '\xEC', '\xED', '\xEE', '\xEF', '\xF0', '\x0B', '\xF2', '\xF3',
    '\xF4', '\xF5', '\xF6', '\xF7', '\xF8', '\xF9', '\xFA', '\xFB', '\xFC',
    '\xFD', '\x0C', '\xFF', '\x0A', '\x01', '\x02', '\x03', '\x04', '\x05',
    '\x06', '\x07', '\x08', '\x09', '\x0A', '\x0B', '\x0C', '\x0D', '\x0E',
    '\x0F', '\x10', '\x11', '\x12', '\x13', '\x14', '\x15', '\x16', '\x17',
    '\x18', '\x19', '\x1A', '\x1B', '\x1C', '\x1D', '\x1E', '\x1F', '\x20',
    '\x21', '\x22', '\x23', '\x24', '\x25', '\x26', '\x27', '\x28', '\x29',
    '\x2A', '\x0B', '\x0C', '\x0D', '\x0E', '\x0D', '\x10', '\x11', '\x12',
    '\x13', '\x14', '\x15', '\x16', '\x17', '\x18', '\x19', '\x1A', '\x1B',
    '\x1C', '\x1D', '\x1E', '\x1F', '\x20', '\x21', '\x22', '\x23', '\x24',
    '\x25', '\x26', '\x27', '\x28', '\x29', '\x2A', '\x2B', '\x2C', '\x2D',
    '\x2E', '\x2F', '\x50', '\x51', '\x52', '\x53', '\x54', '\x55', '\x56',
    '\x57', '\x58', '\x59', '\x5A', '\x5B', '\x5C', '\x5D', '\x5E', '\x5F',
    '\x60', '\x61', '\x62', '\x63', '\x64', '\x65', '\x66', '\x67', '\x68',
    '\x69', '\x6A', '\x6B', '\x6C', '\x6D', '\x6E', '\x6F', '\x70', '\x71',
    '\x72', '\x73', '\x74', '\x75', '\x76', '\x77', '\x78', '\x79', '\x7A',
    '\x7B', '\x7C', '\x7D', '\x7E', '\x7F', '\x80', '\x81', '\x82', '\x83',
    '\x0B', '\x85', '\x86', '\x87', '\x88', '\x89', '\x8A', '\x8B', '\x8C',
    '\x8D', '\x8E', '\x8F', '\x90', '\x91', '\x92', '\x93', '\x94', '\x95',
    '\x96', '\x97', '\x98', '\x99', '\x9A', '\x9B', '\x9C', '\x9D', '\x9E',
    '\x9F', '\xA0', '\xA1', '\xA2', '\xA3', '\xA4', '\xA5', '\xA6', '\xA7',
    '\xA8', '\xA9', '\xAA', '\xAB', '\xAC', '\xAD', '\xAE', '\xAF', '\xB0',
    '\xB1', '\xB2', '\xB3', '\xB4', '\xB5', '\xB6', '\xB7', '\xB8', '\xB9',
    '\xBA', '\xBB', '\xBC', '\xBD', '\xBE', '\xBF', '\xC0', '\xC1', '\xC2',
    '\xC3', '\xC4', '\xC5', '\xC6', '\xC7', '\xC8', '\xC9', '\xCA', '\xCB',
    '\xCC', '\xCD', '\xCE', '\xCF'};
const unsigned char untable[] = {
    0,   '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 0,   '.', '_', 0,
    0,   0,   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'};

int32_t hash(char *name) /* convert 3.1/'95 topic name to hash value */
{
  int32_t hash;
  unsigned char *ptr;

  if (*name == '\0')
    return 1;
  for (hash = 0, ptr = (unsigned char *)name; *ptr; ptr++) {
    hash = hash * 43 + table[*ptr];
  }
  return hash;
}

char *unhash(uint32_t hash) /* deliver 3.1 context id that fits hash value */
{
  static char buffer[15];
  int32_t i, j, k;
  uint32_t hashlo, divlo, result, mask;
  unsigned char hashhi, divhi;
  char ch;

  i = 0;
  j = ctx->hashrec.count;
  while (i < j) {
    k = (i + j) / 2;
    if (ctx->hashrec.entry[k].hash < (int32_t)hash) {
      i = k + 1;
    } else if (ctx->hashrec.entry[k].hash > (int32_t)hash) {
      j = k;
    } else
      return ctx->hashrec.entry[k].name;
  }
  for (i = 0; i < 43; i++) {
    buffer[j = 14] = '\0';
    hashlo = hash;
    hashhi = i;
    while (1) {
      divhi = 21;
      divlo = 0x80000000UL;
      result = 0UL;
      for (mask = 0x80000000UL; mask; mask >>= 1) {
        if (hashhi > divhi || (hashhi == divhi && hashlo >= divlo)) {
          result |= mask;
          hashhi -= divhi;
          if (divlo > hashlo)
            hashhi--;
          hashlo -= divlo;
        }
        divlo >>= 1;
        if (divhi & 1)
          divlo |= 0x80000000UL;
        divhi >>= 1;
      }
      ch = untable[(int32_t)hashlo];
      if (!ch)
        break;
      buffer[--j] = ch;
      if (result == 0)
        return buffer + j;
      hashlo = result;
    }
  }
  /* should never happen */
  error("Can not find a matching string for hash value %08lx", hash);
  sprintf(buffer, "HASH%08lx", hash);
  return buffer;
}

char *ContextId(uint32_t hash) /* unhash and verify for legal entry point */
{
  char *ptr;
  legacy_int i;

  for (i = 0; i < ctx->context_rec.count; i++) {
    if (ctx->context_rec.entry[i].HashValue == hash) {
      return unhash(hash);
    }
  }
  ptr = unhash(hash);
  printf(
      "Help Compiler will issue Warning 4113: Unresolved jump or popup '%s'\n",
      ptr);
  return ptr;
}

void AddTopic(char *TopicName,
              BOOL derived) /* adds a known topic name to hash decode list */
{
  int32_t x;
  legacy_int i, j, k;

  x = hash(TopicName);
  i = 0;
  j = ctx->hashrec.count;
  while (i < j) {
    k = (i + j) / 2;
    if (ctx->hashrec.entry[k].hash < x) {
      i = k + 1;
    } else if (ctx->hashrec.entry[k].hash > x) {
      j = k;
    } else {
      if (stricmp(TopicName, ctx->hashrec.entry[k].name) != 0) {
        if (!ctx->hashrec.entry[k].derived) {
          if (!derived)
            fprintf(stderr, "ContextId %s already defined as %s\n", TopicName,
                    ctx->hashrec.entry[k].name);
          return;
        }
        if (derived)
          return;
        free(ctx->hashrec.entry[k].name);
        ctx->hashrec.entry[k].name = NULL;
        ctx->hashrec.entry[k].name = helpdeco_strdup(TopicName);
      }
      if (!derived && ctx->hashrec.entry[k].derived) {
        ctx->guessed--;
        ctx->hashrec.entry[k].derived = FALSE;
      }
      return;
    }
  }
  /* %100 to decrease memory fragmentation */
  if (ctx->hashrec.count % 100 == 0)
    ctx->hashrec.entry = helpdeco_realloc(
        ctx->hashrec.entry, (ctx->hashrec.count + 100) * sizeof(HASHREC));
  if (i < ctx->hashrec.count)
    memmove(ctx->hashrec.entry + i + 1, ctx->hashrec.entry + i,
            sizeof(HASHREC) * (ctx->hashrec.count - i));
  ctx->hashrec.entry[i].name = helpdeco_strdup(TopicName);
  ctx->hashrec.entry[i].derived = derived;
  ctx->hashrec.entry[i].hash = x;
  if (derived)
    ctx->guessed++;
  ctx->hashrec.count++;
}

/* ContextRec is sorted by TopicOffset. Binary search for an entry for
// TopicOffset topic. If more than one entry with identical TopicOffset
// is stored, return index of the first one, if not found return -1 */
legacy_int FindContext(int32_t topic) {
  legacy_int i, lwb, upb;

  lwb = 0;
  upb = ctx->context_rec.count;
  while (lwb < upb) {
    i = (lwb + upb) / 2;
    if (ctx->context_rec.entry[i].TopicOffset > topic) {
      upb = i;
    } else if (ctx->context_rec.entry[i].TopicOffset < topic) {
      lwb = i + 1;
    } else {
      while (i > 0 && ctx->context_rec.entry[i - 1].TopicOffset == topic)
        i--;
      return i;
    }
  }
  return -1;
}

/* Look if buf contains str (caseinsensitive) and update case of str so it is
// matching case appearance in buf and return TRUE, FALSE if not found */
legacy_int FindString(unsigned char *buf, legacy_int buflen, unsigned char *str,
                      legacy_int len) {
  legacy_int i, j;
  unsigned char ch;

  buflen -= len;
  for (i = 0; i <= buflen; i++) {
    for (j = 0; j < len; j++) {
      if (table[ch = buf[j]] != table[str[j]])
        break;
      str[j] = ch;
    }
    if (j == len) {
      return TRUE;
    }
    buf++;
  }
  return FALSE;
}

/* Many people and authoring systems create context ids from topic titles,
// either by replacing every illegal char with _ or . or leaving it out,
// sometimes using only part of the topic title or prefixing it with idh_
// or helpid_. This function tries all variants and may find a context id
// matching the hash value.
// And if it doesn't derive a context id, don't bother, unhash delivers
// one matching the hash code. This code is just to please the user.
// Win95 allows for nearly every char to be used as a context ident. */
BOOL Derive(unsigned char *str, uint32_t desiredhash, char *buffer) {
  legacy_int i, j, k, l, m, n, o, p, s;
  uint32_t hash, h, x, y;
  char *ptr;
  unsigned char ch;

  l = 2;
  s = strlen(str);
  for (i = !ctx->win95; i < l;
       i++) /* three variants what to do with illegal characters */
  {         /* but only if an illegal character found (see below) */
    for (j = 0; ctx->prefix[j]; j++) {
      k = 0;
      while (str[k]) {
        hash = ctx->prefixhash[j];
        strcpy(buffer, ctx->prefix[j]);
        ptr = strchr(buffer, '\0');
        for (m = k; str[m]; m++) {
          ch = str[m];
          if (i) {
            n = ctx->oldtable[ch];
            if (n == 0) {
              if (i == 2) {
                n = ctx->oldtable[ch = '_'];
              } else {
                continue;
              }
            }
          } else /* Win95 */
          {
            n = table[ch];
            if (n == 0)
              continue;
          }
          *ptr++ = ch;
          hash = 43 * hash + n;
          for (x = 1, o = 0; o < 6; o++, x *= 43) {
            h = desiredhash - x * hash;
            if (h < x) {
              y = x;
              p = 0;
              while (h) {
                y /= 43;
                ch = untable[(unsigned char)(h / y)];
                if (!ch)
                  break;
                h %= y;
                ptr[p++] = ch;
              }
              if (h == 0 && (FindString(str, s, ptr, p) || p < 3)) {
                ptr[p] = '\0';
                return TRUE;
              }
            }
          }
        }
        if (i) {
          while (ctx->oldtable[ch = str[k]])
            k++;
          if (ch)
            l = 3;
          while ((ch = str[k]) != '\0' && !ctx->oldtable[ch])
            k++;
        } else /* win95 */
        {
          while (table[ch = str[k]])
            k++;
          if (ch)
            l = 3;
          while ((ch = str[k]) != '\0' && !table[ch])
            k++;
        }
      }
    }
  }
  return FALSE;
}

void Guess(char *str, TOPICOFFSET topic) {
  legacy_int i, j, k, m;
  int32_t hash;

  i = FindContext(topic);
  if (i != -1) /* iff there is a # footnote assigned to that topic offset */
  {
    do {
      hash = ctx->context_rec.entry[i].HashValue;
      j = 0;
      k = ctx->hashrec.count;
      while (j < k) {
        m = (j + k) / 2;
        if (ctx->hashrec.entry[m].hash < hash) {
          j = m + 1;
        } else if (ctx->hashrec.entry[m].hash > hash) {
          k = m;
        } else
          break;
      }
      if (j >= k)
        if (Derive(str, hash, scratch_buffer)) {
          if (ctx->opt_reportderived)
            printf("Derived %s\n", scratch_buffer);
          AddTopic(scratch_buffer, TRUE);
        }
    } while (++i < ctx->context_rec.count &&
             ctx->context_rec.entry[i].TopicOffset == topic);
  }
}

void SysLoad(FILE *HelpFile) /* gets global values from SYSTEM file */
{
  SYSTEMRECORD *SysRec;
  SYSTEMHEADER SysHdr;
  SECWINDOW *SWin;
  legacy_int i;
  char kwdata[10];
  char kwbtree[10];

  if (!SearchFile(HelpFile, "|SYSTEM", NULL)) {
    helpdeco_errorf("Internal |SYSTEM file not found. Can't continue.\n");
  }
  read_SYSTEMHEADER(&SysHdr, HelpFile);
  ctx->before31 = SysHdr.Minor < 16;
  ctx->after31 = SysHdr.Minor > 21;
  ctx->multi = SysHdr.Minor == 27;
  ctx->lzcompressed =
      !ctx->before31 && (SysHdr.Flags == 4 || SysHdr.Flags == 8);
  ctx->win95 = SysHdr.Minor == 33 && !ctx->mvp;
  ctx->windowname.count = 0;
  ctx->windowname.entry = NULL;
  if (ctx->before31) {
    ctx->DecompressSize = ctx->topic_block_size = 2048;
  } else {
    ctx->DecompressSize = 0x4000;
    if (SysHdr.Flags == 8) {
      ctx->topic_block_size = 2048;
    } else {
      ctx->topic_block_size = 4096;
    }
  }
  if (ctx->before31) {
    helpdeco_gets(ctx->title, 33, HelpFile);
  } else {
    for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
         SysRec = GetNextSystemRecord(SysRec)) {
      switch (SysRec->RecordType) {
      case 0x0001:
        strlcpy(ctx->title, SysRec->Data, sizeof(ctx->title));
        break;
      case 0x0006:
        SWin = (SECWINDOW *)SysRec->Data;
        ctx->windowname.entry =
            helpdeco_realloc(ctx->windowname.entry,
                             (ctx->windowname.count + 1) * sizeof(char *));
        ctx->windowname.entry[ctx->windowname.count] = NULL;
        if (SWin->Flags & WSYSFLAG_NAME) {
          ctx->windowname.entry[ctx->windowname.count] = helpdeco_strdup(SWin->Name);
        }
        ctx->windowname.count++;
        break;
      case 0x000E:
        ctx->keyindex[SysRec->Data[1] - '0'] = TRUE;
        break;
      }
    }
  }
  for (i = 'A'; i <= 'z'; i++) {
    sprintf(kwdata, "|%cWDATA", i);
    sprintf(kwbtree, "|%cWBTREE", i);
    if (SearchFile(HelpFile, kwdata, NULL) &&
        SearchFile(HelpFile, kwbtree, NULL)) {
      ctx->lists[i - '0'] = TRUE;
    }
  }
}

/*********************************************************************************

  GRAPHICS STUFF
  ==============
  ExtractBitmap(..) extracts the graphics stored as |bmXXX in the Windows
  HelpFile to separate files. In this version multiple resolution pictures
  (with and without hotspots) are saved as MRB's, pictures with hotspots are
  converted to the SHG-format, single resolution Bitmaps without hotspots
  to standard BMP-format and all Metafiles without hotspots to the Aldus
  Placeable Metafile WMF format.

  GENERAL NOTES ABOUT |bmXXX
  --------------------------

  |bmXXX contains one MRB-File. This can be directly recompiled. If hotspots
  in the graphics contained in the MRB-File shall be added or changed with
  SHED, the MRB-File has to be split into one SHG file for each graphic. This
  is very easy as the graphics-data will only be slightly rewrapped. If the
  graphics themselves shall be edited with a standard drawing program, the
  SHG files have to be converted into bitmaps or metafiles (as flagged in
  the SHG). The hotspot data is lost by this step.

  MRB-FILES
  ---------

  MRBC takes the input-file(s), converts them to "lp"-Files if needed, adds
  a resolution specification if none is given in the file and finally outputs
  a "lp"-File with the ending ".MRB". Depending on the given display type
  MRBC sets the following resolutions:

               |	   |		   | x-resolution | y-resolution
  display-type | extension | std-extension |	(ppi)	  |    (ppi)
  -------------+-----------+---------------+--------------+-------------
       CGA     |   ".C*"   |	 ".CGA"    |	  96	  |	 48
       EGA     |   ".E*"   |	 ".EGA"    |	  96	  |	 72
       VGA     |   ".V*"   |	 ".VGA"    |	  96	  |	 96
       8514    |   ".8*"   |	 ".854"    |	 120	  |	120

  SHG-Files
  ---------

  Structure of SHG-Data:

   Offset    | Size	  | Ref | Description
   ----------+------------+-----+-----------------------------------------------
      0      | 1 Byte	  |	| always 0x01, maybe a version-number
      1      | 1 Word	  |  N	| the number of hotspots
      3      | 1 DWord	  |  M	| size of macro-data (in bytes)
      7      | 15xN Bytes | HSD | Hotspot-Data (see below)
    7 + 15*N | M Bytes	  |	| Macro-Data (ASCIIZ-Strings)
    7+15*N+M | 2*N*ASCIIZ |	| Hotspot-Id and Context- or Macro-Name as
             |		  |	| ASCIIZ-String

  Structure of Hotspot-Data:

   Offset    | Size	  | Ref | Description
   ----------+------------+-----+-------------------------------------------------
      0      | 3 Bytes	  |	| Hotspot-Type: 0xE3 0x00 0x00 = jump, visible
             |		  |	|		0xE7 0x04 0x00 = jump, invisible
             |		  |	|		0xE2 0x00 0x00 = pop-up, visible
             |		  |	|		0xE6 0x04 0x00 = pop-up,
invisible |		  |	|		0xC8 0x00 0x00 = macro, visible
             |		  |	|		0xCC 0x04 0x00 = macro,
invisible 3      | 1 Word	  |	| Left border of hotspot 5      | 1 Word
|	| Top border of hotspot 7      | 1 Word	  |	| Right border - Left
border 9      | 1 Word	  |	| Bottom border - Top border 11      | 1 DWord
|	| 0x00000000 => nothing |		  |	| 0x00000001 => this is
a macro-hotspot |		  |	|   others   => hash-value of the
context-string |		  |	|		according to the WinHelp
standard

  03-06-1995 Holger Haase
**********************************************************************************/

/* compares filename a[.HLP] and filename b[.HLP], returning 0 if match */
int filenamecmp(const char *a, const char *b) {
  char aname[NAME_MAX], bname[NAME_MAX];
  char aext[_MAX_EXT], bext[_MAX_EXT];
  legacy_int i;

  _splitpath(a, NULL, NULL, aname, aext);
  _splitpath(b, NULL, NULL, bname, bext);
  if (aext[0] == '\0')
    strcpy(aext, ".hlp");
  if (bext[0] == '\0')
    strcpy(bext, ".hlp");
  i = strcmpi(aname, bname);
  if (i)
    return i;
  return strcmpi(aext, bext);
}

/* store external reference in list, checked later */
void StoreReference(char *filename, legacy_int type, char *id, int32_t hash) {
  CHECKREC *ptr;
  FILEREF *ref;
  PLACEREC *place;

  for (ref = ctx->external; ref; ref = ref->next) {
    if (filenamecmp(filename, ref->filename) == 0)
      break;
  }
  if (!ref) {
    ref = helpdeco_malloc(sizeof(FILEREF) + strlen(filename));
    strcpy(ref->filename, filename);
    ref->check = NULL;
    ref->next = ctx->external;
    ctx->external = ref;
  }
  for (ptr = ref->check; ptr; ptr = ptr->next) {
    if (ptr->type == type && ptr->hash == hash)
      break;
  }
  if (!ptr) {
    ptr = helpdeco_malloc(sizeof(CHECKREC));
    ptr->type = type;
    ptr->hash = hash;
    ptr->id = helpdeco_strdup(id);
    ptr->here = NULL;
    ptr->next = ref->check;
    ref->check = ptr;
  }
  if (ctx->opt_listtopic && topic_title[0]) {
    place = helpdeco_malloc(sizeof(PLACEREC) + strlen(topic_title));
    strcpy(place->topicname, topic_title);
    place->next = ptr->here;
    ptr->here = place;
  }
}

/* validate each entry in list of external references */
void CheckReferences(void) {
  FILEREF *ref;
  CHECKREC *ptr;
  FILE *f;
  BOOL found;
  legacy_long offset;
  legacy_int i, n;
  CTXOMAPREC CTXORec;
  BTREEHEADER BTreeHdr;
  BTREENODEHEADER CurrNode;
  CONTEXTREC ContextRec;

  for (ref = ctx->external; ref; ref = ref->next) {
    f = fopen(ref->filename, "rb");
    if (!f) {
      printf("%s not found\n", ref->filename);
    } else {
      if (SearchFile(f, NULL, NULL)) {
        for (ptr = ref->check; ptr; ptr = ptr->next) {
          found = FALSE;
          if (ptr->type == CONTEXT) {
            if (SearchFile(f, "|CTXOMAP", NULL)) {
              n = helpdeco_getw(f);
              for (i = 0; i < n; i++) {
                read_CTXOMAPREC(&CTXORec, f);
                if (CTXORec.MapID == ptr->hash) /* hash is context id */
                {
                  found = TRUE;
                  break;
                }
              }
            }
          } else {
            if (SearchFile(f, "|CONTEXT", NULL)) {
              read_BTREEHEADER(&BTreeHdr, f);
              offset = ftell(f);
              CurrNode.PreviousPage = BTreeHdr.RootPage;
              for (n = 1; n < BTreeHdr.NLevels; n++) {
                fseek(f,
                      offset + CurrNode.PreviousPage *
                                   (legacy_long)BTreeHdr.PageSize,
                      SEEK_SET);
                read_BTREEINDEXHEADER_to_BTREENODEHEADER(&CurrNode, f);
                for (i = 0; i < CurrNode.NEntries; i++) {
                  ContextRec.HashValue = helpdeco_getdw(f);
                  if (ContextRec.HashValue > ptr->hash)
                    break;
                  CurrNode.PreviousPage = helpdeco_getw(f); /* Page */
                }
              }
              fseek(f,
                    offset +
                        CurrNode.PreviousPage * (legacy_long)BTreeHdr.PageSize,
                    SEEK_SET);
              read_BTREENODEHEADER(&CurrNode, f);
              for (i = 0; i < CurrNode.NEntries; i++) {
                read_CONTEXTREC(&ContextRec, f);
                if (ContextRec.HashValue == ptr->hash)
                  found = TRUE;
                if (ContextRec.HashValue >= ptr->hash)
                  break;
              }
            }
          }
          if (!found) {
            if (ptr->id) {
              printf("%s@%s not found\n", ptr->id, ref->filename);
            } else {
              printf("0x%08lx@%s not found\n", ptr->hash, ref->filename);
            }
            while (ptr->here) {
              printf("  %s\n", ptr->here->topicname);
              ptr->here = ptr->here->next;
            }
          }
        }
      } else {
        printf("%s isn't a valid WinHelp file !\n", ref->filename);
      }
      fclose(f);
    }
  }
}

/* list each entry in list of external references */
void ListReferences(void) {
  FILEREF *ref;
  CHECKREC *ptr;

  ctx->hashrec.count = 0;
  for (ref = ctx->external; ref; ref = ref->next) {
    for (ptr = ref->check; ptr; ptr = ptr->next) {
      printf("%s ", ref->filename);
      switch (ptr->type) {
      case TOPIC:
        printf("topic id ");
        if (ptr->id) {
          printf("'%s'", ptr->id);
        } else {
          printf("0x%08lx='%s'", ptr->hash, unhash(ptr->hash));
        }
        break;
      case CONTEXT:
        printf("[MAP] id ");
        if (ptr->id) {
          printf("'%s'", ptr->id);
        } else {
          printf("0x%08lx=(%ld)", ptr->hash, ptr->hash);
        }
        break;
      }
      if (ptr->here) {
        printf(" referenced from:");
        while (ptr->here) {
          printf("\n	%s", ptr->here->topicname);
          ptr->here = ptr->here->next;
        }
      }
      putchar('\n');
    }
  }
}

void unquote(char *ptr) {
  char *dest;

  dest = ptr;
  do {
    if (*ptr == '\\' && ptr[1] != '\0') {
      ptr++;
    }
  } while ((*dest++ = *ptr++) != '\0');
}

char *findMatchingChar(char *p) /* searches for matching '`', '"', or '('. */
{
  char match = '\'';

  if (*p == '"')
    match = '"';
  else if (*p == '(')
    match = ')';
  else if (*p != '`')
    return p; /* no match for this string */
  for (p++; *p != '\0' && *p != match; p++) {
    if (*p == '\\' && p[1] != '\0') {
      p++;
    } else if (*p == '`' || *p == '"' || *p == '(') {
      if (!*(p = findMatchingChar(p)))
        break; /* error: end of string */
    }
  }
  return p;
}

/* scan macro string for topic names and external references */
BOOL CheckMacroX(char *ptr) {
  const static char *const macro[] = {"AddAccelerator(ssm",
                                      "AA(ssm",
                                      "AppendItem(sssm",
                                      "AI(sssm",
                                      "AL(ssts",
                                      "ChangeButtonBinding(sm",
                                      "CBB(sm",
                                      "CB(ssm",
                                      "CE(sm",
                                      "ChangeItemBinding(sm",
                                      "CIB(sm",
                                      "ExtInsertItem(sssmss",
                                      "IfThen(mm",
                                      "IT(mm",
                                      "IfThenElse(mmm",
                                      "IE(mmm",
                                      "ExecFile(sssm",
                                      "EF(sssm",
                                      "InsertItem(sssms",
                                      "II(sssms",
                                      "JumpContext(x",
                                      "JC(x",
                                      "JumpContext(fx",
                                      "JC(fx",
                                      "JumpHash(h",
                                      "JH(h",
                                      "JumpHash(fh",
                                      "JH(fh",
                                      "JumpId(c",
                                      "JI(c",
                                      "JumpId(fc",
                                      "JI(fc",
                                      "KL(ssts",
                                      "MPrintID(c",
                                      "Not(m",
                                      "PopupId(c",
                                      "PI(c",
                                      "PopupId(fc",
                                      "PI(fc",
                                      "PopupContext(x",
                                      "PC(x",
                                      "PopupContext(fx",
                                      "PC(fx",
                                      "PopupHash(h",
                                      "PopupHash(fh",
                                      "SetContents(fx",
                                      "SE(sssssc",
                                      "SH(sssst",
                                      "UpdateWindow(fc",
                                      "UW(fc"};
  char *name;
  legacy_int namelen, parms, i, n, l;
  char *parm[20];

  while (1) {
    while (*ptr == ' ')
      ptr++;
    if (*ptr == '\0')
      return TRUE;
    if (!isalpha(*ptr))
      return FALSE;
    for (name = ptr, namelen = 0; isalnum(*ptr) || *ptr == '_'; namelen++)
      ptr++;
    while (*ptr == ' ')
      ptr++;
    if (*ptr != '(')
      return FALSE;
    ptr++;
    parms = 0;
    while (1) {
      while (*ptr == ' ')
        ptr++;
      parm[parms] = ptr; /* save after skip spaces */
      if (*ptr == '`' || *ptr == '"') {
        parm[parms++] = ptr + 1; /* advance past quote */
        ptr = findMatchingChar(ptr);
        if (*ptr == '\0')
          return FALSE; /* unexpected end of string */
        *ptr++ = '\0';  /* zap terminating quote */
        while (*ptr == ' ' || *ptr == '\t')
          ptr++;
      } else if (*ptr != ')' && *ptr != '\0') {
        parm[parms++] = ptr;
        while (*ptr != ',' && *ptr != ')' && *ptr != '\0') {
          if (*ptr == '(' || *ptr == '`' ||
              *ptr == '"') /* `, " probably not needed */
          {
            ptr = findMatchingChar(ptr);
            if (*ptr == '\0')
              return FALSE;
          }
          ptr++;
        }
      }
      if (*ptr == ')')
        break;
      if (*ptr != ',')
        return FALSE;
      *ptr++ = '\0';
    }
    *ptr++ = '\0';
    for (i = 0; i < sizeof(macro) / sizeof(macro[0]); i++) {
      if (strcspn(macro[i], "(") == namelen &&
          memicmp(macro[i], name, namelen) == 0 &&
          strlen(macro[i] + namelen + 1) >= parms)
        break;
    }
    if (i < sizeof(macro) / sizeof(macro[0])) /* macro of interest */
    {
      char *at;

      for (n = 0; n < parms; n++)
        unquote(parm[n]);
      for (n = 0; n < parms; n++) {
        if (macro[i][namelen + 1 + n] == 'm') {
          CheckMacroX(parm[n]); /* recursive */
        } else if (macro[i][namelen + 1 + n] == 'c') {
          if (ctx->opt_extractmacros) {
            while (parm[n][0] == ' ')
              parm[n]++;
            for (l = strlen(parm[n]); l > 0 && parm[n][l - 1] == ' '; l--)
              ;
            parm[n][l] = '\0';
            AddTopic(parm[n], FALSE);
          }
        } else if (macro[i][namelen + 1 + n] == 'f') {
          at = strchr(parm[n], '>');
          if (at) {
            if (filenamecmp(parm[n], name) != 0) {
              if (macro[i][namelen + 2 + n] == 'c') {
                StoreReference(parm[n], TOPIC, parm[n + 1], hash(parm[n + 1]));
                n++;
              } else if (macro[i][namelen + 2 + n] == 'x') {
                StoreReference(parm[n], CONTEXT, parm[n + 1],
                               strtoul(parm[n + 1], NULL, 0));
                n++;
              } else if (macro[i][namelen + 2 + n] == 'h') {
                StoreReference(parm[n], TOPIC, parm[n + 1],
                               strtoul(parm[n + 1], NULL, 0));
                n++;
              }
            }
          }
        } else if (macro[i][namelen + 1 + n] == 't') {
          at = strchr(parm[n], '@');
          if (at) {
            if (filenamecmp(at + 1, name) != 0) {
              *at = '\0';
              StoreReference(at + 1, TOPIC, parm[n], hash(parm[n]));
            } else {
              AddTopic(parm[n], FALSE);
            }
          } else {
            AddTopic(parm[n], FALSE);
          }
        }
      }
    }
    while (*ptr == ' ')
      ptr++;
    if (*ptr != ':' && *ptr != ';')
      break;
    ptr++;
  }
  return TRUE;
}

void CheckMacro(char *ptr) {
  char *temp;

  if (!ctx->multi) {
    temp = helpdeco_strdup(ptr);
    if (!CheckMacroX(temp))
      fprintf(stderr, "Bad macro: %s\n", ptr);
    free(temp);
  }
}

/* check hotspot info of bitmap for topic names and external references and
// extract (write to file) if checkexternal not set, write out first bitmap
// or metafile only (no SHG/MRB) if exportplain (create lookalike rtf) set */
legacy_int ExtractBitmap(char *szFilename, MFILE *f) {
  FILE *fTarget;
  char *filename;
  legacy_int type;
  unsigned_legacy_int i, n, j;
  unsigned char byType, byPacked;
  legacy_long l, pos = 0, offset = 0, nextpict, FileStart;
  BITMAPFILEHEADER bmfh;
  BITMAPINFOHEADER bmih;
  APMFILEHEADER afh;
  uint16_t *wp;
  uint16_t wMagic, mapmode = 0, colors = 0;
  uint32_t dwRawSize = 0, dwDataSize, dwHotspotOffset, dwOffsBitmap,
           dwHotspotSize, dwPictureOffset, xPels = 0, yPels = 0;

  FileStart = f->tell(f);
  wMagic = mfile_get_word(f);
  if (wMagic != 0x506C /* SHG */ && wMagic != 0x706C /* MRB */) {
    error("Unknown magic 0x%04X (%c%c)", wMagic, wMagic & 0x00FF, wMagic >> 8);
    return 0;
  }
  fTarget = NULL;
  n = mfile_get_word(f);
  type = !ctx->exportplain && n > 1; /* contains multiple resolutions */
  /* do not depend on wMagic, because it is sometimes incorrect */
  nextpict = 4 + 4 * n;
  for (j = 0; j < n; j++) {
    f->seek(f, FileStart + 4 + 4 * j);
    dwOffsBitmap = mfile_get_dword(f);
    f->seek(f, FileStart + dwOffsBitmap);
    byType = f->get(f); /* type of picture: 5=DDB, 6=DIB, 8=METAFILE */
    byPacked =
        f->get(f); /* packing method: 0=unpacked, 1=RunLen, 2=LZ77, 3=both */
    if ((byType == 6 && byPacked < 4) || (byType == 5 && byPacked < 2)) {
      type |= 2; /* contains bitmap */
      memset(&bmfh, 0, sizeof(bmfh));
      memset(&bmih, 0, sizeof(bmih));
      bmfh.bfType = 0x4D42; /* bitmap magic ("BM") */
      bmih.biSize = sizeof(bmih);
      xPels = mfile_get_cdword(f);
      /* HC30 doesn't like certain PelsPerMeter */
      if (!ctx->before31)
        bmih.biXPelsPerMeter = (xPels * 79 + 1) / 2;
      yPels = mfile_get_cdword(f);
      if (!ctx->before31)
        bmih.biYPelsPerMeter = (yPels * 79 + 1) / 2;
      bmih.biPlanes = mfile_get_cword(f);
      bmih.biBitCount = mfile_get_cword(f);
      bmih.biWidth = mfile_get_cdword(f);
      bmih.biHeight = mfile_get_cdword(f);
      colors = (legacy_int)(bmih.biClrUsed = mfile_get_cdword(f));
      if (!colors)
        colors = (uint16_t)1 << bmih.biBitCount;
      bmih.biClrImportant = mfile_get_cdword(f);
      if (ctx->after31 && bmih.biClrImportant == 1)
        type |= 0x20; /* contains transparent bitmap */
      dwDataSize = mfile_get_cdword(f);
      dwHotspotSize = mfile_get_cdword(f);
      dwPictureOffset = mfile_get_dword(f);
      dwHotspotOffset = mfile_get_dword(f);
      if ((ctx->exportplain || n == 1) &&
          (dwHotspotOffset == 0 || dwHotspotSize == 0)) {
        if (ctx->checkexternal)
          break;
        strcat(szFilename, ".bmp");
        fTarget = helpdeco_fopen(szFilename, "wb");
        if (fTarget) {
          fwrite(&bmfh, 1, sizeof(bmfh), fTarget);
          fwrite(&bmih, 1, sizeof(bmih), fTarget);
          if (byType == 6) {
            mfile_copy_bytes(f, colors * 4, fTarget);
          } else {
            helpdeco_putdw(0x000000, fTarget);
            helpdeco_putdw(0xFFFFFF, fTarget);
          }
          bmfh.bfOffBits = sizeof(bmfh) + sizeof(bmih) + colors * 4;
          bmih.biSizeImage =
              (((bmih.biWidth * bmih.biBitCount + 31) / 32) * 4) *
              bmih.biHeight;
          if (byType == 5) /* convert DDB to DIB */
          {
            legacy_long width, length;
            unsigned char count, value;
            legacy_int pad;

            width = ((bmih.biWidth * bmih.biBitCount + 15) / 16) * 2;
            pad = (legacy_int)(((width + 3) / 4) * 4 - width);
            count = value = 0;
            for (l = 0; l < bmih.biHeight; l++) {
              if (byPacked == 1) {
                for (length = 0; length < width; length++) {
                  if ((count & 0x7F) == 0) {
                    count = f->get(f);
                    value = f->get(f);
                  } else if (count & 0x80) {
                    value = f->get(f);
                  }
                  putc(value, fTarget);
                  count--;
                }
              } else {
                mfile_copy_bytes(f, width, fTarget);
              }
              if (pad)
                fwrite(scratch_buffer, pad, 1, fTarget);
            }
          } else {
            mfile_decompress_into_file(byPacked, f, dwDataSize, fTarget);
          }
          /* update bitmap headers */
          bmfh.bfSize = ftell(fTarget);
          fseek(fTarget, 0, SEEK_SET);
          fwrite(&bmfh, 1, sizeof(bmfh), fTarget);
          fwrite(&bmih, 1, sizeof(bmih), fTarget);
        }
        break;
      }
    } else if (byType == 8 && byPacked < 4) /* Windows MetaFile */
    {
      type |= 4; /* contains metafile */
      memset(&afh, 0, sizeof(afh));
      mapmode = mfile_get_cword(f);          /* mapping mode */
      afh.rcBBox.right = mfile_get_word(f);  /* width of metafile-picture */
      afh.rcBBox.bottom = mfile_get_word(f); /* height of metafile-picture */
      dwRawSize = mfile_get_cdword(f);
      dwDataSize = mfile_get_cdword(f);
      dwHotspotSize = mfile_get_cdword(f);
      dwPictureOffset = mfile_get_dword(f);
      dwHotspotOffset = mfile_get_dword(f);
      if ((ctx->exportplain || n == 1) &&
          (dwHotspotOffset == 0 || dwHotspotSize == 0)) {
        if (ctx->checkexternal)
          break;
        afh.dwKey = 0x9AC6CDD7;
        afh.wInch = 2540;
        wp = (uint16_t *)&afh;
        for (i = 0; i < 10; i++)
          afh.wChecksum ^= *wp++;
        strcat(szFilename, ".wmf");
        fTarget = helpdeco_fopen(szFilename, "wb");
        if (fTarget) {
          fwrite(&afh, 1, sizeof(afh), fTarget);
          mfile_decompress_into_file(byPacked, f, dwDataSize, fTarget);
        }
        break;
      }
    } else {
      error("Unknown format (%d) or packing method (%d)", byType, byPacked);
      break;
    }
    type |= 8; /* contains hotspot info (set before accessing bmpext) */
    if (!ctx->checkexternal) {
      if (!fTarget) {
        strcat(szFilename, ".");
        strcat(szFilename, bmpext[type & 0x0F]);
        fTarget = helpdeco_fopen(szFilename, "wb");
        if (!fTarget)
          break;
        helpdeco_putw(wMagic, fTarget);
        helpdeco_putw(n, fTarget);
      }
      fseek(fTarget, 4 + 4 * j, SEEK_SET);
      helpdeco_putdw(nextpict, fTarget);
      fseek(fTarget, nextpict, SEEK_SET);
      putc(byType, fTarget);
      if (ctx->opt_exportLZ77) {
        putc(byPacked, fTarget);
      } else {
        putc(byPacked & 1, fTarget);
      }
      if (byType == 8) {
        helpdeco_putcw(mapmode, fTarget); /* mapping mode */
        helpdeco_putw(afh.rcBBox.right, fTarget);  /* width of metafile-picture */
        helpdeco_putw(afh.rcBBox.bottom, fTarget); /* height of metafile-picture */
        helpdeco_putcdw(dwRawSize, fTarget);
      } else {
        helpdeco_putcdw(xPels, fTarget);
        helpdeco_putcdw(yPels, fTarget);
        helpdeco_putcw(bmih.biPlanes, fTarget);
        helpdeco_putcw(bmih.biBitCount, fTarget);
        helpdeco_putcdw(bmih.biWidth, fTarget);
        helpdeco_putcdw(bmih.biHeight, fTarget);
        helpdeco_putcdw(bmih.biClrUsed, fTarget);
        helpdeco_putcdw(bmih.biClrImportant, fTarget);
      }
      pos = ftell(fTarget);
      helpdeco_putdw(0, fTarget); /* changed later ! */
      helpdeco_putdw(0, fTarget); /* changed later ! */
      helpdeco_putdw(0, fTarget); /* changed later ! */
      helpdeco_putdw(0, fTarget); /* changed later ! */
      if (byType == 6)
        mfile_copy_bytes(f, colors * 4, fTarget);
      offset = ftell(fTarget);
      f->seek(f, FileStart + dwOffsBitmap + dwPictureOffset);
      if (ctx->opt_exportLZ77) {
        dwDataSize = mfile_copy_bytes(f, dwDataSize, fTarget);
      } else {
        dwDataSize = mfile_decompress_into_file(byPacked & 2, f, dwDataSize, fTarget);
      }
    }
    if (dwHotspotSize) {
      f->seek(f, FileStart + dwOffsBitmap + dwHotspotOffset);
      if (f->get(f) != 1) {
        fputs("No hotspots\n", stderr);
        dwHotspotSize = 0;
      } else {
        unsigned_legacy_int hotspots, n, j, l;
        uint32_t MacroDataSize;
        char *ptr;
        HOTSPOT *hotspot;

        hotspots = mfile_get_word(f);
        MacroDataSize = mfile_get_dword(f);
        hotspot = helpdeco_malloc(hotspots * sizeof(HOTSPOT));
        f->read(f, hotspot, sizeof(HOTSPOT) * hotspots);
        if (ctx->checkexternal) {
          while (MacroDataSize-- > 0)
            f->get(f);
        } else {
          putc(1, fTarget);
          helpdeco_putw(hotspots, fTarget);
          helpdeco_putdw(MacroDataSize, fTarget);
          fwrite(hotspot, sizeof(HOTSPOT), hotspots, fTarget);
          if (MacroDataSize)
            mfile_copy_bytes(f, MacroDataSize, fTarget);
        }
        for (n = 0; n < hotspots; n++) {
          j = mfile_read_string(scratch_buffer, sizeof(scratch_buffer), f) + 1;
          l = j +
              mfile_read_string(scratch_buffer + j, sizeof(scratch_buffer) - j, f) + 1;
          if (fTarget)
            fwrite(scratch_buffer, l, 1, fTarget);
          if (ctx->opt_extractmacros)
            switch (hotspot[n].id0) {
            case 0xC8: /* macro (never seen) */
            case 0xCC: /* macro without font change */
              CheckMacro(scratch_buffer + j);
              break;
            case 0xE0: /* popup jump HC30 */
            case 0xE1: /* topic jump HC30 */
            case 0xE2: /* popup jump HC31 */
            case 0xE3: /* topic jump HC31 */
            case 0xE6: /* popup jump without font change */
            case 0xE7: /* topic jump without font change */
              if (hash(scratch_buffer + j) != hotspot[n].hash) {
                fprintf(stderr, "Wrong hash %08lx instead %08lx for '%s'\n",
                        hotspot[n].hash, hash(scratch_buffer + j),
                        scratch_buffer + j);
              }
              AddTopic(scratch_buffer + j, FALSE);
              break;
            case 0xEA: /* popup jump into external file */
            case 0xEB: /* topic jump into external file / secondary window */
            case 0xEE: /* popup jump into external file without font change */
            case 0xEF: /* topic jump into external file / secondary window
                          without font change */
              if ((hotspot[n].id1 != 0 && hotspot[n].id1 != 1 &&
                   hotspot[n].id1 != 4 && hotspot[n].id1 != 6) ||
                  (hotspot[n].id2 != 0)) {
              } else {
                filename = strchr(scratch_buffer + j, '@');
                if (filename)
                  *filename++ = '\0';
                ptr = strchr(scratch_buffer + j, '>');
                if (ptr)
                  *ptr = '\0';
                if (filename) {
                  StoreReference(filename, TOPIC, scratch_buffer + j,
                                 hash(scratch_buffer + j));
                } else {
                  AddTopic(scratch_buffer + j, FALSE);
                }
                break;
              }
            default:
              error("Unknown hotspot %02x %02x %02x X=%u Y=%u W=%u H=%u "
                    "%08lx,%s,%s",
                    hotspot[n].id0, hotspot[n].id1, hotspot[n].id2,
                    hotspot[n].x, hotspot[n].y, hotspot[n].w, hotspot[n].h,
                    hotspot[n].hash, scratch_buffer, scratch_buffer + j);
            }
        }
        free(hotspot);
        hotspot = NULL;
      }
    }
    if (!ctx->checkexternal) {
      dwPictureOffset = offset - nextpict;
      nextpict = ftell(fTarget);
      /* fix up some locations */
      fseek(fTarget, pos, SEEK_SET);
      helpdeco_putdw((dwDataSize << 1) + 1, fTarget);
      helpdeco_putdw((dwHotspotSize << 1) + 1, fTarget);
      helpdeco_putdw(dwPictureOffset, fTarget);
      if (dwHotspotSize)
        helpdeco_putdw(dwPictureOffset + dwDataSize, fTarget);
    }
  }
  if (fTarget)
    helpdeco_fclose(fTarget);
  return type;
}
/****************************************************************************
END OF GRAPHICS STUFF
**************************************************************************/

char *getbitmapname(
    unsigned_legacy_int n) /* retrieve extension of exported bitmap n */
{
  static char name[12];

  if (n < ctx->extension.count && ctx->extension.entry[n]) {
    sprintf(name, "bm%u.%s", n, bmpext[ctx->extension.entry[n] & 0x0F]);
  } else if (n == 65535U) {
    ctx->missing = TRUE;
    fputs("There was a picture file rejected on creation of helpfile.\n",
          stderr);
    strcpy(name, "missing.bmp");
  } else /* should never happen */
  {
    ctx->warnings = TRUE;
    fprintf(stderr, "Bitmap bm%u not exported\n", n);
    sprintf(name, "bm%u.bmp", n);
  }
  return name;
}

void ListBitmaps(FILE *hpj) /* writes out [BITMAPS] section */
{
  legacy_int i;

  if (hpj && ctx->extension.count) {
    fputs("[BITMAPS]\n", hpj);
    for (i = 0; i < ctx->extension.count; i++)
      if (ctx->extension.entry[i]) {
        fprintf(hpj, "bm%u.%s\n", i, bmpext[ctx->extension.entry[i] & 0x0F]);
      }
    putc('\n', hpj);
  }
}

void ExportBitmaps(FILE *HelpFile) /* export all bitmaps */
{
  BUFFER buf;
  MFILE *mf;
  char *leader;
  char FileName[255];
  legacy_long FileLength;
  legacy_int i, num, n, type;
  legacy_long savepos;

  leader = &"|bm"[ctx->before31];
  SearchFile(HelpFile, NULL, NULL);
  for (n = GetFirstPage(HelpFile, &buf, NULL); n;
       n = GetNextPage(HelpFile, &buf)) {
    for (i = 0; i < n; i++) {
      helpdeco_gets(FileName, sizeof(FileName), HelpFile);
      helpdeco_getdw(HelpFile);
      if (memcmp(FileName, leader, strlen(leader)) == 0) {
        savepos = ftell(HelpFile);
        if (SearchFile(HelpFile, FileName, &FileLength)) {
          mf = mfile_create_virtual(HelpFile);
          type = ExtractBitmap(FileName + (FileName[0] == '|'), mf);
          mfile_close(mf);
          if (type) {
            num = atoi(FileName + (FileName[0] == '|') + 2);
            if (num >= ctx->extension.count) {
              ctx->extension.entry =
                  helpdeco_realloc(ctx->extension.entry, (num + 1) * sizeof(char));
              while (ctx->extension.count <= num)
                ctx->extension.entry[ctx->extension.count++] = 0;
            }
            ctx->extension.entry[num] = type;
          }
        }
        fseek(HelpFile, savepos, SEEK_SET);
      }
    }
  }
}

char *TopicName(int32_t topic) {
  static char name[20];
  legacy_int i;

  if (ctx->before31) {
    if (topic == 0)
      topic = ctx->topic.entry[0];
    for (i = 16; i < ctx->topic.count; i++)
      if (ctx->topic.entry[i] == topic) {
        sprintf(name, "TOPIC%d", i);
        return name;
      }
  } else {
    if (topic == -1) {
      ctx->NotInAnyTopic = TRUE;
      return "21KSYK4"; /* evaluates to -1 without generating help compiler
                           warning */
    }
    i = FindContext(topic);
    if (i != -1) {
      return unhash(ctx->context_rec.entry[i].HashValue);
    }
  }
  if (topic)
    fprintf(stderr, "Can not find topic offset %08lx\n", topic);
  return NULL;
}

char *
GetWindowName(legacy_long n) /* secondary window name from window number */
{
  if (ctx->windowname.entry == NULL || n < 0 || n >= ctx->windowname.count ||
      ctx->windowname.entry[n] == NULL)
    return "main";
  return ctx->windowname.entry[n];
}

/* create HPJ file from contents of |SYSTEM internal file */
void SysList(FILE *HelpFile, FILE *hpj, char *IconFileName) {
  legacy_long FileLength;
  SYSTEMHEADER SysHdr;
  SYSTEMRECORD *SysRec;
  STOPHEADER StopHdr;
  char name[51];
  char *ptr;
  legacy_long color;
  FILE *f;
  legacy_int fbreak, macro, windows, i, keywords, dllmaps, n;

  if (hpj && SearchFile(HelpFile, "|SYSTEM", NULL)) {
    read_SYSTEMHEADER(&SysHdr, HelpFile);
    if (SysHdr.Minor == 15) {
      strcpy(ctx->suggested_compiler, "HC30");
    } else if (SysHdr.Minor == 21) {
      strcpy(ctx->suggested_compiler, "HC31/HCP");
    } else if (SysHdr.Minor == 27) {
      strcpy(ctx->suggested_compiler, "WMVC/MVCC");
    } else if (SysHdr.Minor == 33) {
      if (ctx->mvp) {
        strcpy(ctx->suggested_compiler, "MVC");
      } else {
        strcpy(ctx->suggested_compiler, "HCRTF");
      }
    }
    fputs("[OPTIONS]\n", hpj);
    if (ctx->before31) /* If 3.0 get title */
    {
      helpdeco_gets(ctx->title, 33, HelpFile);
      if (ctx->title[0] != '\0' && ctx->title[0] != '\n') {
        fprintf(hpj, "TITLE=%s\n", ctx->title);
      }
      fprintf(hpj, "INDEX=%s\n", TopicName(0));
      if (ctx->phrase.count) {
        fputs("COMPRESS=TRUE\n", hpj);
      } else {
        fputs("COMPRESS=FALSE\n", hpj);
      }
      for (i = 'A'; i <= 'z'; i++)
        if (ctx->lists[i - '0'] && i != 'K') {
          fprintf(hpj, "MULTIKEY=%c\n", i);
        }
      putc('\n', hpj);
    } else /* else get 3.1 System records */
    {
      macro = 0;
      fbreak = 0;
      windows = 0;
      keywords = 0;
      dllmaps = 0;
      for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
           SysRec = GetNextSystemRecord(SysRec)) {
        switch (SysRec->RecordType) {
        case 0x0001:
          if (SysRec->Data[0])
            fprintf(hpj, "TITLE=%s\n", SysRec->Data);
          break;
        case 0x0002:
          ptr = strchr(SysRec->Data, '\r');
          if (ptr)
            strcpy(ptr, "%date");
          if (SysRec->Data[0])
            fprintf(hpj, "COPYRIGHT=%s\n", SysRec->Data);
          break;
        case 0x0003:
          ptr = TopicName(*(int32_t *)SysRec->Data);
          if (ptr)
            fprintf(hpj, "CONTENTS=%s\n", ptr);
          break;
        case 0x0004:
          macro = 1;
          break;
        case 0x0005:
          fprintf(hpj, "ICON=%s\n", IconFileName);
          f = helpdeco_fopen(IconFileName, "wb");
          if (f) {
            fwrite(SysRec->Data, SysRec->DataSize, 1, f);
            helpdeco_fclose(f);
          }
          break;
        case 0x0006:
          windows++;
          break;
        case 0x0008:
          if (SysRec->Data[0])
            fprintf(hpj, "CITATION=%s\n", SysRec->Data);
          break;
        case 0x0009:
          if (!ctx->mvp)
            fprintf(hpj, "LCID=0x%X 0x%X 0x%X\n",
                    *(int16_t *)(SysRec->Data + 8), *(int16_t *)SysRec->Data,
                    *(int16_t *)(SysRec->Data + 2));
          break;
        case 0x000A:
          if (!ctx->mvp && SysRec->Data[0])
            fprintf(hpj, "CNT=%s\n", SysRec->Data);
          break;
        case 0x000B:
          //		    if(!mvp) fprintf(hpj,"CHARSET=%d\n",*(unsigned char
          //*)(SysRec->Data+1));
          break;
        case 0x000C:
          if (ctx->mvp) {
            fbreak = 1;
          } else {
            fprintf(hpj, "DEFFONT=%s,%d,%d\n", SysRec->Data + 2,
                    *(unsigned char *)SysRec->Data,
                    *(unsigned char *)(SysRec->Data + 1));
          }
          break;
        case 0x000D:
          if (ctx->mvp)
            ctx->group.count++;
          break;
        case 0x000E:
          if (ctx->mvp) {
            keywords = 1;
          } else {
            fprintf(hpj, "INDEX_SEPARATORS=\"%s\"\n", SysRec->Data);
            strlcpy(ctx->index_separators, SysRec->Data,
                    sizeof(ctx->index_separators));
          }
          break;
        case 0x0012:
          if (SysRec->Data[0])
            fprintf(hpj, "LANGUAGE=%s\n", SysRec->Data);
          break;
        case 0x0013:
          dllmaps = 1;
          break;
        }
      }
      if (ctx->win95) {
        i = 0;
        if (ctx->lzcompressed)
          i |= 8;
        if (ctx->phrase_style == Hall)
          i |= 4;
        else if (ctx->phrase.count)
          i |= 2;
        fprintf(hpj, "COMPRESS=%d\n", i);
      } else if (!ctx->lzcompressed) {
        fputs("COMPRESS=OFF\n", hpj);
      } else if (ctx->phrase.count) {
        fputs("COMPRESS=HIGH\n", hpj);
      } else {
        fputs("COMPRESS=MEDIUM\n", hpj);
      }
      if (SysHdr.Flags == 8)
        fputs("CDROMOPT=TRUE\n", hpj);
      for (i = 'A'; i <= 'z'; i++)
        if (ctx->lists[i - '0'] && i != 'K' && (i != 'A' || !ctx->win95)) {
          fprintf(hpj, "MULTIKEY=%c\n", i);
        }
      putc('\n', hpj);
      if (windows) {
        fputs("[WINDOWS]\n", hpj);
        for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
             SysRec = GetNextSystemRecord(SysRec)) {
          if (SysRec->RecordType == 0x0006) {
            if (SysRec->DataSize == sizeof(SECWINDOW)) {
              PrintWindow(hpj, (SECWINDOW *)SysRec->Data);
            } else {
              PrintMVBWindow(hpj, (MVBWINDOW *)SysRec->Data);
            }
          }
        }
        putc('\n', hpj);
      }
      if (macro) {
        fputs("[CONFIG]\n", hpj);
        for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
             SysRec = GetNextSystemRecord(SysRec)) {
          if (SysRec->RecordType == 0x0004) {
            if (sscanf(SysRec->Data, "SPC(%ld)%n", &color, &n) > 0) {
              fprintf(hpj, "SPC(%u,%u,%u)%s\n", (unsigned char)(color),
                      (unsigned char)(color >> 8), (unsigned char)(color >> 16),
                      SysRec->Data + n);
            } else {
              fprintf(hpj, "%s\n", SysRec->Data);
            }
          }
        }
        putc('\n', hpj);
      }
      if (fbreak) {
        fputs("[FTINDEX]\n", hpj);
        for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
             SysRec = GetNextSystemRecord(SysRec)) {
          if (SysRec->RecordType == 0x000C) {
            ptr = strtok(SysRec->Data, " ");
            if (ptr) {
              fprintf(hpj, "dtype%s", ptr);
              ptr = strtok(NULL, " ");
              if (ptr) {
                fprintf(hpj, "=%s", ptr);
                ptr = strtok(NULL, " ");
                if (ptr) {
                  fprintf(hpj, "!%s", ptr);
                  ptr = strtok(NULL, " ");
                  if (ptr) {
                    fprintf(hpj, ",%s", ptr + 1);
                    if (SearchFile(HelpFile, ptr, NULL)) {
                      for (n = 0; n < ctx->stopword_filename.count; n++) {
                        if (strcmp(ctx->stopword_filename.entry[n], ptr) == 0)
                          break;
                      }
                      if (n == ctx->stopword_filename.count) {
                        ctx->stopword_filename.entry =
                            helpdeco_realloc(ctx->stopword_filename.entry,
                            (ctx->stopword_filename.count + 1) *
                                sizeof(char *));
                        ctx->stopword_filename
                            .entry[ctx->stopword_filename.count++] =
                            helpdeco_strdup(ptr);
                        f = helpdeco_fopen(ptr + 1, "wt");
                        if (f) {
                          read_STOPHEADER(&StopHdr, HelpFile);
                          for (n = 0; n < StopHdr.BytesUsed;
                               n += 1 + strlen(scratch_buffer)) {
                            i = getc(HelpFile);
                            helpdeco_fread(scratch_buffer, i, HelpFile);
                            scratch_buffer[i] = '\0';
                            fprintf(f, "%s\n", scratch_buffer);
                          }
                          helpdeco_fclose(f);
                        }
                      }
                    }
                    ptr = strtok(NULL, " ");
                    if (ptr)
                      fprintf(hpj, ",%s", ptr);
                  }
                }
              }
              putc('\n', hpj);
            }
          }
        }
        putc('\n', hpj);
      }
      if ((ctx->group.count || ctx->multi) && (ctx->browsenums > 1)) {
        ctx->group.entry = helpdeco_malloc(ctx->group.count * sizeof(GROUP));
        fputs("[GROUPS]\n", hpj);
        i = 0;
        for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
             SysRec = GetNextSystemRecord(SysRec)) {
          if (SysRec->RecordType == 0x000D) {
            ptr = strchr(SysRec->Data, ' ');
            if (ptr)
              *ptr++ = '\0';
            ctx->group.count = SearchFile(HelpFile, SysRec->Data, NULL);
            n = strcspn(SysRec->Data, ".");
            SysRec->Data[n] = '\0';
            if (ptr && strcmp(ptr, "\"\" ") == 0) {
              fprintf(hpj, "group=%s\n", SysRec->Data);
            } else {
              fprintf(hpj, "group=%s,%s\n", SysRec->Data, ptr);
            }
            ctx->group.entry[i].Name = helpdeco_strdup(SysRec->Data);
            if (ctx->group.count) {
              read_GROUPHEADER(&ctx->group.entry[i].GroupHeader, HelpFile);
              if (ctx->group.entry[i].GroupHeader.GroupType == 2) {
                ctx->group.entry[i].Bitmap =
                    helpdeco_malloc(ctx->group.entry[i].GroupHeader.BitmapSize);
                helpdeco_fread(ctx->group.entry[i].Bitmap,
                         ctx->group.entry[i].GroupHeader.BitmapSize, HelpFile);
              }
            } else {
              ctx->group.entry[i].GroupHeader.GroupType = 0;
            }
            i++;
          }
        }
        if (ctx->multi)
          for (i = 1; i < ctx->browsenums; i++)
            fprintf(hpj, "group=BROWSE%04x\n", i);
        if (SearchFile(HelpFile, "|GMACROS", &FileLength)) {
          legacy_long len;
          legacy_long pos;
          legacy_long off;

          helpdeco_getdw(HelpFile); /* possible count or group number */
          for (pos = 4; pos < FileLength; pos += len) {
            len = helpdeco_getdw(HelpFile); /* length of record containing two longs and
                                      two strings */
            off = helpdeco_getdw(HelpFile); /* offset of second string in record (first
                                      is at pos 8) */
            if (off <= 0)
              off = len;
            if (len < 8)
              break;
            if (len < off)
              break;
            if (off > 8) {
              helpdeco_fread(scratch_buffer, off - 8, HelpFile);
              scratch_buffer[off - 8] = '\0';
              fprintf(hpj, "entry=%s\n", scratch_buffer);
            }
            if (len > off) {
              helpdeco_fread(scratch_buffer, len - off, HelpFile);
              scratch_buffer[len - off] = '\0';
              fprintf(hpj, "exit=%s\n", scratch_buffer);
            }
          }
        }
        putc('\n', hpj);
      }
      if (dllmaps) {
        fputs("[DLLMAPS]\n", hpj);
        for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
             SysRec = GetNextSystemRecord(SysRec)) {
          if (SysRec->RecordType == 0x0013) {
            if (strcmp(SysRec->Data, "MVMCI") != 0 &&
                strcmp(SysRec->Data, "MVIMAGE") != 0 &&
                strcmp(SysRec->Data, "MVBRKR") != 0) {
              ptr = SysRec->Data + strlen(SysRec->Data) + 1;
              fprintf(hpj, "%s=%s,", SysRec->Data, ptr);
              ptr = ptr + strlen(ptr) + 1;
              fprintf(hpj, "%s,", ptr);
              ptr = ptr + strlen(ptr) + 1;
              fprintf(hpj, "%s,", ptr);
              ptr = ptr + strlen(ptr) + 1;
              fprintf(hpj, "%s\n", ptr);
            }
          }
        }
        putc('\n', hpj);
      }
      if (keywords) {
        fputs("[KEYINDEX]\n", hpj);
        for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
             SysRec = GetNextSystemRecord(SysRec)) {
          if (SysRec->RecordType == 0x000E) {
            fprintf(hpj, "keyword=%c,\"%s\"\n", SysRec->Data[1],
                    SysRec->Data + 30);
          }
        }
        putc('\n', hpj);
      }
      for (i = 0; i < windows; i++) {
        sprintf(name, "|CF%d", i);
        if (SearchFile(HelpFile, name, &FileLength)) {
          fprintf(hpj, "[CONFIG:%d]\n", i);
          /* may use [CONFIG-GetWindowName] instead, but WindowName need not be
           * defined */
          for (n = 0; n < FileLength; n += strlen(scratch_buffer) + 1) {
            helpdeco_gets(scratch_buffer, sizeof(scratch_buffer), HelpFile);
            fprintf(hpj, "%s\n", scratch_buffer);
          }
          putc('\n', hpj);
        }
      }
    }
  }
}

const char *FontFamily(unsigned_legacy_int i) {
  const static char *const familyname[] = {"nil",    "roman", "swiss", "modern",
                                           "script", "decor", "tech"};

  if (i > 0 && i < 7)
    return familyname[i];
  return familyname[0];
}

/* collect another color into color table */
unsigned char AddColor(unsigned char r, unsigned char g, unsigned char b) {
  legacy_int n;

  for (n = 0; n < ctx->color.count; n++) {
    if (r == ctx->color.entry[n].r && g == ctx->color.entry[n].g &&
        b == ctx->color.entry[n].b)
      break;
  }
  if (n == ctx->color.count) {
    ctx->color.entry[ctx->color.count].r = r;
    ctx->color.entry[ctx->color.count].g = g;
    ctx->color.entry[ctx->color.count].b = b;
    ctx->color.count++;
  }
  return n;
}

/* load fonts from help file, filling up internal font structure,
// writing fonttbl, colortbl, and styletbl to rtf file */
void FontLoadRTF(FILE *HelpFile, FILE *rtf, FILE *hpj) {
  const static char *const BestFonts[] = {
      "Arial",  "Times New Roman", "MS Sans Serif", "MS Serif",    "Helv",
      "TmsRmn", "MS Sans Serif",   "Helvetica",     "Times Roman", "Times"};
  legacy_int default_font = 0;
  CHARMAPHEADER CharmapHeader;
  FONTHEADER FontHdr;
  FILE *f;
#define FontName_len 33
  char FontName[FontName_len];
#define CharMap_len 33
  char CharMap[CharMap_len];
  char *ptr;
  char *p;
  legacy_long FontStart;
  legacy_int i, j, k, l, len;
  unsigned char *family;
  BOOL charmap;
  OLDFONT oldfont;
  NEWFONT newfont;
  MVBFONT mvbfont;
  MVBSTYLE *mvbstyle;
  NEWSTYLE *newstyle;
  FONTDESCRIPTOR *fd;

  if (SearchFile(HelpFile, "|FONT", NULL)) {
    FontStart = ftell(HelpFile);
    read_FONTHEADER(&FontHdr, HelpFile);
    ctx->fontname.count = FontHdr.NumFacenames;
    len = (FontHdr.DescriptorsOffset - FontHdr.FacenamesOffset) /
          ctx->fontname.count;
    if (len > FontName_len) {
      helpdeco_errorf("malformed |FONT file\n");
    }
    ctx->fontname.entry = helpdeco_malloc(ctx->fontname.count * sizeof(char *));
    family = helpdeco_malloc(ctx->fontname.count * sizeof(unsigned char));
    memset(family, 0, ctx->fontname.count * sizeof(unsigned char));
    charmap = FALSE;
    mvbstyle = NULL;
    newstyle = NULL;
    for (i = 0; i < ctx->fontname.count; i++) {
      fseek(HelpFile, FontStart + FontHdr.FacenamesOffset + len * i, SEEK_SET);
      helpdeco_fread(FontName, len, HelpFile);
      FontName[len] = '\0';
      if (FontName[0] == '\000') {
        strcpy(FontName, BestFonts[default_font]);
      }
      ptr = strchr(FontName, ',');
      if (ptr && FontHdr.FacenamesOffset >= 16) {
        *ptr++ = '\0';
        fseek(HelpFile, FontStart + FontHdr.CharmapsOffset, SEEK_SET);
        for (j = 0; hpj && j < FontHdr.NumCharmaps; j++) {
          helpdeco_fread(CharMap, 32, HelpFile);
          CharMap[32] = '\0';
          p = strchr(CharMap, ',');
          if (p && strcmp(p + 1, ptr) == 0 &&
              strcmp(CharMap, "|MVCHARTAB,0") != 0) {
            if (!charmap) {
              fputs("[CHARMAP]\n", hpj);
              charmap = TRUE;
            }
            *p++ = '\0';
            if (strcmp(p, "0") == 0) {
              fprintf(hpj, "DEFAULT=%s\n", CharMap);
            } else {
              fprintf(hpj, "%s=%s\n", FontName, CharMap);
            }
            break;
          }
        }
      }
      ctx->fontname.entry[i] = helpdeco_strdup(FontName);
    }
    if (charmap)
      putc('\n', hpj);
    if (hpj && FontHdr.FacenamesOffset >= 16)
      for (j = 0; j < FontHdr.NumCharmaps; j++) {
        fseek(HelpFile, FontStart + FontHdr.CharmapsOffset + j * 32, SEEK_SET);
        helpdeco_fread(CharMap, 32, HelpFile);
        CharMap[32] = '\0';
        p = strchr(CharMap, ',');
        if (p && strcmp(CharMap, "|MVCHARTAB,0") != 0) {
          *p++ = '\0';
          if (SearchFile(HelpFile, CharMap, NULL)) {
            read_CHARMAPHEADER(&CharmapHeader, HelpFile);
            f = helpdeco_fopen(CharMap, "wt");
            if (f) {
              fprintf(f, "%d,\n", CharmapHeader.Entries);
              for (k = 0; k < CharmapHeader.Entries; k++) {
                fprintf(f, "%5u,", helpdeco_getw(HelpFile));
                fprintf(f, "%5u,", helpdeco_getw(HelpFile));
                fprintf(f, "%3u,", getc(HelpFile));
                fprintf(f, "%3u,", getc(HelpFile));
                fprintf(f, "%3u,", getc(HelpFile));
                fprintf(f, "%3u,\n", getc(HelpFile));
                helpdeco_getw(HelpFile);
              }
              fprintf(f, "%d,\n", CharmapHeader.Ligatures);
              for (k = 0; k < CharmapHeader.Ligatures; k++) {
                for (l = 0; l < CharmapHeader.LigLen; l++) {
                  fprintf(f, "%3u,", getc(HelpFile));
                }
                putc('\n', f);
              }
              helpdeco_fclose(f);
            }
          }
        }
      }
    fseek(HelpFile, FontStart + FontHdr.DescriptorsOffset, SEEK_SET);
    ctx->color.count = 1; /* auto */
    ctx->color.entry[0].r = 1;
    ctx->color.entry[0].g = 1;
    ctx->color.entry[0].b = 0;
    ctx->font.count = FontHdr.NumDescriptors;
    if (ctx->font.entry)
      free(ctx->font.entry);
    ctx->font.entry = helpdeco_malloc(ctx->font.count * sizeof(FONTDESCRIPTOR));
    memset(ctx->font.entry, 0, ctx->font.count * sizeof(FONTDESCRIPTOR));
    if (FontHdr.FacenamesOffset >= 16) {
      ctx->scaling = 1;
      ctx->rounderr = 0;
      for (i = 0; i < FontHdr.NumDescriptors; i++) {
        read_MVBFONT(&mvbfont, HelpFile);
        fd = ctx->font.entry + i;
        fd->FontName = mvbfont.FontName;
        fd->HalfPoints = -2 * mvbfont.Height;
        fd->Bold = mvbfont.Weight > 500;
        fd->Italic = mvbfont.Italic != 0;
        fd->Underline = mvbfont.Underline != 0;
        fd->StrikeOut = mvbfont.StrikeOut != 0;
        fd->DoubleUnderline = mvbfont.DoubleUnderline != 0;
        fd->SmallCaps = mvbfont.SmallCaps != 0;
        fd->textcolor =
            AddColor(mvbfont.FGRGB[0], mvbfont.FGRGB[1], mvbfont.FGRGB[2]);
        fd->backcolor =
            AddColor(mvbfont.BGRGB[0], mvbfont.BGRGB[1], mvbfont.BGRGB[2]);
        fd->FontFamily = mvbfont.PitchAndFamily >> 4;
        fd->style = mvbfont.style;
        fd->up = mvbfont.up;
        fd->expndtw = mvbfont.expndtw;
      }
      fseek(HelpFile, FontStart + FontHdr.FormatsOffset, SEEK_SET);
      mvbstyle = helpdeco_malloc(FontHdr.NumFormats * sizeof(MVBSTYLE));
      for (i = 0; i < FontHdr.NumFormats; i++) {
        MVBSTYLE *m = mvbstyle + i;
        ;
        read_MVBSTYLE(m, HelpFile);
        m->font.FGRGB[0] =
            AddColor(m->font.FGRGB[0], m->font.FGRGB[1], m->font.FGRGB[2]);
        m->font.BGRGB[0] =
            AddColor(m->font.BGRGB[0], m->font.BGRGB[1], m->font.BGRGB[2]);
      }
    } else if (FontHdr.FacenamesOffset >= 12) {
      ctx->scaling = 1;
      ctx->rounderr = 0;
      for (i = 0; i < FontHdr.NumDescriptors; i++) {
        read_NEWFONT(&newfont, HelpFile);
        fd = ctx->font.entry + i;
        fd->Bold = newfont.Weight > 500;
        fd->Italic = newfont.Italic != 0;
        fd->Underline = newfont.Underline != 0;
        fd->StrikeOut = newfont.StrikeOut != 0;
        fd->DoubleUnderline = newfont.DoubleUnderline != 0;
        fd->SmallCaps = newfont.SmallCaps != 0;
        fd->FontName = newfont.FontName;
        fd->HalfPoints = -2 * newfont.Height;
        fd->textcolor =
            AddColor(newfont.FGRGB[0], newfont.FGRGB[1], newfont.FGRGB[2]);
        fd->backcolor =
            AddColor(newfont.BGRGB[0], newfont.BGRGB[1], newfont.BGRGB[2]);
        fd->FontFamily = newfont.PitchAndFamily >> 4;
      }
      fseek(HelpFile, FontStart + FontHdr.FormatsOffset, SEEK_SET);
      newstyle = helpdeco_malloc(FontHdr.NumFormats * sizeof(NEWSTYLE));
      for (i = 0; i < FontHdr.NumFormats; i++) {
        NEWSTYLE *m = newstyle + i;
        ;
        read_NEWSTYLE(m, HelpFile);
        m->font.FGRGB[0] =
            AddColor(m->font.FGRGB[0], m->font.FGRGB[1], m->font.FGRGB[2]);
        m->font.BGRGB[0] =
            AddColor(m->font.BGRGB[0], m->font.BGRGB[1], m->font.BGRGB[2]);
      }
    } else {
      ctx->scaling = 10;
      ctx->rounderr = 5;
      for (i = 0; i < FontHdr.NumDescriptors; i++) {
        read_OLDFONT(&oldfont, HelpFile);
        fd = ctx->font.entry + i;
        fd->Bold = (oldfont.Attributes & FONT_BOLD) != 0;
        fd->Italic = (oldfont.Attributes & FONT_ITAL) != 0;
        fd->Underline = (oldfont.Attributes & FONT_UNDR) != 0;
        fd->StrikeOut = (oldfont.Attributes & FONT_STRK) != 0;
        fd->DoubleUnderline = (oldfont.Attributes & FONT_DBUN) != 0;
        fd->SmallCaps = (oldfont.Attributes & FONT_SMCP) != 0;
        fd->FontName = oldfont.FontName;
        fd->HalfPoints = oldfont.HalfPoints;
        fd->textcolor =
            AddColor(oldfont.FGRGB[0], oldfont.FGRGB[1], oldfont.FGRGB[2]);
        fd->backcolor =
            AddColor(oldfont.BGRGB[0], oldfont.BGRGB[1], oldfont.BGRGB[2]);
        if (oldfont.FontFamily < 6) {
          fd->FontFamily = lookup[oldfont.FontFamily];
        } else {
          fd->FontFamily = oldfont.FontFamily;
        }
      }
    }
    for (i = 0; i < FontHdr.NumDescriptors; i++) {
      if (ctx->font.entry[i].FontName < ctx->fontname.count) {
        family[ctx->font.entry[i].FontName] = ctx->font.entry[i].FontFamily;
      }
    }
    ctx->default_font = 0;
    l = sizeof(BestFonts) / sizeof(BestFonts[0]);
    if (ctx->fontname.entry) {
      for (i = 0; i < ctx->fontname.count; i++)
        if (family[i]) {
          for (j = 0; j < l; j++) {
            if (stricmp(ctx->fontname.entry[i], BestFonts[j]) == 0) {
              ctx->default_font = i;
              l = j;
              break;
            }
          }
        }
    }
    fprintf(rtf, "{\\rtf1\\ansi\\deff%d\n{\\fonttbl", ctx->default_font);
    for (i = 0; i < ctx->fontname.count; i++) {
      fprintf(rtf, "{\\f%d\\f%s %s;}", i, FontFamily(family[i]),
              ctx->fontname.entry[i]);
      free(ctx->fontname.entry[i]);
      ctx->fontname.entry[i] = NULL;
    }
    free(ctx->fontname.entry);
    ctx->fontname.entry = NULL;
    fputs("}\n", rtf);
    if (ctx->color.count > 1) {
      fputs("{\\colortbl;", rtf);
      for (i = 1; i < ctx->color.count; i++)
        fprintf(rtf, "\\red%d\\green%d\\blue%d;", ctx->color.entry[i].r,
                ctx->color.entry[i].g, ctx->color.entry[i].b);
      fputs("}\n", rtf);
    }
    fprintf(rtf, "{\\stylesheet{\\fs%d \\snext0 Normal;}\n",
            ctx->font.entry[0].HalfPoints);
    if (mvbstyle) {
      for (i = 0; i < FontHdr.NumFormats; i++) {
        MVBSTYLE *m, *n;

        m = mvbstyle + i;
        fprintf(rtf, "{\\*\\cs%u \\additive", m->StyleNum + 9);
        if (m->BasedOn) {
          n = mvbstyle + (m->BasedOn - 1);
          if (m->font.FontName != n->font.FontName)
            fprintf(rtf, "\\f%d", m->font.FontName);
          if (m->font.expndtw != n->font.expndtw)
            fprintf(rtf, "\\expndtw%d", m->font.expndtw);
          if (m->font.FGRGB[0] != n->font.FGRGB[0])
            fprintf(rtf, "\\cf%d", m->font.FGRGB[0]);
          if (m->font.BGRGB[0] != n->font.BGRGB[0])
            fprintf(rtf, "\\cb%d", m->font.BGRGB[0]);
          if (m->font.Height != n->font.Height)
            fprintf(rtf, "\\fs%ld", -2L * m->font.Height);
          if ((m->font.Weight > 500) != (n->font.Weight > 500))
            fprintf(rtf, "\\b%d", m->font.Weight > 500);
          if (m->font.Italic != n->font.Italic)
            fprintf(rtf, "\\i%d", m->font.Italic);
          if (m->font.Underline != n->font.Underline)
            fprintf(rtf, "\\ul%d", m->font.Underline);
          if (m->font.StrikeOut != n->font.StrikeOut)
            fprintf(rtf, "\\strike%d", m->font.StrikeOut);
          if (m->font.DoubleUnderline != n->font.DoubleUnderline)
            fprintf(rtf, "\\uldb%d", m->font.DoubleUnderline);
          if (m->font.SmallCaps != n->font.SmallCaps)
            fprintf(rtf, "\\scaps%d", m->font.SmallCaps);
          if (m->font.up != n->font.up)
            fprintf(rtf, "\\up%d", abs(m->font.up));
          fprintf(rtf, " \\sbasedon%u", m->BasedOn + 9);
        } else {
          fprintf(rtf, "\\f%d", m->font.FontName);
          if (m->font.Italic)
            fputs("\\i", rtf);
          if (m->font.Weight > 500)
            fputs("\\b", rtf);
          if (m->font.Underline)
            fputs("\\ul", rtf);
          if (m->font.StrikeOut)
            fputs("\\strike", rtf);
          if (m->font.DoubleUnderline)
            fputs("\\uldb", rtf);
          if (m->font.SmallCaps)
            fputs("\\scaps", rtf);
          if (m->font.expndtw)
            fprintf(rtf, "\\expndtw%d", m->font.expndtw);
          if (m->font.up > 0)
            fprintf(rtf, "\\up%d", m->font.up);
          else if (m->font.up < 0)
            fprintf(rtf, "\\dn%d", -m->font.up);
          fprintf(rtf, "\\fs%ld", -2 * m->font.Height);
          if (m->font.FGRGB[0])
            fprintf(rtf, "\\cf%d", m->font.FGRGB[0]);
          if (m->font.BGRGB[0])
            fprintf(rtf, "\\cb%d", m->font.BGRGB[0]);
        }
        fprintf(rtf, " %s;}\n", m->StyleName);
      }
      free(mvbstyle);
      mvbstyle = NULL;
    } else if (newstyle) {
      for (i = 0; i < FontHdr.NumFormats; i++) {
        NEWSTYLE *m, *n;

        m = newstyle + i;
        fprintf(rtf, "{\\*\\cs%u \\additive", m->StyleNum + 9);
        if (m->BasedOn) {
          n = newstyle + (m->BasedOn - 1);
          if (m->font.FontName != n->font.FontName)
            fprintf(rtf, "\\f%d", m->font.FontName);
          if (m->font.FGRGB[0] != n->font.FGRGB[0])
            fprintf(rtf, "\\cf%d", m->font.FGRGB[0]);
          if (m->font.BGRGB[0] != n->font.BGRGB[0])
            fprintf(rtf, "\\cb%d", m->font.BGRGB[0]);
          if (m->font.Height != n->font.Height)
            fprintf(rtf, "\\fs%ld", -2L * m->font.Height);
          if ((m->font.Weight > 500) != (n->font.Weight > 500))
            fprintf(rtf, "\\b%d", m->font.Weight > 500);
          if (m->font.Italic != n->font.Italic)
            fprintf(rtf, "\\i%d", m->font.Italic);
          if (m->font.Underline != n->font.Underline)
            fprintf(rtf, "\\ul%d", m->font.Underline);
          if (m->font.StrikeOut != n->font.StrikeOut)
            fprintf(rtf, "\\strike%d", m->font.StrikeOut);
          if (m->font.DoubleUnderline != n->font.DoubleUnderline)
            fprintf(rtf, "\\uldb%d", m->font.DoubleUnderline);
          if (m->font.SmallCaps != n->font.SmallCaps)
            fprintf(rtf, "\\scaps%d", m->font.SmallCaps);
          fprintf(rtf, " \\sbasedon%u", m->BasedOn + 9);
        } else {
          fprintf(rtf, "\\f%d", m->font.FontName);
          if (m->font.Italic)
            fputs("\\i", rtf);
          if (m->font.Weight > 500)
            fputs("\\b", rtf);
          if (m->font.Underline)
            fputs("\\ul", rtf);
          if (m->font.StrikeOut)
            fputs("\\strike", rtf);
          if (m->font.DoubleUnderline)
            fputs("\\uldb", rtf);
          if (m->font.SmallCaps)
            fputs("\\scaps", rtf);
          fprintf(rtf, "\\fs%ld", -2 * m->font.Height);
          if (m->font.FGRGB[0])
            fprintf(rtf, "\\cf%d", m->font.FGRGB[0]);
          if (m->font.BGRGB[0])
            fprintf(rtf, "\\cb%d", m->font.BGRGB[0]);
        }
        fprintf(rtf, " %s;}\n", m->StyleName);
      }
      free(newstyle);
      newstyle = NULL;
    }
    if (family) {
      free(family);
      family = NULL;
    }
    fputs("}\\pard\\plain\n", rtf);
    memset(&ctx->current_font, 0, sizeof(ctx->current_font));
    ctx->current_font.FontName = ctx->default_font;
    if (hpj) {
      fprintf(stderr, "%u font names, %u font descriptors", ctx->fontname.count,
              FontHdr.NumDescriptors);
      if (FontHdr.FacenamesOffset >= 12)
        printf(", %u font styles", FontHdr.NumFormats);
      fputs(" loaded\n", stderr);
    }
  }
}

/* read NumBytes from |TOPIC starting at TopicPos (or if TopicPos is 0
// where last left off) into dest, returning number of bytes read.
// TopicRead handles LZ77 decompression and the crossing of topic blocks */
legacy_long TopicRead(FILE *HelpFile, legacy_long TopicPos, void *dest,
                      legacy_long NumBytes) {
  static unsigned char TopicBuffer[0x4000];
  unsigned int TopicBlockOffset;
  unsigned int n;

  if (!ctx->topic_file_start) /* first call: HelpFile is at start of |TOPIC */
  {
    ctx->topic_file_start = ftell(HelpFile);
    ctx->topic_block_num = -1;
  }
  if (!TopicPos)
    TopicPos = ctx->topic_last_pos; /* continue where left off */
  if ((TopicPos - sizeof(TOPICBLOCKHEADER)) / ctx->DecompressSize !=
      ctx->topic_block_num) /* other topic block */
  {
    ctx->topic_block_num =
        (TopicPos - sizeof(TOPICBLOCKHEADER)) / ctx->DecompressSize;
    if (ctx->topic_block_num * ctx->topic_block_size >= ctx->topic_file_length)
      return 0;
    fseek(HelpFile,
          ctx->topic_file_start + ctx->topic_block_num * ctx->topic_block_size,
          SEEK_SET);
    n = ctx->topic_block_size;
    if (n + ctx->topic_block_num * ctx->topic_block_size > ctx->topic_file_length) {
      n = (unsigned_legacy_int)(ctx->topic_file_length -
                                ctx->topic_block_num * ctx->topic_block_size);
    }
    read_TOPICBLOCKHEADER(&ctx->topic_block_header, HelpFile);
    n -= sizeof(TOPICBLOCKHEADER);
    if (ctx->lzcompressed) {
      ctx->decompressed_size = mfile_decompress_into_buffer(2, HelpFile, n, TopicBuffer,
                                                    sizeof(TopicBuffer));
    } else {
      ctx->decompressed_size = helpdeco_fread(TopicBuffer, n, HelpFile);
    }
  }
  TopicBlockOffset =
      (TopicPos - sizeof(TOPICBLOCKHEADER)) % ctx->DecompressSize;
  if (TopicBlockOffset + NumBytes >
      ctx->decompressed_size) /* more than available in this block */
  {
    n = ctx->decompressed_size - TopicBlockOffset;
    if (n)
      memcpy(dest, TopicBuffer + TopicBlockOffset, n);
    return n + TopicRead(HelpFile,
                         (ctx->topic_block_num + 1) * ctx->DecompressSize +
                             sizeof(TOPICBLOCKHEADER),
                         (char *)dest + n, NumBytes - n);
  }
  if (NumBytes)
    memcpy(dest, TopicBuffer + TopicBlockOffset, NumBytes);
  ctx->topic_last_pos = TopicPos + NumBytes;
  return NumBytes;
}

/* reads next chunk from |TOPIC like TopicRead, but does phrase decompression
// if Length > NumBytes, suitable to read LinkData2. If phrase decompression
// doesn't expands to Length bytes, buffer is padded using 0. TopicPhraseRead
// always NUL-terminates at dest[Length] just to be save */
legacy_long topic_read_phrase(FILE *HelpFile, legacy_long TopicPos, char *dest,
                              legacy_long NumBytes, legacy_long Length) {
  char *buffer;
  legacy_long BytesRead;
  legacy_long i;

  if (Length <= NumBytes) /* no phrase compression in this case */
  {
    BytesRead = TopicRead(HelpFile, TopicPos, dest, Length);
    for (i = BytesRead; i <= Length; i++)
      dest[i] = '\0';
    if (BytesRead == Length &&
        Length <
            NumBytes) /* some trailing bytes are not used (bug in HCRTF ?) */
    {
      buffer = helpdeco_malloc(NumBytes - Length);
      BytesRead += TopicRead(HelpFile, 0, buffer, NumBytes - Length);
      free(buffer);
      buffer = NULL;
    }
  } else {
    buffer = helpdeco_malloc(NumBytes);
    BytesRead = TopicRead(HelpFile, TopicPos, buffer, NumBytes);
    NumBytes = phrase_expand(buffer, NumBytes, dest) - dest;
    free(buffer);
    buffer = NULL;
    if (NumBytes > Length) {
      helpdeco_errorf("Phrase replacement delivers %ld bytes instead of %ld", NumBytes,
            Length);
    }
  }
  while (NumBytes <= Length)
    dest[NumBytes++] = '\0';
  return BytesRead;
}

void Annotate(legacy_long pos, FILE *rtf) {
  legacy_long FileLength;
  char FileName[19];
  legacy_int i;
  legacy_long l;

  sprintf(FileName, "%ld!0", pos);
  if (SearchFile(ctx->annotation_file, FileName, &FileLength)) {
    fputs("{\\v {\\*\\atnid ANN}\\chatn {\\*\\annotation \\pard\\plain "
          "{\\chatn }",
          rtf);
    for (l = 0; l < FileLength && (i = getc(ctx->annotation_file)) != -1; l++) {
      if (i == 0x0D) {
        fputs("\\par\n", rtf);
      } else if (i != '{' && i != '}' && i != '\\' && isprint(i)) {
        putc(i, rtf);
      } else if (i == '{') {
        fputs("\\{\\-", rtf);
      } else if (i != '\0' && i != 0x0A) {
        fprintf(rtf, "\\'%02x", i);
      }
    }
    fputs("}}", rtf);
  }
}

/* collect all keywords assigned to positions starting at NextKeywordOffset
// from all keyword lists, saving the first MAXKEYWORDS in KeywordRec
// (which is allocated to MAXKEYWORDS if NULL) for use in ListKeywords.
// updates NextKeywordOffset, clears NextKeywordRec, sets KeywordRecs. */
void CollectKeywords(FILE *HelpFile) {
  uint16_t j, m;
  int i, n, k, l, map;
  legacy_long FileLength, savepos, KWDataOffset, from;
  legacy_long *keytopic;
  BUFFER buf;
  char kwdata[10];
  char kwbtree[10];

  fputs("Collecting keywords...", stderr);
  savepos = ftell(HelpFile);
  if (ctx->keyword_rec.entry) /* free old keywords */
  {
    for (i = 0; i < ctx->keyword_rec.count; i++) {
      if (ctx->keyword_rec.entry[i].Keyword) {
        free(ctx->keyword_rec.entry[i].Keyword);
        ctx->keyword_rec.entry[i].Keyword = NULL;
      }
    }
  } else {
    ctx->keyword_rec.entry = helpdeco_malloc(MAXKEYWORDS * sizeof(KEYWORDREC));
  }
  ctx->NextKeywordRec = ctx->keyword_rec.count = 0;
  from = ctx->NextKeywordOffset;
  ctx->NextKeywordOffset = 0x7FFFFFFFL;
  for (k = 0; k < 2; k++)
    for (map = '0'; map <= 'z'; map++) {
      if (k) {
        if (!ctx->keyindex[map - '0'])
          continue;
        sprintf(kwdata, "|%cKWDATA", map);
        sprintf(kwbtree, "|%cKWBTREE", map);
      } else {
        if (!ctx->lists[map - '0'])
          continue;
        sprintf(kwdata, "|%cWDATA", map);
        sprintf(kwbtree, "|%cWBTREE", map);
      }
      if (SearchFile(HelpFile, kwdata, &FileLength)) {
        keytopic = helpdeco_malloc(FileLength);
        helpdeco_fread(keytopic, FileLength, HelpFile);
        if (SearchFile(HelpFile, kwbtree, NULL)) {
          for (n = GetFirstPage(HelpFile, &buf, NULL); n;
               n = GetNextPage(HelpFile, &buf)) {
            for (i = 0; i < n; i++) {
              helpdeco_gets(keyword, sizeof(keyword), HelpFile);
              m = helpdeco_getw(HelpFile);
              KWDataOffset = helpdeco_getdw(HelpFile);
              for (j = 0; j < m; j++) {
                if (keytopic[KWDataOffset / 4 + j] >= from) {
                  if (ctx->keyword_rec.count >= MAXKEYWORDS) {
                    ctx->NextKeywordOffset =
                        ctx->keyword_rec.entry[ctx->keyword_rec.count - 1]
                            .TopicOffset;
                    while (ctx->keyword_rec.entry[ctx->keyword_rec.count - 1]
                               .TopicOffset == ctx->NextKeywordOffset) {
                      ctx->keyword_rec.count--;
                      if (ctx->keyword_rec.entry[ctx->keyword_rec.count]
                              .Keyword) {
                        free(ctx->keyword_rec.entry[ctx->keyword_rec.count]
                                 .Keyword);
                        ctx->keyword_rec.entry[ctx->keyword_rec.count].Keyword =
                            NULL;
                      }
                    }
                  }
                  l = ctx->keyword_rec.count;
                  while (l > 0 && ctx->keyword_rec.entry[l - 1].TopicOffset >
                                      keytopic[KWDataOffset / 4 + j]) {
                    ctx->keyword_rec.entry[l].KeyIndex =
                        ctx->keyword_rec.entry[l - 1].KeyIndex;
                    ctx->keyword_rec.entry[l].Footnote =
                        ctx->keyword_rec.entry[l - 1].Footnote;
                    ctx->keyword_rec.entry[l].Keyword =
                        ctx->keyword_rec.entry[l - 1].Keyword;
                    ctx->keyword_rec.entry[l].TopicOffset =
                        ctx->keyword_rec.entry[l - 1].TopicOffset;
                    l--;
                  }
                  ctx->keyword_rec.entry[l].KeyIndex = k > 0;
                  ctx->keyword_rec.entry[l].Footnote = map;
                  ctx->keyword_rec.entry[l].Keyword = helpdeco_strdup(keyword);
                  ctx->keyword_rec.entry[l].TopicOffset =
                      keytopic[KWDataOffset / 4 + j];
                  ctx->keyword_rec.count++;
                }
              }
            }
          }
          free(keytopic);
          keytopic = NULL;
        }
      }
    }
  fseek(HelpFile, savepos, SEEK_SET);
  for (i = 0; i < 22; i++)
    fputs("\b \b", stderr);
}

/* writes out all keywords appearing up to position TopicOffset and eats
// them up so they are not written out again. Merges keywords if possible */
void ListKeywords(FILE *HelpFile, FILE *rtf, legacy_long TopicOffset) {
  legacy_int len, footnote, keyindex;

  if (ctx->NextKeywordRec >= ctx->keyword_rec.count) {
    if (ctx->NextKeywordOffset == 0x7FFFFFFFL)
      return;
    CollectKeywords(HelpFile);
  }
  footnote = keyindex = len = 0;
  while (ctx->NextKeywordRec < ctx->keyword_rec.count &&
         ctx->keyword_rec.entry[ctx->NextKeywordRec].TopicOffset <=
             TopicOffset) {
    if (len > 0 &&
        (ctx->keyword_rec.entry[ctx->NextKeywordRec].Footnote != footnote ||
         ctx->keyword_rec.entry[ctx->NextKeywordRec].KeyIndex != keyindex ||
         len + strlen(ctx->keyword_rec.entry[ctx->NextKeywordRec].Keyword) >
             (ctx->after31 ? 1023 : 254))) {
      fputs("}\n", rtf);
      len = 0;
    }
    if (len > 0) {
      putc(';', rtf);
    } else if (ctx->keyword_rec.entry[ctx->NextKeywordRec].KeyIndex) {
      fprintf(rtf, "{\\up K}{\\footnote\\pard\\plain{\\up K} %c:",
              ctx->keyword_rec.entry[ctx->NextKeywordRec].Footnote);
    } else {
      fprintf(rtf, "{\\up %c}{\\footnote\\pard\\plain{\\up %c} ",
              ctx->keyword_rec.entry[ctx->NextKeywordRec].Footnote,
              ctx->keyword_rec.entry[ctx->NextKeywordRec].Footnote);
    }
    len += strlen(ctx->keyword_rec.entry[ctx->NextKeywordRec].Keyword) + 1;
    putrtf(rtf, ctx->keyword_rec.entry[ctx->NextKeywordRec].Keyword);
    footnote = ctx->keyword_rec.entry[ctx->NextKeywordRec].Footnote;
    keyindex = ctx->keyword_rec.entry[ctx->NextKeywordRec].KeyIndex;
    ctx->NextKeywordRec++;
  }
  if (len)
    fputs("}\n", rtf);
}

/* create > footnote if topic at TopicOffset has a window assigned to
// using the |VIOLA internal file. Read VIOLA sequentially, reloading
// next page only if necessary, because it is properly ordered. */
legacy_int ListWindows(FILE *HelpFile, legacy_long TopicOffset) {
  legacy_long savepos;
  static legacy_int n, i;
  static BUFFER buf;
  static legacy_int VIOLAfound = -1;
  static VIOLAREC *Viola;
  legacy_int result;

  if (VIOLAfound == 0)
    return -1;
  savepos = ftell(HelpFile);
  if (VIOLAfound == -1) {
    VIOLAfound = 0;
    if (SearchFile(HelpFile, "|VIOLA", NULL)) {
      n = GetFirstPage(HelpFile, &buf, NULL);
      if (n) {
        Viola = helpdeco_malloc(n * sizeof(VIOLAREC));
        read_VIOLARECs(Viola, n, HelpFile);
        i = 0;
        VIOLAfound = 1;
      }
    }
  }
  result = -1;
  if (VIOLAfound == 1) {
    while (i >= n || TopicOffset > Viola[i].TopicOffset) {
      if (i >= n) {
        free(Viola);
        Viola = NULL;
        n = GetNextPage(HelpFile, &buf);
        if (n == 0) {
          VIOLAfound = 0;
          break;
        }
        Viola = helpdeco_malloc(n * sizeof(VIOLAREC));
        read_VIOLARECs(Viola, n, HelpFile);
        i = 0;
      } else {
        i++;
      }
    }
    if (i < n && Viola[i].TopicOffset == TopicOffset) {
      result = Viola[i].WindowNumber;
    }
  }
  fseek(HelpFile, savepos, SEEK_SET);
  return result;
}

/* Browse sequence handling support functions. As an efficient means to
// resolve browse sequences, FirstPass numbers and saves all browse
// sequence start positions (that are topics where BrowseNext and
// BrowsePrev both point to some topic behind) and links them with the
// following topics. Whenever a topic is merged to the beginning of a
// browse sequence, it's start topics browse subnumber is incremented.
// The interesting part is a topic where BrowsePrev and BrowseNext both
// point to earlier topics. Two different browse sequences need to be
// merged at this point, that is will get the same browse start number
// and one start topics subnumber needs to accommodate the other browse
// sequence. Using the start records, TopicDump does know which browse
// sequence starts at which topic and knows the browse sequence number
// and subnumber assigned. */
void AddStart(legacy_long StartTopic, legacy_int BrowseNum, legacy_int Count) {
  ctx->start.entry =
      helpdeco_realloc(ctx->start.entry, (ctx->start.count + 1) * sizeof(START));
  ctx->start.entry[ctx->start.count].StartTopic = StartTopic;
  ctx->start.entry[ctx->start.count].BrowseNum = BrowseNum;
  ctx->start.entry[ctx->start.count].Start = Count;
  ctx->start.count++;
}

void FixStart(legacy_int BrowseNum, legacy_int NewBrowseNum,
              legacy_int AddCount) {
  legacy_int i;

  for (i = 0; i < ctx->start.count; i++)
    if (ctx->start.entry[i].BrowseNum == BrowseNum) {
      ctx->start.entry[i].BrowseNum = NewBrowseNum;
      ctx->start.entry[i].Start += AddCount;
    }
}

void AddBrowse(legacy_long StartTopic, legacy_long NextTopic,
               legacy_long PrevTopic) {
  legacy_int i;

  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic == -1)
      break;                  /* empty space in array ? */
  if (i == ctx->browse.count) /* no empty space, add to array */
  {
    ctx->browse.entry =
        helpdeco_realloc(ctx->browse.entry, ++ctx->browse.count * sizeof(BROWSE));
  }
  ctx->browse.entry[i].StartTopic = StartTopic;
  ctx->browse.entry[i].NextTopic = NextTopic;
  ctx->browse.entry[i].PrevTopic = PrevTopic;
  ctx->browse.entry[i].BrowseNum = ctx->browsenums++;
  ctx->browse.entry[i].Start = 1;
  ctx->browse.entry[i].Count = 1;
}

void MergeBrowse(legacy_long TopicOffset, legacy_long OtherTopicOffset,
                 legacy_long NextTopic, legacy_long PrevTopic) {
  legacy_int i, j;

  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic != -1) {
      if (ctx->browse.entry[i].NextTopic == TopicOffset ||
          ctx->browse.entry[i].NextTopic == OtherTopicOffset)
        break;
    }
  for (j = 0; j < ctx->browse.count; j++)
    if (ctx->browse.entry[j].StartTopic != -1) {
      if (ctx->browse.entry[j].PrevTopic == TopicOffset ||
          ctx->browse.entry[j].PrevTopic == OtherTopicOffset)
        break;
    }
  if (i < ctx->browse.count && j < ctx->browse.count) {
    ctx->browse.entry[i].Count++;
    ctx->browse.entry[i].NextTopic = ctx->browse.entry[j].NextTopic;
    FixStart(ctx->browse.entry[j].BrowseNum, ctx->browse.entry[i].BrowseNum,
             ctx->browse.entry[i].Count);
    ctx->browse.entry[j].Start += ctx->browse.entry[i].Count;
    AddStart(ctx->browse.entry[j].StartTopic, ctx->browse.entry[i].BrowseNum,
             ctx->browse.entry[j].Start);
    ctx->browse.entry[i].Count += ctx->browse.entry[j].Count;
    ctx->browse.entry[j].StartTopic = -1;
    if (ctx->browse.entry[i].NextTopic == -1 &&
        ctx->browse.entry[i].PrevTopic == -1) {
      AddStart(ctx->browse.entry[i].StartTopic, ctx->browse.entry[i].BrowseNum,
               ctx->browse.entry[i].Start);
      ctx->browse.entry[i].StartTopic = -1;
    }
  } else {
    ctx->warnings = TRUE;
    fprintf(stderr, "Can not merge %08lx %08lx %08lx\n", TopicOffset, NextTopic,
            PrevTopic);
  }
}

void LinkBrowse(legacy_long TopicOffset, legacy_long OtherTopicOffset,
                legacy_long NextTopic, legacy_long PrevTopic) {
  legacy_int i;

  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic != -1) {
      if (ctx->browse.entry[i].NextTopic == TopicOffset ||
          ctx->browse.entry[i].NextTopic == OtherTopicOffset)
        break;
    }
  if (i < ctx->browse.count) {
    ctx->browse.entry[i].NextTopic = NextTopic;
    ctx->browse.entry[i].Count++;
    if (ctx->browse.entry[i].NextTopic == -1 &&
        ctx->browse.entry[i].PrevTopic == -1) {
      AddStart(ctx->browse.entry[i].StartTopic, ctx->browse.entry[i].BrowseNum,
               ctx->browse.entry[i].Start);
      ctx->browse.entry[i].StartTopic = -1;
    }
  } else {
    ctx->warnings = TRUE;
    fprintf(stderr, "Can not link %08lx %08lx %08lx\n", TopicOffset, NextTopic,
            PrevTopic);
    for (i = 0; i < ctx->browse.count; i++)
      if (ctx->browse.entry[i].StartTopic != -1) {
        fprintf(stderr, "Open browse %08lx %08lx\n",
                ctx->browse.entry[i].PrevTopic, ctx->browse.entry[i].NextTopic);
      }
  }
}

void BackLinkBrowse(legacy_long TopicOffset, legacy_long OtherTopicOffset,
                    legacy_long NextTopic, legacy_long PrevTopic) {
  legacy_int i;

  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic != -1) {
      if (ctx->browse.entry[i].PrevTopic == TopicOffset ||
          ctx->browse.entry[i].PrevTopic == OtherTopicOffset)
        break;
    }
  if (i < ctx->browse.count) {
    ctx->browse.entry[i].PrevTopic = PrevTopic;
    ctx->browse.entry[i].Count++;
    ctx->browse.entry[i].Start++;
    FixStart(ctx->browse.entry[i].BrowseNum, ctx->browse.entry[i].BrowseNum, 1);
    if (ctx->browse.entry[i].NextTopic == -1 &&
        ctx->browse.entry[i].PrevTopic == -1) {
      AddStart(ctx->browse.entry[i].StartTopic, ctx->browse.entry[i].BrowseNum,
               ctx->browse.entry[i].Start);
      ctx->browse.entry[i].StartTopic = -1;
    }
  } else {
    ctx->warnings = TRUE;
    fprintf(stderr, "Can not backlink %08lx %08lx %08lx\n", TopicOffset,
            NextTopic, PrevTopic);
  }
}

uint32_t AddLink(legacy_long StartTopic, legacy_long NextTopic,
                 legacy_long PrevTopic) {
  legacy_int i, j;
  uint32_t result;

  result = 0;
  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic == -1)
      break;
  if (i == ctx->browse.count)
    ctx->browse.entry = helpdeco_realloc(ctx->browse.entry,
                                         ++ctx->browse.count * sizeof(BROWSE));
  for (j = 0; j < ctx->start.count; j++)
    if (ctx->start.entry[j].StartTopic == StartTopic)
      break;
  if (j < ctx->start.count) {
    ctx->browse.entry[i].StartTopic = ctx->start.entry[j].StartTopic;
    ctx->browse.entry[i].BrowseNum = ctx->start.entry[j].BrowseNum;
    ctx->browse.entry[i].Start = ctx->start.entry[j].Start;
    ctx->browse.entry[i].Count = ctx->start.entry[j].Start;
    ctx->browse.entry[i].NextTopic = NextTopic;
    ctx->browse.entry[i].PrevTopic = PrevTopic;
    result = ctx->browse.entry[i].BrowseNum +
             ((legacy_long)ctx->browse.entry[i].Start << 16);
  } else {
    ctx->warnings = TRUE;
    fprintf(stderr, "Browse start %08lx not found\n", StartTopic);
  }
  return result;
}

uint32_t MergeLink(legacy_long TopicOffset, legacy_long OtherTopicOffset,
                   legacy_long NextTopic, legacy_long PrevTopic) {
  legacy_int i, j;
  uint32_t result;

  result = 0;
  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic != -1) {
      if (ctx->browse.entry[i].NextTopic == TopicOffset ||
          ctx->browse.entry[i].NextTopic == OtherTopicOffset)
        break;
    }
  for (j = 0; j < ctx->browse.count; j++)
    if (ctx->browse.entry[j].StartTopic != -1) {
      if (ctx->browse.entry[j].PrevTopic == TopicOffset ||
          ctx->browse.entry[j].PrevTopic == OtherTopicOffset)
        break;
    }
  if (i < ctx->browse.count && j < ctx->browse.count) {
    ctx->browse.entry[i].Count++;
    ctx->browse.entry[j].Start--;
    if (ctx->browse.entry[i].Count != ctx->browse.entry[j].Start) {
      ctx->warnings = TRUE;
      fprintf(stderr, "Prev browse end %d doen't match next browse start %d\n",
              ctx->browse.entry[i].Count, ctx->browse.entry[j].Start);
    }
    result = ctx->browse.entry[i].BrowseNum +
             ((legacy_long)ctx->browse.entry[i].Count << 16);
    ctx->browse.entry[i].NextTopic = ctx->browse.entry[j].NextTopic;
    ctx->browse.entry[i].Count = ctx->browse.entry[j].Count;
    ctx->browse.entry[j].StartTopic = -1;
  } else {
    ctx->warnings = TRUE;
    fprintf(stderr, "Can not merge %08lx %08lx %08lx\n", TopicOffset, NextTopic,
            PrevTopic);
  }
  return result;
}

uint32_t LinkLink(legacy_long TopicOffset, legacy_long OtherTopicOffset,
                  legacy_long NextTopic, legacy_long PrevTopic) {
  legacy_int i;
  uint32_t result;

  result = 0;
  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic != -1) {
      if (ctx->browse.entry[i].NextTopic == TopicOffset ||
          ctx->browse.entry[i].NextTopic == OtherTopicOffset)
        break;
    }
  if (i < ctx->browse.count) {
    ctx->browse.entry[i].NextTopic = NextTopic;
    ctx->browse.entry[i].Count++;
    result = ctx->browse.entry[i].BrowseNum +
             ((legacy_long)ctx->browse.entry[i].Count << 16);
    if (ctx->browse.entry[i].NextTopic == -1 &&
        ctx->browse.entry[i].PrevTopic == -1) {
      ctx->browse.entry[i].StartTopic = -1;
    }
  } else {
    ctx->warnings = TRUE;
    fprintf(stderr, "Can not link %08lx %08lx %08lx\n", TopicOffset, NextTopic,
            PrevTopic);
  }
  return result;
}

uint32_t BackLinkLink(legacy_long TopicOffset, legacy_long OtherTopicOffset,
                      legacy_long NextTopic, legacy_long PrevTopic) {
  legacy_int i;
  uint32_t result;

  result = 0;
  for (i = 0; i < ctx->browse.count; i++)
    if (ctx->browse.entry[i].StartTopic != -1) {
      if (ctx->browse.entry[i].PrevTopic == TopicOffset ||
          ctx->browse.entry[i].PrevTopic == OtherTopicOffset)
        break;
    }
  if (i < ctx->browse.count) {
    ctx->browse.entry[i].PrevTopic = PrevTopic;
    ctx->browse.entry[i].Start--;
    result = ctx->browse.entry[i].BrowseNum +
             ((legacy_long)ctx->browse.entry[i].Start << 16);
    if (ctx->browse.entry[i].NextTopic == -1 &&
        ctx->browse.entry[i].PrevTopic == -1) {
      ctx->browse.entry[i].StartTopic = -1;
    }
  } else {
    ctx->warnings = TRUE;
    fprintf(stderr, "Can not backlink %08lx %08lx %08lx\n", TopicOffset,
            NextTopic, PrevTopic);
  }
  return result;
}

/* create numbered rtf file names, no numbering if i=0 */
void BuildRTFName(char *buffer, legacy_int i) {
  char num[7];

  strcpy(buffer, ctx->name);
  if (i) {
    snprintf(num, 7, "%d", i);
    if (strlen(buffer) + strlen(num) > 8) {
      buffer[8 - strlen(num)] = '\0';
    }
    strcat(buffer, num);
  }
  strcat(buffer, ".rtf");
}

/* emit rtf commands to change to font i.
// ul forces underline on, uldb forces doubleunderline on */
void ChangeFont(FILE *rtf, unsigned_legacy_int i, BOOL ul, BOOL uldb) {
  FONTDESCRIPTOR *f;
  legacy_long pos;

  if (i < ctx->font.count) {
    pos = ftell(rtf);
    f = ctx->font.entry + i;
    if (f->style) {
      fprintf(rtf, "\\plain\\cs%d", f->style + 9);
      if (uldb)
        fputs("\\uldb", rtf);
      else if (ul)
        fputs("\\ul", rtf);
    } else {
      /* HC30 can't reset, so reset using \plain */
      if ((ctx->current_font.Bold && !f->Bold) ||
          (ctx->current_font.Italic && !f->Italic) ||
          (ctx->current_font.Underline && !(!uldb && (ul || f->Underline))) ||
          (ctx->current_font.StrikeOut && !f->StrikeOut) ||
          (ctx->current_font.DoubleUnderline && !(uldb || f->DoubleUnderline)) ||
          (ctx->current_font.SmallCaps && !f->SmallCaps) ||
          (ctx->current_font.FontName && !f->FontName) ||
          (ctx->current_font.textcolor && !f->textcolor) ||
          (ctx->current_font.backcolor && !f->backcolor) ||
          (ctx->current_font.up && !f->up) ||
          (ctx->current_font.style && !f->style)) {
        fputs("\\plain", rtf);
        memset(&ctx->current_font, 0, sizeof(ctx->current_font));
        ctx->current_font.FontName = ctx->default_font;
      }
      if (f->FontName != ctx->current_font.FontName)
        fprintf(rtf, "\\f%d", f->FontName);
      if (f->Italic && !ctx->current_font.Italic)
        fputs("\\i", rtf);
      if (f->Bold && !ctx->current_font.Bold)
        fputs("\\b", rtf);
      if (!uldb && (ul || f->Underline) && !ctx->current_font.Bold)
        fputs("\\ul", rtf);
      if (f->StrikeOut && !ctx->current_font.StrikeOut)
        fputs("\\strike", rtf);
      if ((uldb || f->DoubleUnderline) && !ctx->current_font.DoubleUnderline)
        fputs("\\uldb", rtf);
      if (f->SmallCaps && !ctx->current_font.SmallCaps)
        fputs("\\scaps", rtf);
      if (f->expndtw != ctx->current_font.expndtw)
        fprintf(rtf, "\\expndtw%d", f->expndtw);
      if (f->up != ctx->current_font.up) {
        if (f->up > 0)
          fprintf(rtf, "\\up%d", f->up);
        else if (f->up < 0)
          fprintf(rtf, "\\dn%d", -f->up);
      }
      if (f->HalfPoints != ctx->current_font.HalfPoints)
        fprintf(rtf, "\\fs%d", f->HalfPoints);
      if (f->textcolor != ctx->current_font.textcolor)
        fprintf(rtf, "\\cf%d", f->textcolor);
      if (f->backcolor != ctx->current_font.backcolor)
        fprintf(rtf, "\\cb%d", f->backcolor);
    }
    memcpy(&ctx->current_font, f, sizeof(ctx->current_font));
    if (ul)
      ctx->current_font.Underline = 1;
    if (uldb) {
      ctx->current_font.Underline = 0;
      ctx->current_font.DoubleUnderline = 1;
    }
    if (ftell(rtf) != pos)
      putc(' ', rtf);
  }
}

/* list all groups the topic TopicNum is assigned to and/or emit footnote
// for browse sequence of this topic as + footnote into rtf file */
void ListGroups(FILE *rtf, legacy_long TopicNum, uint32_t BrowseNum) {
  legacy_int i;
  BOOL grouplisted;

  grouplisted = FALSE;
  for (i = 0; i < ctx->group.count; i++)
    if (ctx->group.entry[i].GroupHeader.GroupType == 1 ||
        ctx->group.entry[i].GroupHeader.GroupType == 2) {
      if ((TopicNum >= ctx->group.entry[i].GroupHeader.FirstTopic &&
           TopicNum <= ctx->group.entry[i].GroupHeader.LastTopic) &&
          ((ctx->group.entry[i].GroupHeader.GroupType == 1 ||
            ctx->group.entry[i].GroupHeader.GroupType == 2) &&
           (ctx->group.entry[i].Bitmap[TopicNum >> 3] &
            (1 << (TopicNum & 7))))) {
        if (!grouplisted) {
          fputs("{\\up +}{\\footnote\\pard\\plain{\\up +} ", rtf);
          if (BrowseNum)
            fprintf(rtf, "BROWSE%04x:%04x", (uint16_t)BrowseNum,
                    (uint16_t)(BrowseNum >> 16));
          grouplisted = TRUE;
        }
        fprintf(rtf, ";%s", ctx->group.entry[i].Name);
      }
    }
  if (grouplisted) {
    fputs("}\n", rtf);
  } else if (BrowseNum) {
    fprintf(rtf, "{\\up +}{\\footnote\\pard\\plain{\\up +} BROWSE%04x:%04x}\n",
            (uint16_t)BrowseNum, (uint16_t)(BrowseNum >> 16));
  }
}

/* advances TopicOffset to next block in |TOPIC if setting of TopicPos to
// NextBlock crosses TOPICBLOCKHEADER */
TOPICOFFSET NextTopicOffset(TOPICOFFSET TopicOffset, TOPICPOS NextBlock,
                            TOPICPOS TopicPos) {
  /* it should never be necessary to subtract sizeof(TOPICBLOCKHEADER), as no
  // TOPICLINK may start in the last (12..21) bytes, but just to make shure...
*/
  if ((NextBlock - sizeof(TOPICBLOCKHEADER)) / ctx->DecompressSize !=
      (TopicPos - sizeof(TOPICBLOCKHEADER)) / ctx->DecompressSize) {
    return ((NextBlock - sizeof(TOPICBLOCKHEADER)) / ctx->DecompressSize) *
           0x8000;
  }
  return TopicOffset;
}

/* TopicDump: converts the internal |TOPIC file to RTF format suitable for
// recompilation inserting footnotes with information from other internal
// files as required */
FILE *TopicDumpRTF(FILE *HelpFile, FILE *rtf, FILE *hpj, BOOL makertf) {
  TOPICLINK TopicLink;
  char *LinkData1; /* Data associated with this link */
  legacy_long nonscroll = -1;
  char *LinkData2; /* Second set of data */
  legacy_int fontset, i;
  legacy_int NextContextRec;
  uint32_t BrowseNum;
  char *hotspot;
  char *arg;
  BOOL firsttopic = TRUE;
  BOOL ul, uldb;
  legacy_int nextbitmap, TopicInRTF, NumberOfRTF;
  legacy_long TopicNum, TopicOffset, TopicPos;
  legacy_int col, cols, lastcol;
  int16_t *iptr;
  uint16_t x1, x2, x3;
  int16_t y1;
  legacy_long l1 = 0;
  char *ptr;
  char *cmd;
  char *str;
  legacy_long ActualTopicOffset = 0, MaxTopicOffset = 0;
  TOPICHEADER30 *TopicHdr30;
  TOPICHEADER *TopicHdr;
  legacy_long BogusTopicOffset;

  if (SearchFile(HelpFile, "|TOPIC", &ctx->topic_file_length)) {
    fontset = -1;
    nextbitmap = 1;
    if (ctx->browse.entry)
      free(ctx->browse.entry);
    ctx->browse.entry = NULL;
    ctx->browse.count = 0;
    NextContextRec = 0;
    ul = uldb = FALSE;
    hotspot = NULL;
    TopicOffset = 0;
    TopicPos = 12;
    TopicNum = 16;
    TopicInRTF = 0;
    NumberOfRTF = 1;
    while (TopicRead(HelpFile, TopicPos, &TopicLink, sizeof(TopicLink)) ==
           sizeof(TOPICLINK)) {
      if (ctx->before31) {
        if (TopicPos + TopicLink.NextBlock >= ctx->topic_file_length)
          break;
      } else {
        if (TopicLink.NextBlock <= 0)
          break;
      }
      if (TopicLink.DataLen1 > sizeof(TOPICLINK)) {
        LinkData1 = helpdeco_malloc(TopicLink.DataLen1 - sizeof(TOPICLINK) + 1);
        if (TopicRead(HelpFile, 0, LinkData1,
                      TopicLink.DataLen1 - sizeof(TOPICLINK)) !=
            TopicLink.DataLen1 - sizeof(TOPICLINK))
          break;
      } else
        LinkData1 = NULL;
      if (TopicLink.DataLen1 <
          TopicLink.BlockSize) /* read LinkData2 using phrase replacement */
      {
        LinkData2 = helpdeco_malloc(TopicLink.DataLen2 + 1);
        if (topic_read_phrase(HelpFile, 0, LinkData2,
                              TopicLink.BlockSize - TopicLink.DataLen1,
                              TopicLink.DataLen2) !=
            TopicLink.BlockSize - TopicLink.DataLen1)
          break;
      } else
        LinkData2 = NULL;
      if (LinkData1 && TopicLink.RecordType ==
                           TL_TOPICHDR) /* display a Topic Header record */
      {
        if (ctx->opt_topics_per_rtf &&
            ++TopicInRTF >= ctx->opt_topics_per_rtf) {
          putc('}', rtf);
          helpdeco_fclose(rtf);
          BuildRTFName(scratch_buffer, ++NumberOfRTF);
          if (hpj)
            fprintf(hpj, "%s\n", scratch_buffer);
          rtf = helpdeco_fopen(scratch_buffer, "wt");
          FontLoadRTF(HelpFile, rtf, NULL);
          TopicInRTF = 0;
        } else if (!firsttopic) {
          if (makertf && ctx->opt_nopagebreak) {
            fputs("\\par\n", rtf);
          } else {
            fputs("\\page\n", rtf);
          }
        }
        firsttopic = FALSE;
        helpdeco_logf("\nTopic %ld...", TopicNum - 15);
        if (!makertf) {
          BrowseNum = 0;
          if (ctx->before31) {
            TopicHdr30 = (TOPICHEADER30 *)LinkData1;
            fprintf(rtf, "{\\up #}{\\footnote\\pard\\plain{\\up #} TOPIC%ld}\n",
                    TopicNum);
            if (ctx->opt_resolvebrowse) {
              if ((TopicHdr30->NextTopicNum > TopicNum &&
                   TopicHdr30->PrevTopicNum > TopicNum) ||
                  (TopicHdr30->NextTopicNum == -1 &&
                   TopicHdr30->PrevTopicNum > TopicNum) ||
                  (TopicHdr30->NextTopicNum > TopicNum &&
                   TopicHdr30->PrevTopicNum == -1)) {
                BrowseNum = AddLink(TopicNum, TopicHdr30->NextTopicNum,
                                    TopicHdr30->PrevTopicNum);
              } else if (TopicHdr30->NextTopicNum != -1 &&
                         TopicHdr30->NextTopicNum < TopicNum &&
                         TopicHdr30->PrevTopicNum != -1 &&
                         TopicHdr30->PrevTopicNum < TopicNum) {
                BrowseNum =
                    MergeLink(TopicNum, TopicNum, TopicHdr30->NextTopicNum,
                              TopicHdr30->PrevTopicNum);
              } else if (TopicHdr30->NextTopicNum != -1 &&
                         TopicHdr30->NextTopicNum < TopicNum &&
                         (TopicHdr30->PrevTopicNum == -1 ||
                          TopicHdr30->PrevTopicNum > TopicNum)) {
                BrowseNum =
                    BackLinkLink(TopicNum, TopicNum, TopicHdr30->NextTopicNum,
                                 TopicHdr30->PrevTopicNum);
              } else if (TopicHdr30->PrevTopicNum != -1 &&
                         TopicHdr30->PrevTopicNum < TopicNum &&
                         (TopicHdr30->NextTopicNum == -1 ||
                          TopicHdr30->NextTopicNum > TopicNum)) {
                BrowseNum =
                    LinkLink(TopicNum, TopicNum, TopicHdr30->NextTopicNum,
                             TopicHdr30->PrevTopicNum);
              }
            }
            ListKeywords(HelpFile, rtf, TopicPos);
          } else {
            BogusTopicOffset =
                NextTopicOffset(TopicOffset, TopicLink.NextBlock, TopicPos);
            TopicHdr = (TOPICHEADER *)LinkData1;
            if (TopicHdr->Scroll != -1) {
              nonscroll = TopicHdr->Scroll;
            } else {
              nonscroll = TopicHdr->NextTopic;
            }
            if (ctx->opt_resolvebrowse) {
              if ((TopicHdr->BrowseFor > TopicOffset &&
                   TopicHdr->BrowseBck > TopicOffset) ||
                  (TopicHdr->BrowseFor == -1 &&
                   TopicHdr->BrowseBck > TopicOffset) ||
                  (TopicHdr->BrowseFor > TopicOffset &&
                   TopicHdr->BrowseBck == -1)) {
                BrowseNum = AddLink(TopicOffset, TopicHdr->BrowseFor,
                                    TopicHdr->BrowseBck);
              } else if (TopicHdr->BrowseFor != -1 &&
                         TopicHdr->BrowseFor < TopicOffset &&
                         TopicHdr->BrowseBck != -1 &&
                         TopicHdr->BrowseBck < TopicOffset) {
                BrowseNum = MergeLink(TopicOffset, BogusTopicOffset,
                                      TopicHdr->BrowseFor, TopicHdr->BrowseBck);
              } else if (TopicHdr->BrowseFor != -1 &&
                         TopicHdr->BrowseFor < TopicOffset &&
                         (TopicHdr->BrowseBck == -1 ||
                          TopicHdr->BrowseBck > TopicOffset)) {
                BrowseNum =
                    BackLinkLink(TopicOffset, BogusTopicOffset,
                                 TopicHdr->BrowseFor, TopicHdr->BrowseBck);
              } else if (TopicHdr->BrowseBck != -1 &&
                         TopicHdr->BrowseBck < TopicOffset &&
                         (TopicHdr->BrowseFor == -1 ||
                          TopicHdr->BrowseFor > TopicOffset)) {
                BrowseNum = LinkLink(TopicOffset, BogusTopicOffset,
                                     TopicHdr->BrowseFor, TopicHdr->BrowseBck);
              }
            }
          }
          ListGroups(rtf, TopicNum - 16, BrowseNum);
          if (LinkData2 && TopicLink.DataLen2 > 0) {
            if (*LinkData2) {
              fputs("{\\up $}{\\footnote\\pard\\plain{\\up $} ", rtf);
              putrtf(rtf, LinkData2);
              fputs("}\n", rtf);
            }
            for (i = strlen(LinkData2) + 1; i < TopicLink.DataLen2;
                 i += strlen(LinkData2 + i) + 1) {
              fputs("{\\up !}{\\footnote\\pard\\plain{\\up !} ", rtf);
              if (!ctx->after31 && strlen(LinkData2 + i) > 254) {
                printf("Help compiler will issue Warning 3511: Macro '%s' "
                       "exceeds limit of 254 characters\n",
                       LinkData2 + i);
              }
              putrtf(rtf, LinkData2 + i);
              fputs("}\n", rtf);
            }
          }
          while (NextContextRec < ctx->context_rec.count &&
                 ctx->context_rec.entry[NextContextRec].TopicOffset <=
                     TopicOffset) {
            fputs("{\\up #}{\\footnote\\pard\\plain{\\up #} ", rtf);
            putrtf(rtf,
                   unhash(ctx->context_rec.entry[NextContextRec].HashValue));
            fputs("}\n", rtf);
            if (!ctx->mvp)
              while (
                  NextContextRec + 1 < ctx->context_rec.count &&
                  ctx->context_rec.entry[NextContextRec].TopicOffset ==
                      ctx->context_rec.entry[NextContextRec + 1].TopicOffset) {
                NextContextRec++;
              }
            NextContextRec++;
          }
          i = ListWindows(HelpFile, TopicOffset);
          if (i != -1)
            fprintf(rtf, "{\\up >}{\\footnote\\pard\\plain{\\up >} %s}\n",
                    GetWindowName(i));
        }
        TopicNum++;
      } else if (LinkData1 && LinkData2 &&
                 (TopicLink.RecordType == TL_DISPLAY30 ||
                  TopicLink.RecordType == TL_DISPLAY ||
                  TopicLink.RecordType == TL_TABLE)) {
        if (ctx->annotation_file)
          Annotate(TopicPos, rtf);
        ptr = LinkData1;
        scanlong(&ptr);
        if (TopicLink.RecordType == TL_DISPLAY ||
            TopicLink.RecordType == TL_TABLE) {
          x1 = scanword(&ptr);
          ActualTopicOffset = TopicOffset;
          MaxTopicOffset = ActualTopicOffset + x1;
          TopicOffset += x1;
        }
        if (TopicLink.RecordType == TL_TABLE) {
          fputs("\\trowd", rtf);
          cols = (unsigned char)*ptr++;
          x1 = (unsigned char)*ptr++;
          switch (x1) {
          case 0:
          case 2:
            l1 = *(int16_t *)ptr; /* min table width */
            ptr += 2;
            fputs("\\trqc", rtf);
            break;
          case 1:
          case 3:
            l1 = 32767;
            break;
          }
          iptr = (int16_t *)ptr;
          if (cols > 1) {
            x1 = iptr[0] + iptr[1] + iptr[3] / 2;
            fprintf(rtf, "\\trgaph%ld\\trleft%ld \\cellx%ld\\cellx%ld",
                    ((iptr[3] * ctx->scaling - ctx->rounderr) * l1) / 32767,
                    (((iptr[1] - iptr[3]) * ctx->scaling - ctx->rounderr) * l1 -
                     32767) /
                        32767,
                    ((x1 * ctx->scaling - ctx->rounderr) * l1) / 32767,
                    (((x1 + iptr[2] + iptr[3]) * ctx->scaling - ctx->rounderr) *
                     l1) /
                        32767);
            x1 += iptr[2] + iptr[3];
            for (col = 2; col < cols; col++) {
              x1 += iptr[2 * col] + iptr[2 * col + 1];
              fprintf(rtf, "\\cellx%ld",
                      ((x1 * ctx->scaling - ctx->rounderr) * l1) / 32767);
            }
          } else {
            fprintf(rtf, "\\trleft%ld \\cellx%ld ",
                    ((iptr[1] * ctx->scaling - ctx->rounderr) * l1 - 32767) /
                        32767,
                    ((iptr[0] * ctx->scaling - ctx->rounderr) * l1) / 32767);
          }
          ptr = (char *)(iptr + 2 * cols);
        }
        lastcol = -1;
        str = LinkData2;
        for (col = 0; (TopicLink.RecordType == TL_TABLE ? *(int16_t *)ptr != -1
                                                        : col == 0) &&
                      ptr < LinkData1 + TopicLink.DataLen1 - sizeof(TOPICLINK);
             col++) {
          fputs("\\pard", rtf);
          if (TopicPos < nonscroll)
            fputs("\\keepn", rtf);
          if (TopicLink.RecordType == TL_TABLE) {
            fputs("\\intbl", rtf);
            lastcol = *(int16_t *)ptr;
            ptr += 5;
          }
          ptr += 4;
          x2 = *(uint16_t *)ptr;
          ptr += 2;
          if (x2 & 0x1000)
            fputs("\\keep", rtf);
          if (x2 & 0x0400)
            fputs("\\qr", rtf);
          if (x2 & 0x0800)
            fputs("\\qc", rtf);
          if (x2 & 0x0001)
            scanlong(&ptr);
          if (x2 & 0x0002)
            fprintf(rtf, "\\sb%ld",
                    scanint(&ptr) * ctx->scaling - ctx->rounderr);
          if (x2 & 0x0004)
            fprintf(rtf, "\\sa%ld",
                    scanint(&ptr) * ctx->scaling - ctx->rounderr);
          if (x2 & 0x0008)
            fprintf(rtf, "\\sl%ld",
                    scanint(&ptr) * ctx->scaling - ctx->rounderr);
          if (x2 & 0x0010)
            fprintf(rtf, "\\li%ld",
                    scanint(&ptr) * ctx->scaling - ctx->rounderr);
          if (x2 & 0x0020)
            fprintf(rtf, "\\ri%ld",
                    scanint(&ptr) * ctx->scaling - ctx->rounderr);
          if (x2 & 0x0040)
            fprintf(rtf, "\\fi%ld",
                    scanint(&ptr) * ctx->scaling - ctx->rounderr);
          if (x2 & 0x0100) {
            x1 = (unsigned char)*ptr++;
            if (x1 & 1)
              fputs("\\box", rtf);
            if (x1 & 2)
              fputs("\\brdrt", rtf);
            if (x1 & 4)
              fputs("\\brdrl", rtf);
            if (x1 & 8)
              fputs("\\brdrb", rtf);
            if (x1 & 0x10)
              fputs("\\brdrr", rtf);
            if (x1 & 0x20)
              fputs("\\brdrth", rtf);
            else
              fputs("\\brdrs", rtf);
            if (x1 & 0x40)
              fputs("\\brdrdb", rtf);
            ptr += 2;
          }
          if (x2 & 0x0200) {
            y1 = scanint(&ptr);
            while (y1-- > 0) {
              x1 = scanword(&ptr);
              if (x1 & 0x4000) {
                switch (scanword(&ptr)) {
                case 1:
                  fputs("\\tqr", rtf);
                  break;
                case 2:
                  fputs("\\tqc", rtf);
                  break;
                }
              }
              fprintf(rtf, "\\tx%ld",
                      (x1 & 0x3FFF) * ctx->scaling - ctx->rounderr);
            }
          }
          putc(' ', rtf);
          while (
              1) /* ptr<LinkData1+TopicLink.DataLen1-sizeof(TOPICLINK)&&str<end)
                  */
          {
            if (*str && fontset >= 0 && fontset < ctx->font.count &&
                ctx->font.entry && ctx->font.entry[fontset].SmallCaps)
              strlwr(str);
            do {
              if (!makertf) {
                while (NextContextRec < ctx->context_rec.count &&
                       ctx->context_rec.entry[NextContextRec].TopicOffset <=
                           ActualTopicOffset &&
                       ctx->context_rec.entry[NextContextRec].TopicOffset <
                           MaxTopicOffset) {
                  fputs("{\\up #}{\\footnote\\pard\\plain{\\up #} ", rtf);
                  putrtf(
                      rtf,
                      unhash(ctx->context_rec.entry[NextContextRec].HashValue));
                  fputs("}\n", rtf);
                  if (!ctx->mvp)
                    while (NextContextRec + 1 < ctx->context_rec.count &&
                           ctx->context_rec.entry[NextContextRec].TopicOffset ==
                               ctx->context_rec.entry[NextContextRec + 1]
                                   .TopicOffset) {
                      NextContextRec++;
                    }
                  NextContextRec++;
                }
                if (!ctx->before31)
                  ListKeywords(HelpFile, rtf,
                               ActualTopicOffset < MaxTopicOffset
                                   ? ActualTopicOffset
                                   : MaxTopicOffset - 1);
              }
              if (*str) {
                if (*str != '{' && *str != '}' && *str != '\\' &&
                    isprint((unsigned char)*str)) {
                  putc(*str, rtf);
                } else if (!makertf && *str == '{') {
                  fputs("\\{\\-", rtf); /* emit invisible dash after { brace */
                  /* because bmc or another legal command may follow, but this
                   */
                  /* command was not parsed the help file was build, so it was
                   */
                  /* used just as an example. The dash will be eaten up by the
                   */
                  /* help compiler on recompile. */
                } else {
                  fprintf(rtf, "\\'%02x", (unsigned char)*str);
                }
              }
              if (ActualTopicOffset < MaxTopicOffset)
                ActualTopicOffset++;
            } while (*str++);
            if ((unsigned char)ptr[0] == 0xFF) {
              ptr++;
              break;
            } else
              switch ((unsigned char)ptr[0]) {
              case 0x20: /* vfld MVB */
                if (*(legacy_long *)(ptr + 1)) {
                  fprintf(rtf, "\\{vfld%ld\\}", *(legacy_long *)(ptr + 1));
                } else {
                  fputs("\\{vfld\\}", rtf);
                }
                ptr += 5;
                break;
              case 0x21: /* dtype MVB */
                if (*(int16_t *)(ptr + 1)) {
                  fprintf(rtf, "\\{dtype%d\\}", *(int16_t *)(ptr + 1));
                } else {
                  fputs("\\{dtype\\}", rtf);
                }
                ptr += 3;
                break;
              case 0x80: /* font change */
                ChangeFont(rtf, fontset = *(int16_t *)(ptr + 1), ul, uldb);
                ptr += 3;
                break;
              case 0x81:
                fputs("\\line\n", rtf);
                ptr++;
                break;
              case 0x82:
                if (TopicLink.RecordType == TL_TABLE) {
                  if ((unsigned char)ptr[1] != 0xFF) {
                    fputs("\n\\par\\intbl ", rtf);
                  } else if (*(int16_t *)(ptr + 2) == -1) {
                    fputs("\\cell\\intbl\\row\n", rtf);
                  } else if (*(int16_t *)(ptr + 2) == lastcol) {
                    fputs("\\par\\pard ", rtf);
                  } else {
                    fputs("\\cell\\pard ", rtf);
                  }
                } else {
                  fputs("\n\\par ", rtf);
                }
                ptr++;
                break;
              case 0x83:
                fputs("\\tab ", rtf);
                ptr++;
                break;
              case 0x86:
                x3 = (unsigned char)*ptr++;
                x1 = *ptr++;
                if (x1 == 0x05)
                  cmd = "ewc";
                else
                  cmd = "bmc";
                goto picture;
              case 0x87:
                x3 = (unsigned char)*ptr++;
                x1 = *ptr++;
                if (x1 == 0x05)
                  cmd = "ewl";
                else
                  cmd = "bml";
                goto picture;
              case 0x88:
                x3 = (unsigned char)*ptr++;
                x1 = *ptr++;
                if (x1 == 0x05)
                  cmd = "ewr";
                else
                  cmd = "bmr";
                goto picture;
              picture:
                l1 = scanlong(&ptr);
                switch (x1) {
                case 0x22: /* HC31 */
                  ActualTopicOffset +=
                      scanword(&ptr); /* number of hotspots in picture */
                  if (ActualTopicOffset > MaxTopicOffset)
                    ActualTopicOffset = MaxTopicOffset;
                  /* fall thru */
                case 0x03: /* HC30 */
                  x1 = ((uint16_t *)ptr)[0];
                  switch (x1) {
                  case 1:
                    while (nextbitmap < ctx->extension.count &&
                           ctx->extension.entry[nextbitmap] < 0x10)
                      nextbitmap++;
                    if (nextbitmap >= ctx->extension.count) {
                      error("Bitmap never saved");
                      break;
                    }
                    x2 = nextbitmap++;
                    goto other;
                  case 0:
                    x2 = ((uint16_t *)ptr)[1];
                  other:
                    if (makertf) {
                      switch (x3) {
                      case 0x86:
                        fprintf(rtf, "{\\field {\\*\\fldinst import %s}}",
                                getbitmapname(x2));
                        break;
                      case 0x87:
                        fprintf(rtf,
                                "{\\pvpara {\\field {\\*\\fldinst import "
                                "%s}}\\par}\n",
                                getbitmapname(x2));
                        break;
                      case 0x88:
                        fprintf(rtf,
                                "{\\pvpara\\posxr{\\field {\\*\\fldinst import "
                                "%s}}\\par}\n",
                                getbitmapname(x2));
                        break;
                      }
                    } else {
                      if (x2 < ctx->extension.count &&
                          (ctx->extension.entry[x2] & 0x20)) {
                        if (strcmp(cmd, "bmc") == 0)
                          cmd = "bmct";
                        else if (strcmp(cmd, "bml") == 0)
                          cmd = "bmlt";
                        else if (strcmp(cmd, "bmr") == 0)
                          cmd = "bmrt";
                      }
                      fprintf(rtf, "\\{%s %s\\}", cmd, getbitmapname(x2));
                    }
                    break;
                  }
                  break;
                case 0x05: /* ewc,ewl,ewr */
                  if (ptr[6] == '!') {
                    fprintf(rtf, "\\{button %s\\}", ptr + 7);
                  } else if (ptr[6] == '*') {
                    char *plus;
                    legacy_int n, c1, c2;

                    sscanf(ptr + 7, "%d,%d,%n", &c1, &c2, &n);
                    plus = strchr(ptr + 7 + n, '+');
                    if ((c1 & 0xFFF5) != 0x8400)
                      fprintf(stderr, "mci c1=%04x\n", c1);
                    fputs("\\{mci", rtf);
                    if (cmd[2] == 'r')
                      fputs("_right", rtf);
                    if (cmd[2] == 'l')
                      fputs("_left", rtf);
                    if (c2 == 1)
                      fputs(" REPEAT", rtf);
                    if (c2 == 2)
                      fputs(" PLAY", rtf);
                    if (!plus)
                      fputs(" EXTERNAL", rtf);
                    if (c1 & 8)
                      fputs(" NOMENU", rtf);
                    if (c1 & 2)
                      fputs(" NOPLAYBAR", rtf);
                    fprintf(rtf, ",%s\\}\n", plus ? plus + 1 : ptr + 7 + n);
                  } else {
                    fprintf(rtf, "\\{%s %s\\}", cmd, ptr + 6);
                  }
                  break;
                }
                ptr += l1;
                break;
              case 0x89: /* end of hotspot */
                if (!makertf) {
                  if (hotspot[0] == '%' && fontset >= 0 &&
                      fontset < ctx->font.count &&
                      ctx->font.entry[fontset].Underline) {
                    hotspot[0] = '*';
                  }
                }
                ChangeFont(rtf, fontset, ul = FALSE, uldb = FALSE);
                if (!makertf) {
                  if (!ctx->after31 && strlen(hotspot) > 255) {
                    puts("Help compiler will issue Warning 4072: Context "
                         "string exceeds limit of 255 characters");
                  }
                  fputs("{\\v ", rtf);
                  putrtf(rtf,
                         ctx->multi && (hotspot[0] == '%' || hotspot[0] == '*')
                             ? hotspot + 1
                             : hotspot);
                  fputc('}', rtf);
                }
                ptr++;
                break;
              case 0xC8: /* macro */
                ChangeFont(rtf, fontset, FALSE, uldb = TRUE);
                if (!makertf) {
                  hotspot = helpdeco_realloc(hotspot, strlen(ptr + 3) + 2);
                  sprintf(hotspot, "!%s", ptr + 3);
                }
                ptr += *(int16_t *)(ptr + 1) + 3;
                break;
              case 0xCC: /* macro without font change */
                ChangeFont(rtf, fontset, FALSE, uldb = TRUE);
                if (!makertf) {
                  hotspot = helpdeco_realloc(hotspot, strlen(ptr + 3) + 3);
                  sprintf(hotspot, "%%!%s", ptr + 3);
                }
                ptr += *(int16_t *)(ptr + 1) + 3;
                break;
              case 0xE0: /* popup jump HC30 */
                ChangeFont(rtf, fontset, ul = TRUE, FALSE);
                goto label0;
              case 0xE1: /* topic jump HC30 */
                ChangeFont(rtf, fontset, FALSE, uldb = TRUE);
              label0:
                if (!makertf) {
                  hotspot = helpdeco_realloc(hotspot, 128);
                  sprintf(hotspot, "TOPIC%ld", *(legacy_long *)(ptr + 1));
                }
                ptr += 5;
                break;
              case 0xE2: /* popup jump HC31 */
                ChangeFont(rtf, fontset, ul = TRUE, FALSE);
                goto label1;
              case 0xE3: /* topic jump HC31 */
                ChangeFont(rtf, fontset, FALSE, uldb = TRUE);
              label1:
                if (!makertf) {
                  arg = ContextId(*(legacy_long *)(ptr + 1));
                  hotspot = helpdeco_realloc(hotspot, strlen(arg) + 1);
                  sprintf(hotspot, "%s", arg);
                }
                ptr += 5;
                break;
              case 0xE6: /* popup jump without font change */
                ChangeFont(rtf, fontset, ul = TRUE, FALSE);
                goto label2;
              case 0xE7: /* topic jump without font change */
                ChangeFont(rtf, fontset, FALSE, uldb = TRUE);
              label2:
                if (!makertf) {
                  arg = ContextId(*(legacy_long *)(ptr + 1));
                  hotspot = helpdeco_realloc(hotspot, strlen(arg) + 2);
                  sprintf(hotspot, "%%%s", arg);
                }
                ptr += 5;
                break;
              case 0xEA: /* popup jump into external file */
              case 0xEE:
                ChangeFont(rtf, fontset, ul = TRUE, FALSE);
                goto label3;
              case 0xEB: /* topic jump into external file / secondary window */
              case 0xEF:
                ChangeFont(rtf, fontset, FALSE, uldb = TRUE);
              label3:
                if (!makertf) {
                  if ((unsigned char)ptr[0] == 0xEE ||
                      (unsigned char)ptr[0] == 0xEF) {
                    cmd = "%";
                  } else {
                    cmd = "";
                  }
                  arg = unhash(
                      *(legacy_long *)(ptr + 4)); // no ContextId, it may jump
                                                  // into external file
                  switch ((unsigned char)ptr[3]) {
                  case 0:
                    hotspot =
                        helpdeco_realloc(hotspot, strlen(cmd) + strlen(arg) + 1);
                    sprintf(hotspot, "%s%s", cmd, arg);
                    break;
                  case 1:
                    hotspot = helpdeco_realloc(hotspot,
                                         strlen(cmd) + strlen(arg) + 1 +
                                     strlen(GetWindowName(ptr[8])) + 1);
                    sprintf(hotspot, "%s%s>%s", cmd, arg,
                            GetWindowName(ptr[8]));
                    break;
                  case 4:
                    hotspot = helpdeco_realloc(hotspot, strlen(cmd) + strlen(arg) +
                                                      1 + strlen(ptr + 8) + 1);
                    sprintf(hotspot, "%s%s@%s", cmd, arg, ptr + 8);
                    break;
                  case 6:
                    hotspot = helpdeco_realloc(
                        hotspot, strlen(cmd) + strlen(arg) + 1 +
                                     strlen(ptr + 8) + 1 +
                                     strlen(strchr(ptr + 8, '\0') + 1) + 1);
                    sprintf(hotspot, "%s%s>%s@%s", cmd, arg, ptr + 8,
                            strchr(ptr + 8, '\0') + 1);
                    break;
                  }
                }
                ptr += *(int16_t *)(ptr + 1) + 3;
                break;
              case 0x8B:
                fputs("\\~", rtf);
                ptr++;
                break;
              case 0x8C:
                fputs("\\-", rtf);
                ptr++;
                break;
              default:
                ptr++;
              }
          }
        }
      }
      if (LinkData1)
        free(LinkData1);
      if (LinkData2)
        free(LinkData2);
      if (ctx->before31) {
        TopicPos += TopicLink.NextBlock;
      } else {
        TopicOffset =
            NextTopicOffset(TopicOffset, TopicLink.NextBlock, TopicPos);
        TopicPos = TopicLink.NextBlock;
      }
    }
  }
  return rtf;
}

int ContextRecCmp(const void *a, const void *b) {
  if (((const CONTEXTREC *)a)->TopicOffset <
      ((const CONTEXTREC *)b)->TopicOffset)
    return -1;
  if (((const CONTEXTREC *)a)->TopicOffset >
      ((const CONTEXTREC *)b)->TopicOffset)
    return 1;
  return 0;
}

void ContextLoad(FILE *HelpFile) {
  BUFFER buf;
  legacy_int n;
  legacy_long entries;

  if (SearchFile(HelpFile, "|CONTEXT", NULL)) {
    n = GetFirstPage(HelpFile, &buf, &entries);
    if (entries) {
      ctx->context_rec.entry = helpdeco_malloc(entries * sizeof(CONTEXTREC));
      ctx->context_rec.count = 0;
      while (n) {
        if (ctx->context_rec.count + n > entries) {
          helpdeco_errorf("malformed |CONTEXT file\n");
        }
        read_CONTEXTRECs(ctx->context_rec.entry + ctx->context_rec.count, n,
                         HelpFile);
        ctx->context_rec.count += n;
        n = GetNextPage(HelpFile, &buf);
      }
      helpdeco_logf("%d topic offsets and hash values loaded\n",
                    ctx->context_rec.count);
      qsort(ctx->context_rec.entry, ctx->context_rec.count, sizeof(CONTEXTREC),
            ContextRecCmp);
    }
  } else if (SearchFile(HelpFile, "|TOMAP", &entries)) {
    ctx->topic.entry = helpdeco_malloc(entries);
    helpdeco_fread(ctx->topic.entry, entries, HelpFile);
    ctx->topic.count = (legacy_int)(entries / sizeof(int32_t));
  }
}

void GenerateContent(
    FILE *HelpFile, FILE *ContentFile) /* create a simple Win95 contents file */
{
  VIOLAREC *WindowRec = NULL;
  legacy_long FileLength, offset;
  legacy_int n, i, j, WindowRecs;
  BUFFER buf;
  char *ptr;

  fprintf(ContentFile, ":Base %s%s>main\n", ctx->name, ctx->ext);
  if (ctx->title[0])
    fprintf(ContentFile, ":Title %s\n", ctx->title);
  WindowRecs = 0;
  if (SearchFile(HelpFile, "|VIOLA", NULL)) {
    n = GetFirstPage(HelpFile, &buf, &FileLength);
    if (FileLength) {
      WindowRec = helpdeco_malloc(FileLength * sizeof(VIOLAREC));
      while (n) {
        read_VIOLARECs(WindowRec + WindowRecs, n, HelpFile);
        WindowRecs += n;
        n = GetNextPage(HelpFile, &buf);
      }
    }
  }
  if (SearchFile(HelpFile, "|TTLBTREE", NULL)) {
    for (n = GetFirstPage(HelpFile, &buf, NULL); n;
         n = GetNextPage(HelpFile, &buf)) {
      for (i = 0; i < n; i++) {
        offset = helpdeco_getdw(HelpFile);
        if (helpdeco_gets(scratch_buffer, sizeof(scratch_buffer), HelpFile)) {
          ptr = TopicName(offset);
          if (ptr) {
            fprintf(ContentFile, "1 %s=%s", scratch_buffer, ptr);
            for (j = 0; j < WindowRecs; j++) {
              if (WindowRec[j].TopicOffset == offset) {
                fprintf(ContentFile, ">%s",
                        GetWindowName(WindowRec[j].WindowNumber));
                break;
              }
            }
            putc('\n', ContentFile);
          }
        }
      }
    }
  }
}

void ListRose(FILE *HelpFile, FILE *hpj) {
  legacy_long FileLength, offset, hash, h, pos, savepos;
  unsigned char *ptr;
  legacy_long *keytopic;
  legacy_int n, i, l, e;
  uint16_t j, count;
  BUFFER buf, buf2;

  if (SearchFile(HelpFile, "|Rose", NULL)) {
    savepos = ftell(HelpFile);
    if (SearchFile(HelpFile, "|KWDATA", &FileLength)) {
      keytopic = helpdeco_malloc(FileLength);
      helpdeco_fread(keytopic, FileLength, HelpFile);
      if (SearchFile(HelpFile, "|KWBTREE", NULL)) {
        fputs("[MACROS]\n", hpj);
        for (n = GetFirstPage(HelpFile, &buf, NULL); n;
             n = GetNextPage(HelpFile, &buf)) {
          for (i = 0; i < n; i++) {
            helpdeco_gets(keyword, sizeof(keyword), HelpFile);
            for (hash = 0, ptr = (unsigned char *)keyword; *ptr; ptr++) {
              hash = hash * 43 + table[*ptr];
            }
            count = helpdeco_getw(HelpFile);
            offset = helpdeco_getdw(HelpFile);
            for (j = 0; j < count; j++) {
              if (keytopic[offset / 4 + j] == -1) {
                pos = ftell(HelpFile);
                fseek(HelpFile, savepos, SEEK_SET);
                for (l = GetFirstPage(HelpFile, &buf2, NULL); l;
                     l = GetNextPage(HelpFile, &buf2)) {
                  for (e = 0; e < l; e++) {
                    h = helpdeco_getdw(HelpFile);
                    helpdeco_gets(scratch_buffer, sizeof(scratch_buffer), HelpFile);
                    if (h == hash) {
                      fprintf(hpj, "%s\n%s\n", keyword, scratch_buffer);
                      helpdeco_gets(scratch_buffer, sizeof(scratch_buffer), HelpFile);
                      fprintf(hpj, "%s\n", scratch_buffer);
                    } else {
                      helpdeco_gets(scratch_buffer, sizeof(scratch_buffer), HelpFile);
                    }
                  }
                }
                fseek(HelpFile, pos, SEEK_SET);
                break;
              }
            }
          }
        }
        putc('\n', hpj);
      }
      free(keytopic);
      keytopic = NULL;
    }
  }
}

/* dump section: all the dump-routines are used to display internal files
// of the help file with known format of contents for debugging reasons */
void PrintNewFont(legacy_int i, NEWFONT *newfont) {
  printf("%3d: %-32.32s %6ld %-6s %02X%02X%02X %02X%02X%02X ", i,
         ctx->fontname.entry[newfont->FontName], newfont->Height,
         FontFamily(newfont->PitchAndFamily >> 4), newfont->FGRGB[2],
         newfont->FGRGB[1], newfont->FGRGB[0], newfont->BGRGB[2],
         newfont->BGRGB[1], newfont->BGRGB[0]);
  if (newfont->Weight > 500)
    putchar('b');
  if (newfont->Italic)
    putchar('i');
  if (newfont->Underline)
    putchar('u');
  if (newfont->StrikeOut)
    putchar('s');
  if (newfont->DoubleUnderline)
    putchar('d');
  if (newfont->SmallCaps)
    putchar('c');
  putchar('\n');
}

void PrintMvbFont(legacy_int i, MVBFONT *mvbfont) {
  printf("%3d: %-32.32s %6ld %-6s %02X%02X%02X %02X%02X%02X ", i,
         ctx->fontname.entry[mvbfont->FontName], mvbfont->Height,
         FontFamily(mvbfont->PitchAndFamily >> 4), mvbfont->FGRGB[2],
         mvbfont->FGRGB[1], mvbfont->FGRGB[0], mvbfont->BGRGB[2],
         mvbfont->BGRGB[1], mvbfont->BGRGB[0]);
  if (mvbfont->Weight > 500)
    putchar('b');
  if (mvbfont->Italic)
    putchar('i');
  if (mvbfont->Underline)
    putchar('u');
  if (mvbfont->StrikeOut)
    putchar('s');
  if (mvbfont->DoubleUnderline)
    putchar('d');
  if (mvbfont->SmallCaps)
    putchar('c');
  putchar('\n');
}

void FontDump(FILE *HelpFile) {
  FONTHEADER FontHdr;
  legacy_long FileStart;
  OLDFONT oldfont;
  NEWFONT newfont;
  NEWSTYLE newstyle;
  MVBFONT mvbfont;
  MVBSTYLE mvbstyle;
  legacy_int i, n;

  /* Go to the FONT file and get the headers */
  FileStart = ftell(HelpFile);
  read_FONTHEADER(&FontHdr, HelpFile);
  n = (FontHdr.DescriptorsOffset - FontHdr.FacenamesOffset) /
      FontHdr.NumFacenames;
  ctx->fontname.entry = helpdeco_malloc(FontHdr.NumFacenames * sizeof(char *));
  fseek(HelpFile, FileStart + FontHdr.FacenamesOffset, SEEK_SET);
  for (i = 0; i < FontHdr.NumFacenames; i++) {
    helpdeco_fread(scratch_buffer, n, HelpFile);
    scratch_buffer[n] = '\0';
    printf("Font name %d: %s\n", i, scratch_buffer);
    ctx->fontname.entry[i] = helpdeco_strdup(scratch_buffer);
  }
  puts("Font Facename 			Height Family Foregr Backgr Style");
  fseek(HelpFile, FileStart + FontHdr.DescriptorsOffset, SEEK_SET);
  if (FontHdr.FacenamesOffset >= 16) {
    for (i = 0; i < FontHdr.NumDescriptors; i++) {
      read_MVBFONT(&mvbfont, HelpFile);
      PrintMvbFont(i, &mvbfont);
    }
    fseek(HelpFile, FileStart + FontHdr.FormatsOffset, SEEK_SET);
    for (i = 0; i < FontHdr.NumFormats; i++) {
      read_MVBSTYLE(&mvbstyle, HelpFile);
      printf("Style %d", mvbstyle.StyleNum);
      if (mvbstyle.BasedOn)
        printf(" based on %d", mvbstyle.BasedOn);
      printf(" named '%s':\n", mvbstyle.StyleName);
      PrintMvbFont(i, &mvbstyle.font);
    }
  } else if (FontHdr.FacenamesOffset >= 12) {
    for (i = 0; i < FontHdr.NumDescriptors; i++) {
      read_NEWFONT(&newfont, HelpFile);
      PrintNewFont(i, &newfont);
    }
    fseek(HelpFile, FileStart + FontHdr.FormatsOffset, SEEK_SET);
    for (i = 0; i < FontHdr.NumFormats; i++) {
      read_NEWSTYLE(&newstyle, HelpFile);
      printf("Style %d", newstyle.StyleNum);
      if (newstyle.BasedOn)
        printf(" based on %d", newstyle.BasedOn);
      printf(" named '%s':\n", newstyle.StyleName);
      PrintNewFont(i, &newstyle.font);
    }
  } else {
    for (i = 0; i < FontHdr.NumDescriptors; i++) {
      read_OLDFONT(&oldfont, HelpFile);
      printf("%3d: %-32.32s %4d.%d %-6s %02X%02X%02X %02X%02X%02X ", i,
             ctx->fontname.entry[oldfont.FontName], oldfont.HalfPoints / 2,
             (oldfont.HalfPoints & 1) * 5,
             FontFamily(oldfont.FontFamily < 6 ? lookup[oldfont.FontFamily]
                                               : oldfont.FontFamily),
             oldfont.FGRGB[2], oldfont.FGRGB[1], oldfont.FGRGB[0],
             oldfont.BGRGB[2], oldfont.BGRGB[1], oldfont.BGRGB[0]);
      if (oldfont.Attributes & FONT_BOLD)
        putchar('b');
      if (oldfont.Attributes & FONT_ITAL)
        putchar('i');
      if (oldfont.Attributes & FONT_UNDR)
        putchar('u');
      if (oldfont.Attributes & FONT_STRK)
        putchar('s');
      if (oldfont.Attributes & FONT_DBUN)
        putchar('d');
      if (oldfont.Attributes & FONT_SMCP)
        putchar('c');
      putchar('\n');
    }
  }
}

void PhrImageDump(FILE *HelpFile) {
  legacy_long FileLength;
  unsigned_legacy_int bytes;
  PHRINDEXHDR PhrIndexHdr;
  unsigned char *ptr;

  if (SearchFile(HelpFile, "|PhrIndex", NULL)) {
    read_PHRINDEXHDR(&PhrIndexHdr, HelpFile);
    if (SearchFile(HelpFile, "|PhrImage", &FileLength)) {
      if (PhrIndexHdr.phrimagesize == PhrIndexHdr.phrimagecompressedsize) {
        HexDump(HelpFile, FileLength, 0);
      } else {
        if (FileLength != PhrIndexHdr.phrimagecompressedsize) {
          fprintf(stderr, "PhrImage FileSize %ld, in PhrIndex.FileHdr %ld\n",
                  PhrIndexHdr.phrimagecompressedsize, FileLength);
        }
        ptr = helpdeco_malloc(PhrIndexHdr.phrimagesize);
        bytes = mfile_decompress_into_buffer(2, HelpFile, FileLength, ptr,
                                             PhrIndexHdr.phrimagesize);
        HexDumpMemory(ptr, bytes);
        free(ptr);
        ptr = NULL;
      }
    }
  }
}

void BTreeDump(FILE *HelpFile, char text[]) {
  legacy_int n, i, j;
  legacy_long count;
  BUFFER buf;
  char format[10];
  char *ptr;

  n = GetFirstPage(HelpFile, &buf, NULL);
  while (n) {
    for (i = 0; i < n; i++) {
      for (ptr = text; *ptr; ptr++) {
        if (*ptr == '%') {
          j = strcspn(ptr, "hsdiouxX!");
          memcpy(format, ptr, j + 1);
          format[j + 1] = '\0';
          if (format[j] == '!') {
            count = helpdeco_getdw(HelpFile);
            while (count >= 8) {
              printf(" (%ld)", helpdeco_getdw(HelpFile));
              printf("%08lx", helpdeco_getdw(HelpFile));
              count -= 8;
            }
          } else if (format[j] == 'h') {
            format[j] = 's';
            printf(format, unhash(helpdeco_getdw(HelpFile)));
          } else if (format[j] == 's') {
            helpdeco_gets(scratch_buffer, sizeof(scratch_buffer), HelpFile);
            printf(format, scratch_buffer);
          } else if (strchr(format, 'l')) {
            printf(format, helpdeco_getdw(HelpFile));
          } else {
            printf(format, helpdeco_getw(HelpFile));
          }
          ptr += j;
        } else {
          putchar(*ptr);
        }
      }
    }
    n = GetNextPage(HelpFile, &buf);
  }
}

void SysDump(FILE *HelpFile) {
  SYSTEMHEADER SysHdr;
  SYSTEMRECORD *SysRec;
  struct tm *TimeRec;
  char *ptr;

  read_SYSTEMHEADER(&SysHdr, HelpFile);
  if (SysHdr.Minor == 15) {
    ptr = "HC30";
  } else if (SysHdr.Minor == 21) {
    ptr = "HC31/HCP";
  } else if (SysHdr.Minor == 27) {
    ptr = "WMVC/MVCC";
  } else if (SysHdr.Minor == 33) {
    if (ctx->mvp) {
      ptr = "MVC";
    } else {
      ptr = "HCRTF";
    }
  } else
    ptr = "Unknown";
  printf("%s Help Compiler used.\n", ptr);
  printf("System Flags & Compression Method=0x%04x\n", SysHdr.Flags);
  if (SysHdr.GenDate) {
    TimeRec = localtime(&SysHdr.GenDate);
    printf("Help File Generated: %s", asctime(TimeRec));
  }
  if (SysHdr.Minor < 16) {
    helpdeco_gets(ctx->title, 33, HelpFile);
    printf("TITLE=%s\n", ctx->title);
  } else
    for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
         SysRec = GetNextSystemRecord(SysRec)) {
      switch (SysRec->RecordType) {
      case 0x0001:
        printf("TITLE=%s\n", SysRec->Data);
        break;
      case 0x0002:
        printf("COPYRIGHT=%s\n", SysRec->Data);
        break;
      case 0x0003:
        printf("CONTENTS=0x%08lX\n", *(legacy_long *)SysRec->Data);
        break;
      case 0x0004:
        printf("[MACRO] %s\n", SysRec->Data);
        break;
      case 0x0005:
        puts("Icon in System record");
        break;
      case 0x0006:
        printf("[WINDOW] ");
        if (SysRec->DataSize == sizeof(SECWINDOW)) {
          PrintWindow(stdout, (SECWINDOW *)SysRec->Data);
        } else if (SysRec->DataSize == sizeof(MVBWINDOW)) {
          PrintMVBWindow(stdout, (MVBWINDOW *)SysRec->Data);
        } else {
          HexDumpMemory(SysRec->Data, SysRec->DataSize);
          error("[WINDOW] data size does not match");
        }
        break;
      case 0x0008:
        printf("CITATION=%s\n", SysRec->Data);
        break;
      case 0x0009:
        if (!ctx->mvp)
          printf("LCID=0x%X 0x%X 0x%X\n", *(int16_t *)(SysRec->Data + 8),
                 *(int16_t *)SysRec->Data, *(int16_t *)(SysRec->Data + 2));
        break;
      case 0x000A:
        if (!ctx->mvp)
          printf("CNT=%s\n", SysRec->Data);
        break;
      case 0x000B:
        //	    if(!mvp) printf("CHARSET=%d\n",*(unsigned char
        //*)(SysRec->Data+1));
        break;
      case 0x000C:
        if (ctx->mvp) {
          printf("[FTINDEX] dtype %s\n", SysRec->Data);
        } else {
          printf("DEFFONT=%s,%d,%d\n", SysRec->Data + 2,
                 *(unsigned char *)SysRec->Data,
                 *(unsigned char *)(SysRec->Data + 1));
        }
        break;
      case 0x000D:
        if (ctx->mvp)
          printf("[GROUPS] %s\n", SysRec->Data);
        break;
      case 0x000E:
        if (ctx->mvp) {
          printf("[KEYINDEX] keyword=%c, \"%s\"\n", SysRec->Data[1],
                 SysRec->Data + 30);
        } else {
          printf("INDEX_SEPARATORS=\"%s\"\n", SysRec->Data);
        }
        break;
      case 0x0012:
        if (SysRec->Data[0])
          printf("LANGUAGE=%s\n", SysRec->Data);
        break;
      case 0x0013:
        ptr = SysRec->Data + strlen(SysRec->Data) + 1;
        printf("[DLLMAPS] %s=%s,", SysRec->Data, ptr);
        ptr += strlen(ptr) + 1;
        printf("%s,", ptr);
        ptr += strlen(ptr) + 1;
        printf("%s,", ptr);
        ptr += strlen(ptr) + 1;
        printf("%s\n", ptr);
        break;
      default:
        fprintf(stderr, "Unknown record type: 0x%04X\n", SysRec->RecordType);
        HexDumpMemory(SysRec->Data, SysRec->DataSize);
      }
    }
}

/* dump the contents of |TOPIC for debugging */
void DumpTopic(FILE *HelpFile, legacy_long TopicPos) {
  TOPICLINK TopicLink;
  TOPICHEADER30 *TopicHdr30;
  TOPICHEADER *TopicHdr;
  char *ptr;
  char *str;
  char *cmd;
  legacy_long l;
  uint16_t x1, x2;
  unsigned char b;
  legacy_int cols, col;
  int16_t i;
  char *LinkData1;
  char *LinkData2;
  legacy_long TopicNum;
  legacy_long TopicOffset;

  if (!SearchFile(HelpFile, "|TOPIC", &ctx->topic_file_length))
    return;
  TopicOffset = 0;
  if (TopicPos < 12)
    TopicPos = 12;
  TopicNum = 16;
  while (TopicRead(HelpFile, TopicPos, &TopicLink, sizeof(TopicLink)) ==
         sizeof(TOPICLINK)) {
    puts("---------------------------------------------------------------------"
         "-------");
    printf(
        "TopicLink Type %02x: BlockSize=%08lx DataLen1=%08lx DataLen2=%08lx\n",
        TopicLink.RecordType, TopicLink.BlockSize, TopicLink.DataLen1,
        TopicLink.DataLen2);
    printf("TopicPos=%08lx TopicOffset=%08lx PrevBlock=%08lx NextBlock=%08lx\n",
           TopicPos, TopicOffset, TopicLink.PrevBlock, TopicLink.NextBlock);
    if (TopicLink.DataLen1 > sizeof(TOPICLINK)) {
      LinkData1 = helpdeco_malloc(TopicLink.DataLen1 - sizeof(TOPICLINK));
      if (TopicRead(HelpFile, 0, LinkData1,
                    TopicLink.DataLen1 - sizeof(TOPICLINK)) !=
          TopicLink.DataLen1 - sizeof(TOPICLINK))
        break;
    } else
      LinkData1 = NULL;
    if (TopicLink.DataLen1 <
        TopicLink.BlockSize) /* read LinkData2 using phrase replacement */
    {
      LinkData2 = helpdeco_malloc(TopicLink.DataLen2 + 1);
      if (topic_read_phrase(
              HelpFile, 0, LinkData2, TopicLink.BlockSize - TopicLink.DataLen1,
              TopicLink.DataLen2) != TopicLink.BlockSize - TopicLink.DataLen1)
        break;
    } else
      LinkData2 = NULL;
    if (LinkData1)
      HexDumpMemory(LinkData1, TopicLink.DataLen1 - sizeof(TOPICLINK));
    if (TopicLink.RecordType == TL_TOPICHDR) {
      if (ctx->before31) {
        TopicHdr30 = (TOPICHEADER30 *)LinkData1;
        puts("================================================================="
             "===========");
        printf("TopicHeader TopicNum=%ld BlockSize=%ld PrevTopicNum=%d "
               "NextTopicNum=%d\n",
               TopicNum, TopicHdr30->BlockSize, TopicHdr30->PrevTopicNum,
               TopicHdr30->NextTopicNum);
        TopicNum++;
      } else {
        TopicHdr = (TOPICHEADER *)LinkData1;
        puts("================================================================="
             "===========");
        printf("TopicHeader TopicNum=%ld BlockSize=%ld NextTopicOffset=%08lx\n",
               TopicHdr->TopicNum, TopicHdr->BlockSize, TopicHdr->NextTopic);
        printf("NonScroll=%08lx Scroll=%08lx BrowseBck=%08lx BrowseFor=%08lx\n",
               TopicHdr->NonScroll, TopicHdr->Scroll, TopicHdr->BrowseBck,
               TopicHdr->BrowseFor);
      }
    } else if (TopicLink.RecordType == TL_DISPLAY30 ||
               TopicLink.RecordType == TL_DISPLAY ||
               TopicLink.RecordType == TL_TABLE) {
      switch (TopicLink.RecordType) {
      case TL_DISPLAY30:
        fputs("Text ", stdout);
        break;
      case TL_DISPLAY:
        fputs("Display ", stdout);
        break;
      case TL_TABLE:
        fputs("Table ", stdout);
        break;
      }
      ptr = LinkData1;
      printf("expandedsize=%ld ", scanlong(&ptr));
      if (TopicLink.RecordType == TL_DISPLAY ||
          TopicLink.RecordType == TL_TABLE) {
        x1 = scanword(&ptr);
        TopicOffset += x1;
        printf("topicoffsetincrement=%u ", x1);
      }
      if (TopicLink.RecordType == TL_TABLE) {
        cols = (unsigned char)*ptr++;
        x1 = *ptr++;
        printf("columns=%d type=%d ", cols, x1);
        switch (x1) {
        case 0:
        case 2:
          printf("minwidth=%d ", *(int16_t *)ptr);
          ptr += 2;
        case 1:
        case 3:
          break;
        default:
          error("Unknown TableType %d", x1);
        }
        for (col = 0; col < cols; col++) {
          printf("width=%d gap=%d ", *(int16_t *)ptr, *(int16_t *)(ptr + 2));
          ptr += 4;
        }
      }
      putchar('\n');
      str = LinkData2;
      for (col = 0; (TopicLink.RecordType == TL_TABLE ? *(int16_t *)ptr != -1
                                                      : col == 0) &&
                    ptr < LinkData1 + TopicLink.DataLen1 - sizeof(TOPICLINK);
           col++) {
        if (TopicLink.RecordType == TL_TABLE) {
          printf("column=%d %04x %d ", *(int16_t *)ptr, *(uint16_t *)(ptr + 2),
                 *(unsigned char *)(ptr + 4) - 0x80);
          ptr += 5;
        }
        printf("%02x %d id=%04x ", *(unsigned char *)ptr,
               *(unsigned char *)(ptr + 1) - 0x80, *(uint16_t *)(ptr + 2));
        ptr += 4;
        x2 = *((uint16_t *)ptr);
        ptr = ptr + sizeof(uint16_t);
        if (x2 & 0x0001)
          printf("unknownbit01=%ld ",
                 scanlong(&ptr)); /* found in MVBs, purpose unknown, may mean
                                     that x2 is compressed long */
        if (x2 & 0x0002)
          printf("topspacing=%d ", scanint(&ptr));
        if (x2 & 0x0004)
          printf("bottomspacing=%d ", scanint(&ptr));
        if (x2 & 0x0008)
          printf("linespacing=%d ", scanint(&ptr));
        if (x2 & 0x0010)
          printf("leftindent=%d ", scanint(&ptr));
        if (x2 & 0x0020)
          printf("rightindent=%d ", scanint(&ptr));
        if (x2 & 0x0040)
          printf("firstlineindent=%d ", scanint(&ptr));
        if (x2 & 0x0080)
          fputs("unknownbit80set", stdout);
        if (x2 & 0x0100) {
          x1 = (unsigned char)*ptr++;
          if (x1 & 1)
            fputs("box ", stdout);
          if (x1 & 2)
            fputs("topborder ", stdout);
          if (x1 & 4)
            fputs("leftborder ", stdout);
          if (x1 & 8)
            fputs("bottomborder ", stdout);
          if (x1 & 0x10)
            fputs("rightborder ", stdout);
          if (x1 & 0x20)
            fputs("thickborder ", stdout);
          if (x1 & 0x40)
            fputs("doubleborder ", stdout);
          if (x1 & 0x80)
            fputs("unknownborder", stdout);
          printf("%04x ", *((uint16_t *)ptr));
          ptr = ptr + sizeof(uint16_t);
        }
        if (x2 & 0x0200) {
          i = scanint(&ptr);
          printf("tabs=%d ", i);
          while (i-- > 0) {
            x1 = scanword(&ptr);
            printf("stop=%d ", x1 & 0x3FFF);
            if (x1 & 0x4000) {
              x1 = scanword(&ptr);
              if (x1 == 1) {
                fputs("right ", stdout);
              } else if (x1 == 2) {
                fputs("center ", stdout);
              } else {
                error("unknowntabmodifier=%02x", x1);
              }
            }
          }
        }
        if (x2 & 0x0400)
          fputs("rightalign ", stdout);
        if (x2 & 0x0800)
          fputs("centeralign ", stdout);
        if (x2 & 0x1000)
          fputs("keeplinestogether ", stdout);
        if (x2 & 0x2000)
          fputs("unknownbit2000set ", stdout); /* found in PRINTMAN.HLP */
        if (x2 & 0x4000)
          fputs("unknownbit4000set ",
                stdout); /* found in PRINTMAN.HLP, RATTLER.HLP */
        if (x2 & 0x8000)
          fputs("unknownbit8000set", stdout);
        putchar('\n');
        while (1) {
          str = PrintString(str, strlen(str)) + 1;
          if ((unsigned char)ptr[0] == 0xFF) {
            ptr++;
            break;
          } else
            switch ((unsigned char)ptr[0]) {
            case 0x20:
              printf("{vfld%ld}", *(legacy_long *)(ptr + 1));
              ptr += 5;
              break;
            case 0x21:
              printf("{dtype%d}", *(int16_t *)(ptr + 1));
              ptr += 3;
              break;
            case 0x80: /* font change */
              printf("[font=%u]", *(int16_t *)(ptr + 1));
              ptr += 3;
              break;
            case 0x81:
              puts("[LF]");
              ptr++;
              break;
            case 0x82:
              puts("[CR]");
              ptr++;
              break;
            case 0x83:
              fputs("[TAB]", stdout);
              ptr++;
              break;
            case 0x86:
              ptr++;
              b = *ptr++;
              if (b == 0x05)
                cmd = "ewc";
              else
                cmd = "bmc";
              goto picture;
            case 0x87:
              ptr++;
              b = *ptr++;
              if (b == 0x05)
                cmd = "ewl";
              else
                cmd = "bml";
              goto picture;
            case 0x88:
              ptr++;
              b = *ptr++;
              if (b == 0x05)
                cmd = "ewr";
              else
                cmd = "bmr";
            picture:
              printf("[%s %02x ", cmd, b);
              l = scanlong(&ptr);
              switch (b) {
              case 0x22: /* HC31 */
                x1 = scanword(&ptr);
                printf("hotspots=%u ", x1);
              case 0x03: /* HC30 */
                switch (*(uint16_t *)ptr) {
                case 0:
                  fputs("baggage ", stdout);
                  break;
                case 1:
                  fputs("embedded ", stdout);
                  break;
                default:
                  error("Unknown %04x", ((uint16_t *)ptr)[0]);
                }
                printf("bm%u]", ((uint16_t *)ptr)[1]);
                break;
              case 0x05:
                printf("%04x ", ((uint16_t *)ptr)[0]);
                printf("%04x ", ((uint16_t *)ptr)[1]);
                printf("%04x ", ((uint16_t *)ptr)[2]);
                printf("%s]", ptr + 6);
                break;
              default:
                error("Unknown picture flag %02x", b);
              }
              ptr += l;
              break;
            case 0x89: /* end of hot spot */
              fputs("[U]", stdout);
              ptr++;
              break;
            case 0x8B: /* non-break-space */
              fputs("[~]", stdout);
              ptr++;
              break;
            case 0x8C: /* non-break-hyphen */
              fputs("[-]", stdout);
              ptr++;
              break;
            case 0xC8: /* macro */
              printf("[!%s]", ptr + 3);
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            case 0xCC: /* macro without font change */
              printf("[*!%s]", ptr + 3);
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            case 0xE0: /* Popup HC30 */
              printf("[^TOPIC%ld]", *(legacy_long *)(ptr + 1));
              ptr += 5;
              break;
            case 0xE1: /* Jump HC30 */
              printf("[TOPIC%ld]", *(legacy_long *)(ptr + 1));
              ptr += 5;
              break;
            case 0xE2: /* Popup HC31 */
              printf("[^%08lx]", *(legacy_long *)(ptr + 1));
              ptr += 5;
              break;
            case 0xE3: /* Jump HC31 */
              printf("[%08lx]", *(legacy_long *)(ptr + 1));
              ptr += 5;
              break;
            case 0xE6: /* Popup without font change */
              printf("[*^%08lx]", *(legacy_long *)(ptr + 1));
              ptr += 5;
              break;
            case 0xE7: /* Jump without font change */
              printf("[*%08lx]", *(legacy_long *)(ptr + 1));
              ptr += 5;
              break;
            case 0xEA: /* Popup into external file / secondary window */
              cmd = "^";
              goto jump;
            case 0xEB: /* Jump into external file / secondary window */
              cmd = "";
              goto jump;
            case 0xEE: /* Popup into external file / secondary window without
                          font change */
              cmd = "^*";
              goto jump;
            case 0xEF: /* Jump into external file / secondary window without
                          font change */
              cmd = "*";
            jump:
              switch (ptr[3]) {
              case 0:
                printf("[%s%08lx] ", cmd, *(legacy_long *)(ptr + 4));
                break;
              case 1: /* Popup into secondary window (silly) */
                printf("[%s%08lx>%d]", cmd, *(legacy_long *)(ptr + 4),
                       (unsigned char)ptr[8]);
                break;
              case 4:
                printf("[%s%08lx@%s] ", cmd, *(legacy_long *)(ptr + 4),
                       ptr + 8);
                break;
              case 6: /* Popup into external file / secondary window (silly) */
                printf("[%s%08lx>%s@%s] ", cmd, *(legacy_long *)(ptr + 4),
                       ptr + 8, strchr(ptr + 8, '\0') + 1);
                break;
              default:
                putchar('[');
                for (i = 0; i < *(int16_t *)(ptr + 1); i++)
                  printf("%02x", (unsigned char)ptr[i]);
                putchar(']');
              }
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            default:
              printf("[%02x]", (unsigned char)*ptr++);
            }
        }
        putchar('\n');
      }
    }
    if (LinkData2 && (TopicLink.RecordType != TL_DISPLAY30 &&
                      TopicLink.RecordType != TL_DISPLAY &&
                      TopicLink.RecordType != TL_TABLE)) {
      PrintString(LinkData2, TopicLink.DataLen2);
      putchar('\n');
    }
    if (LinkData1) {
      free(LinkData1);
      LinkData1 = NULL;
    }
    if (LinkData2) {
      free(LinkData2);
      LinkData2 = NULL;
    }
    if (ctx->before31) {
      TopicPos += TopicLink.NextBlock;
      if (TopicPos >= ctx->topic_file_length)
        break;
    } else {
      if (TopicLink.NextBlock <= 0)
        break;
      TopicOffset = NextTopicOffset(TopicOffset, TopicLink.NextBlock, TopicPos);
      TopicPos = TopicLink.NextBlock;
    }
  }
}

void AliasList(FILE *hpj) /* write [ALIAS] section to HPJ file */
{
  legacy_int i, n;
  BOOL headerwritten;

  headerwritten = FALSE;
  for (i = 0; i < ctx->context_rec.count; i = n) {
    for (n = i + 1; n < ctx->context_rec.count &&
                    ctx->context_rec.entry[i].TopicOffset ==
                        ctx->context_rec.entry[n].TopicOffset;
         n++) {
      if (!headerwritten) {
        fputs("Creating [ALIAS] section...\n", stderr);
        fputs("[ALIAS]\n", hpj);
        headerwritten = TRUE;
      }
      fprintf(hpj, "%s=", unhash(ctx->context_rec.entry[n].HashValue));
      fprintf(hpj, "%s\n", unhash(ctx->context_rec.entry[i].HashValue));
    }
  }
  if (headerwritten)
    putc('\n', hpj);
}

void CTXOMAPList(FILE *HelpFile,
                 FILE *hpj) /* write [MAP] section to HPJ file */
{
  CTXOMAPREC CTXORec;
  uint16_t n, i;
  char *ptr;

  if (SearchFile(HelpFile, "|CTXOMAP", NULL)) {
    n = helpdeco_getw(HelpFile);
    if (n) {
      fputs("Creating [MAP] section...\n", stderr);
      fputs("[MAP]\n", hpj);
      for (i = 0; i < n; i++) {
        read_CTXOMAPREC(&CTXORec, HelpFile);
        ptr = TopicName(CTXORec.TopicOffset);
        if (ptr) {
          fprintf(hpj, "%s %ld\n", ptr, CTXORec.MapID);
        } else {
          fprintf(hpj, "TOPIC%08lx %ld\n", CTXORec.TopicOffset, CTXORec.MapID);
        }
      }
      putc('\n', hpj);
    }
  }
}

void GuessFromKeywords(FILE *HelpFile) {
  legacy_long *keytopic;
  char kwdata[10];
  char kwbtree[10];
  legacy_int m, i, n, k, l, j, map;
  legacy_long FileLength, KWDataOffset, TopicOffset;
  BUFFER buf;

  fputs("Guessing...", stderr);
  for (k = 0; k < 2; k++)
    for (map = '0'; map <= 'z'; map++) {
      if (k) {
        if (!ctx->keyindex[map - '0'])
          continue;
        sprintf(kwdata, "|%cKWDATA", map);
        sprintf(kwbtree, "|%cKWBTREE", map);
      } else {
        if (!ctx->lists[map - '0'])
          continue;
        sprintf(kwdata, "|%cWDATA", map);
        sprintf(kwbtree, "|%cWBTREE", map);
      }
      if (SearchFile(HelpFile, kwdata, &FileLength)) {
        keytopic = helpdeco_malloc(FileLength);
        helpdeco_fread(keytopic, FileLength, HelpFile);
        if (SearchFile(HelpFile, kwbtree, NULL)) {
          for (n = GetFirstPage(HelpFile, &buf, NULL); n;
               n = GetNextPage(HelpFile, &buf)) {
            for (i = 0; i < n; i++) {
              helpdeco_gets(keyword, sizeof(keyword), HelpFile);
              m = helpdeco_getw(HelpFile);
              KWDataOffset = helpdeco_getdw(HelpFile);
              if (KWDataOffset / 4 + m > FileLength) {
                helpdeco_errorf("malformed keytopic file\n");
              }
              for (j = 0; j < m; j++) {
                TopicOffset = keytopic[KWDataOffset / 4 + j];
                Guess(keyword, TopicOffset);
                for (l = 0; l < ctx->alternative.count; l++) {
                  if (ctx->alternative.entry[l].OtherTopicOffset ==
                      TopicOffset) {
                    Guess(keyword, ctx->alternative.entry[l].TopicOffset);
                  }
                }
              }
            }
            fputc('.', stderr);
          }
          free(keytopic);
          keytopic = NULL;
        }
      }
    }
  if (ctx->guessed > 0) {
    helpdeco_logf("%ld context ids found\n", ctx->guessed);
  } else {
    helpdeco_warnf("no context ids found\n(you may use option /g to turn off guessing "
        "on this help file)\n");
  }
}

/* 1. extract topic names from topic macros, embedded pictures, and hotspot
 * macros */
/* 2. build browse sequence start list */
/* 3. extract embedded pictures */
void FirstPass(FILE *HelpFile) {
  SYSTEMRECORD *SysRec;
  TOPICLINK TopicLink;
  char *LinkData1;
  char *LinkData2;
  char *ptr;
  legacy_long l1, TopicNum, TopicPos, TopicOffset, BogusTopicOffset;
  legacy_int n, i, col, cols;
  BUFFER buf;
  TOPICHEADER30 *TopicHdr30;
  TOPICHEADER *TopicHdr;
  char filename[20];
  uint16_t x1, x2;
  int16_t y1;
  MFILE *f;

  if (ctx->opt_extractmacros) {
    for (SysRec = GetFirstSystemRecord(HelpFile); SysRec;
         SysRec = GetNextSystemRecord(SysRec)) {
      if (SysRec->RecordType == 0x0004) {
        strcpy(topic_title, "[CONFIG] section");
        CheckMacro(SysRec->Data);
      }
    }
    if (SearchFile(HelpFile, "|TopicId", NULL)) {
      for (n = GetFirstPage(HelpFile, &buf, NULL); n;
           n = GetNextPage(HelpFile, &buf)) {
        for (i = 0; i < n; i++) {
          helpdeco_getdw(HelpFile);
          helpdeco_gets(scratch_buffer, sizeof(scratch_buffer), HelpFile);
          AddTopic(scratch_buffer, FALSE);
        }
      }
      ctx->opt_guessing =
          FALSE; /* it's not necessary to guess context ids if you know them */
    }
  }
  ctx->browse.count = 0;
  ctx->browsenums = 1;
  if (!SearchFile(HelpFile, "|TOPIC", &ctx->topic_file_length))
    return;
  TopicOffset = 0;
  TopicPos = 12;
  TopicNum = 16;
  while (TopicRead(HelpFile, TopicPos, &TopicLink, sizeof(TopicLink)) ==
         sizeof(TOPICLINK)) {
    if (ctx->before31) {
      if (TopicPos + TopicLink.NextBlock >= ctx->topic_file_length)
        break;
    } else {
      if (TopicLink.NextBlock <= 0)
        break;
    }
    if (TopicLink.DataLen1 > sizeof(TOPICLINK)) {
      LinkData1 = helpdeco_malloc(TopicLink.DataLen1 - sizeof(TOPICLINK) + 1);
      if (TopicRead(HelpFile, 0L, LinkData1,
                    TopicLink.DataLen1 - sizeof(TOPICLINK)) !=
          TopicLink.DataLen1 - sizeof(TOPICLINK))
        break;
    } else
      LinkData1 = NULL;
    if (TopicLink.DataLen1 <
        TopicLink.BlockSize) /* read LinkData2 using phrase replacement */
    {
      LinkData2 = helpdeco_malloc(TopicLink.DataLen2 + 1);
      if (topic_read_phrase(
              HelpFile, 0L, LinkData2, TopicLink.BlockSize - TopicLink.DataLen1,
              TopicLink.DataLen2) != TopicLink.BlockSize - TopicLink.DataLen1)
        break;
    } else
      LinkData2 = NULL;
    if (TopicLink.RecordType == TL_TOPICHDR) /* display a topic header record */
    {
      helpdeco_logf("\nTopic %ld...", TopicNum - 15);
      if (ctx->before31) {
        TopicHdr30 = (TOPICHEADER30 *)LinkData1;
        if (ctx->opt_resolvebrowse) {
          if ((TopicHdr30->NextTopicNum > TopicNum &&
               TopicHdr30->PrevTopicNum > TopicNum) ||
              (TopicHdr30->NextTopicNum == -1 &&
               TopicHdr30->PrevTopicNum > TopicNum) ||
              (TopicHdr30->NextTopicNum > TopicNum &&
               TopicHdr30->PrevTopicNum == -1)) {
            AddBrowse(TopicNum, TopicHdr30->NextTopicNum,
                      TopicHdr30->PrevTopicNum);
          } else if (TopicHdr30->NextTopicNum != -1 &&
                     TopicHdr30->NextTopicNum < TopicNum &&
                     TopicHdr30->PrevTopicNum != -1 &&
                     TopicHdr30->PrevTopicNum < TopicNum) {
            MergeBrowse(TopicNum, TopicNum, TopicHdr30->NextTopicNum,
                        TopicHdr30->PrevTopicNum);
          } else if (TopicHdr30->NextTopicNum != -1 &&
                     TopicHdr30->NextTopicNum < TopicNum &&
                     (TopicHdr30->PrevTopicNum == -1 ||
                      TopicHdr30->PrevTopicNum > TopicNum)) {
            BackLinkBrowse(TopicNum, TopicNum, TopicHdr30->NextTopicNum,
                           TopicHdr30->PrevTopicNum);
          } else if (TopicHdr30->PrevTopicNum != -1 &&
                     TopicHdr30->PrevTopicNum < TopicNum &&
                     (TopicHdr30->NextTopicNum == -1 ||
                      TopicHdr30->NextTopicNum > TopicNum)) {
            LinkBrowse(TopicNum, TopicNum, TopicHdr30->NextTopicNum,
                       TopicHdr30->PrevTopicNum);
          }
        }
      } else {
        BogusTopicOffset =
            NextTopicOffset(TopicOffset, TopicLink.NextBlock, TopicPos);
        if (BogusTopicOffset != TopicOffset) {
          ctx->alternative.entry =
              helpdeco_realloc(ctx->alternative.entry,
              (ctx->alternative.count + 1) * sizeof(ALTERNATIVE));
          ctx->alternative.entry[ctx->alternative.count].TopicOffset =
              TopicOffset;
          ctx->alternative.entry[ctx->alternative.count].OtherTopicOffset =
              BogusTopicOffset;
          ctx->alternative.count++;
        }
        TopicHdr = (TOPICHEADER *)LinkData1;
        if (ctx->opt_resolvebrowse) {
          if ((TopicHdr->BrowseFor > TopicOffset &&
               TopicHdr->BrowseBck > TopicOffset) ||
              (TopicHdr->BrowseFor == -1 &&
               TopicHdr->BrowseBck > TopicOffset) ||
              (TopicHdr->BrowseFor > TopicOffset &&
               TopicHdr->BrowseBck == -1)) {
            AddBrowse(TopicOffset, TopicHdr->BrowseFor, TopicHdr->BrowseBck);
          } else if (TopicHdr->BrowseFor != -1 &&
                     TopicHdr->BrowseFor < TopicOffset &&
                     TopicHdr->BrowseBck != -1 &&
                     TopicHdr->BrowseBck < TopicOffset) {
            MergeBrowse(TopicOffset, BogusTopicOffset, TopicHdr->BrowseFor,
                        TopicHdr->BrowseBck);
          } else if (TopicHdr->BrowseFor != -1 &&
                     TopicHdr->BrowseFor < TopicOffset &&
                     (TopicHdr->BrowseBck == -1 ||
                      TopicHdr->BrowseBck > TopicOffset)) {
            BackLinkBrowse(TopicOffset, BogusTopicOffset, TopicHdr->BrowseFor,
                           TopicHdr->BrowseBck);
          } else if (TopicHdr->BrowseBck != -1 &&
                     TopicHdr->BrowseBck < TopicOffset &&
                     (TopicHdr->BrowseFor == -1 ||
                      TopicHdr->BrowseFor > TopicOffset)) {
            LinkBrowse(TopicOffset, BogusTopicOffset, TopicHdr->BrowseFor,
                       TopicHdr->BrowseBck);
          }
        }
        if (ctx->opt_extractmacros) {
          if (TopicLink.DataLen2 && *LinkData2) {
            strlcpy(topic_title, LinkData2, sizeof(topic_title));
            if (ctx->opt_guessing) {
              Guess(LinkData2, TopicOffset);
              if (BogusTopicOffset != TopicOffset) {
                Guess(LinkData2, BogusTopicOffset);
              }
            }
          } else {
            strcpy(topic_title, "<< untitled topic >>");
          }
          if (TopicLink.DataLen2) {
            for (i = strlen(LinkData2) + 1; i < TopicLink.DataLen2;
                 i += strlen(LinkData2 + i) + 1) {
              CheckMacro(LinkData2 + i);
            }
          }
        }
      }
      TopicNum++;
    } else if (TopicLink.RecordType == TL_DISPLAY30 ||
               TopicLink.RecordType == TL_DISPLAY ||
               TopicLink.RecordType == TL_TABLE) {
      ptr = LinkData1;
      scanlong(&ptr);
      if (TopicLink.RecordType == TL_DISPLAY ||
          TopicLink.RecordType == TL_TABLE) {
        TopicOffset += scanword(&ptr);
      }
      if (TopicLink.RecordType == TL_TABLE) {
        cols = (unsigned char)*ptr++;
        x1 = (unsigned char)*ptr++;
        switch (x1) {
        case 0: /* found in CALC.HLP and TERMINAL.HLP */
        case 2:
          ptr += 2;
        case 1:
        case 3:
          break;
        default:
          error("Unknown TableType %d", x1);
        }
        ptr += 4 * cols;
      }
      for (col = 0; (TopicLink.RecordType == TL_TABLE ? *(int16_t *)ptr != -1
                                                      : col == 0) &&
                    ptr < LinkData1 + TopicLink.DataLen1 - sizeof(TOPICLINK);
           col++) {
        if (TopicLink.RecordType == TL_TABLE)
          ptr += 5;
        ptr += 4;
        x2 = *(uint16_t *)ptr;
        ptr += 2;
        if (x2 & 0x0001)
          scanlong(&ptr); /* found in MVBs, purpose */
        /* unknown, may mean that x2 is really compressed long */
        if (x2 & 0x0002)
          scanint(&ptr);
        if (x2 & 0x0004)
          scanint(&ptr);
        if (x2 & 0x0008)
          scanint(&ptr);
        if (x2 & 0x0010)
          scanint(&ptr);
        if (x2 & 0x0020)
          scanint(&ptr);
        if (x2 & 0x0040)
          scanint(&ptr);
        if (x2 & 0x0100)
          ptr += 3;
        if (x2 & 0x0200) {
          y1 = scanint(&ptr);
          while (y1-- > 0)
            if (scanword(&ptr) & 0x4000)
              scanword(&ptr);
        }
        while (ptr < LinkData1 + TopicLink.DataLen1 - sizeof(TOPICLINK)) {
          if ((unsigned char)ptr[0] == 0xFF) {
            ptr++;
            break;
          } else
            switch ((unsigned char)ptr[0]) {
            case 0x21: /* dtype (MVB) */
            case 0x80: /* font change */
              ptr += 3;
              break;
            case 0x81:
            case 0x82:
            case 0x83:
            case 0x89: /* end of hotspot */
            case 0x8B: /* non-break-space */
            case 0x8C: /* non-break-hyphen */
              ptr++;
              break;
            case 0x86:
            case 0x87:
            case 0x88:
              ptr++;
              x1 = *ptr++;
              l1 = scanlong(&ptr);
              switch (x1) {
              case 0x22: /* HC31 */
                x1 = scanword(&ptr);
                /* fall thru */
              case 0x03: /* HC30 */
                switch (((uint16_t *)ptr)[0]) {
                case 1:
                  for (x2 = 1; x2 < ctx->extension.count; x2++)
                    if (!ctx->extension.entry[x2])
                      break;
                  if (x2 >= ctx->extension.count) {
                    ctx->extension.entry = helpdeco_realloc(ctx->extension.entry,
                                                      (x2 + 1) * sizeof(char));
                    while (ctx->extension.count <= x2)
                      ctx->extension.entry[ctx->extension.count++] = 0;
                  }
                  sprintf(filename, "bm%u", x2);
                  f = mfile_create_map(ptr + 2, l1 - 2);
                  x1 = ExtractBitmap(filename, f);
                  mfile_close(f);
                  ctx->extension.entry[x2] = x1 | 0x10;
                  break;
                }
                break;
              case 0x05:
                if (ptr[6] == '!' && strchr(ptr + 7, ',')) {
                  CheckMacro(strchr(ptr + 7, ',') + 1);
                }
                break;
              }
              ptr += l1;
              break;
            case 0xC8: /* macro */
            case 0xCC: /* macro without font change */
              CheckMacro(ptr + 3);
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            case 0x20: /* vfld (MVC) */
            case 0xE0: /* popup jump HC30 */
            case 0xE1: /* topic jump HC30 */
            case 0xE2: /* popup jump HC31 */
            case 0xE3: /* topic jump HC31 */
            case 0xE6: /* popup jump without font change */
            case 0xE7: /* topic jump without font change */
              ptr += 5;
              break;
            case 0xEA: /* popup jump into external file */
            case 0xEB: /* topic jump into external file / secondary window */
            case 0xEE: /* popup jump into external file without font change */
            case 0xEF: /* topic jump into external file / secondary window
                          without font change */
              switch ((unsigned char)ptr[3]) {
              case 0:
              case 1:
                break;
              case 4:
                StoreReference(ptr + 8, TOPIC, NULL, *(legacy_long *)(ptr + 4));
                break;
              case 6:
                StoreReference(strchr(ptr + 8, '\0') + 1, TOPIC, NULL,
                               *(legacy_long *)(ptr + 4));
                break;
              default:
                error("Unknown modifier %02x in tag %02x",
                      (unsigned char)ptr[3], (unsigned char)ptr[0]);
              }
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            default:
              error("Unknown %02x", *(unsigned char *)ptr);
              ptr++;
            }
        }
      }
    }
    if (LinkData1) {
      free(LinkData1);
      LinkData1 = NULL;
    }
    if (LinkData2) {
      free(LinkData2);
      LinkData2 = NULL;
    }
    if (ctx->before31) {
      TopicPos += TopicLink.NextBlock;
    } else {
      TopicOffset = NextTopicOffset(TopicOffset, TopicLink.NextBlock, TopicPos);
      TopicPos = TopicLink.NextBlock;
    }
  }
}

int CTXOMAPRecCmp(const void *a, const void *b) {
  if (((CTXOMAPREC *)a)->TopicOffset < ((CTXOMAPREC *)b)->TopicOffset)
    return -1;
  if (((CTXOMAPREC *)a)->TopicOffset > ((CTXOMAPREC *)b)->TopicOffset)
    return 1;
  return 0;
}

void ContextList(FILE *HelpFile) {
  uint16_t maprecs, m;
  legacy_int j, window, len;
  BOOL morekeywords;
  CTXOMAPREC *map = NULL;
  char filename[255];
  TOPICLINK TopicLink;
  char *LinkData1;
  char *LinkData2;
  char *ptr;
  legacy_long TopicPos, TopicNum, TopicOffset;

  if (SearchFile(HelpFile, "|CTXOMAP", NULL)) {
    maprecs = helpdeco_getw(HelpFile);
    if (maprecs) {
      map = helpdeco_malloc((legacy_long)maprecs * sizeof(CTXOMAPREC));
      read_CTXOMAPRECs(map, maprecs, HelpFile);
      qsort(map, maprecs, sizeof(CTXOMAPREC), CTXOMAPRecCmp);
    }
  } else {
    maprecs = 0;
  }
  strcpy(filename, ctx->name);
  strcat(filename, ctx->ext);
  if (!SearchFile(HelpFile, "|TOPIC", &ctx->topic_file_length))
    return;
  TopicOffset = 0;
  TopicPos = 12;
  TopicNum = 1;
  j = 0;
  len = 80;
  window = -1;
  morekeywords = TRUE;
  m = 0;
  while (TopicRead(HelpFile, TopicPos, &TopicLink, sizeof(TopicLink)) ==
         sizeof(TOPICLINK)) {
    if (TopicLink.DataLen1 > sizeof(TOPICLINK)) {
      LinkData1 = helpdeco_malloc(TopicLink.DataLen1 - sizeof(TOPICLINK) + 1);
      if (TopicRead(HelpFile, 0, LinkData1,
                    TopicLink.DataLen1 - sizeof(TOPICLINK)) !=
          TopicLink.DataLen1 - sizeof(TOPICLINK))
        break;
    } else
      LinkData1 = NULL;
    if (TopicLink.DataLen1 <
        TopicLink.BlockSize) /* read LinkData2 using phrase replacement */
    {
      LinkData2 = helpdeco_malloc(TopicLink.DataLen2 + 1);
      if (topic_read_phrase(
              HelpFile, 0, LinkData2, TopicLink.BlockSize - TopicLink.DataLen1,
              TopicLink.DataLen2) != TopicLink.BlockSize - TopicLink.DataLen1)
        break;
    } else
      LinkData2 = NULL;
    if (TopicLink.NextBlock <= 0 ||
        TopicLink.RecordType == TL_TOPICHDR) /* display a topic header record */
    {
      if (TopicNum > 1)
        putchar('\n');
      if (ctx->before31)
        TopicOffset = TopicPos;
      while (m < maprecs && map[m].TopicOffset < TopicOffset) {
        printf("  WinHelp(wnd,\"%s\",HELP_CONTEXT,%lu)\n", filename,
               map[m].MapID);
        m++;
      }
      if (!ctx->before31) {
        while (j < ctx->context_rec.count &&
               ctx->context_rec.entry[j].TopicOffset < TopicOffset) {
          if (len == 80) {
            fputs("  Jump", stdout);
          } else {
            fputs("  Popup", stdout);
          }
          printf("Id(`%s", filename);
          if (window != -1)
            printf(">%s", GetWindowName(window));
          printf("',`%s')\n", unhash(ctx->context_rec.entry[j].HashValue));
          j++;
        }
        if (morekeywords) {
          if (ctx->NextKeywordRec >= ctx->keyword_rec.count) {
            if (ctx->NextKeywordOffset < 0x7FFFFFFFL) {
              CollectKeywords(HelpFile);
            } else {
              morekeywords = FALSE;
            }
          }
          while (ctx->NextKeywordRec < ctx->keyword_rec.count &&
                 ctx->keyword_rec.entry[ctx->NextKeywordRec].TopicOffset <
                     TopicOffset) {
            if (ctx->keyword_rec.entry[ctx->NextKeywordRec].Footnote == 'K') {
              printf("  JumpKeyword(`%s',`%s')\n", filename,
                     ctx->keyword_rec.entry[ctx->NextKeywordRec].Keyword);
            } else if (ctx->keyword_rec.entry[ctx->NextKeywordRec].Footnote ==
                       'A') {
              printf("  ALink(`%s@%s')\n",
                     ctx->keyword_rec.entry[ctx->NextKeywordRec].Keyword,
                     filename);
            }
            ctx->NextKeywordRec++;
          }
        }
        window = ListWindows(HelpFile, TopicOffset);
      }
      if (TopicLink.NextBlock <= 0)
        break;
      if (LinkData2 && *LinkData2) {
        printf("Topic %ld: %s", TopicNum, LinkData2);
        len = 80;
      } else {
        len = printf("Topic %ld: untitled: ", TopicNum);
      }
      TopicNum++;
    } else if (LinkData2 && len < 79) {
      for (ptr = LinkData2; len < 79 && ptr < LinkData2 + TopicLink.DataLen2;
           ptr++)
        if (*ptr) {
          putchar(*ptr);
          len++;
        }
    }
    if (TopicLink.RecordType == TL_DISPLAY ||
        TopicLink.RecordType == TL_TABLE) {
      ptr = LinkData1;
      scanlong(&ptr);
      TopicOffset += scanword(&ptr);
    }
    if (LinkData1) {
      free(LinkData1);
      LinkData1 = NULL;
    }
    if (LinkData2) {
      free(LinkData2);
      LinkData2 = NULL;
    }
    if (ctx->before31) {
      if (TopicPos + TopicLink.NextBlock >= ctx->topic_file_length)
        break;
      TopicPos += TopicLink.NextBlock;
    } else {
      if (TopicLink.NextBlock <= 0)
        break;
      TopicOffset = NextTopicOffset(TopicOffset, TopicLink.NextBlock, TopicPos);
      TopicPos = TopicLink.NextBlock;
    }
  }
}

#pragma mark - Phrases
BOOL load_phrases_from_index(FILE *HelpFile, legacy_long FileLength) {
  PHRINDEXHDR PhrIndexHdr;
  unsigned_legacy_int n;
  legacy_long l, offset;
  legacy_long SavePos;

  read_PHRINDEXHDR(&PhrIndexHdr, HelpFile);
  SavePos = ftell(HelpFile);
  if (SearchFile(HelpFile, "|PhrImage", &FileLength)) {
    if (FileLength != PhrIndexHdr.phrimagecompressedsize) {
      helpdeco_warnf("PhrImage FileSize %ld, in PhrIndex.FileHdr %ld\n",
                     PhrIndexHdr.phrimagecompressedsize, FileLength);
    }
    ctx->phrase.count = (unsigned_legacy_int)PhrIndexHdr.entries;
    ctx->phrase.offset =
        helpdeco_malloc(sizeof(unsigned_legacy_int) * (ctx->phrase.count + 1));
    ctx->phrases = helpdeco_malloc(PhrIndexHdr.phrimagesize);
    if (PhrIndexHdr.phrimagesize == PhrIndexHdr.phrimagecompressedsize) {
      helpdeco_fread(ctx->phrases, PhrIndexHdr.phrimagesize, HelpFile);
    } else {
      mfile_decompress_into_buffer(2, HelpFile, FileLength, ctx->phrases,
                                   PhrIndexHdr.phrimagesize);
    }
    fseek(HelpFile, SavePos, SEEK_SET);

    offset = 0;
    ctx->phrase.offset[0] = offset;
    for (l = 0; l < PhrIndexHdr.entries; l++) {
      for (n = 1; GetBit(HelpFile); n += 1 << PhrIndexHdr.bits)
        ;
      if (GetBit(HelpFile))
        n += 1;
      if (PhrIndexHdr.bits > 1)
        if (GetBit(HelpFile))
          n += 2;
      if (PhrIndexHdr.bits > 2)
        if (GetBit(HelpFile))
          n += 4;
      if (PhrIndexHdr.bits > 3)
        if (GetBit(HelpFile))
          n += 8;
      if (PhrIndexHdr.bits > 4)
        if (GetBit(HelpFile))
          n += 16;
      offset += n;
      ctx->phrase.offset[(legacy_int)l + 1] = offset;
    }
  }
  ctx->phrase_style = Hall;
  return TRUE;
}

BOOL load_phrases_from_phrases(FILE *HelpFile, legacy_long FileLength) {
  char junk[30];
  BOOL newphrases;
  unsigned_legacy_int n;
  legacy_long l, offset;

  ctx->phrase.count = helpdeco_getw(HelpFile);
  newphrases = ctx->phrase.count == 0x0800; /* VC4.0: MSDEV\HELP\MSDEV40.MVB */
  if (newphrases)
    ctx->phrase.count = helpdeco_getw(HelpFile);
  if (helpdeco_getw(HelpFile) != 0x0100) {
    error("Unknown |Phrases file structure");
    return FALSE;
  }
  if (ctx->phrase.count) {
    if (ctx->before31) {
      offset = (ctx->phrase.count + 1) * sizeof(int16_t);
      FileLength -= (ctx->phrase.count + 1) * sizeof(int16_t) + 4;
      l = FileLength;
    } else {
      l = helpdeco_getdw(HelpFile);
      if (newphrases) {
        helpdeco_fread(&junk, sizeof(junk), HelpFile);
        offset = (ctx->phrase.count + 1) * sizeof(int16_t);
        FileLength -=
            (ctx->phrase.count + 1) * sizeof(int16_t) + sizeof(junk) + 10;
      } else {
        offset = (ctx->phrase.count + 1) * sizeof(int16_t);
        FileLength -= (ctx->phrase.count + 1) * sizeof(int16_t) + 8;
      }
    }
    ctx->phrase.offset =
        helpdeco_malloc(sizeof(unsigned_legacy_int) * (ctx->phrase.count + 1));
    for (n = 0; n <= ctx->phrase.count; n++)
      ctx->phrase.offset[n] = helpdeco_getw(HelpFile) - offset;
    ctx->phrases = helpdeco_malloc(l);
    mfile_decompress_into_buffer((ctx->before31 ? 0 : 2), HelpFile, FileLength,
                                 ctx->phrases, l);
  }
  return TRUE;
}

/* load phrases for decompression from old Phrases file or new PhrIndex,
// PhrImage files of HCRTF */
BOOL helpdeco_load_phrases(FILE *HelpFile) {
  legacy_long FileLength;

  if (SearchFile(HelpFile, "|PhrIndex", &FileLength)) {
    load_phrases_from_index(HelpFile, FileLength);
    helpdeco_logf("%u phrases loaded\n", ctx->phrase.count);
  } else if (SearchFile(HelpFile, "|Phrases", &FileLength)) {
    load_phrases_from_phrases(HelpFile, FileLength);
    fprintf(stderr, "%u phrases loaded\n", ctx->phrase.count);

    ctx->phrase_style = Old;
  } else {
    return FALSE;
  }

  return TRUE;
}

/* write phrase PhraseNum to out and returns advanced out
// or to f it out = NULL or uses PrintString if f = NULL, returns NULL then */
char *phrase_print(unsigned_legacy_int PhraseNum, char *out, FILE *f) {
  char *ptr;
  unsigned_legacy_int len;

  if (PhraseNum >= ctx->phrase.count) {
    error("Phrase %u does not exist", PhraseNum);
    return out;
  }
  ptr = ctx->phrases + ctx->phrase.offset[PhraseNum];
  len = ctx->phrase.offset[PhraseNum + 1] - ctx->phrase.offset[PhraseNum];
  if (out) {
    memcpy(out, ptr, len);
    return out + len;
  }
  if (f) {
    fwrite(ptr, len, 1, f);
  } else {
    PrintString(ptr, len);
  }
  return NULL;
}

/* writeout .PH file from already loaded phrases */
void phrase_dump(char *FileName) {
  FILE *f;
  unsigned_legacy_int n;

  if (ctx->phrase.count) {
    f = helpdeco_fopen(FileName, "wt");
    if (f) {
      for (n = 0; n < ctx->phrase.count; n++) {
        phrase_print(n, NULL, f);
        putc('\n', f);
      }
      helpdeco_fclose(f);
    }
  }
}

/* Hall or oldstyle Phrase replacement of str into out */
char *phrase_expand(unsigned char *str, legacy_long len, char *out) {
  legacy_int CurChar;

  if (ctx->phrase_style == Hall) {
    while (len) {
      CurChar = *str++;
      len--;
      if ((CurChar & 1) == 0) /* phrases 0..127 */
      {
        out = phrase_print(CurChar / 2, out, NULL);
      } else if ((CurChar & 3) == 1) /* phrases 128..16511 */
      {
        CurChar = 128 + (CurChar / 4) * 256 + *str++;
        len--;
        out = phrase_print(CurChar, out, NULL);
      } else if ((CurChar & 7) == 3) /* copy next n characters */
      {
        while (CurChar > 0) {
          *out++ = *str++;
          len--;
          CurChar -= 8;
        }
      } else if ((CurChar & 0x0F) == 0x07) {
        while (CurChar > 0) {
          *out++ = ' ';
          CurChar -= 16;
        }
      } else /* if((CurChar&0x0F)==0x0F) */
      {
        while (CurChar > 0) {
          *out++ = '\0';
          CurChar -= 16;
        }
      }
    }
  } else {
    while (len) {
      CurChar = *str++;
      len--;
      if (CurChar > 0 && CurChar < 16) /* phrase 0..1919 */
      {
        CurChar = 256 * (CurChar - 1) + *str++;
        len--;
        out = phrase_print(CurChar / 2, out, NULL);
        if (CurChar & 1)
          *out++ = ' ';
      } else {
        *out++ = CurChar;
      }
    }
  }
  return out;
}

#pragma mark -
#pragma mark HTML Output
BOOL html_dump(FILE *HelpFile, FILE *__html_output) {
#if 0
#include <stdio.h>
char debug_buffer[4096];
FILE *dev_null = fopen("/dev/null", "w");
#define __rtf_output dev_null
#else
#define __rtf_output __html_output
#endif

#define html_puts(str) fputs(str, __html_output)
#define html_putc(c) fputc(c, __html_output)
#define html_printf(format, ...) fprintf(__html_output, format, ##__VA_ARGS__);
#define rtf_printf(format, ...)                                                \
  fprintf(__rtf_output,                                                        \
          "<span style=\"color: red !important;\">" format "</span>",          \
          ##__VA_ARGS__)
#define rtf_puts(str) rtf_printf("%s", str)
#define rtf_putc(c) rtf_printf("%c", c)

  TOPICLINK TopicLink;
  char *LinkData1; /* Data associated with this link */
  char *LinkData2; /* Second set of data */
  legacy_int fontset;
  legacy_int NextContextRec;
  char *hotspot;
  BOOL firsttopic = TRUE;
  BOOL ul, uldb;
  legacy_int nextbitmap, TopicInRTF, NumberOfRTF;
  legacy_long TopicNum, TopicOffset, TopicPos;
  legacy_int col, cols, lastcol;
  int16_t *iptr;
  uint16_t x1, x2, x3;
  int16_t y1;
  legacy_long l1 = 0;
  char *ptr;
  char *cmd;
  char *str;
  legacy_long ActualTopicOffset = 0, MaxTopicOffset = 0;

  if (!SearchFile(HelpFile, "|TOPIC", &ctx->topic_file_length)) {
    helpdeco_warnf("No topic file found\n");
    return FALSE;
  }

  fontset = -1;
  nextbitmap = 1;
  if (ctx->browse.entry)
    free(ctx->browse.entry);
  ctx->browse.entry = NULL;
  ctx->browse.count = 0;
  NextContextRec = 0;
  ul = uldb = FALSE;
  hotspot = NULL;
  TopicOffset = 0;
  TopicPos = 12;
  TopicNum = 16;
  TopicInRTF = 0;
  NumberOfRTF = 1;
  while (TopicRead(HelpFile, TopicPos, &TopicLink, sizeof(TopicLink)) ==
         sizeof(TOPICLINK)) {
    if (ctx->before31) {
      if (TopicPos + TopicLink.NextBlock >= ctx->topic_file_length)
        break;
    } else {
      if (TopicLink.NextBlock <= 0)
        break;
    }
    if (TopicLink.DataLen1 > sizeof(TOPICLINK)) {
      LinkData1 = helpdeco_malloc(TopicLink.DataLen1 - sizeof(TOPICLINK) + 1);
      if (TopicRead(HelpFile, 0, LinkData1,
                    TopicLink.DataLen1 - sizeof(TOPICLINK)) !=
          TopicLink.DataLen1 - sizeof(TOPICLINK))
        break;
    } else
      LinkData1 = NULL;
    if (TopicLink.DataLen1 <
        TopicLink.BlockSize) /* read LinkData2 using phrase replacement */
    {
      LinkData2 = helpdeco_malloc(TopicLink.DataLen2 + 1);
      if (topic_read_phrase(
              HelpFile, 0, LinkData2, TopicLink.BlockSize - TopicLink.DataLen1,
              TopicLink.DataLen2) != TopicLink.BlockSize - TopicLink.DataLen1)
        break;
    } else
      LinkData2 = NULL;

    if (LinkData1 &&
        TopicLink.RecordType == TL_TOPICHDR) /* display a Topic Header record */
    {
      if (!firsttopic) {
        // fputs("\\page\n",rtf); /* RTF: Required page break. */
        html_puts("</span></p>");
        html_puts("</helpdeco-topic>\n");
      }
      if (TopicLink.NextBlock != -1) {
        html_puts("\n\n<helpdeco-topic><p><span>\n");
      } else
        firsttopic = FALSE;
      firsttopic = FALSE;
      helpdeco_logf("\nTopic %ld...\n", TopicNum - 15);
      TopicNum++;
    } else if (LinkData1 && LinkData2 &&
               (TopicLink.RecordType == TL_DISPLAY30 ||
                TopicLink.RecordType == TL_DISPLAY ||
                TopicLink.RecordType == TL_TABLE)) {
      if (ctx->annotation_file)
        Annotate(TopicPos, __rtf_output);
      ptr = LinkData1;
      scanlong(&ptr);
      if (TopicLink.RecordType == TL_DISPLAY ||
          TopicLink.RecordType == TL_TABLE) {
        x1 = scanword(&ptr);
        ActualTopicOffset = TopicOffset;
        MaxTopicOffset = ActualTopicOffset + x1;
        TopicOffset += x1;
      }
      if (TopicLink.RecordType == TL_TABLE) {
        rtf_puts("\\trowd"); /* RTF: Sets table row defaults. */
        cols = (unsigned char)*ptr++;
        x1 = (unsigned char)*ptr++;
        switch (x1) {
        case 0:
        case 2:
          l1 = *(int16_t *)ptr; /* min table width */
          ptr += 2;
          rtf_puts("\\trqc"); /* RTF: Centers a table row with respect to its
                                 containing column. */
          break;
        case 1:
        case 3:
          l1 = 32767;
          break;
        }
        iptr = (int16_t *)ptr;
        if (cols > 1) {
          x1 = iptr[0] + iptr[1] + iptr[3] / 2;
          rtf_printf(
              "\\trgaph%ld\\trleft%ld \\cellx%ld\\cellx%ld",
              ((iptr[3] * ctx->scaling - ctx->rounderr) * l1) / 32767,
              (((iptr[1] - iptr[3]) * ctx->scaling - ctx->rounderr) * l1 -
               32767) /
                  32767,
              ((x1 * ctx->scaling - ctx->rounderr) * l1) / 32767,
              (((x1 + iptr[2] + iptr[3]) * ctx->scaling - ctx->rounderr) * l1) /
                  32767);
          x1 += iptr[2] + iptr[3];
          for (col = 2; col < cols; col++) {
            x1 += iptr[2 * col] + iptr[2 * col + 1];
            rtf_printf("\\cellx%ld",
                       ((x1 * ctx->scaling - ctx->rounderr) * l1) / 32767);
          }
        } else {
          rtf_printf("\\trleft%ld \\cellx%ld ",
                     ((iptr[1] * ctx->scaling - ctx->rounderr) * l1 - 32767) /
                         32767,
                     ((iptr[0] * ctx->scaling - ctx->rounderr) * l1) / 32767);
        }
        ptr = (char *)(iptr + 2 * cols);
      }
      lastcol = -1;
      str = LinkData2;
      for (col = 0; (TopicLink.RecordType == TL_TABLE ? *(int16_t *)ptr != -1
                                                      : col == 0) &&
                    ptr < LinkData1 + TopicLink.DataLen1 - sizeof(TOPICLINK);
           col++) {
        // fputs("\\pard",rtf); /* RTF: Resets to default paragraph properties.
        // */ if(TopicPos<nonscroll) fputs("\\keepn",rtf); /* RTF: Keep
        // paragraph with the next paragraph. */
        if (TopicLink.RecordType == TL_TABLE) {
          rtf_puts("\\intbl"); /* RTF: Paragraph is part of a table. */
          lastcol = *(int16_t *)ptr;
          ptr += 5;
        }
        ptr += 4;
        x2 = *(uint16_t *)ptr;
        ptr += 2;
        if (x2 & 0x1000)
          rtf_puts("\\keep"); /* RTF: Keep paragraph intact. */
        if (x2 & 0x0400)
          rtf_puts("\\qr"); /* RTF: Right-aligned. */
        if (x2 & 0x0800)
          rtf_puts("\\qc"); /* RTF: Centered. */
        if (x2 & 0x0001)
          scanlong(&ptr);
        if (x2 & 0x0002)
          rtf_printf("\\sb%ld",
                     scanint(&ptr)); /* RTF: Space before (the default is 0). */
        if (x2 & 0x0004)
          rtf_printf("\\sa%ld",
                     scanint(&ptr)); /* RTF: Space after (the default is 0). */
        if (x2 & 0x0008)
          rtf_printf("\\sl%ld", scanint(&ptr)); /* RTF: Space between lines. */
        if (x2 & 0x0010)
          rtf_printf("\\li%ld",
                     scanint(&ptr)); /* RTF: Left indent (the default is 0). */
        if (x2 & 0x0020)
          rtf_printf("\\ri%ld",
                     scanint(&ptr)); /* RTF: Right indent (the default is 0). */
        if (x2 & 0x0040)
          rtf_printf(
              "\\fi%ld",
              scanint(&ptr)); /* RTF: First-line indent (the default is 0). */
        if (x2 & 0x0100) {
          x1 = (unsigned char)*ptr++;
          if (x1 & 1)
            rtf_puts("\\box"); /* RTF: Border around the paragraph (box
                                  paragraph). */
          if (x1 & 2)
            rtf_puts("\\brdrt"); /* RTF: Border top. */
          if (x1 & 4)
            rtf_puts("\\brdrl"); /* RTF: Border left. */
          if (x1 & 8)
            rtf_puts("\\brdrb"); /* RTF: Border bottom. */
          if (x1 & 0x10)
            rtf_puts("\\brdrr"); /* RTF: Border right. */
          if (x1 & 0x20)
            rtf_puts("\\brdrth");
          else
            rtf_puts("\\brdrs"); /* RTF: Double-thickness border. else RTF:
                                    Single-thickness border. */
          if (x1 & 0x40)
            rtf_puts("\\brdrdb"); /* RTF: Double border. */
          ptr += 2;
        }
        if (x2 & 0x0200) {
          y1 = scanint(&ptr);
          while (y1-- > 0) {
            x1 = scanword(&ptr);
            if (x1 & 0x4000) {
              switch (scanword(&ptr)) {
              case 1:
                rtf_puts("\\tqr"); /* RTF: Flush-right tab. */
                break;
              case 2:
                rtf_puts("\\tqc"); /* RTF: Centered tab. */
                break;
              }
            }
            rtf_printf("\\tx%ld",
                       (x1 & 0x3FFF) * ctx->scaling -
                           ctx->rounderr); /* RTF: Tab position in twips from
                                              the left margin. */
          }
        }

        while (
            1) /* ptr<LinkData1+TopicLink.DataLen1-sizeof(TOPICLINK)&&str<end)
                */
        {
          if (*str && fontset >= 0 && fontset < ctx->font.count &&
              ctx->font.entry && ctx->font.entry[fontset].SmallCaps)
            strlwr(str);
          do {
            if (*str) {
              if (*str == '<') {
                html_puts("&lt;");
              } else if (*str == '>') {
                html_puts("&gt;");
              } else if (*str == '"') {
                html_puts("&quot;");
              } else if (isprint((unsigned char)*str)) {
                html_putc(*str);
              } else {
                // rtf_printf("\\'%02x",(unsigned char)*str);
                html_printf("&#%d;", (unsigned char)*str);
              }
            }
            if (ActualTopicOffset < MaxTopicOffset)
              ActualTopicOffset++;
          } while (*str++);
          if ((unsigned char)ptr[0] == 0xFF) {
            ptr++;
            break;
          } else
            switch ((unsigned char)ptr[0]) {
            case 0x20: /* vfld MVB */
              if (*(legacy_long *)(ptr + 1)) {
                rtf_printf("\\{vfld%ld\\}", *(legacy_long *)(ptr + 1));
              } else {
                rtf_puts("\\{vfld\\}");
              }
              ptr += 5;
              break;
            case 0x21: /* dtype MVB */
              if (*(int16_t *)(ptr + 1)) {
                rtf_printf("\\{dtype%d\\}", *(int16_t *)(ptr + 1));
              } else {
                rtf_puts("\\{dtype\\}");
              }
              ptr += 3;
              break;
            case 0x80: /* font change */
              html_change_font(__html_output, fontset = *(int16_t *)(ptr + 1), ul,
                             uldb);
              ptr += 3;
              break;
            case 0x81:
              html_puts("<br>"); // rtf_puts("\\line\n"); /* RTF: Required line
                                 // break (no paragraph break). */
              ptr++;
              break;
            case 0x82:
              if (TopicLink.RecordType == TL_TABLE) {
                if ((unsigned char)ptr[1] != 0xFF) {
                  rtf_puts("\n\\par\\intbl ");
                } else if (*(int16_t *)(ptr + 2) == -1) {
                  rtf_puts("\\cell\\intbl\\row\n");
                } else if (*(int16_t *)(ptr + 2) == lastcol) {
                  rtf_puts("\\par\\pard ");
                } else {
                  rtf_puts("\\cell\\pard ");
                }
              } else {
                // rtf_puts("\n\\par "); /* RTF: End of paragraph. */
              }
              ptr++;
              break;
            case 0x83:
              fputs("&#9;", __html_output); // rtf_puts("\\tab "); /* RTF: Tab
                                            // character. */
              ptr++;
              break;
            case 0x86:
              x3 = (unsigned char)*ptr++;
              x1 = *ptr++;
              if (x1 == 0x05)
                cmd = "ewc";
              else
                cmd = "bmc";
              goto picture;
            case 0x87:
              x3 = (unsigned char)*ptr++;
              x1 = *ptr++;
              if (x1 == 0x05)
                cmd = "ewl";
              else
                cmd = "bml";
              goto picture;
            case 0x88:
              x3 = (unsigned char)*ptr++;
              x1 = *ptr++;
              if (x1 == 0x05)
                cmd = "ewr";
              else
                cmd = "bmr";
              goto picture;
            picture:
              l1 = scanlong(&ptr);
              switch (x1) {
              case 0x22: /* HC31 */
                ActualTopicOffset +=
                    scanword(&ptr); /* number of hotspots in picture */
                if (ActualTopicOffset > MaxTopicOffset)
                  ActualTopicOffset = MaxTopicOffset;
                /* fall thru */
              case 0x03: /* HC30 */
                x1 = ((uint16_t *)ptr)[0];
                switch (x1) {
                case 1:
                  while (nextbitmap < ctx->extension.count &&
                         ctx->extension.entry[nextbitmap] < 0x10)
                    nextbitmap++;
                  if (nextbitmap >= ctx->extension.count) {
                    error("Bitmap never saved");
                    break;
                  }
                  x2 = nextbitmap++;
                  goto other;
                case 0:
                  x2 = ((uint16_t *)ptr)[1];
                other:
                  switch (x3) {
                  case 0x86:
                    rtf_puts("{\\field {\\*\\fldinst");
                    html_printf("<img src=\"%s\">", getbitmapname(x2));
                    rtf_puts("}}");
                    break;
                  case 0x87:
                    rtf_printf("{\\pvpara {\\field {\\*\\fldinst\n");
                    html_printf("<img src=\"%s\">", getbitmapname(x2));
                    rtf_puts("}}\\par}");
                    break;
                  case 0x88:
                    rtf_printf("{\\pvpara\\posxr{\\field {\\*\\fldinst");
                    html_printf("<img src=\"%s\">", getbitmapname(x2));
                    rtf_puts("}}\\par}");
                    break;
                  }
                  break;
                }
                break;
              case 0x05: /* ewc,ewl,ewr */
                if (ptr[6] == '!') {
                  rtf_printf("\\{button %s\\}", ptr + 7);
                } else if (ptr[6] == '*') {
                  char *plus;
                  legacy_int n, c1, c2;

                  sscanf(ptr + 7, "%d,%d,%n", &c1, &c2, &n);
                  plus = strchr(ptr + 7 + n, '+');
                  if ((c1 & 0xFFF5) != 0x8400)
                    helpdeco_warnf("mci c1=%04x\n", c1);
                  rtf_puts("\\{mci");
                  if (cmd[2] == 'r')
                    rtf_puts("_right");
                  if (cmd[2] == 'l')
                    rtf_puts("_left");
                  if (c2 == 1)
                    rtf_puts(" REPEAT");
                  if (c2 == 2)
                    rtf_puts(" PLAY");
                  if (!plus)
                    rtf_puts(" EXTERNAL");
                  if (c1 & 8)
                    rtf_puts(" NOMENU");
                  if (c1 & 2)
                    rtf_puts(" NOPLAYBAR");
                  rtf_printf(",%s\\}\n", plus ? plus + 1 : ptr + 7 + n);
                } else {
                  rtf_printf("\\{%s %s\\}", cmd, ptr + 6);
                }
                break;
              }
              ptr += l1;
              break;
            case 0x89: /* end of hotspot */
              html_change_font(__html_output, fontset, ul = FALSE, uldb = FALSE);
              ptr++;
              break;
            case 0xC8: /* macro */
              html_change_font(__html_output, fontset, FALSE, uldb = TRUE);
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            case 0xCC: /* macro without font change */
              html_change_font(__html_output, fontset, FALSE, uldb = TRUE);
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            case 0xE0: /* popup jump HC30 */
              html_change_font(__html_output, fontset, ul = TRUE, FALSE);
              goto label0;
            case 0xE1: /* topic jump HC30 */
              html_change_font(__html_output, fontset, FALSE, uldb = TRUE);
            label0:
              ptr += 5;
              break;
            case 0xE2: /* popup jump HC31 */
              html_change_font(__html_output, fontset, ul = TRUE, FALSE);
              goto label1;
            case 0xE3: /* topic jump HC31 */
              html_change_font(__html_output, fontset, FALSE, uldb = TRUE);
            label1:
              ptr += 5;
              break;
            case 0xE6: /* popup jump without font change */
              html_change_font(__html_output, fontset, ul = TRUE, FALSE);
              goto label2;
            case 0xE7: /* topic jump without font change */
              html_change_font(__html_output, fontset, FALSE, uldb = TRUE);
            label2:
              ptr += 5;
              break;
            case 0xEA: /* popup jump into external file */
            case 0xEE:
              html_change_font(__html_output, fontset, ul = TRUE, FALSE);
              goto label3;
            case 0xEB: /* topic jump into external file / secondary window */
            case 0xEF:
              html_change_font(__html_output, fontset, FALSE, uldb = TRUE);
            label3:
              ptr += *(int16_t *)(ptr + 1) + 3;
              break;
            case 0x8B:
              rtf_puts("\\~");
              ptr++;
              break;
            case 0x8C:
              rtf_puts("\\-");
              ptr++;
              break;
            default:
              ptr++;
            }
        }
      }
    }
    if (LinkData1)
      free(LinkData1);
    if (LinkData2)
      free(LinkData2);

    if (ctx->before31) {
      TopicPos += TopicLink.NextBlock;
    } else {
      TopicOffset = NextTopicOffset(TopicOffset, TopicLink.NextBlock, TopicPos);
      TopicPos = TopicLink.NextBlock;
    }
  }

#ifdef dev_null
  fclose(dev_null);
#endif

  return TRUE;
}

void html_change_font(FILE *rtf, unsigned_legacy_int i, BOOL ul, BOOL uldb) {
  FONTDESCRIPTOR *f;
  legacy_long pos;
  FILE *__html_output = rtf;

  fprintf(__html_output, "</span>");
  fprintf(__html_output, "<span");
  if (i < ctx->font.count) {
    pos = ftell(rtf);
    f = ctx->font.entry + i;
    if (f->style) {
      // fprintf(rtf,"\\plain\\cs%d",f->style+9);
      fprintf(__html_output, " style=\"");
      if (uldb)
        fprintf(__html_output, "text-decoration: underline;border-bottom: 1px "
                               "solid #000;"); // fputs("\\uldb",rtf);
      else if (ul)
        fprintf(__html_output,
                "text-decoration: underline;"); // fputs("\\ul",rtf);
      fprintf(__html_output, "\"");
    } else {
      fprintf(__html_output, " class=\"font-%d\"", i);
    }
  }
  fprintf(__html_output, ">");
}

const char *html_font_name(HELPDECO_CTX *ctx, legacy_int id) {
  if (0 <= id && id < ctx->fontname.count)
    return ctx->fontname.entry[id];

  return ctx->fontname.count ? ctx->fontname.entry[0] : FontFamily(id);
}

BOOL html_define_fonts(FILE *HelpFile, FILE *rtf) {
  FILE *__html_output = rtf;

  static char *BestFonts[] = {
      "Arial",  "Times New Roman", "MS Sans Serif", "MS Serif",    "Helv",
      "TmsRmn", "MS Sans Serif",   "Helvetica",     "Times Roman", "Times"};
  legacy_int default_font = 0;
  FONTHEADER FontHdr;
#define FontName_len 33
  char FontName[FontName_len];
#define CharMap_len 33
  char *ptr;
  legacy_long FontStart;
  legacy_int i, j, l, len;
  unsigned char *family;
  BOOL charmap;
  OLDFONT oldfont;
  NEWFONT newfont;
  MVBFONT mvbfont;
  MVBSTYLE *mvbstyle;
  NEWSTYLE *newstyle;
  FONTDESCRIPTOR *fd;

  if (!SearchFile(HelpFile, "|FONT", NULL)) {
    return FALSE;
  }

  FontStart = ftell(HelpFile);
  read_FONTHEADER(&FontHdr, HelpFile);
  ctx->fontname.count = FontHdr.NumFacenames;
  len = (FontHdr.DescriptorsOffset - FontHdr.FacenamesOffset) /
        ctx->fontname.count;
  if (len > FontName_len) {
    helpdeco_errorf("malformed |FONT file\n");
  }
  ctx->fontname.entry = helpdeco_malloc(ctx->fontname.count * sizeof(char *));
  family = helpdeco_malloc(ctx->fontname.count * sizeof(unsigned char));
  memset(family, 0, ctx->fontname.count * sizeof(unsigned char));
  charmap = FALSE;
  mvbstyle = NULL;
  newstyle = NULL;
  for (i = 0; i < ctx->fontname.count; i++) {
    fseek(HelpFile, FontStart + FontHdr.FacenamesOffset + len * i, SEEK_SET);
    helpdeco_fread(FontName, len, HelpFile);
    FontName[len] = '\0';
    if (FontName[0] == '\000') {
      strcpy(FontName, BestFonts[default_font]);
    }
    ptr = strchr(FontName, ',');
    if (ptr && FontHdr.FacenamesOffset >= 16) {
      *ptr++ = '\0';
      fseek(HelpFile, FontStart + FontHdr.CharmapsOffset, SEEK_SET);
    }
    ctx->fontname.entry[i] = helpdeco_strdup(FontName);
  }

  fseek(HelpFile, FontStart + FontHdr.DescriptorsOffset, SEEK_SET);
  ctx->color.count = 1; /* auto */
  ctx->color.entry[0].r = 1;
  ctx->color.entry[0].g = 1;
  ctx->color.entry[0].b = 0;
  ctx->font.count = FontHdr.NumDescriptors;
  helpdeco_logf("%hd descriptors found\n", ctx->font.count);
  if (ctx->font.entry)
    free(ctx->font.entry);
  ctx->font.entry = helpdeco_malloc(ctx->font.count * sizeof(FONTDESCRIPTOR));
  memset(ctx->font.entry, 0, ctx->font.count * sizeof(FONTDESCRIPTOR));
  if (FontHdr.FacenamesOffset >= 16) {
    ctx->scaling = 1;
    ctx->rounderr = 0;
    for (i = 0; i < FontHdr.NumDescriptors; i++) {
      read_MVBFONT(&mvbfont, HelpFile);
      fd = &ctx->font.entry[i];
      fd->FontName = mvbfont.FontName;
      fd->HalfPoints = -2 * mvbfont.Height;
      fd->Bold = mvbfont.Weight > 500;
      fd->Italic = mvbfont.Italic != 0;
      fd->Underline = mvbfont.Underline != 0;
      fd->StrikeOut = mvbfont.StrikeOut != 0;
      fd->DoubleUnderline = mvbfont.DoubleUnderline != 0;
      fd->SmallCaps = mvbfont.SmallCaps != 0;
      fd->textcolor =
          AddColor(mvbfont.FGRGB[0], mvbfont.FGRGB[1], mvbfont.FGRGB[2]);
      fd->backcolor =
          AddColor(mvbfont.BGRGB[0], mvbfont.BGRGB[1], mvbfont.BGRGB[2]);
      fd->FontFamily = mvbfont.PitchAndFamily >> 4;
      fd->style = mvbfont.style;
      fd->up = mvbfont.up;
      fd->expndtw = mvbfont.expndtw;
    }
    fseek(HelpFile, FontStart + FontHdr.FormatsOffset, SEEK_SET);
    mvbstyle = helpdeco_malloc(FontHdr.NumFormats * sizeof(MVBSTYLE));
    for (i = 0; i < FontHdr.NumFormats; i++) {
      MVBSTYLE *m = mvbstyle + i;
      ;
      read_MVBSTYLE(m, HelpFile);
      m->font.FGRGB[0] =
          AddColor(m->font.FGRGB[0], m->font.FGRGB[1], m->font.FGRGB[2]);
      m->font.BGRGB[0] =
          AddColor(m->font.BGRGB[0], m->font.BGRGB[1], m->font.BGRGB[2]);
    }
  } else if (FontHdr.FacenamesOffset >= 12) {
    ctx->scaling = 1;
    ctx->rounderr = 0;
    for (i = 0; i < FontHdr.NumDescriptors; i++) {
      read_NEWFONT(&newfont, HelpFile);
      fd = &ctx->font.entry[i];
      fd->Bold = newfont.Weight > 500;
      fd->Italic = newfont.Italic != 0;
      fd->Underline = newfont.Underline != 0;
      fd->StrikeOut = newfont.StrikeOut != 0;
      fd->DoubleUnderline = newfont.DoubleUnderline != 0;
      fd->SmallCaps = newfont.SmallCaps != 0;
      fd->FontName = newfont.FontName;
      fd->HalfPoints = -2 * newfont.Height;
      fd->textcolor =
          AddColor(newfont.FGRGB[0], newfont.FGRGB[1], newfont.FGRGB[2]);
      fd->backcolor =
          AddColor(newfont.BGRGB[0], newfont.BGRGB[1], newfont.BGRGB[2]);
      fd->FontFamily = newfont.PitchAndFamily >> 4;
    }
    fseek(HelpFile, FontStart + FontHdr.FormatsOffset, SEEK_SET);
    newstyle = helpdeco_malloc(FontHdr.NumFormats * sizeof(NEWSTYLE));
    for (i = 0; i < FontHdr.NumFormats; i++) {
      NEWSTYLE *m = newstyle + i;
      ;
      read_NEWSTYLE(m, HelpFile);
      m->font.FGRGB[0] =
          AddColor(m->font.FGRGB[0], m->font.FGRGB[1], m->font.FGRGB[2]);
      m->font.BGRGB[0] =
          AddColor(m->font.BGRGB[0], m->font.BGRGB[1], m->font.BGRGB[2]);
    }
  } else {
    ctx->scaling = 10;
    ctx->rounderr = 5;
    for (i = 0; i < FontHdr.NumDescriptors; i++) {
      read_OLDFONT(&oldfont, HelpFile);
      fd = &ctx->font.entry[i];
      fd->Bold = (oldfont.Attributes & FONT_BOLD) != 0;
      fd->Italic = (oldfont.Attributes & FONT_ITAL) != 0;
      fd->Underline = (oldfont.Attributes & FONT_UNDR) != 0;
      fd->StrikeOut = (oldfont.Attributes & FONT_STRK) != 0;
      fd->DoubleUnderline = (oldfont.Attributes & FONT_DBUN) != 0;
      fd->SmallCaps = (oldfont.Attributes & FONT_SMCP) != 0;
      fd->FontName = oldfont.FontName;
      fd->HalfPoints = oldfont.HalfPoints;
      fd->textcolor =
          AddColor(oldfont.FGRGB[0], oldfont.FGRGB[1], oldfont.FGRGB[2]);
      fd->backcolor =
          AddColor(oldfont.BGRGB[0], oldfont.BGRGB[1], oldfont.BGRGB[2]);
      if (oldfont.FontFamily < 6) {
        fd->FontFamily = lookup[oldfont.FontFamily];
      } else {
        fd->FontFamily = oldfont.FontFamily;
      }
    }
  }
  for (i = 0; i < FontHdr.NumDescriptors; i++) {
    if (ctx->font.entry[i].FontName < ctx->fontname.count) {
      family[ctx->font.entry[i].FontName] = ctx->font.entry[i].FontFamily;
    }
  }
  ctx->default_font = 0;
  l = sizeof(BestFonts) / sizeof(BestFonts[0]);
  if (ctx->fontname.entry) {
    for (i = 0; i < ctx->fontname.count; i++)
      if (family[i]) {
        for (j = 0; j < l; j++) {
          if (stricmp(ctx->fontname.entry[i], BestFonts[j]) == 0) {
            ctx->default_font = i;
            l = j;
            break;
          }
        }
      }
  }

  fprintf(__html_output, "<style>");
  helpdeco_logf("Writing %d fonts to style tag\n", ctx->fontname.count);
  for (i = 0; i < ctx->font.count; i++) {
    fprintf(__html_output,
            ".font-%d { "
            "font-family: \"%s\";"
            "%s%s%s%s%s%s%s"
            "}\n",
            i, html_font_name(ctx, ctx->font.entry[i].FontName),
            ctx->font.entry[i].Bold ? "font-weight: bold;" : "",
            ctx->font.entry[i].Italic ? "font-style: italic;" : "",
            ctx->font.entry[i].DoubleUnderline
                ? "text-decoration: underline; border-bottom: 1px solid"
                : "",
            ctx->font.entry[i].Underline ? "text-decoration: underline;" : "",
            ctx->font.entry[i].StrikeOut ? "text-decoration: line-through;"
                                         : "",
            ctx->font.entry[i].Underline && ctx->font.entry[i].StrikeOut
                ? "text-decoration: underline line-through;"
                : "",
            ctx->font.entry[i].SmallCaps ? "font-variant: small-caps;" : "");

    /* TODO: unhandled / unchecked font attributes
     unsigned char HalfPoints;
     unsigned char FontFamily;
     uint16_t FontName;
     unsigned char textcolor;
     unsigned char backcolor;
     uint16_t style;
     int16_t expndtw;
     signed char up;
     */
  }
  fprintf(__html_output, "</style>\n");

  fprintf(__html_output, "<style>");
  for (i = 0; i < ctx->color.count; i++) {
    // fprintf(__html_output, ".color-%d { color: rgb(%d, %d, %d); }\n",i,
    // color[i].r,color[i].g,color[i].b);
  }
  fprintf(__html_output, "</style>\n");

  /*
   fprintf(html,"{\\stylesheet{\\fs%d \\snext0 Normal;}\n",font[0].HalfPoints);
  if(mvbstyle)
  {
      for(i=0;i<FontHdr.NumFormats;i++)
      {
      MVBSTYLE *m,*n;

      m=mvbstyle+i;
      fprintf(html,"{\\*\\cs%u \\additive",m->StyleNum+9);
      if(m->BasedOn)
      {
          n=mvbstyle+(m->BasedOn-1);
          if(m->font.FontName!=n->font.FontName)
  fprintf(html,"\\f%d",m->font.FontName); if(m->font.expndtw!=n->font.expndtw)
  fprintf(html,"\\expndtw%d",m->font.expndtw);
          if(m->font.FGRGB[0]!=n->font.FGRGB[0])
  fprintf(html,"\\cf%d",m->font.FGRGB[0]);
          if(m->font.BGRGB[0]!=n->font.BGRGB[0])
  fprintf(html,"\\cb%d",m->font.BGRGB[0]); if(m->font.Height!=n->font.Height)
  fprintf(html,"\\fs%ld",-2L*m->font.Height);
          if((m->font.Weight>500)!=(n->font.Weight>500))
  fprintf(html,"\\b%d",m->font.Weight>500); if(m->font.Italic!=n->font.Italic)
  fprintf(html,"\\i%d",m->font.Italic); if(m->font.Underline!=n->font.Underline)
  fprintf(html,"\\ul%d",m->font.Underline);
          if(m->font.StrikeOut!=n->font.StrikeOut)
  fprintf(html,"\\strike%d",m->font.StrikeOut);
          if(m->font.DoubleUnderline!=n->font.DoubleUnderline)
  fprintf(html,"\\uldb%d",m->font.DoubleUnderline);
          if(m->font.SmallCaps!=n->font.SmallCaps)
  fprintf(html,"\\scaps%d",m->font.SmallCaps); if(m->font.up!=n->font.up)
  fprintf(html,"\\up%d",abs(m->font.up)); fprintf(html,"
  \\sbasedon%u",m->BasedOn+9);
      }
      else
      {
          fprintf(html,"\\f%d",m->font.FontName);
          if(m->font.Italic) fputs("\\i",html);
          if(m->font.Weight>500) fputs("\\b",html);
          if(m->font.Underline) fputs("\\ul",html);
          if(m->font.StrikeOut) fputs("\\strike",html);
          if(m->font.DoubleUnderline) fputs("\\uldb",html);
          if(m->font.SmallCaps) fputs("\\scaps",html);
          if(m->font.expndtw) fprintf(html,"\\expndtw%d",m->font.expndtw);
          if(m->font.up>0) fprintf(html,"\\up%d",m->font.up);
          else if(m->font.up<0) fprintf(html,"\\dn%d",-m->font.up);
          fprintf(html,"\\fs%ld",-2*m->font.Height);
          if(m->font.FGRGB[0]) fprintf(html,"\\cf%d",m->font.FGRGB[0]);
          if(m->font.BGRGB[0]) fprintf(html,"\\cb%d",m->font.BGRGB[0]);
      }
      fprintf(html," %s;}\n",m->StyleName);
      }
      free(mvbstyle);
  }
  else if(newstyle)
  {
      for(i=0;i<FontHdr.NumFormats;i++)
      {
      NEWSTYLE *m,*n;

      m=newstyle+i;
      fprintf(html,"{\\*\\cs%u \\additive",m->StyleNum+9);
      if(m->BasedOn)
      {
          n=newstyle+(m->BasedOn-1);
          if(m->font.FontName!=n->font.FontName)
  fprintf(html,"\\f%d",m->font.FontName); if(m->font.FGRGB[0]!=n->font.FGRGB[0])
  fprintf(html,"\\cf%d",m->font.FGRGB[0]);
          if(m->font.BGRGB[0]!=n->font.BGRGB[0])
  fprintf(html,"\\cb%d",m->font.BGRGB[0]); if(m->font.Height!=n->font.Height)
  fprintf(html,"\\fs%ld",-2L*m->font.Height);
          if((m->font.Weight>500)!=(n->font.Weight>500))
  fprintf(html,"\\b%d",m->font.Weight>500); if(m->font.Italic!=n->font.Italic)
  fprintf(html,"\\i%d",m->font.Italic); if(m->font.Underline!=n->font.Underline)
  fprintf(html,"\\ul%d",m->font.Underline);
          if(m->font.StrikeOut!=n->font.StrikeOut)
  fprintf(html,"\\strike%d",m->font.StrikeOut);
          if(m->font.DoubleUnderline!=n->font.DoubleUnderline)
  fprintf(html,"\\uldb%d",m->font.DoubleUnderline);
          if(m->font.SmallCaps!=n->font.SmallCaps)
  fprintf(html,"\\scaps%d",m->font.SmallCaps); fprintf(html,"
  \\sbasedon%u",m->BasedOn+9);
      }
      else
      {
          fprintf(html,"\\f%d",m->font.FontName);
          if(m->font.Italic) fputs("\\i",html);
          if(m->font.Weight>500) fputs("\\b",html);
          if(m->font.Underline) fputs("\\ul",html);
          if(m->font.StrikeOut) fputs("\\strike",html);
          if(m->font.DoubleUnderline) fputs("\\uldb",html);
          if(m->font.SmallCaps) fputs("\\scaps",html);
          fprintf(html,"\\fs%ld",-2*m->font.Height);
          if(m->font.FGRGB[0]) fprintf(html,"\\cf%d",m->font.FGRGB[0]);
          if(m->font.BGRGB[0]) fprintf(html,"\\cb%d",m->font.BGRGB[0]);
      }
      fprintf(html," %s;}\n",m->StyleName);
      }
      free(newstyle);
  }
  if(family) free(family);
  fputs("}\\pard\\plain\n",html);
  */
  memset(&ctx->current_font, 0, sizeof(ctx->current_font));
  ctx->current_font.FontName = ctx->default_font;

  return TRUE;
}

#pragma mark -
void dump_phrases(void) {
  unsigned_legacy_int n;

  for (n = 0; n < ctx->phrase.count; n++) {
    printf("%-5d - ", n);
    phrase_print(n, NULL, NULL);
    putchar('\n');
  }
}

BOOL HelpDeCompile(FILE *HelpFile, char *dumpfile, legacy_int mode,
                   char *exportname, legacy_long offset) {
  char filename[PATH_MAX];
  char hpjfilename[PATH_MAX];
  legacy_long FileLength;
  FILE *rtf;
  FILE *hpj;
  legacy_int d;
  legacy_long topic;

  if (!SearchFile(HelpFile, NULL, &FileLength))
    return FALSE;
  if (!dumpfile) {
    switch (mode) {
    case 0:
      SysLoad(HelpFile);
      helpdeco_logf("Decompiling %s...\n", ctx->title);
      ContextLoad(HelpFile);
      helpdeco_load_phrases(HelpFile);
      ExportBitmaps(HelpFile);
      helpdeco_logf("Pass 1...\n");
      FirstPass(HelpFile); /* valid only after ExportBitmaps */
      putc('\n', stderr);
      if (!ctx->before31 && ctx->opt_guessing)
        GuessFromKeywords(HelpFile); /* after FirstPass, before SysList */
      strcpy(hpjfilename, ctx->name);
      if (ctx->mvp) {
        strcat(hpjfilename, ".mvp");
      } else {
        strcat(hpjfilename, ".hpj");
      }
      hpj = helpdeco_fopen(hpjfilename, "wt");
      if (hpj) {
        strcpy(filename, ctx->name);
        strcat(filename, ".ico");
        SysList(HelpFile, hpj, filename); /* after ContextLoad */
        ListBaggage(HelpFile, hpj, ctx->before31);
        if (!ctx->mvp)
          AliasList(hpj); /* after ContextLoad, before TopicDump */
        strcpy(filename, ctx->name);
        strcat(filename, ".ph");
        phrase_dump(filename); /* after PhraseLoad */
        BuildRTFName(filename, ctx->opt_topics_per_rtf > 0);
        rtf = helpdeco_fopen(filename, "wt");
        if (rtf) {
          FontLoadRTF(HelpFile, rtf, hpj);
          helpdeco_logf("Pass 2...\n");
          fprintf(hpj, "[FILES]\n%s\n\n", filename);
          rtf = TopicDumpRTF(HelpFile, rtf, hpj, FALSE);
          putc('}', rtf);
          putc('\n', stderr);
          helpdeco_fclose(rtf);
        }
        ctx->NotInAnyTopic = FALSE;
        CTXOMAPList(HelpFile, hpj);
        if (ctx->extension.count && ctx->before31)
          ListBitmaps(hpj);
        if (ctx->win95)
          ListRose(HelpFile, hpj);
        helpdeco_fclose(hpj);
      }
      if (ctx->phrase.offset) {
        if (ctx->win95) {
          helpdeco_warnf("Help Compiler will issue Note HC1002: Using existing phrase "
              "table");
        } else {
          helpdeco_warnf("Help Compiler will issue Warning 5098: Using old key-phrase "
              "table");
        }
      }
      if (ctx->missing)
        helpdeco_warnf("Help Compiler will issue Error 1230: File 'missing.bmp' not "
            "found");
      if (ctx->NotInAnyTopic)
        helpdeco_warnf("Help Compiler will issue Warning 4098: Context string(s) in "
            "[MAP] section not defined in any topic");
      if (!ctx->opt_extractmacros)
        helpdeco_warnf("Help Compiler may issue Warning 4131: Hash conflict between 'x' "
            "and 'y'.");
      if (ctx->warnings) {
        helpdeco_warnf(
            "HELPDECO had problems with %s. Rebuilt helpfile may behave bad.\n",
            ctx->filename);
      }
      if (ctx->suggested_compiler[0]) {
        if (ctx->win95 && SearchFile(HelpFile, "|Petra", NULL))
          strcat(ctx->suggested_compiler, " /a");
        helpdeco_logf("Use %s %s to recompile ", ctx->suggested_compiler,
                      hpjfilename);
        if (ctx->annotation_file)
          helpdeco_logf("annotated ");
        helpdeco_logf("helpfile.");
      }
      break;
    case 1:
      HexDump(HelpFile, FileLength, offset);
      break;
    case 2:
      ListFiles(HelpFile);
      break;
    case 3: /* create lookalike RTF */
      SysLoad(HelpFile);
      helpdeco_logf("Writing %s...\n", ctx->title);
      ctx->exportplain = TRUE;
      ExportBitmaps(HelpFile);
      helpdeco_load_phrases(HelpFile);
      BuildRTFName(filename, ctx->opt_topics_per_rtf > 0);
      rtf = helpdeco_fopen(filename, "wt");
      if (rtf) {
        FontLoadRTF(HelpFile, rtf, NULL);
        rtf = TopicDumpRTF(HelpFile, rtf, NULL, TRUE);
        putc('}', rtf);
        putc('\n', stderr);
        helpdeco_fclose(rtf);
      }
      break;
    case 4: /* generate contents file */
      SysLoad(HelpFile);
      helpdeco_logf("Scanning %s...\n", ctx->title);
      ContextLoad(HelpFile);
      helpdeco_load_phrases(HelpFile);
      ctx->checkexternal = TRUE;
      ExportBitmaps(HelpFile);
      FirstPass(HelpFile);
      putc('\n', stderr);
      if (!ctx->before31 && ctx->opt_guessing)
        GuessFromKeywords(HelpFile); /* after FirstPass, before SysList */
      strcpy(filename, ctx->name);
      strcat(filename, ".cnt");
      rtf = helpdeco_fopen(filename, "wt");
      if (rtf) {
        GenerateContent(HelpFile, rtf);
        helpdeco_fclose(rtf);
      }
      break;
    case 5: /* create entry point list  */
      ctx->opt_resolvebrowse = FALSE;
      ctx->checkexternal = TRUE;
      SysLoad(HelpFile);
      helpdeco_logf("Parsing %s...\n", ctx->title);
      ContextLoad(HelpFile);
      helpdeco_load_phrases(HelpFile);
      ExportBitmaps(HelpFile);
      FirstPass(HelpFile);
      putc('\n', stderr);
      if (!ctx->before31 && ctx->opt_guessing)
        GuessFromKeywords(HelpFile); /* after FirstPass, before SysList */
      ContextList(HelpFile);
      break;
    case 6: /* check external references */
    case 7:
      ctx->opt_resolvebrowse = FALSE;
      ctx->checkexternal = TRUE;
      SysLoad(HelpFile);
      helpdeco_logf("Checking %s...\n", ctx->title);
      helpdeco_load_phrases(HelpFile);
      FirstPass(HelpFile);
      putc('\n', stderr);
      if (!ctx->external) {
        helpdeco_logf("No references to external files found in %s.\n", ctx->filename);
      } else if (mode == 6) {
        CheckReferences();
      } else {
        ListReferences();
      }
      break;
    case 8: /* create lookalike HTML */
      SysLoad(HelpFile);
      helpdeco_logf("Writing %s...\n", ctx->title);
      ctx->exportplain = TRUE;
      SysLoad(HelpFile);
      ExportBitmaps(HelpFile);
      helpdeco_load_phrases(HelpFile);
      snprintf(filename, sizeof(filename), "%s.html", ctx->name);
      rtf = helpdeco_fopen(filename, "wt");
      if (rtf) {
        helpdeco_logf("Writing html\n");
        FILE *__html_output = rtf;
        fprintf(__html_output, "<!doctype html>\n");
        fprintf(__html_output, "<html lang=\"en\">\n");
        fprintf(__html_output, "<head>\n");
        fprintf(__html_output, "<meta charset=\"utf-8\">\n");
        fprintf(__html_output, "<title>%s</title>\n",
                strlen(ctx->title) ? ctx->title : "untitled");
        fprintf(__html_output, "<style>"
                               "body { margin: auto; max-width: 600px; }"
                               "helpdeco-document { display: block; }"
                               "helpdeco-topic { display: block; border: 1px "
                               "solid black; padding: 10px; margin: 10px;}"
                               "</style>"
                               "\n");
        helpdeco_logf("Loading fonts\n");
        html_define_fonts(HelpFile, rtf);
        fprintf(__html_output, "</head>\n");
        fprintf(__html_output, "<body>\n");
        fprintf(__html_output, "<helpdeco-document>");
        helpdeco_logf("Dumpic topics\n");
        html_dump(HelpFile, rtf);
        fprintf(__html_output, "</helpdeco-document>");
        fprintf(__html_output, "</body>\n");
        fprintf(__html_output, "</html>\n");
        helpdeco_logf("HTML output complete\n");

        helpdeco_logf("\n");
        helpdeco_fclose(__html_output);
      } else {
        helpdeco_warnf("Could not open output file %s!", filename);
      }
      break;
    }
  } else {
    if (!SearchFile(HelpFile, dumpfile, &FileLength)) {
      filename[0] = '|';
      strlcpy(filename + 1, dumpfile, sizeof(filename) - 1);
      if (!SearchFile(HelpFile, filename, &FileLength)) {
        helpdeco_logf("Internal file %s not found.\n", dumpfile);
        return TRUE;
      }
      dumpfile = filename;
    }
    helpdeco_logf("FileName: %s FileSize: %ld\n", dumpfile, FileLength);
    if (exportname) /* export internal file */
    {
      FILE *f;

      f = helpdeco_fopen(exportname, "wb");
      if (f) {
        copy(HelpFile, FileLength, f);
        helpdeco_fclose(f);
      }
    } else if (mode == 1) {
      HexDump(HelpFile, FileLength, offset);
    } else if (strcmp(dumpfile, "|TOPIC") == 0) {
      SysLoad(HelpFile);
      helpdeco_load_phrases(HelpFile);
      DumpTopic(HelpFile, offset);
    } else if (strcmp(dumpfile + strlen(dumpfile) - 4, ".grp") == 0) {
      GroupDump(HelpFile);
    } else if (strcmp(dumpfile, "@LINK") == 0) {
      LinkDump(HelpFile);
    } else if (sscanf(dumpfile, "%ld!%d", &topic, &d) == 2 && topic != 0 &&
               d == 0) {
      AnnotationDump(HelpFile, FileLength, dumpfile);
    } else if (strcmp(dumpfile, "|Phrases") == 0 ||
               strcmp(dumpfile, "|PhrIndex") == 0) {
      SysLoad(HelpFile);
      helpdeco_load_phrases(HelpFile);
      dump_phrases();
    } else if (strcmp(dumpfile, "|SYSTEM") == 0) {
      SysDump(HelpFile);
    } else if (strcmp(dumpfile, "|TOMAP") == 0) {
      ToMapDump(HelpFile, FileLength);
    } else if (strcmp(dumpfile, "|CONTEXT") == 0) {
      BTreeDump(HelpFile, "ContextId: %h TopicOffset: 0x%08lx\n");
    } else if (dumpfile[0] == '|' && (strcmp(dumpfile + 2, "WBTREE") == 0 ||
                                      strcmp(dumpfile + 2, "KWBTREE") == 0)) {
      fseek(HelpFile, 7, SEEK_CUR);
      d = getc(HelpFile);
      fseek(HelpFile, -8, SEEK_CUR);
      if (d == '!') {
        BTreeDump(HelpFile, "Keyword: '%s' Occurrance: %!\n");
      } else {
        BTreeDump(HelpFile, "Keyword: '%s' Count: %u KWDataAddress: 0x%08lx\n");
      }
    } else if (dumpfile[0] == '|' && (strcmp(dumpfile + 2, "WMAP") == 0 ||
                                      strcmp(dumpfile + 2, "KWMAP") == 0)) {
      KWMapDump(HelpFile);
    } else if (dumpfile[0] == '|' && (strcmp(dumpfile + 2, "WDATA") == 0 ||
                                      strcmp(dumpfile + 2, "KWDATA") == 0)) {
      KWDataDump(HelpFile, FileLength);
    } else if (strcmp(dumpfile, "|VIOLA") == 0) {
      BTreeDump(HelpFile, "TopicOffset: 0x%08lx WindowNumber: %ld\n");
    } else if (strcmp(dumpfile, "|CTXOMAP") == 0) {
      CTXOMAPDump(HelpFile);
    } else if (strcmp(dumpfile, "|CATALOG") == 0) {
      CatalogDump(HelpFile);
    } else if (strcmp(dumpfile, "|Petra") == 0) {
      BTreeDump(HelpFile, "TopicOffset: 0x%08lx SourceFileName: %s\n");
    } else if (strcmp(dumpfile, "|TopicId") == 0) {
      BTreeDump(HelpFile, "TopicOffset: 0x%08lx ContextId: %s\n");
    } else if (strcmp(dumpfile, "|Rose") == 0) {
      BTreeDump(HelpFile, "KeywordHashValue: 0x%08lx\nMacro: %s\nTitle: %s\n");
    } else if (strcmp(dumpfile, "|TTLBTREE") == 0) {
      BTreeDump(HelpFile, "TopicOffset: 0x%08lx TopicTitle: %s\n");
    } else if (strcmp(dumpfile, "|FONT") == 0) {
      FontDump(HelpFile);
    } else /* generic  */
    {
      topic = ftell(HelpFile);
      if (helpdeco_getw(HelpFile) == 0x293B) /* if it's a B+ tree */
      {
        helpdeco_getw(HelpFile);
        helpdeco_getw(HelpFile);
        filename[0] = '\0';
        while ((d = getc(HelpFile)) > 0) /* format according to Structure */
        {
          switch (d) {
          case 'L':
          case '4':
          case 'a':
            strcat(filename, "0x%08lx ");
            break;
          case '2':
            strcat(filename, "%5u ");
            break;
          case 'F':
          case 'i':
          case 'z':
            strcat(filename, "'%s' ");
            break;
          case '!':
            strcat(filename, "%!");
            break;
          default:
            error("Unknown Btree field type '%c'", d);
          }
        }
        strcat(filename, "\n");
        fseek(HelpFile, topic, SEEK_SET);
        BTreeDump(HelpFile, filename);
      } else {
        fseek(HelpFile, topic, SEEK_SET);
        HexDump(HelpFile, FileLength, offset);
      }
    }
  }
  return TRUE;
}

#if !defined(__EMSCRIPTEN__)
int main(int argc, char *argv[]) {
  ctx = helpdeco_make_ctx();
  ctx->opt_interactive = TRUE;

  char AnnoFileName[NAME_MAX];
  char drive[_MAX_DRIVE];
  char dir[PATH_MAX];
  FILE *f;
  legacy_int mode;
  BOOL annotate;
  char *filename;
  char *dumpfile;
  char *exportname;
  legacy_long offset;
  legacy_int i, j;

  exportname = dumpfile = filename = NULL;
  AnnoFileName[0] = '\0';
  mode = 0;
  offset = 0;
  annotate = FALSE;
  /* scan arguments */
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == OPTCHAR) {
      switch (tolower((unsigned char)argv[i][1])) {
      case 'a':
        if (argv[i][2]) {
          strlcpy(AnnoFileName, argv[i] + 2, sizeof(AnnoFileName));
        } else if (argv[i + 1] && argv[i + 1][0] != '/' &&
                   argv[i + 1][0] != '-') {
          strlcpy(AnnoFileName, argv[i + 1], sizeof(AnnoFileName));
          i++;
        }
        annotate = TRUE;
        break;
      case 'b':
        ctx->opt_resolvebrowse = FALSE;
        break;
      case 'c':
        mode = 4;
        break;
      case 'd':
        mode = 2;
        break;
      case 'e':
        mode = 7;
        break;
      case 'f':
        ctx->opt_listtopic = TRUE;
        break;
      case 'g':
        ctx->opt_guessing = FALSE;
        break;
      case 'h': // add entry to prefix table
        for (j = 0;
             j < sizeof(ctx->prefix) / sizeof(ctx->prefix[0]) && ctx->prefix[j];
             j++)
          ;
        if (j < sizeof(ctx->prefix) / sizeof(ctx->prefix[0])) {
          if (argv[i][2]) {
            ctx->prefix[j] = argv[i] + 2;
          } else if (argv[i + 1] && argv[i + 1][0] != '/' &&
                     argv[i + 1][0] != '-') {
            ctx->prefix[j] = argv[i + 1];
            i++;
          }
        } else {
          helpdeco_warnf("Prefix table full.\n");
        }
        break;
      case 'i':
        ctx->opt_reportderived = TRUE;
        break;
      case 'l':
        mode = 5;
        break;
      case 'm':
        ctx->opt_extractmacros = FALSE;
        break;
      case 'n':
        ctx->opt_nopagebreak = TRUE;
        break;
      case 'p':
        mode = 6;
        break;
      case 'r':
        mode = 3;
        break;
      case 'w':
        mode = 8;
        break;
      case 's':
        if (argv[i][2]) {
          ctx->opt_topics_per_rtf = atoi(argv[i] + 2);
        } else if (argv[i + 1] && argv[i + 1][0] != '/' &&
                   argv[i + 1][0] != '-') {
          ctx->opt_topics_per_rtf = atoi(argv[i + 1]);
          i++;
        }
        break;
      case 't':
        if (argv[i][2]) {
          offset = strtoul(argv[i] + 2, NULL, 0);
        } else if (argv[i + 1] && argv[i + 1][0] != '/' &&
                   argv[i + 1][0] != '-') {
          offset = strtoul(argv[i + 1], NULL, 0);
          i++;
        }
        break;
      case 'x':
        mode = 1;
        break;
      case 'y':
        ctx->opt_overwrite = TRUE;
        break;
      case 'z':
        ctx->opt_exportLZ77 = TRUE;
        break;
      default:
        helpdeco_warnf("unknown option '%s' ignored\n", argv[i]);
      }
    } else if (exportname) {
      helpdeco_warnf("additional parameter '%s' ignored\n", argv[i]);
    } else if (dumpfile) {
      exportname = argv[i];
    } else if (filename) {
      dumpfile = argv[i];
    } else {
      filename = argv[i];
    }
  }
  if (filename) {
    _splitpath(filename, drive, dir, ctx->name, ctx->ext);
    if (ctx->ext[0] == '\0')
      strcpy(ctx->ext, ".hlp");
    ctx->mvp = ctx->ext[1] == 'M' || ctx->ext[1] == 'm';
    _makepath(ctx->filename, drive, dir, ctx->name, ctx->ext);
    f = fopen(ctx->filename, "rb");
    if (f) {
      if (annotate) {
        if (AnnoFileName[0] == '\0')
          _makepath(AnnoFileName, drive, dir, ctx->name, ".ann");
        ctx->annotation_file = fopen(AnnoFileName, "rb");
        if (!ctx->annotation_file) {
          helpdeco_warnf("Couldn't find annotation file '%s'\n", AnnoFileName);
        }
      }
      ctx->prefixhash[0] = 0;
      for (i = 1; ctx->prefix[i]; i++) {
        ctx->prefixhash[i] = hash(ctx->prefix[i]);
      }
      if (!HelpDeCompile(f, dumpfile, mode, exportname, offset)) {
        helpdeco_warnf("%s isn't a valid WinHelp file !\n", ctx->filename);
      }
      if (annotate && ctx->annotation_file)
        fclose(ctx->annotation_file);
      helpdeco_fclose(f);
    } else {
      helpdeco_warnf("Can not open '%s'\n", ctx->filename);
    }
  } else {
    helpdeco_warnf(
        "HELPDECO - decompile *.HLP/*.MVB files of Windows 3.x / 95 - %lu "
        "bit Version 2.1.4\n"
        "M.Winterhoff <mawin@gmx.net>, Geschw.-Scholl-Ring 17, 38444 "
        "Wolfsburg, Germany\n"
        "\n"
        "usage:   HELPDECO helpfile[.hlp]    [" OPTSTR
        "y]  - decompile helpfile into all sources\n"
        "         HELPDECO helpfile[.hlp]    [" OPTSTR "y] " OPTSTR
        "a[annfile.ANN]  - and add annotations\n"
            "         HELPDECO helpfile[.hlp] " OPTSTR "r [" OPTSTR
            "y] [" OPTSTR "n]    - decompile into lookalike RTF\n"
        "         HELPDECO helpfile[.hlp] " OPTSTR
        "w    - decompile into lookalike HTML\n"
        "         HELPDECO helpfile[.hlp] " OPTSTR "c [" OPTSTR
        "y]  - generate Win95 .CNT content file\n"
        "         HELPDECO helpfile[.hlp] " OPTSTR
        "l       - list entry points of this helpfile\n"
        "         HELPDECO helpfile[.hlp] " OPTSTR "e [" OPTSTR
        "f]  - list references to other helpfiles\n"
        "         HELPDECO helpfile[.hlp] " OPTSTR "p [" OPTSTR
        "f]  - check references to other helpfiles\n"
        "         HELPDECO helpfile[.hlp] " OPTSTR "d [" OPTSTR
        "x]  - display internal directory\n"
        "         HELPDECO helpfile[.hlp] \"internalfile\" [" OPTSTR
        "x]    - display internal file\n"
        "         HELPDECO helpfile[.hlp] \"internalfile\" filename - "
        "export internal file\n"
        "options: " OPTSTR "y overwrite without warning, " OPTSTR
        "f list referencing topics, " OPTSTR "x hex dump\n"
        "         " OPTSTR "g no guessing, " OPTSTR
        "hprefix add known contextid prefix, " OPTSTR "n no page breaks\n"
        "To recreate all source files necessary to rebuild a Windows "
        "helpfile, create\n"
        "a directory, change to this directory and call HELPDECO with the "
        "path and name\n"
        "of the helpfile to dissect. HELPDECO will extract all files "
        "contained in the\n"
        "helpfile in two passes and deposit them in the current directory. "
        "You may then\n"
        "rebuild the helpfile using the appropriate help compiler HC30, "
        "HC31, HCP, HCW,\n"
        "HCRTF, MVC, WMVC or MVCC. The file will not be identical, but "
        "should look and\n"
        "work like the original.\n",
        sizeof(int) * 8);
  }

  helpdeco_free_ctx(ctx);
  ctx = NULL;

  return 0;
}
#endif
