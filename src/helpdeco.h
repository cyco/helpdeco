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

/* HELPDECO.H - declares functions stored external in HELPDEC1.C */
#ifndef HELPDECO_H
#define HELPDECO_H
#include <time.h>
#if defined(__APPLE__) || defined(__EMSCRIPTEN__)
#define HAVE_STRNCPY HAVE_STRNCPY
#include <stdlib.h>
#else
#include <malloc.h>
#endif /* __APPLE__ */
#include <stdarg.h>
#include <stdio.h>
#ifndef _WIN32
#include <stdint.h>
#endif /* _WIN32 */
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include "helper.h"
#else
#include "compat.h"
#endif /* _WIN32 */

#include "mfile.h"
#include "types.h"

#ifdef __TURBOC__
typedef struct {
    char a, b, c;
} align;
#if sizeof(align) != 3
#error Compile bytealigned !
#endif
#elif defined(__APPLE__) || defined(__EMSCRIPTEN__)
#pragma pack(push, 1)
#else
#pragma pack(1)
#endif

#define sizeof_BYTE 1
#define sizeof_WORD 2
#define sizeof_DWORD 4

typedef struct /* structure at beginning of help file */
{
    int32_t Magic; /* 0x00035F3F */
    int32_t DirectoryStart; /* offset of FILEHEADER of internal direcory */
    int32_t FreeChainStart; /* offset of FILEHEADER or -1 */
    int32_t EntireFileSize; /* size of entire help file in bytes */
} HELPHEADER;
BOOL read_HELPHEADER(HELPHEADER* obj, FILE* file);
#define sizeof_HELPHEADER (sizeof_DWORD * 4)

typedef struct FILEHEADER /* structure at FileOffset of each internal file */
{
    int32_t ReservedSpace; /* reserved space in help file incl. FILEHEADER */
    int32_t UsedSpace; /* used space in help file excl. FILEHEADER */
    unsigned char FileFlags; /* normally 4 */
} FILEHEADER;
BOOL read_FILEHEADER(FILEHEADER* obj, FILE* file);
#define sizeof_FILEHEADER (sizeof_DWORD * 2 + sizeof_BYTE)

typedef struct BTREEHEADER /* structure after FILEHEADER of each Btree */
{
    uint16_t Magic; /* 0x293B */
    uint16_t Flags; /* bit 0x0002 always 1, bit 0x0400 1 if direcory */
    uint16_t PageSize; /* 0x0400=1k if directory, 0x0800=2k else */
    unsigned char Structure[16]; /* string describing structure of data */
    int16_t MustBeZero; /* 0 */
    int16_t PageSplits; /* number of page splits Btree has suffered */
    int16_t RootPage; /* page number of Btree root page */
    int16_t MustBeNegOne; /* 0xFFFF */
    int16_t TotalPages; /* number of Btree pages */
    int16_t NLevels; /* number of levels of Btree */
    int32_t TotalBtreeEntries; /* number of entries in Btree */
} BTREEHEADER;
BOOL read_BTREEHEADER(BTREEHEADER* obj, FILE* file);
#define sizeof_BTREEHEADER \
    (sizeof_WORD * 3 + sizeof_BYTE * 16 + sizeof_WORD * 6 + sizeof_DWORD)

typedef struct BTREEINDEXHEADER /* structure at beginning of every index-page */
{
    uint16_t Unknown; /* sorry, no ID to identify an index-page */
    int16_t NEntries; /* number of entries in this index-page */
    int16_t PreviousPage; /* page number of previous page */
} BTREEINDEXHEADER;
BOOL read_BTREEINDEXHEADER(BTREEINDEXHEADER* obj, FILE* file);
#define sizeof_BTREEINDEXHEADER (sizeof_WORD * 3)

typedef struct BTREENODEHEADER /* structure at beginning of every leaf-page */
{
    uint16_t Unknown; /* Sorry, no ID to identify a leaf-page */
    int16_t NEntries; /* number of entires in this leaf-page */
    int16_t PreviousPage; /* page number of preceeding leaf-page or -1 */
    int16_t NextPage; /* page number of next leaf-page or -1 */
} BTREENODEHEADER;
BOOL read_BTREENODEHEADER(BTREENODEHEADER* obj, FILE* file);
#define sizeof_BTREENODEHEADER (sizeof_WORD * 4)

BOOL read_BTREEINDEXHEADER_to_BTREENODEHEADER(BTREENODEHEADER* obj, FILE* file);

typedef struct SYSTEMHEADER /* structure at beginning of |SYSTEM file */
{
    uint16_t Magic; /* 0x036C */
    uint16_t Minor; /* help file format version number */
    uint16_t Major; /* 1 */
    time_t GenDate; /* date/time the help file was generated or 0 */
    uint16_t Flags; /* tells you how the help file is compressed */
} SYSTEMHEADER;
BOOL read_SYSTEMHEADER(SYSTEMHEADER* obj, FILE* file);
#define sizeof_SYSTEMHEADER (sizeof_WORD * 3 + sizeof_DWORD + sizeof_WORD)

typedef struct /* internal structure */
{
    FILE* File;
    int32_t SavePos;
    int32_t Remaining;
    uint16_t RecordType; /* type of data in record */
    uint16_t DataSize; /* size of data */
    char Data[10];
} SYSTEMRECORD;

typedef struct SECWINDOW /* structure of data following RecordType 6 */
{
    uint16_t Flags; /* flags (see below) */
    char Type[10]; /* type of window */
    char Name[9]; /* window name */
    char Caption[51]; /* caption for window */
    int16_t X; /* x coordinate of window (0..1000) */
    int16_t Y; /* y coordinate of window (0..1000) */
    int16_t Width; /* width of window (0..1000) */
    int16_t Height; /* height of window (0..1000) */
    int16_t Maximize; /* maximize flag and window styles */
    unsigned char Rgb[3]; /* color of scrollable region */
    unsigned char Unknown1;
    unsigned char RgbNsr[3]; /* color of non-scrollable region */
    unsigned char Unknown2;
} SECWINDOW;

typedef struct {
    uint16_t Flags; /* flags (see below) */
    char Type[10]; /* type of window */
    char Name[9]; /* window name */
    char Caption[51]; /* caption for window */
    unsigned char MoreFlags;
    int16_t X; /* x coordinate of window (0..1000) */
    int16_t Y; /* y coordinate of window (0..1000) */
    int16_t Width; /* width of window (0..1000) */
    int16_t Height; /* height of window (0..1000) */
    int16_t Maximize; /* maximize flag and window styles */
    unsigned char TopRgb[3];
    unsigned char Unknown0;
    unsigned char Unknown[25];
    unsigned char Rgb[3]; /* color of scrollable region */
    unsigned char Unknown1;
    unsigned char RgbNsr[3]; /* color of non-scrollable region */
    unsigned char Unknown2;
    int16_t X2;
    int16_t Y2;
    int16_t Width2;
    int16_t Height2;
    int16_t X3;
    int16_t Y3;
} MVBWINDOW;

typedef struct /* structure of data following RecordType 14 */
{
    char btreename[10];
    char mapname[10];
    char dataname[10];
    char title[80];
} KEYINDEX;

#define WSYSFLAG_TYPE 0x0001 /* Type is valid */
#define WSYSFLAG_NAME 0x0002 /* Name is valid */
#define WSYSFLAG_CAPTION 0x0004 /* Caption is valid */
#define WSYSFLAG_X 0x0008 /* X is valid */
#define WSYSFLAG_Y 0x0010 /* Y is valid */
#define WSYSFLAG_WIDTH 0x0020 /* Width is valid */
#define WSYSFLAG_HEIGHT 0x0040 /* Height is valid */
#define WSYSFLAG_MAXIMIZE 0x0080 /* Maximize is valid */
#define WSYSFLAG_RGB 0x0100 /* Rgb is valid */
#define WSYSFLAG_RGBNSR 0x0200 /* RgbNsr is valid */
#define WSYSFLAG_TOP 0x0400 /* On top was set in HPJ file */
#define WSYSFLAG_AUTOSIZEHEIGHT 0x0800 /* Auto-Size Height */

typedef struct PHRINDEXHDR /* structure of beginning of |PhrIndex file */
{
    int32_t always4A01; /* sometimes 0x0001 */
    int32_t entries; /* number of phrases */
    int32_t compressedsize; /* size of PhrIndex file */
    int32_t phrimagesize; /* size of decompressed PhrImage file */
    int32_t phrimagecompressedsize; /* size of PhrImage file */
    int32_t always0;
    uint16_t bits : 4;
    uint16_t unknown : 12;
    uint16_t always4A00; /* sometimes 0x4A01, 0x4A02 */
} PHRINDEXHDR;
BOOL read_PHRINDEXHDR(PHRINDEXHDR* obj, FILE* file);
/* bits and unknown are combined into the same WORD */
#define sizeof_PHRINDEXHDR (sizeof_DWORD * 6 + sizeof_WORD + sizeof_WORD)

typedef struct FONTHEADER /* structure of beginning of |FONT file */
{
    uint16_t NumFacenames; /* number of face names */
    uint16_t NumDescriptors; /* number of font descriptors */
    uint16_t FacenamesOffset; /* offset of face name array */
    uint16_t DescriptorsOffset; /* offset of descriptors array */
    uint16_t NumFormats; /* only if FacenamesOffset >= 12 */
    uint16_t FormatsOffset; /* offset of formats array */
    uint16_t NumCharmaps; /* only if FacenamesOffset >= 16 */
    uint16_t CharmapsOffset; /* offset of charmapnames array */
} FONTHEADER;
BOOL read_FONTHEADER(FONTHEADER* obj, FILE* file);
#define sizeof_FONTHEADER (sizeof_WORD * 8)

typedef struct FONTDESCRIPTOR /* internal font descriptor */
{
    unsigned char Bold;
    unsigned char Italic;
    unsigned char Underline;
    unsigned char StrikeOut;
    unsigned char DoubleUnderline;
    unsigned char SmallCaps;
    unsigned char HalfPoints;
    unsigned char FontFamily;
    uint16_t FontName;
    unsigned char textcolor;
    unsigned char backcolor;
    uint16_t style;
    int16_t expndtw;
    signed char up;
} FONTDESCRIPTOR;

typedef struct /* non-Multimedia font descriptor */
{
    unsigned char Attributes; /* Font Attributes See values below */
    unsigned char HalfPoints; /* PointSize * 2 */
    unsigned char FontFamily; /* Font Family. See values below */
    uint16_t FontName; /* Number of font in Font List */
    unsigned char FGRGB[3]; /* RGB values of foreground */
    unsigned char BGRGB[3]; /* unused background RGB Values */
} OLDFONT;
BOOL read_OLDFONT(OLDFONT* obj, FILE* file);
#define sizeof_OLDFONT \
    (sizeof_BYTE * 3 + sizeof_WORD + sizeof_BYTE * 3 + sizeof_BYTE * 3)

typedef struct NEWFONT /* structure located at DescriptorsOffset */
{
    unsigned char unknown1;
    int16_t FontName;
    unsigned char FGRGB[3];
    unsigned char BGRGB[3];
    unsigned char unknown5;
    unsigned char unknown6;
    unsigned char unknown7;
    unsigned char unknown8;
    unsigned char unknown9;
    int32_t Height;
    unsigned char mostlyzero[12];
    int16_t Weight;
    unsigned char unknown10;
    unsigned char unknown11;
    unsigned char Italic;
    unsigned char Underline;
    unsigned char StrikeOut;
    unsigned char DoubleUnderline;
    unsigned char SmallCaps;
    unsigned char unknown17;
    unsigned char unknown18;
    unsigned char PitchAndFamily;
} NEWFONT;
BOOL read_NEWFONT(NEWFONT* obj, FILE* file);
#define sizeof_NEWFONT \
    (sizeof_BYTE + sizeof_WORD + sizeof_BYTE * 3 + sizeof_BYTE * 5 + sizeof_DWORD + sizeof_BYTE * 12 + sizeof_WORD + sizeof_BYTE * 10)

typedef struct {
    uint16_t StyleNum;
    uint16_t BasedOn;
    NEWFONT font;
    char unknown[35];
    char StyleName[65];
} NEWSTYLE;
BOOL read_NEWSTYLE(NEWSTYLE* obj, FILE* file);
#define sizeof_NEWSTYLE \
    (sizeof_WORD * 2 + sizeof_NEWFONT + sizeof_BYTE * 35 + sizeof_BYTE * 65)

typedef struct MVBFONT /* structure located at DescriptorsOffset */
{
    int16_t FontName;
    int16_t expndtw;
    uint16_t style;
    unsigned char FGRGB[3];
    unsigned char BGRGB[3];
    int32_t Height;
    unsigned char mostlyzero[12];
    int16_t Weight;
    unsigned char unknown10;
    unsigned char unknown11;
    unsigned char Italic;
    unsigned char Underline;
    unsigned char StrikeOut;
    unsigned char DoubleUnderline;
    unsigned char SmallCaps;
    unsigned char unknown17;
    unsigned char unknown18;
    unsigned char PitchAndFamily;
    unsigned char unknown20;
    signed char up;
} MVBFONT;
BOOL read_MVBFONT(MVBFONT* obj, FILE* file);
#define sizeof_MVBFONT \
    (sizeof_WORD * 3 + sizeof_BYTE * 3 * 2 + sizeof_DWORD + sizeof_BYTE * 12 + sizeof_WORD + sizeof_BYTE * 12)

typedef struct {
    uint16_t StyleNum;
    uint16_t BasedOn;
    MVBFONT font;
    char unknown[35];
    char StyleName[65];
} MVBSTYLE;
BOOL read_MVBSTYLE(MVBSTYLE* obj, FILE* file);
#define sizeof_MVBSTYLE \
    (sizeof_WORD * 2 + sizeof_MVBFONT + sizeof_BYTE * 35 + sizeof_BYTE * 65)

typedef struct {
    uint16_t Magic; /* 0x5555 */
    uint16_t Size;
    uint16_t Unknown1;
    uint16_t Unknown2;
    uint16_t Entries;
    uint16_t Ligatures;
    uint16_t LigLen;
    uint16_t Unknown[13];
} CHARMAPHEADER;
BOOL read_CHARMAPHEADER(CHARMAPHEADER* obj, FILE* file);
#define sizeof_CHARMAPHEADER (sizeof_WORD * 7 + sizeof_WORD * 13)

/* Font Attributes */
#define FONT_NORM 0x00 /* Normal */
#define FONT_BOLD 0x01 /* Bold */
#define FONT_ITAL 0x02 /* Italics */
#define FONT_UNDR 0x04 /* Underline */
#define FONT_STRK 0x08 /* Strike Through */
#define FONT_DBUN 0x10 /* Dbl Underline */
#define FONT_SMCP 0x20 /* Small Caps */
/* Font Families */
#define FAM_MODERN 0x01
#define FAM_ROMAN 0x02
#define FAM_SWISS 0x03
#define FAM_TECH 0x03
#define FAM_NIL 0x03
#define FAM_SCRIPT 0x04
#define FAM_DECOR 0x05

typedef struct KWMAPREC /* structure of |xWMAP leaf-page entries */
{
    int32_t FirstRec; /* index number of first keyword on leaf page */
    uint16_t PageNum; /* page number that keywords are associated with */
} KWMAPREC;
BOOL read_KWMAPREC(KWMAPREC* obj, FILE* file);
#define sizeof_KWMAPREC (sizeof_DWORD + sizeof_WORD)

typedef uint32_t
    TOPICPOS; /* TOPICPOS/DecompressSize = block number, TOPICPOS%DecompressSize
                 = offset into decompression buffer (including
                 sizeof(TOPICBLOCKHEADER)) */
#define sizeof_TOPICPOS 4

typedef uint32_t
    TOPICOFFSET; /* TOPICOFFSET/0x8000 = block number, TOPICOFFSET/0x8000 =
                    number of characters and hotspots counting from first
                    TOPICLINK of this block */
#define sizeof_TOPICOFFSET 4

typedef struct /* structure every TopicBlockSize in |TOPIC */
{
    TOPICPOS LastTopicLink; /* points to last TOPICLINK in previous block */
    TOPICPOS FirstTopicLink; /* points to first TOPICLINK in this block */
    TOPICPOS LastTopicHeader; /* points to TOPICLINK of last TOPICHEADER */
} TOPICBLOCKHEADER;
BOOL read_TOPICBLOCKHEADER(TOPICBLOCKHEADER* obj, FILE* file);
#define sizeof_TOPICBLOCKHEADER (sizeof_TOPICPOS * 3)

typedef struct /* structure pointed to by FirstTopicLink */
{
    uint32_t BlockSize; /* size of this link + LinkData1 + LinkData2 */
    uint32_t DataLen2; /* length of decompressed LinkData2 */
    TOPICPOS PrevBlock;
    /* Windows 3.0 (HC30): number of bytes the TOPICLINK of the previous
  // block is located before this TOPICLINK, that is the block size of
  // the previous TOPICLINK plus eventually skipped TOPICBLOCKHEADER.
  // Windows 3.1 (HC31): TOPICPOS of previous TOPICLINK */
    TOPICPOS NextBlock;
    /* Windows 3.0 (HC30): number of bytes the TOPICLINK of the next block
  // is located behind this block, including skipped TOPICBLOCKHEADER.
  // Windows 3.1 (HC31): TOPICPOS of next TOPICLINK */
    uint32_t DataLen1; /* includes size of TOPICLINK */
    unsigned char RecordType; /* See below */
} TOPICLINK;
/* Known RecordTypes for TOPICLINK */
#define TL_DISPLAY30 0x01 /* version 3.0 displayable information */
#define TL_TOPICHDR 0x02 /* topic header information */
#define TL_DISPLAY 0x20 /* version 3.1 displayable information */
#define TL_TABLE 0x23 /* version 3.1 table */

typedef struct /* structure of LinkData1 of RecordType 2 */
{
    int32_t BlockSize; /* size of topic, including internal topic links */
    TOPICOFFSET BrowseBck; /* topic offset for prev topic in browse sequence */
    TOPICOFFSET BrowseFor; /* topic offset for next topic in browse sequence */
    int32_t TopicNum; /* topic Number */
    TOPICPOS NonScroll; /* start of non-scrolling region (topic offset) or -1 */
    TOPICPOS Scroll; /* start of scrolling region (topic offset) */
    TOPICPOS NextTopic; /* start of next type 2 record */
} TOPICHEADER;

typedef struct /* structure of LinkData1 of RecordType 2 */
{
    int32_t BlockSize;
    int16_t PrevTopicNum;
    int16_t unused1;
    int16_t NextTopicNum;
    int16_t unused2;
} TOPICHEADER30;

typedef struct /* structure of |CTXOMAP file entries */
{
    int32_t MapID;
    int32_t TopicOffset;
} CTXOMAPREC;
BOOL read_CTXOMAPREC(CTXOMAPREC* obj, FILE* file);
BOOL read_CTXOMAPRECs(CTXOMAPREC* objs, int n, FILE* file);
#define sizeof_CTXOMAPREC (sizeof_DWORD * 2)

typedef struct /* structure of |CONTEXT leaf-page entry */
{
    int32_t HashValue; /* Hash value of context id */
    TOPICOFFSET TopicOffset; /* Topic offset */
} CONTEXTREC;
BOOL read_CONTEXTREC(CONTEXTREC* obj, FILE* file);
BOOL read_CONTEXTRECs(CONTEXTREC* objs, int n, FILE* file);
#define sizeof_CONTEXTREC (sizeof_DWORD + sizeof_TOPICOFFSET)

typedef struct /* structure of *.GRP file header */
{
    uint32_t Magic; /* 0x000A3333 */
    uint32_t BitmapSize;
    uint32_t LastTopic;
    uint32_t FirstTopic;
    uint32_t TopicsUsed;
    uint32_t TopicCount;
    uint32_t GroupType;
    uint32_t Unknown1;
    uint32_t Unknown2;
    uint32_t Unknown3;
} GROUPHEADER;
BOOL read_GROUPHEADER(GROUPHEADER* obj, FILE* file);
#define sizeof_GROUPHEADER (sizeof_DWORD * 10)

typedef struct /* internal use */
{
    GROUPHEADER GroupHeader;
    char* Name;
    unsigned char* Bitmap;
} GROUP;

typedef struct /* structure of STOPn.STP header */
{
    uint32_t Magic; /* 0x00082222 */
    uint16_t BytesUsed;
    uint16_t Unused[17];
} STOPHEADER;
BOOL read_STOPHEADER(STOPHEADER* obj, FILE* file);
#define sizeof_STOPHEADER (sizeof_DWORD + sizeof_WORD + sizeof_WORD * 17)

typedef struct /* structure of |VIOLA leaf-page entry */
{
    TOPICOFFSET TopicOffset; /* topic offset */
    int32_t WindowNumber; /* number of window assigned to topic */
} VIOLAREC;
BOOL read_VIOLARECs(VIOLAREC* objs, int n, FILE* file);
#define sizeof_VIOLAREC (sizeof_TOPICOFFSET + sizeof_DWORD)

typedef struct /* structure of |CATALOG header */
{
    uint16_t magic; /* 0x1111 */
    uint16_t always8;
    uint16_t always4;
    int32_t entries;
    unsigned char zero[30];
} CATALOGHEADER;
BOOL read_CATALOGHEADER(CATALOGHEADER* obj, FILE* file);
#define sizeof_CATALOGHEADER (sizeof_WORD * 3 + sizeof_DWORD + sizeof_BYTE * 30)

typedef struct /* structure of Windows Bitmap BMP file */
{
    uint16_t bfType;
    uint32_t bfSize;
    uint16_t bfReserved1;
    uint16_t bfReserved2;
    uint32_t bfOffBits;
} BITMAPFILEHEADER;

typedef struct /* structure of Windows Bitmap header */
{
    uint32_t biSize;
    int32_t biWidth;
    int32_t biHeight;
    uint16_t biPlanes;
    uint16_t biBitCount;
    uint32_t biCompression;
    uint32_t biSizeImage;
    int32_t biXPelsPerMeter;
    int32_t biYPelsPerMeter;
    uint32_t biClrUsed;
    uint32_t biClrImportant;
} BITMAPINFOHEADER;

typedef struct tagRECT /* Windows rectangle */
{
    int16_t left;
    int16_t top;
    int16_t right;
    int16_t bottom;
} RECT;

typedef struct /* Windows Aldus placeable metafile header */
{
    uint32_t dwKey;
    uint16_t hMF;
    RECT rcBBox;
    uint16_t wInch;
    uint32_t dwReserved;
    uint16_t wChecksum;
} APMFILEHEADER;

typedef struct /* structure of hotspot info */
{
    unsigned char id0, id1, id2;
    uint16_t x, y, w, h;
    uint32_t hash;
} HOTSPOT;

typedef struct /* structure used as buf of GetFirstPage */
{
    int32_t FirstLeaf;
    uint16_t PageSize;
    int16_t NextPage;
} BUFFER;

typedef struct /* internal use. 16 bit: max. 3640 */
{
    int32_t StartTopic;
    int32_t NextTopic;
    int32_t PrevTopic;
    int16_t BrowseNum;
    int16_t Start;
    int16_t Count;
} BROWSE;

typedef struct /* internal use. 16 bit: max. 8191 */
{
    int32_t StartTopic;
    int16_t BrowseNum;
    int16_t Start;
} START;

typedef struct /* internal use. 16 bit: max. 6553 */
{
    char* name;
    int32_t hash;
    BOOL derived;
} HASHREC;

typedef struct /* internal use to store keyword definitions */
{
    BOOL KeyIndex;
    char Footnote;
    char* Keyword;
    TOPICOFFSET TopicOffset;
} KEYWORDREC;

typedef struct placerec /* internal use to store external references */
{
    struct placerec* next;
    char topicname[1];
} PLACEREC;

typedef struct checkrec /* internal use to store external references */
{
    struct checkrec* next;
    enum { TOPIC,
        CONTEXT } type;
    int32_t hash;
    char* id;
    PLACEREC* here;
} CHECKREC;

typedef struct fileref /* internal use to store external references */
{
    struct fileref* next;
    CHECKREC* check;
    char filename[1];
} FILEREF;

typedef struct /* internal use */
{
    TOPICOFFSET TopicOffset;
    TOPICOFFSET OtherTopicOffset;
} ALTERNATIVE;

extern void error(const char* format, ...);
#ifdef HAVE_STRNCPY
#ifndef strlcpy
#define strlcpy strncpy
#endif
#elif !defined(HAVE_STRLCPY)
extern size_t strlcpy(char* dest, const char* src,
    size_t len); /* limited string copy */
#endif /* !defined(HAVE_STRLCPY) */
#ifndef OPTCHAR
#ifdef _WIN32
#define OPTCHAR '/'
#define OPTSTR "/"
#else
#define OPTCHAR '-'
#define OPTSTR "-"
#endif
#endif

#include "helpdec1.h"

extern legacy_long copy(FILE* f, legacy_long bytes, FILE* out);
extern void HexDump(FILE* f, legacy_long FileLength, legacy_long offset);
extern void HexDumpMemory(void* bypMem, unsigned_legacy_int FileLength);
extern char* PrintString(char* str, unsigned_legacy_int len);
extern BOOL GetBit(FILE* f);
extern int16_t scanint(char** ptr); /* scan a compressed short */
extern uint16_t scanword(char** ptr); /* scan a compressed unsiged short */
extern uint32_t scanlong(char** ptr); /* scan a compressed long */
extern BOOL SearchFile(FILE* HelpFile, const char* FileName,
    legacy_long* FileLength);
extern int16_t GetFirstPage(FILE* HelpFile, BUFFER* buf,
    legacy_long* TotalEntries);
extern int16_t GetNextPage(FILE* HelpFile, BUFFER* buf); /* walk Btree */
extern SYSTEMRECORD* GetNextSystemRecord(SYSTEMRECORD* SysRec);
extern SYSTEMRECORD* GetFirstSystemRecord(FILE* HelpFile);
extern void ListFiles(FILE* HelpFile); /* display internal directory */
extern void ListBaggage(FILE* HelpFile, FILE* hpj,
    BOOL before31); /* writes out [BAGGAGE] section */
extern void PrintWindow(FILE* hpj, SECWINDOW* SWin);
extern void PrintMVBWindow(FILE* hpj, MVBWINDOW* SWin);
extern void ToMapDump(FILE* HelpFile, legacy_long FileLength);
extern void GroupDump(FILE* HelpFile);
extern void KWMapDump(FILE* HelpFile);
extern void KWDataDump(FILE* HelpFile, legacy_long FileLength);
extern void CatalogDump(FILE* HelpFile);
extern void CTXOMAPDump(FILE* HelpFile);
extern void LinkDump(FILE* HelpFile);
extern void AnnotationDump(FILE* HelpFile, legacy_long FileLength,
    const char* name);

int32_t hash(char* name);
#if defined(__APPLE__) || defined(__EMSCRIPTEN__)
#pragma pack(pop)
#endif

#pragma mark -
typedef enum { Hall,
    Old } PhraseStyle;

typedef struct {
    BOOL opt_exportLZ77;
    BOOL opt_extractmacros;
    BOOL opt_guessing;
    BOOL opt_interactive;
    BOOL opt_listtopic;
    BOOL opt_nopagebreak;
    BOOL opt_overwrite;
    BOOL opt_reportderived;
    BOOL opt_resolvebrowse;
    int opt_topics_per_rtf;

    char filename[NAME_MAX];
    char name[NAME_MAX];
    char ext[_MAX_EXT];
    char title[NAME_MAX];
    char suggested_compiler[13];

    BOOL after31;
    BOOL before31;
    BOOL checkexternal;
    BOOL exportplain;
    BOOL keyindex['z' - '0' + 1];
    BOOL lists['z' - '0' + 1];
    BOOL lzcompressed;
    BOOL missing;
    BOOL multi;
    BOOL mvp;
    BOOL NotInAnyTopic;
    BOOL warnings;
    BOOL win95;
    char* phrases;
    PhraseStyle phrase_style;
    char* prefix[8];
    char index_separators[40];
    char oldtable[256];
    FILE* annotation_file;
    FILEREF* external;
    FONTDESCRIPTOR current_font;
    legacy_int browsenums;
    legacy_int DecompressSize; /* 4k or 16k */
    legacy_int NextKeywordRec;
    legacy_int rounderr;
    legacy_int topic_block_size; /* 2k or 4k */
    legacy_long guessed;
    legacy_long prefixhash[8];
    legacy_long scaling;
    legacy_long topic_file_length;
    long topic_last_pos;
    long topic_block_num;
    long topic_file_start;
    TOPICBLOCKHEADER topic_block_header;
    TOPICOFFSET NextKeywordOffset;
    unsigned char default_font;
    unsigned int decompressed_size;

    struct {
        legacy_int count;
        ALTERNATIVE* entry;
    } alternative;
    struct {
        legacy_int count;
        BROWSE* entry;
    } browse;
    struct {
        legacy_int count;
        struct {
            unsigned char r, g, b;
        } entry[128];
    } color;
    struct {
        legacy_int count;
        CONTEXTREC* entry;
    } context_rec;
    struct {
        legacy_int count;
        char* entry;
    } extension;
    struct {
        legacy_int count;
        FONTDESCRIPTOR* entry;
    } font;
    struct {
        legacy_int count;
        char** entry;
    } fontname;
    struct {
        legacy_int count;
        GROUP* entry;
    } group;
    struct {
        legacy_int count;
        HASHREC* entry;
    } hashrec;
    struct {
        unsigned_legacy_int count;
        unsigned_legacy_int* offset;
    } phrase;
    struct {
        legacy_int count;
        START* entry;
    } start;
    struct {
        legacy_int count;
        int32_t* entry;
    } topic;
    struct {
        legacy_int count;
        char** entry;
    } windowname;
    struct {
        legacy_int count;
        char** entry;
    } stopword_filename;
    struct {
        legacy_int count;
        KEYWORDREC* entry;
    } keyword_rec;
} HELPDECO_CTX;

extern HELPDECO_CTX* ctx;

HELPDECO_CTX* helpdeco_make_ctx(void);
BOOL HelpDeCompile(FILE* HelpFile, char* dumpfile, legacy_int mode,
    char* exportname, legacy_long offset);
void helpdeco_free_ctx(HELPDECO_CTX* ctx);

#ifndef helpdeco_logf
#define helpdeco_logf(format, ...) fprintf(stdout, format, ##__VA_ARGS__);
#define helpdeco_warnf(format, ...) fprintf(stderr, format, ##__VA_ARGS__);
#define helpdeco_errorf(format, ...)            \
    do {                                        \
        fprintf(stderr, format, ##__VA_ARGS__); \
        exit(1);                                \
    } while (0);
#endif
#endif
