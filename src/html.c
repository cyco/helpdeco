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

#include <stdio.h>

/* emit rtf commands to change to font i.
// ul forces underline on, uldb forces doubleunderline on */
void ChangeHTMLFont(FILE *rtf,unsigned_legacy_int i,BOOL ul,BOOL uldb)
{
    FONTDESCRIPTOR *f;
    legacy_long pos;
    FILE *__html_output = rtf;

    fprintf(__html_output, "</span>");
    fprintf(__html_output, "<span");
    if(i<fonts)
    {
        pos=ftell(rtf);
        f=font+i;
        if(f->style)
        {
            // fprintf(rtf,"\\plain\\cs%d",f->style+9);
            fprintf(__html_output, " style=\"");
            if(uldb) fprintf(__html_output, "text-decoration: underline;border-bottom: 1px solid #000;"); // fputs("\\uldb",rtf);
            else if(ul) fprintf(__html_output, "text-decoration: underline;"); // fputs("\\ul",rtf);
            fprintf(__html_output, "\"");
        }
        else
        {
            fprintf(__html_output, " class=\"font-%d\"", i);
        }
    }
    fprintf(__html_output, ">");
}


BOOL LoadFontsIntoHTML(FILE *HelpFile,FILE *rtf)
{
    FILE *__html_output = rtf;
    
    static char *BestFonts[]={"Arial","Times New Roman","MS Sans Serif","MS Serif","Helv","TmsRmn","MS Sans Serif","Helvetica","Times Roman","Times"};
    legacy_int default_font = 0;
    FONTHEADER FontHdr;
    #define FontName_len 33
    char FontName[FontName_len];
    #define CharMap_len 33
    char *ptr;
    legacy_long FontStart;
    legacy_int i,j,l,len;
    unsigned char *family;
    BOOL charmap;
    OLDFONT oldfont;
    NEWFONT newfont;
    MVBFONT mvbfont;
    MVBSTYLE *mvbstyle;
    NEWSTYLE *newstyle;
    FONTDESCRIPTOR *fd;

    if(!SearchFile(HelpFile,"|FONT",NULL))
    {
        return FALSE;
    }
    
    FontStart=ftell(HelpFile);
    read_FONTHEADER(&FontHdr,HelpFile);
    fontnames=FontHdr.NumFacenames;
    len=(FontHdr.DescriptorsOffset-FontHdr.FacenamesOffset)/fontnames;
       if( len > FontName_len ){
           fprintf(stderr,"malformed |FONT file\n");
           exit(1);
       }
    fontname=my_malloc(fontnames*sizeof(char *));
    family=my_malloc(fontnames*sizeof(unsigned char));
    memset(family,0,fontnames*sizeof(unsigned char));
    charmap=FALSE;
    mvbstyle=NULL;
    newstyle=NULL;
    for(i=0;i<fontnames;i++)
    {
        fseek(HelpFile,FontStart+FontHdr.FacenamesOffset+len*i,SEEK_SET);
        my_fread(FontName,len,HelpFile);
        FontName[len]='\0';
        if (FontName[0] == '\000') {
        strcpy(FontName, BestFonts[default_font]);
        }
        ptr=strchr(FontName,',');
        if(ptr&&FontHdr.FacenamesOffset>=16)
        {
        *ptr++='\0';
        fseek(HelpFile,FontStart+FontHdr.CharmapsOffset,SEEK_SET);
        }
        fontname[i]=my_strdup(FontName);
    }

    fseek(HelpFile,FontStart+FontHdr.DescriptorsOffset,SEEK_SET);
    colors=1;     /* auto */
    color[0].r=1;
    color[0].g=1;
    color[0].b=0;
    fonts=FontHdr.NumDescriptors;
    if(font) free(font);
    font=my_malloc(fonts*sizeof(FONTDESCRIPTOR));
    memset(font,0,fonts*sizeof(FONTDESCRIPTOR));
    if(FontHdr.FacenamesOffset>=16)
    {
        scaling=1;
        rounderr=0;
        for(i=0;i<FontHdr.NumDescriptors;i++)
        {
        read_MVBFONT(&mvbfont,HelpFile);
        fd=font+i;
        fd->FontName=mvbfont.FontName;
        fd->HalfPoints=-2*mvbfont.Height;
        fd->Bold=mvbfont.Weight>500;
        fd->Italic=mvbfont.Italic!=0;
        fd->Underline=mvbfont.Underline!=0;
        fd->StrikeOut=mvbfont.StrikeOut!=0;
        fd->DoubleUnderline=mvbfont.DoubleUnderline!=0;
        fd->SmallCaps=mvbfont.SmallCaps!=0;
        fd->textcolor=AddColor(mvbfont.FGRGB[0],mvbfont.FGRGB[1],mvbfont.FGRGB[2]);
        fd->backcolor=AddColor(mvbfont.BGRGB[0],mvbfont.BGRGB[1],mvbfont.BGRGB[2]);
        fd->FontFamily=mvbfont.PitchAndFamily>>4;
        fd->style=mvbfont.style;
        fd->up=mvbfont.up;
        fd->expndtw=mvbfont.expndtw;
        }
        fseek(HelpFile,FontStart+FontHdr.FormatsOffset,SEEK_SET);
        mvbstyle=my_malloc(FontHdr.NumFormats*sizeof(MVBSTYLE));
        for(i=0;i<FontHdr.NumFormats;i++)
        {
        MVBSTYLE *m=mvbstyle+i;;
        read_MVBSTYLE(m,HelpFile);
        m->font.FGRGB[0]=AddColor(m->font.FGRGB[0],m->font.FGRGB[1],m->font.FGRGB[2]);
        m->font.BGRGB[0]=AddColor(m->font.BGRGB[0],m->font.BGRGB[1],m->font.BGRGB[2]);
        }
    }
    else if(FontHdr.FacenamesOffset>=12)
    {
        scaling=1;
        rounderr=0;
        for(i=0;i<FontHdr.NumDescriptors;i++)
        {
        read_NEWFONT(&newfont,HelpFile);
        fd=font+i;
        fd->Bold=newfont.Weight>500;
        fd->Italic=newfont.Italic!=0;
        fd->Underline=newfont.Underline!=0;
        fd->StrikeOut=newfont.StrikeOut!=0;
        fd->DoubleUnderline=newfont.DoubleUnderline!=0;
        fd->SmallCaps=newfont.SmallCaps!=0;
        fd->FontName=newfont.FontName;
        fd->HalfPoints=-2*newfont.Height;
        fd->textcolor=AddColor(newfont.FGRGB[0],newfont.FGRGB[1],newfont.FGRGB[2]);
        fd->backcolor=AddColor(newfont.BGRGB[0],newfont.BGRGB[1],newfont.BGRGB[2]);
        fd->FontFamily=newfont.PitchAndFamily>>4;
        }
        fseek(HelpFile,FontStart+FontHdr.FormatsOffset,SEEK_SET);
        newstyle=my_malloc(FontHdr.NumFormats*sizeof(NEWSTYLE));
        for(i=0;i<FontHdr.NumFormats;i++)
        {
        NEWSTYLE *m=newstyle+i;;
        read_NEWSTYLE(m,HelpFile);
        m->font.FGRGB[0]=AddColor(m->font.FGRGB[0],m->font.FGRGB[1],m->font.FGRGB[2]);
        m->font.BGRGB[0]=AddColor(m->font.BGRGB[0],m->font.BGRGB[1],m->font.BGRGB[2]);
        }
    }
    else
    {
        scaling=10;
        rounderr=5;
        for(i=0;i<FontHdr.NumDescriptors;i++)
        {
        read_OLDFONT(&oldfont,HelpFile);
        fd=font+i;
        fd->Bold=(oldfont.Attributes&FONT_BOLD)!=0;
        fd->Italic=(oldfont.Attributes&FONT_ITAL)!=0;
        fd->Underline=(oldfont.Attributes&FONT_UNDR)!=0;
        fd->StrikeOut=(oldfont.Attributes&FONT_STRK)!=0;
        fd->DoubleUnderline=(oldfont.Attributes&FONT_DBUN)!=0;
        fd->SmallCaps=(oldfont.Attributes&FONT_SMCP)!=0;
        fd->FontName=oldfont.FontName;
        fd->HalfPoints=oldfont.HalfPoints;
        fd->textcolor=AddColor(oldfont.FGRGB[0],oldfont.FGRGB[1],oldfont.FGRGB[2]);
        fd->backcolor=AddColor(oldfont.BGRGB[0],oldfont.BGRGB[1],oldfont.BGRGB[2]);
        if(oldfont.FontFamily<6)
        {
            fd->FontFamily=lookup[oldfont.FontFamily];
        }
        else
        {
            fd->FontFamily=oldfont.FontFamily;
        }
        }
    }
    for(i=0;i<FontHdr.NumDescriptors;i++)
    {
        if(font[i].FontName<fontnames)
        {
        family[font[i].FontName]=font[i].FontFamily;
        }
    }
    DefFont=0;
    l=sizeof(BestFonts)/sizeof(BestFonts[0]);
    if(fontname)
    {
        for(i=0;i<fontnames;i++) if(family[i])
        {
        for(j=0;j<l;j++)
        {
            if(stricmp(fontname[i],BestFonts[j])==0)
            {
            DefFont=i;
            l=j;
            break;
            }
        }
        }
    }

    fprintf(__html_output, "<style>");
    for(i=0;i<fontnames;i++) {
        fprintf(__html_output, ".font-%d { "
                "font-family: \"%s\";"
                "%s%s%s%s%s%s%s"
                "}\n"
                ,i, FontFamily(family[i]),
                font[i].Bold ? "font-weight: bold;" : "",
                font[i].Italic ? "font-style: italic;" : "",
                font[i].DoubleUnderline ? "text-decoration: underline; border-bottom: 1px solid" : "",
                font[i].Underline ? "text-decoration: underline;" : "",
                font[i].StrikeOut ? "text-decoration: line-through;" : "",
                font[i].Underline && font[i].StrikeOut  ? "text-decoration: underline line-through;" : "",
                font[i].SmallCaps  ? "font-variant: small-caps;" : ""
        );
        free(fontname[i]);
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
    for(i=0;i<colors;i++) {
        // fprintf(__html_output, ".color-%d { color: rgb(%d, %d, %d); }\n",i, color[i].r,color[i].g,color[i].b);
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
            if(m->font.FontName!=n->font.FontName) fprintf(html,"\\f%d",m->font.FontName);
            if(m->font.expndtw!=n->font.expndtw) fprintf(html,"\\expndtw%d",m->font.expndtw);
            if(m->font.FGRGB[0]!=n->font.FGRGB[0]) fprintf(html,"\\cf%d",m->font.FGRGB[0]);
            if(m->font.BGRGB[0]!=n->font.BGRGB[0]) fprintf(html,"\\cb%d",m->font.BGRGB[0]);
            if(m->font.Height!=n->font.Height) fprintf(html,"\\fs%ld",-2L*m->font.Height);
            if((m->font.Weight>500)!=(n->font.Weight>500)) fprintf(html,"\\b%d",m->font.Weight>500);
            if(m->font.Italic!=n->font.Italic) fprintf(html,"\\i%d",m->font.Italic);
            if(m->font.Underline!=n->font.Underline) fprintf(html,"\\ul%d",m->font.Underline);
            if(m->font.StrikeOut!=n->font.StrikeOut) fprintf(html,"\\strike%d",m->font.StrikeOut);
            if(m->font.DoubleUnderline!=n->font.DoubleUnderline) fprintf(html,"\\uldb%d",m->font.DoubleUnderline);
            if(m->font.SmallCaps!=n->font.SmallCaps) fprintf(html,"\\scaps%d",m->font.SmallCaps);
            if(m->font.up!=n->font.up) fprintf(html,"\\up%d",abs(m->font.up));
            fprintf(html," \\sbasedon%u",m->BasedOn+9);
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
            if(m->font.FontName!=n->font.FontName) fprintf(html,"\\f%d",m->font.FontName);
            if(m->font.FGRGB[0]!=n->font.FGRGB[0]) fprintf(html,"\\cf%d",m->font.FGRGB[0]);
            if(m->font.BGRGB[0]!=n->font.BGRGB[0]) fprintf(html,"\\cb%d",m->font.BGRGB[0]);
            if(m->font.Height!=n->font.Height) fprintf(html,"\\fs%ld",-2L*m->font.Height);
            if((m->font.Weight>500)!=(n->font.Weight>500)) fprintf(html,"\\b%d",m->font.Weight>500);
            if(m->font.Italic!=n->font.Italic) fprintf(html,"\\i%d",m->font.Italic);
            if(m->font.Underline!=n->font.Underline) fprintf(html,"\\ul%d",m->font.Underline);
            if(m->font.StrikeOut!=n->font.StrikeOut) fprintf(html,"\\strike%d",m->font.StrikeOut);
            if(m->font.DoubleUnderline!=n->font.DoubleUnderline) fprintf(html,"\\uldb%d",m->font.DoubleUnderline);
            if(m->font.SmallCaps!=n->font.SmallCaps) fprintf(html,"\\scaps%d",m->font.SmallCaps);
            fprintf(html," \\sbasedon%u",m->BasedOn+9);
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
    memset(&CurrentFont,0,sizeof(CurrentFont));
    CurrentFont.FontName=DefFont;

    return TRUE;
}

BOOL TopicDumpToHTML(FILE *HelpFile,FILE *__html_output)
{
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
#define rtf_printf(format, ...) fprintf(__rtf_output, "<span style=\"color: red !important;\">" format "</span>", ##__VA_ARGS__)
#define rtf_puts(str) rtf_printf("%s", str)
#define rtf_putc(c) rtf_printf("%c", c)

    TOPICLINK TopicLink;
    char *LinkData1;  /* Data associated with this link */
    legacy_long nonscroll=-1;
    char *LinkData2;  /* Second set of data */
    legacy_int fontset;
    legacy_int NextContextRec;
    char *hotspot;
    BOOL firsttopic=TRUE;
    BOOL ul,uldb;
    legacy_int nextbitmap,TopicInRTF,NumberOfRTF;
    legacy_long TopicNum,TopicOffset,TopicPos;
    legacy_int col,cols,lastcol;
    int16_t *iptr;
    uint16_t x1,x2,x3;
    int16_t y1;
    legacy_long l1 = 0;
    char *ptr;
    char *cmd;
    char *str;
    legacy_long ActualTopicOffset = 0,MaxTopicOffset = 0;
    
    if(!SearchFile(HelpFile,"|TOPIC",&TopicFileLength))
    {
        return FALSE;
    }
    
    fontset=-1;
    nextbitmap=1;
    if(browse) free(browse);
    browse=NULL;
    browses=0;
    NextContextRec=0;
    ul=uldb=FALSE;
    hotspot=NULL;
    TopicOffset=0;
    TopicPos=12;
    TopicNum=16;
    TopicInRTF=0;
    NumberOfRTF=1;
    while(TopicRead(HelpFile,TopicPos,&TopicLink,sizeof(TopicLink))==sizeof(TOPICLINK))
    {
        if(before31)
        {
            if(TopicPos+TopicLink.NextBlock>=TopicFileLength) break;
        }
        else
        {
            if(TopicLink.NextBlock<=0) break;
        }
        if(TopicLink.DataLen1>sizeof(TOPICLINK))
        {
            LinkData1=my_malloc(TopicLink.DataLen1-sizeof(TOPICLINK)+1);
            if(TopicRead(HelpFile,0,LinkData1,TopicLink.DataLen1-sizeof(TOPICLINK))!=TopicLink.DataLen1-sizeof(TOPICLINK)) break;
        }
        else LinkData1=NULL;
        if(TopicLink.DataLen1<TopicLink.BlockSize) /* read LinkData2 using phrase replacement */
        {
            LinkData2=my_malloc(TopicLink.DataLen2+1);
            if(TopicPhraseRead(HelpFile,0,LinkData2,TopicLink.BlockSize-TopicLink.DataLen1,TopicLink.DataLen2)!=TopicLink.BlockSize-TopicLink.DataLen1) break;
        }
        else LinkData2=NULL;
        
        if(LinkData1 && TopicLink.RecordType==TL_TOPICHDR) /* display a Topic Header record */
        {
            if(!firsttopic)
            {
                // fputs("\\page\n",rtf); /* RTF: Required page break. */
                html_puts("</span></p>");
                html_puts("</helpdeco-topic>\n");
            }
            if(TopicLink.NextBlock != -1) {
                html_puts("\n\n<helpdeco-topic><p><span>\n");
            } else
                firsttopic = FALSE;
            firsttopic=FALSE;
            fprintf(stderr,"\rTopic %ld...",TopicNum-15);
            TopicNum++;
        }
        else if(LinkData1&&LinkData2&&(TopicLink.RecordType==TL_DISPLAY30||TopicLink.RecordType==TL_DISPLAY||TopicLink.RecordType==TL_TABLE))
        {
            if(AnnoFile) Annotate(TopicPos,__rtf_output);
            ptr=LinkData1;
            scanlong(&ptr);
            if(TopicLink.RecordType==TL_DISPLAY||TopicLink.RecordType==TL_TABLE)
            {
                x1=scanword(&ptr);
                ActualTopicOffset=TopicOffset;
                MaxTopicOffset=ActualTopicOffset+x1;
                TopicOffset+=x1;
            }
            if(TopicLink.RecordType==TL_TABLE)
            {
                rtf_puts("\\trowd"); /* RTF: Sets table row defaults. */
                cols=(unsigned char)*ptr++;
                x1=(unsigned char)*ptr++;
                switch(x1)
                {
                    case 0:
                    case 2:
                        l1=*(int16_t *)ptr; /* min table width */
                        ptr+=2;
                        rtf_puts("\\trqc");/* RTF: Centers a table row with respect to its containing column. */
                        break;
                    case 1:
                    case 3:
                        l1=32767;
                        break;
                }
                iptr=(int16_t *)ptr;
                if(cols>1)
                {
                    x1=iptr[0]+iptr[1]+iptr[3]/2;
                    rtf_printf("\\trgaph%ld\\trleft%ld \\cellx%ld\\cellx%ld",((iptr[3]*scaling-rounderr)*l1)/32767,(((iptr[1]-iptr[3])*scaling-rounderr)*l1-32767)/32767,((x1*scaling-rounderr)*l1)/32767,(((x1+iptr[2]+iptr[3])*scaling-rounderr)*l1)/32767);
                    x1+=iptr[2]+iptr[3];
                    for(col=2;col<cols;col++)
                    {
                        x1+=iptr[2*col]+iptr[2*col+1];
                        rtf_printf("\\cellx%ld",((x1*scaling-rounderr)*l1)/32767);
                    }
                }
                else
                {
                    rtf_printf("\\trleft%ld \\cellx%ld ",((iptr[1]*scaling-rounderr)*l1-32767)/32767,((iptr[0]*scaling-rounderr)*l1)/32767);
                }
                ptr=(char *)(iptr+2*cols);
            }
            lastcol=-1;
            str=LinkData2;
            for(col=0;(TopicLink.RecordType==TL_TABLE?*(int16_t *)ptr!=-1:col==0)&&ptr<LinkData1+TopicLink.DataLen1-sizeof(TOPICLINK);col++)
            {
                // fputs("\\pard",rtf); /* RTF: Resets to default paragraph properties. */
                // if(TopicPos<nonscroll) fputs("\\keepn",rtf); /* RTF: Keep paragraph with the next paragraph. */
                if(TopicLink.RecordType==TL_TABLE)
                {
                    rtf_puts("\\intbl"); /* RTF: Paragraph is part of a table. */
                    lastcol=*(int16_t *)ptr;
                    ptr+=5;
                }
                ptr+=4;
                x2=*(uint16_t *)ptr;
                ptr+=2;
                if(x2&0x1000) rtf_puts("\\keep"); /* RTF: Keep paragraph intact. */
                if(x2&0x0400) rtf_puts("\\qr"); /* RTF: Right-aligned. */
                if(x2&0x0800) rtf_puts("\\qc"); /* RTF: Centered. */
                if(x2&0x0001) scanlong(&ptr);
                if(x2&0x0002) rtf_printf("\\sb%ld",scanint(&ptr)); /* RTF: Space before (the default is 0). */
                if(x2&0x0004) rtf_printf("\\sa%ld",scanint(&ptr)); /* RTF: Space after (the default is 0). */
                if(x2&0x0008) rtf_printf("\\sl%ld",scanint(&ptr)); /* RTF: Space between lines. */
                if(x2&0x0010) rtf_printf("\\li%ld",scanint(&ptr)); /* RTF: Left indent (the default is 0). */
                if(x2&0x0020) rtf_printf("\\ri%ld",scanint(&ptr)); /* RTF: Right indent (the default is 0). */
                if(x2&0x0040) rtf_printf("\\fi%ld",scanint(&ptr)); /* RTF: First-line indent (the default is 0). */
                if(x2&0x0100)
                {
                    x1=(unsigned char)*ptr++;
                    if(x1&1) rtf_puts("\\box"); /* RTF: Border around the paragraph (box paragraph). */
                    if(x1&2) rtf_puts("\\brdrt"); /* RTF: Border top. */
                    if(x1&4) rtf_puts("\\brdrl"); /* RTF: Border left. */
                    if(x1&8) rtf_puts("\\brdrb"); /* RTF: Border bottom. */
                    if(x1&0x10) rtf_puts("\\brdrr"); /* RTF: Border right. */
                    if(x1&0x20) rtf_puts("\\brdrth"); else rtf_puts("\\brdrs"); /* RTF: Double-thickness border. else RTF: Single-thickness border. */
                    if(x1&0x40) rtf_puts("\\brdrdb"); /* RTF: Double border. */
                    ptr+=2;
                }
                if(x2&0x0200)
                {
                    y1=scanint(&ptr);
                    while(y1-->0)
                    {
                        x1=scanword(&ptr);
                        if(x1&0x4000)
                        {
                            switch(scanword(&ptr))
                            {
                                case 1:
                                    rtf_puts("\\tqr"); /* RTF: Flush-right tab. */
                                    break;
                                case 2:
                                    rtf_puts("\\tqc"); /* RTF: Centered tab. */
                                    break;
                            }
                        }
                        rtf_printf("\\tx%ld",(x1&0x3FFF)*scaling-rounderr); /* RTF: Tab position in twips from the left margin. */
                    }
                }

                while(1) /* ptr<LinkData1+TopicLink.DataLen1-sizeof(TOPICLINK)&&str<end) */
                {
                    if(*str&&fontset>=0&&fontset<fonts&&font&&font[fontset].SmallCaps) strlwr(str);
                    do
                    {
                        if(*str)
                        {
                            if(*str=='<') {
                                html_puts("&lt;");
                            } else if(*str=='>') {
                                html_puts("&gt;");
                            } else  if(*str=='"') {
                                html_puts("&quot;");
                            } else if(isprint((unsigned char)*str)){
                                html_putc(*str);
                            } else {
                                // rtf_printf("\\'%02x",(unsigned char)*str);
                                html_printf("&#%d;", (unsigned char)*str);
                            }
                        }
                        if(ActualTopicOffset<MaxTopicOffset) ActualTopicOffset++;
                    }
                    while(*str++);
                    if((unsigned char)ptr[0]==0xFF)
                    {
                        ptr++;
                        break;
                    }
                    else switch((unsigned char)ptr[0])
                    {
                        case 0x20: /* vfld MVB */
                            if(*(legacy_long *)(ptr+1))
                            {
                                rtf_printf("\\{vfld%ld\\}",*(legacy_long *)(ptr+1));
                            }
                            else
                            {
                                rtf_puts("\\{vfld\\}");
                            }
                            ptr+=5;
                            break;
                        case 0x21: /* dtype MVB */
                            if(*(int16_t *)(ptr+1))
                            {
                                rtf_printf("\\{dtype%d\\}",*(int16_t *)(ptr+1));
                            }
                            else
                            {
                                rtf_puts("\\{dtype\\}");
                            }
                            ptr+=3;
                            break;
                        case 0x80: /* font change */
                            ChangeHTMLFont(__html_output,fontset=*(int16_t *)(ptr+1),ul,uldb);
                            ptr+=3;
                            break;
                        case 0x81:
                            html_puts("<br>"); // rtf_puts("\\line\n"); /* RTF: Required line break (no paragraph break). */
                            ptr++;
                            break;
                        case 0x82:
                            if(TopicLink.RecordType==TL_TABLE)
                            {
                                if((unsigned char)ptr[1]!=0xFF)
                                {
                                    rtf_puts("\n\\par\\intbl ");
                                }
                                else if(*(int16_t *)(ptr+2)==-1)
                                {
                                    rtf_puts("\\cell\\intbl\\row\n");
                                }
                                else if(*(int16_t *)(ptr+2)==lastcol)
                                {
                                    rtf_puts("\\par\\pard ");
                                }
                                else
                                {
                                    rtf_puts("\\cell\\pard ");
                                }
                            }
                            else
                            {
                                // rtf_puts("\n\\par "); /* RTF: End of paragraph. */
                            }
                            ptr++;
                            break;
                        case 0x83:
                            fputs("&#9;", __html_output); // rtf_puts("\\tab "); /* RTF: Tab character. */
                            ptr++;
                            break;
                        case 0x86:
                            x3=(unsigned char)*ptr++;
                            x1=*ptr++;
                            if(x1==0x05) cmd="ewc"; else cmd="bmc";
                            goto picture;
                        case 0x87:
                            x3=(unsigned char)*ptr++;
                            x1=*ptr++;
                            if(x1==0x05) cmd="ewl"; else cmd="bml";
                            goto picture;
                        case 0x88:
                            x3=(unsigned char)*ptr++;
                            x1=*ptr++;
                            if(x1==0x05) cmd="ewr"; else cmd="bmr";
                            goto picture;
                        picture:
                            l1=scanlong(&ptr);
                            switch(x1)
                            {
                                case 0x22: /* HC31 */
                                    ActualTopicOffset+=scanword(&ptr); /* number of hotspots in picture */
                                    if(ActualTopicOffset>MaxTopicOffset) ActualTopicOffset=MaxTopicOffset;
                                    /* fall thru */
                                case 0x03: /* HC30 */
                                    x1=((uint16_t *)ptr)[0];
                                    switch(x1)
                                    {
                                        case 1:
                                            while(nextbitmap<extensions&&extension[nextbitmap]<0x10) nextbitmap++;
                                            if(nextbitmap>=extensions)
                                            {
                                                error("Bitmap never saved");
                                                break;
                                            }
                                            x2=nextbitmap++;
                                            goto other;
                                        case 0:
                                            x2=((uint16_t *)ptr)[1];
                                        other:
                                            switch(x3)
                                            {
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
                                    if(ptr[6]=='!')
                                    {
                                        rtf_printf("\\{button %s\\}",ptr+7);
                                    }
                                    else if(ptr[6]=='*')
                                    {
                                        char *plus;
                                        legacy_int n,c1,c2;
                                        
                                        sscanf(ptr+7,"%d,%d,%n",&c1,&c2,&n);
                                        plus=strchr(ptr+7+n,'+');
                                        if((c1&0xFFF5)!=0x8400) fprintf(stderr,"mci c1=%04x\n",c1);
                                        rtf_puts("\\{mci");
                                        if(cmd[2]=='r') rtf_puts("_right");
                                        if(cmd[2]=='l') rtf_puts("_left");
                                        if(c2==1) rtf_puts(" REPEAT");
                                        if(c2==2) rtf_puts(" PLAY");
                                        if(!plus) rtf_puts(" EXTERNAL");
                                        if(c1&8) rtf_puts(" NOMENU");
                                        if(c1&2) rtf_puts(" NOPLAYBAR");
                                        rtf_printf(",%s\\}\n",plus?plus+1:ptr+7+n);
                                    }
                                    else
                                    {
                                        rtf_printf("\\{%s %s\\}",cmd,ptr+6);
                                    }
                                    break;
                            }
                            ptr+=l1;
                            break;
                        case 0x89: /* end of hotspot */
                            ChangeHTMLFont(__html_output,fontset,ul=FALSE,uldb=FALSE);
                            ptr++;
                            break;
                        case 0xC8: /* macro */
                            ChangeHTMLFont(__html_output,fontset,FALSE,uldb=TRUE);
                            ptr+=*(int16_t *)(ptr+1)+3;
                            break;
                        case 0xCC: /* macro without font change */
                            ChangeHTMLFont(__html_output,fontset,FALSE,uldb=TRUE);
                            ptr+=*(int16_t *)(ptr+1)+3;
                            break;
                        case 0xE0: /* popup jump HC30 */
                            ChangeHTMLFont(__html_output,fontset,ul=TRUE,FALSE);
                            goto label0;
                        case 0xE1: /* topic jump HC30 */
                            ChangeHTMLFont(__html_output,fontset,FALSE,uldb=TRUE);
                        label0:
                            ptr+=5;
                            break;
                        case 0xE2: /* popup jump HC31 */
                            ChangeHTMLFont(__html_output,fontset,ul=TRUE,FALSE);
                            goto label1;
                        case 0xE3: /* topic jump HC31 */
                            ChangeHTMLFont(__html_output,fontset,FALSE,uldb=TRUE);
                        label1:
                            ptr+=5;
                            break;
                        case 0xE6: /* popup jump without font change */
                            ChangeHTMLFont(__html_output,fontset,ul=TRUE,FALSE);
                            goto label2;
                        case 0xE7: /* topic jump without font change */
                            ChangeHTMLFont(__html_output,fontset,FALSE,uldb=TRUE);
                        label2:
                            ptr+=5;
                            break;
                        case 0xEA: /* popup jump into external file */
                        case 0xEE:
                            ChangeHTMLFont(__html_output,fontset,ul=TRUE,FALSE);
                            goto label3;
                        case 0xEB: /* topic jump into external file / secondary window */
                        case 0xEF:
                            ChangeHTMLFont(__html_output,fontset,FALSE,uldb=TRUE);
                        label3:
                            ptr+=*(int16_t *)(ptr+1)+3;
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
        if(LinkData1) free(LinkData1);
        if(LinkData2) free(LinkData2);
        if(before31)
        {
            TopicPos+=TopicLink.NextBlock;
        }
        else
        {
            TopicOffset=NextTopicOffset(TopicOffset,TopicLink.NextBlock,TopicPos);
            TopicPos=TopicLink.NextBlock;
        }
    }
    
#ifdef dev_null
    fclose(dev_null);
#endif
    
    return TRUE;
}
