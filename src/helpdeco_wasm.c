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

#if defined(__EMSCRIPTEN__)
#include <emscripten.h>
#endif
#include "helpdeco.h"

const char *get_version() { return "2.1.4"; }

extern BOOL mvp;

void initialize(void) {
    size_t i;
    
    /* initialize hash value coding oldtable */
    memset(oldtable,0,sizeof(oldtable));
    for(i=0;i<9;i++) oldtable['1'+i]=i+1;
    oldtable['0']=10;
    oldtable['.']=12;
    oldtable['_']=13;
    for(i=0;i<26;i++) oldtable['A'+i]=oldtable['a'+i]=17+i;
    
    prefixhash[0]=0;
    for(i=1;prefix[i];i++) prefixhash[i]=hash(prefix[i]);
}

const char *render(char *data, size_t len, const char *path) {
    char drive[_MAX_DRIVE];
    char dir[PATH_MAX];
       
    FILE *f = fopen(path, "w+");
    if(!f) {
        fprintf(stderr, "Could not open %s to write out buffer!\n", path);
        return NULL;
    }
    fprintf(stderr, "Writing %ld bytes to %s!\n", len, path);
    fwrite(data, len, 1, f);
    fflush(f);
    fseek(f, 0, SEEK_SET);
    
    initialize();
    
    _splitpath(path,drive,dir,name,ext);
    if(ext[0]=='\0') strcpy(ext,".hlp");
    mvp=ext[1]=='M'||ext[1]=='m';
    _makepath(HelpFileName,drive,dir,name,ext);
    
    if(!HelpDeCompile(f,NULL,8,NULL,0))
    {
        fprintf(stderr,"%s isn't a valid WinHelp file ! (render)\n",HelpFileName);
    }
    
    my_fclose(f);
    
    return 0;
}
