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

const char *render(char *data, size_t len, const char *path) {
    char drive[_MAX_DRIVE];
    char dir[PATH_MAX];
    
    ctx = helpdeco_make_ctx();
    ctx->opt_overwrite = TRUE;
    ctx->opt_interactive = TRUE;
       
    FILE *f = fopen(path, "w+");
    if(!f) {
        fprintf(stderr, "Could not open %s to write out buffer!\n", path);
        return NULL;
    }
    fprintf(stderr, "Writing %ld bytes to %s!\n", len, path);
    fwrite(data, len, 1, f);
    fflush(f);
    fseek(f, 0, SEEK_SET);
    
    _splitpath(path,drive,dir,ctx->name,ctx->ext);
    if(ctx->ext[0]=='\0') strcpy(ctx->ext,".hlp");
    ctx->mvp=ctx->ext[1]=='M'||ctx->ext[1]=='m';
    _makepath(ctx->filename,drive,dir,ctx->name,ctx->ext);
    
    if(!HelpDeCompile(f,NULL,8,NULL,0))
    {
        fprintf(stderr,"%s isn't a valid WinHelp file ! (render)\n",ctx->filename);
    } else {
        fprintf(stderr, "Done, cleaning up!\n");
    }
    
    my_fclose(f);
    helpdeco_free_ctx(ctx);
    ctx = NULL;
    
    return 0;
}
