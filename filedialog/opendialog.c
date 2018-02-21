#include "nfd.h"

#include <stdio.h>
#include <stdlib.h>

/* this test should compile on all supported platforms */

int main( void )
{
    nfdchar_t *outPath = NULL;
    nfdresult_t result = NFD_OpenDialog( "lire", NULL, &outPath );
    if ( result == NFD_OKAY )
    {
        puts(outPath);
        free(outPath);
    }
    else if ( result == NFD_CANCEL )
    {
        return 1;
    }
    else
    {
        return -1;
    }

    return 0;
}
