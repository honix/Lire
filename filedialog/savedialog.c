#include "nfd.h"

#include <stdio.h>
#include <stdlib.h>

/* this test should compile on all supported platforms */

int main( void )
{
    nfdchar_t *savePath = NULL;
    nfdresult_t result = NFD_SaveDialog( NULL, NULL, &savePath );
    if ( result == NFD_OKAY )
    {
        puts(savePath);
        free(savePath);
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
