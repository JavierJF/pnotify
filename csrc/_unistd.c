#include <_unistd.h>
#include <stdlib.h>

int* store_fd(int fd)
{
    int* pfd = (int*)malloc(sizeof(int));
    *pfd = fd;

    return pfd;
}

void close_fd(int* fd)
{
    close(*fd);
    free(fd);
}
