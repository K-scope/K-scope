\#include <stddef.h>
\#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#endif    /* __cplusplus */

/**
 * get second time by gettimeofday
 * @param  second[out]      second time
 * @param  ierr[out]        error no
 * @return    second time
 */
double gettimeofday_seconds(double *second, int *ierr)
{
    struct timeval tp;
    *ierr = gettimeofday(&tp, NULL);
    *second = ((double)tp.tv_sec+(1.0e-6)*tp.tv_usec);
    return *second;
}

/**
 * gettimeofday
 * @return    get time (microseconds)
 */
unsigned long long gettimeofday_microseconds()
{
    struct timeval tp;
    int ierr;
    unsigned long long utime;

    utime = 0;
    ierr = gettimeofday(&tp, NULL);
    if (ierr == 0) {
        utime = tp.tv_sec*(1.0e+6) + tp.tv_usec;
    }

    return utime;
}

/**
 * gettick
 * @return    get tick (flops)
 */
unsigned long long gettick() {
    unsigned long long t=0;
#ifdef __sparcv9
    __asm__ __volatile__("rd %%tick, %0" : "=r" (t));
#elif __x86_64__
    unsigned int eax, edx;
    __asm__ volatile("rdtsc" : "=a" (eax), "=d" (edx));
    t = ((unsigned long long)eax) | (((unsigned long long)edx) << 32);
#endif
    return t;
}

#ifdef __cplusplus
}
#endif    /* __cplusplus */

