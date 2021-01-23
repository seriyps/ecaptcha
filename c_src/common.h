#include <stdint.h>

/* Max font name length */
#define MAX_NAME 128

/* "bytecode" to encode font pixels */
#define BC_NEWLINE -126
#define BC_EOG -127                /* end of glyph */
#define BC_NUM_SPACES(n) (-1 * n)  /* number of consecutive empty pixels in a row */
#define BC_GREY_PIXEL(n) (127 - n / 2)   /* 0-255 greyscale pixel, mapped to 0-127 space */

typedef int8_t* Glyph;

typedef struct Font {
    const char *name;
    const char *alphabet;
    int height;
    int baseline;
    int8_t const **glyphs;
} Font;
