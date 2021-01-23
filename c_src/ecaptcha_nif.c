// Imported from http://github.com/ITikhonov/captcha and updated
// by Sergey Prokhorov <me@seriyps.ru>

#include "erl_nif.h"
#include "fonts.h"
#include <stdint.h>
#include <string.h>

#define WIDTH 200
#define HIGHT 70
#define AREA WIDTH *HIGHT
#define MAX_CHARS 7

#define NDOTS 100

#define NREVDOTS 20
#define MAX_REVDOT_SIZE 3

#define AT(_im, _x, _y) (_im[(_y)*WIDTH + (_x)])
#define MAX(x, y) ((x > y) ? (x) : (y))
#define VISIBLE(V) (V < 0xf0)

static const int8_t sw[WIDTH] = {
    0,    4,    8,    12,   16,   20,   23,   27,   31,   35,   39,   43,
    47,   50,   54,   58,   61,   65,   68,   71,   75,   78,   81,   84,
    87,   90,   93,   96,   98,   101,  103,  105,  108,  110,  112,  114,
    115,  117,  119,  120,  121,  122,  123,  124,  125,  126,  126,  127,
    127,  127,  127,  127,  127,  127,  126,  126,  125,  124,  123,  122,
    121,  120,  119,  117,  115,  114,  112,  110,  108,  105,  103,  101,
    98,   96,   93,   90,   87,   84,   81,   78,   75,   71,   68,   65,
    61,   58,   54,   50,   47,   43,   39,   35,   31,   27,   23,   20,
    16,   12,   8,    4,    0,    -4,   -8,   -12,  -16,  -20,  -23,  -27,
    -31,  -35,  -39,  -43,  -47,  -50,  -54,  -58,  -61,  -65,  -68,  -71,
    -75,  -78,  -81,  -84,  -87,  -90,  -93,  -96,  -98,  -101, -103, -105,
    -108, -110, -112, -114, -115, -117, -119, -120, -121, -122, -123, -124,
    -125, -126, -126, -127, -127, -127, -127, -127, -127, -127, -126, -126,
    -125, -124, -123, -122, -121, -120, -119, -117, -115, -114, -112, -110,
    -108, -105, -103, -101, -98,  -96,  -93,  -90,  -87,  -84,  -81,  -78,
    -75,  -71,  -68,  -65,  -61,  -58,  -54,  -50,  -47,  -43,  -39,  -35,
    -31,  -27,  -23,  -20,  -16,  -12,  -8,   -4};

static int letter(Glyph glyph, int pos, unsigned char im[AREA],
                  unsigned char swr[WIDTH], uint8_t s1, uint8_t s2) {
  unsigned char *r = im + WIDTH * 16 + pos;
  unsigned char *i = r;
  int sk1 = s1 + pos;
  int sk2 = s2 + pos;
  int mpos = pos;
  int row = 0;
  for (; *glyph != BC_EOG; glyph++) {
    if (*glyph < 0) {
      if (*glyph == BC_NEWLINE) {
        r += WIDTH;
        i = r;
        sk1 = s1 + pos;
        row++;
        continue;
      }
      i += -*glyph;
      continue;
    }

    if (sk1 >= WIDTH)
      sk1 = sk1 % WIDTH;
    int skew = sw[sk1] / 16;
    sk1 += (swr[pos + i - r] & 0x1) + 1;

    if (sk2 >= WIDTH)
      sk2 = sk2 % WIDTH;
    int skewh = sw[sk2] / HIGHT;
    sk2 += (swr[row] & 0x1);

    unsigned char *x = i + skew * WIDTH + skewh;
    mpos = MAX(mpos, pos + i - r);

    if ((x - im) < AREA)
      *x = (*glyph) * 2;
    i++;
  }
  return mpos + 3;
}

static void line(unsigned char im[AREA], unsigned char swr[WIDTH], uint8_t s1) {
  int x;
  int sk1 = s1;
  for (x = 0; x < WIDTH - 1; x++) {
    if (sk1 >= WIDTH)
      sk1 = sk1 % WIDTH;
    int skew = sw[sk1] / 20;
    sk1 += swr[x] & (0x3 + 1);
    unsigned char *i = im + (WIDTH * (45 + skew) + x);
    AT(i, 0, 0) = 0;
    AT(i, 1, 0) = 0;
    AT(i, 0, 1) = 0;
    AT(i, 1, 1) = 0;
  }
}

static void dots(unsigned char im[AREA], uint32_t *dr) {
  int n;
  for (n = 0; n < NDOTS; n++) {
    uint32_t v = dr[n];
    unsigned char *pos = im + v % (WIDTH * (HIGHT - 3));
    int x, y;
    // 3x3 box with top-left corner at `pos`
    for (y = 0; y < 3; y++) {
      for (x = 0; x < 3; x++) {
        AT(pos, x, y) = 0xff;
      }
    }
  }
}

static void reverse_dots(unsigned char im[AREA], uint32_t *dr) {
  int n;
  for (n = 0; n < NREVDOTS; n++) {
    uint32_t v = dr[n];
    unsigned char *pos = im + v % (WIDTH * (HIGHT - MAX_REVDOT_SIZE));
    unsigned char size = (v >> 30) % MAX_REVDOT_SIZE + 1;
    int x, y;
    // box of `size` with top-left corner at `pos`
    for (y = 0; y < size; y++) {
      for (x = 0; x < size; x++) {
        AT(pos, x, y) = (0xff - AT(pos, x, y));
      }
    }
  }
}

static void blur(unsigned char im[AREA]) {
  unsigned char *i = im;
  int x, y;
  for (y = 0; y < (HIGHT - 2); y++) {
    for (x = 0; x < (WIDTH - 2); x++) {
      unsigned int c11 = AT(i, 0, 0), c12 = AT(i, 1, 0), c21 = AT(i, 0, 1),
                   c22 = AT(i, 1, 1);
      *i++ = ((c11 + c12 + c21 + c22) / 4);
    }
  }
}

static void filter(unsigned char im[AREA]) {
  unsigned char om[AREA];
  unsigned char *i = &AT(im, 0, 1);
  unsigned char *o = &AT(om, 0, 1);

  memset(om, 0xff, sizeof(om));

  while (i < &AT(im, WIDTH, HIGHT - 1)) {
    // ??B??
    // ?BiB?
    // ??B??
    if (!(VISIBLE(AT(i, 0, 0)) && VISIBLE(AT(i, -1, 0)) && // left
          VISIBLE(AT(i, 1, 0)) &&                          // right
          VISIBLE(AT(i, 0, -1)) &&                         // above
          VISIBLE(AT(i, 0, 1)))                            // below
    ) {
      o[0] = i[0];
    }

    i++;
    o++;
  }

  memmove(im, om, sizeof(om));
}

enum {
  OPT_LINE = 1,
  OPT_DOTS = 1 << 1,
  OPT_REVDOTS = 1 << 2,
  OPT_FILTER = 1 << 3,
  OPT_BLUR = 1 << 4
};

static void captcha(Glyph glyphs[], int length, const unsigned char *rand,
                    unsigned char im[AREA], int opts) {
  unsigned char swr[WIDTH];
  uint8_t s1, s2, s3;
  uint32_t dr[NDOTS];
  uint32_t rdr[NREVDOTS];

  memcpy(swr, rand, WIDTH);
  memcpy(dr, rand += WIDTH, sizeof(dr));
  memcpy(rdr, rand += sizeof(dr), sizeof(rdr));
  memcpy(&s1, rand += sizeof(rdr), 1);
  memcpy(&s2, rand += 1, 1);
  memcpy(&s3, rand += 1, 1);
  memset(im, 0xff, AREA);
  s1 = s1 & 0x7f;
  s2 = s2 & 0x3f;

  int x;
  int p = 5 + s3 % 25;
  for (x = 0; x < length; x++) {
    p = letter(glyphs[x], p, im, swr, s1, s2);
  }

  if (opts & OPT_LINE) {
    line(im, swr, s1);
  }
  if (opts & OPT_DOTS) {
    dots(im, dr);
  }
  if (opts & OPT_REVDOTS) {
    reverse_dots(im, dr);
  }
  if (opts & OPT_FILTER) {
    filter(im);
  }
  if (opts & OPT_BLUR) {
    blur(im);
  }
}

/* ERLANG */

static ERL_NIF_TERM mk_atom(ErlNifEnv *env, const char *atom) {
  ERL_NIF_TERM ret;

  if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
    return enif_make_atom(env, atom);
  }

  return ret;
}

static ERL_NIF_TERM mk_error(ErlNifEnv *env, const char *mesg) {
  return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM mk_pixels(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM opts_head, opts_tail, img_data_bin;
  ErlNifBinary font_name_bin, chars_bin, rand_bin;
  Font *font = NULL;
  Font **fs;
  char *alphabet_idx;
  Glyph glyphs[MAX_CHARS];
  int opts = 0;
  char opt_name[15];

  unsigned char *img;

  if (argc != 4) {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[0], &font_name_bin)) {
    return mk_error(env, "font_name_not_binary");
  }
  for (fs = fonts; *fs != NULL; fs++) {
    if (strncmp((*fs)->name, (char *)font_name_bin.data, font_name_bin.size) ==
        0) {
      font = *fs;
      break;
    }
  }
  if (!font) {
    return mk_error(env, "font_not_found");
  }

  if (!enif_inspect_binary(env, argv[1], &chars_bin)) {
    return mk_error(env, "chars_not_binary");
  }
  if (chars_bin.size < 1 || chars_bin.size > MAX_CHARS) {
    return mk_error(env, "wrong_chars_length");
  }
  for (int i = 0; i < chars_bin.size; i++) {
    /* assert all characters are in the alphabet; lookup glyphs and pass array
     * of glyphs */
    alphabet_idx = strchr(font->alphabet, chars_bin.data[i]);
    if (!alphabet_idx) {
      return mk_error(env, "character_out_of_alphabet_range");
    }
    glyphs[i] = (Glyph)font->glyphs[alphabet_idx - font->alphabet];
  }

  if (!enif_inspect_binary(env, argv[2], &rand_bin)) {
    return mk_error(env, "bad_random");
  }
  if (rand_bin.size < (200 + (NDOTS + NREVDOTS) * sizeof(uint32_t) + 2)) {
    return mk_error(env, "small_rand_binary");
  }

  if (!enif_is_list(env, argv[3])) {
    return mk_error(env, "opts_not_list");
  }

  opts_tail = argv[3];
  while (enif_get_list_cell(env, opts_tail, &opts_head, &opts_tail)) {
    if (!enif_get_atom(env, opts_head, opt_name, sizeof(opt_name),
                       ERL_NIF_LATIN1)) {
      return mk_error(env, "non_atom_option");
    }
    if (!strcmp(opt_name, "line")) {
      opts |= OPT_LINE;
    } else if (!strcmp(opt_name, "blur")) {
      opts |= OPT_BLUR;
    } else if (!strcmp(opt_name, "filter")) {
      opts |= OPT_FILTER;
    } else if (!strcmp(opt_name, "dots")) {
      opts |= OPT_DOTS;
    } else if (!strcmp(opt_name, "reverse_dots")) {
      opts |= OPT_REVDOTS;
    } else {
      return mk_error(env, "unknown_option");
    }
  }
  img = enif_make_new_binary(env, AREA, &img_data_bin);

  captcha(glyphs, chars_bin.size, rand_bin.data, img, opts);
  return img_data_bin;
}

static int upgrade(ErlNifEnv *env, void **priv, void **old_priv,
                   ERL_NIF_TERM info) {
  return 0; // NIF is stateless
}

static ErlNifFunc nif_funcs[] = {{"pixels", 4, mk_pixels}};

ERL_NIF_INIT(ecaptcha_nif, nif_funcs, NULL, NULL, &upgrade, NULL);
