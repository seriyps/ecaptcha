// http://github.com/ITikhonov/captcha
#include <stdint.h>
#include <string.h>
#include "erl_nif.h"
#include "font.h"

#define WIDTH 200
#define HIGH 70
#define AREA WIDTH * HIGH

static int8_t *lt[];

static const int8_t sw[WIDTH]=
    {
     0, 4, 8, 12, 16, 20, 23, 27, 31, 35, 39, 43, 47, 50, 54, 58, 61, 65, 68, 71, 75, 78, 81, 84,
     87, 90, 93, 96, 98, 101, 103, 105, 108, 110, 112, 114, 115, 117, 119, 120, 121, 122, 123, 124,
     125, 126, 126, 127, 127, 127, 127, 127, 127, 127, 126, 126, 125, 124, 123, 122, 121, 120, 119,
     117, 115, 114, 112, 110, 108, 105, 103, 101, 98, 96, 93, 90, 87, 84, 81, 78, 75, 71, 68, 65,
     61, 58, 54, 50, 47, 43, 39, 35, 31, 27, 23, 20, 16, 12, 8, 4, 0, -4, -8, -12, -16, -20, -23,
     -27, -31, -35, -39, -43, -47, -50, -54, -58, -61, -65, -68, -71, -75, -78, -81, -84, -87, -90,
     -93, -96, -98, -101, -103, -105, -108, -110, -112, -114, -115, -117, -119, -120, -121, -122,
     -123, -124, -125, -126, -126, -127, -127, -127, -127, -127, -127, -127, -126, -126, -125,
     -124, -123, -122, -121, -120, -119, -117, -115, -114, -112, -110, -108, -105, -103, -101, -98,
     -96, -93, -90, -87, -84, -81, -78, -75, -71, -68, -65, -61, -58, -54, -50, -47, -43, -39, -35,
     -31, -27, -23, -20, -16, -12, -8, -4};


#define MAX(x,y) ((x>y)?(x):(y))

static int letter(unsigned char n, int pos, unsigned char im[AREA], unsigned char swr[WIDTH],
                  uint8_t s1, uint8_t s2) {
  int8_t *p=lt[n];
  unsigned char *r=im+WIDTH*16+pos;
  unsigned char *i=r;
  int sk1=s1+pos;
  int sk2=s2+pos;
  int mpos=pos;
  int row=0;
  for(;*p!=-101;p++) {
    if(*p<0) {
      if(*p==-100) { r+=WIDTH; i=r; sk1=s1+pos; row++; continue; }
      i+=-*p;
      continue;
    }

    if(sk1>=WIDTH) sk1=sk1%WIDTH;
    int skew=sw[sk1]/16;
    sk1+=(swr[pos+i-r]&0x1)+1;

    if(sk2>=WIDTH) sk2=sk2%WIDTH;
    int skewh=sw[sk2]/HIGH;
    sk2+=(swr[row]&0x1);

    unsigned char *x=i+skew*WIDTH+skewh;
    mpos=MAX(mpos,pos+i-r);

    if((x-im)<AREA) *x=(*p)<<4;
    i++;
  }
  return mpos + 3;
}

static void line(unsigned char im[AREA], unsigned char swr[WIDTH], uint8_t s1) {
  int x;
  int sk1=s1;
  for(x=0;x<WIDTH - 1;x++) {
    if(sk1>=WIDTH) sk1=sk1%WIDTH;
    int skew=sw[sk1]/20;
    sk1+=swr[x]&(0x3+1);
    unsigned char *i= im+(WIDTH*(45+skew)+x);
    i[0]=0; i[1]=0; i[WIDTH]=0; i[WIDTH + 1]=0;
  }
}

#define NDOTS 100

static void dots(unsigned char im[AREA], uint32_t* dr) {
  int n;
  for(n = 0; n < NDOTS; n++) {
    uint32_t v=dr[n];
    unsigned char *i=im+v%(WIDTH * (HIGH - 3));

    i[0]=0xff;
    i[1]=0xff;
    i[2]=0xff;
    i[WIDTH]=0xff;
    i[WIDTH + 1]=0xff;
    i[WIDTH + 2]=0xff;
  }
}

#define NREVDOTS 20
#define MAX_DOT_SIZE 3

static void reverse_dots(unsigned char im[AREA], uint32_t* dr) {
  int n;
  for(n=0;n<NREVDOTS;n++) {
    uint32_t v=dr[n];
    unsigned char *pos = im + v % (WIDTH * (HIGH - MAX_DOT_SIZE));
    /* unsigned char size = (v / (WIDTH * (HIGH - MAX_DOT_SIZE))) % MAX_DOT_SIZE + 1; */
    unsigned char size = (v >> 30) % MAX_DOT_SIZE + 1;
    int x, y;

    for(x=0; x<size; x++) {
        for(y=0; y<size; y++) {
            pos[WIDTH * x + y]=(0xff - pos[WIDTH * x + y]);
        }
    }
  }
}

static void blur(unsigned char im[AREA]) {
  unsigned char *i=im;
  int x,y;
  for(y=0;y<(HIGH - 2);y++) {
      for(x=0;x<(WIDTH - 2);x++) {
      unsigned int c11=*i,c12=i[1],c21=i[WIDTH],c22=i[WIDTH + 1];
      *i++=((c11+c12+c21+c22)/4);
    }
  }
}

#define VISIBLE(V) (V < 0xf0)

static void filter(unsigned char im[AREA]) {
  unsigned char om[AREA];
  unsigned char *i=im + WIDTH;
  unsigned char *o=om + WIDTH;

  memset(om,0xff,sizeof(om));

  while (i < (im + AREA - WIDTH)) {
      // ??B??
      // ?BiB?
      // ??B??
      if (VISIBLE(i[0]) && (VISIBLE(i[-1]) && VISIBLE(i[1]) &&
                            VISIBLE(i[0 - WIDTH]) && VISIBLE(i[WIDTH]))) {
      } else {
          o[0] = i[0];
      }

      i++;
      o++;
  }

  memmove(im,om,sizeof(om));
}

enum {
      OPT_LINE = 1,
      OPT_DOTS = 1 << 1,
      OPT_REVDOTS = 1 << 2,
      OPT_FILTER = 1 << 3,
      OPT_BLUR = 1 << 4
};

static void captcha(const unsigned char* rand, unsigned char im[AREA], const unsigned char* l,
                    int length, int opts) {
  unsigned char swr[WIDTH];
  uint8_t s1,s2;
  uint32_t dr[NDOTS];
  uint32_t rdr[NREVDOTS];

  memcpy(swr, rand, WIDTH);
  memcpy(dr, rand+=WIDTH, sizeof(dr));
  memcpy(dr, rand+=sizeof(dr), sizeof(rdr));
  memcpy(&s1, rand+=sizeof(rdr), 1);
  memcpy(&s2, rand+=1, 1);
  memset(im,0xff,AREA);
  s1=s1&0x7f;
  s2=s2&0x3f;

  int x;
  int p=30;
  for(x=0;x<length;x++){
    p=letter(l[x]-'a',p,im,swr,s1,s2);
  }

  if (opts & OPT_LINE) {
    line(im,swr,s1);
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


static ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM
mk_pixels(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM opts_head, opts_tail, img_data_bin;
    ErlNifBinary chars_bin, rand_bin;
    int opts = 0;
    char opt_name[15];

    unsigned char* img;

    if( argc != 3 ) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_binary(env, argv[0], &chars_bin)) {
        return mk_error(env, "chars_not_binary");
    }
    if(chars_bin.size < 1 || chars_bin.size > 7) {
        return mk_error(env, "wrong_chars_length");
    }
    for(int i = 0; i < chars_bin.size; i++) {
        if (chars_bin.data[i] > 'z' || chars_bin.data[i] < 'a') {
            return mk_error(env, "invalid_character");
        }
    }

    if(!enif_inspect_binary(env, argv[1], &rand_bin)) {
        return mk_error(env, "bad_random");
    }
    if(rand_bin.size < (200 + (NDOTS + NREVDOTS) * sizeof(uint32_t) + 2)) {
        return mk_error(env, "small_rand_binary");
    }

    if(!enif_is_list(env, argv[2])) {
        return mk_error(env, "opts_not_list");
    }

    opts_tail = argv[2];
    while (enif_get_list_cell(env, opts_tail, &opts_head, &opts_tail)) {
        if(!enif_get_atom(env, opts_head, opt_name, sizeof(opt_name), ERL_NIF_LATIN1)) {
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

    captcha(rand_bin.data, img, chars_bin.data, chars_bin.size, opts);
    return img_data_bin;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"pixels", 3, mk_pixels}
};

ERL_NIF_INIT(ecaptcha_nif, nif_funcs, NULL, NULL, &upgrade, NULL);
