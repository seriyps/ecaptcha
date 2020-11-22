// http://github.com/ITikhonov/captcha
// https://github.com/huacnlee/rucaptcha
const int gifsize;
void captcha(const unsigned char* rand, unsigned char im[70*200], unsigned char l[8],
             int length, int i_line, int i_blur, int i_filter, int i_dots);
void makegif(unsigned char im[70*200], unsigned char gif[gifsize], int style);

#include <stdint.h>
#include <string.h>
#include "erl_nif.h"
#include "font.h"
#include "colors.h"

static int8_t *lt[];
const int gifsize=17646;

void makegif(unsigned char im[70*200], unsigned char gif[gifsize], int color_idx) {
  // tag ; widthxheight ; GCT:0:0:7 ; bgcolor + aspect // GCT
  // Image Separator // left x top // widthxheight // Flags
  // LZW code size
  memcpy(gif,colors[color_idx],13+48+10+1);

  int x,y;
  unsigned char *i=im;
  unsigned char *p=gif+13+48+10+1;
  for(y=0;y<70;y++) {
    *p++=250; // Data length 5*50=250
    for(x=0;x<50;x++)
    {
      unsigned char a=i[0]>>4,b=i[1]>>4,c=i[2]>>4,d=i[3]>>4;

      p[0]=16|(a<<5);     // bbb10000
      p[1]=(a>>3)|64|(b<<7);  // b10000xb
      p[2]=b>>1;      // 0000xbbb
      p[3]=1|(c<<1);    // 00xbbbb1
      p[4]=4|(d<<3);    // xbbbb100
      i+=4;
      p+=5;
    }
  }

  // Data length // End of LZW (b10001) // Terminator // GIF End
  memcpy(gif+gifsize-4,"\x01" "\x11" "\x00" ";",4);
}

static const int8_t sw[200]=
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

static int letter(int n, int pos, unsigned char im[70*200], unsigned char swr[200],
                  uint8_t s1, uint8_t s2) {
  int8_t *p=lt[n];
  unsigned char *r=im+200*16+pos;
  unsigned char *i=r;
  int sk1=s1+pos;
  int sk2=s2+pos;
  int mpos=pos;
  int row=0;
  for(;*p!=-101;p++) {
    if(*p<0) {
      if(*p==-100) { r+=200; i=r; sk1=s1+pos; row++; continue; }
      i+=-*p;
      continue;
    }

    if(sk1>=200) sk1=sk1%200;
    int skew=sw[sk1]/16;
    sk1+=(swr[pos+i-r]&0x1)+1;

    if(sk2>=200) sk2=sk2%200;
    int skewh=sw[sk2]/70;
    sk2+=(swr[row]&0x1);

    unsigned char *x=i+skew*200+skewh;
    mpos=MAX(mpos,pos+i-r);

    if((x-im)<70*200) *x=(*p)<<4;
    i++;
  }
  return mpos + 3;
}

static void line(unsigned char im[70*200], unsigned char swr[200], uint8_t s1) {
  int x;
  int sk1=s1;
  for(x=0;x<199;x++) {
    if(sk1>=200) sk1=sk1%200;
    int skew=sw[sk1]/20;
    sk1+=swr[x]&(0x3+1);
    unsigned char *i= im+(200*(45+skew)+x);
    i[0]=0; i[1]=0; i[200]=0; i[201]=0;
  }
}

#define NDOTS 200

static void dots(unsigned char im[70*200], uint32_t* dr) {
  int n;
  for(n=0;n<NDOTS;n++) {
    uint32_t v=dr[n];
    unsigned char *i=im+v%(200*67);

    i[0]=0xff;
    i[1]=0xff;
    i[2]=0xff;
    i[200]=0xff;
    i[201]=0xff;
    i[202]=0xff;
  }
}

static void blur(unsigned char im[70*200]) {
  unsigned char *i=im;
  int x,y;
  for(y=0;y<68;y++) {
    for(x=0;x<198;x++) {
      unsigned int c11=*i,c12=i[1],c21=i[200],c22=i[201];
      *i++=((c11+c12+c21+c22)/4);
    }
  }
}

static void filter(unsigned char im[70*200]) {
  unsigned char om[70*200];
  unsigned char *i=im;
  unsigned char *o=om;

  memset(om,0xff,sizeof(om));

  int x,y;
  for(y=0;y<70;y++) {
    for(x=4;x<200-4;x++) {
      if(i[0]>0xf0 && i[1]<0xf0) { o[0]=0; o[1]=0; }
      else if(i[0]<0xf0 && i[1]>0xf0) { o[0]=0; o[1]=0; }

      i++;
      o++;
    }
  }

  memmove(im,om,sizeof(om));
}

static const char *letters="abcdafahijklmnopqrstuvwxyz";

void captcha(const unsigned char* rand, unsigned char im[70*200], unsigned char l[8],
             int length, int i_line, int i_blur, int i_filter, int i_dots) {
  unsigned char swr[200];
  uint8_t s1,s2;
  uint32_t dr[NDOTS];

  memcpy(l, rand, length);
  memcpy(swr, rand+=length, 200);
  memcpy(dr, rand+=200, sizeof(dr));
  memcpy(&s1, rand+=sizeof(dr), 1);
  memcpy(&s2, rand+=1, 1);
  memset(im,0xff,200*70);
  s1=s1&0x7f;
  s2=s2&0x3f;

  int x;
  for(x=0;x<length;x++){
    l[x]%=25;
  }
  for(x=length;x<8;x++){
    l[length]=0;
  }
  int p=30;
  for(x=0;x<length;x++){
    p=letter(l[x],p,im,swr,s1,s2);
  }

  if (i_line == 1) {
    line(im,swr,s1);
  }
  if (i_dots == 1) {
      dots(im, dr);
  }
  if (i_blur == 1) {
    blur(im);
  }
  if (i_filter == 1) {
    filter(im);
  }

  for(x=0;x<length;x++){
    l[x]=letters[l[x]];
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
mk_captcha(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int as_gif)
{
    ERL_NIF_TERM opts_head, opts_tail, chars_bin, img_data_bin;
    ErlNifBinary rand_bin;
    int len, color, i_line = 0, i_blur = 0, i_filter = 0, i_dots = 0;
    char opt_name[8];

    unsigned char* chars;

    if( (as_gif && argc != 4) || (!as_gif && argc != 3) )
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[0], &len))
    {
        return mk_error(env, "length_not_integer");
    }
    if(len < 1 || len > 7) {
        return mk_error(env, "invalid_num_chars");
    }

    if(!enif_inspect_binary(env, argv[1], &rand_bin)) {
        return mk_error(env, "bad_random");
    }
    if(rand_bin.size < (len + 200 + NDOTS * sizeof(uint32_t) + 2)) {
        return mk_error(env, "small_rand_binary");
    }

    if(!enif_is_list(env, argv[2]))
    {
        return mk_error(env, "opts_not_list");
    }

    opts_tail = argv[2];
    while (enif_get_list_cell(env, opts_tail, &opts_head, &opts_tail)) {
        if(!enif_get_atom(env, opts_head, opt_name, sizeof(opt_name), ERL_NIF_LATIN1)) {
            return mk_error(env, "non_atom_opt");
        }
        if (!strcmp(opt_name, "line")) {
            i_line = 1;
        } else if (!strcmp(opt_name, "blur")) {
            i_blur = 1;
        } else if (!strcmp(opt_name, "filter")) {
            i_filter = 1;
        } else if (!strcmp(opt_name, "dots")) {
            i_dots = 1;
        } else {
            return mk_error(env, "unknown_option");
        }
    }
    if (as_gif) {
        if(!enif_get_int(env, argv[3], &color)) {
            return mk_error(env, "invalid_color");
        }
        if((color < 0) || (color > sizeof(colors))) {
            return mk_error(env, "invalid_color");
        }
    }
    chars = enif_make_new_binary(env, len, &chars_bin);
    if (as_gif) {
        unsigned char img[70*200];
        unsigned char* gif;
        gif = enif_make_new_binary(env, gifsize, &img_data_bin);

        captcha(rand_bin.data, img, chars, len, i_line, i_blur, i_filter, i_dots);
        makegif(img, gif, color);
        return enif_make_tuple2(env, chars_bin, img_data_bin);
    } else {
        unsigned char* img;
        img = enif_make_new_binary(env, 70*200, &img_data_bin);

        captcha(rand_bin.data, img, chars, len, i_line, i_blur, i_filter, i_dots);
        return enif_make_tuple2(env, chars_bin, img_data_bin);
    }

}

static ERL_NIF_TERM
mk_pixels(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return mk_captcha(env, argc, argv, 0);
}

static ERL_NIF_TERM
mk_gif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return mk_captcha(env, argc, argv, 1);
}

static ERL_NIF_TERM
pixels_as_gif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM gif_data_bin;
    ErlNifBinary im_bin;
    unsigned char* gif;
    int color;
    if( argc != 2 )
    {
        return enif_make_badarg(env);
    }
    if(!enif_inspect_binary(env, argv[0], &im_bin)) {
        return mk_error(env, "bad_image");
    }
    if(im_bin.size != (200 * 70)) {
        return mk_error(env, "wrong_pixels_size");
    }
    if(!enif_get_int(env, argv[1], &color)) {
        return mk_error(env, "invalid_color");
    }
    if((color < 0) || (color > sizeof(colors))) {
        return mk_error(env, "invalid_color");
    }
    gif = enif_make_new_binary(env, gifsize, &gif_data_bin);
    makegif(im_bin.data, gif, color);
    return gif_data_bin;
}

static ErlNifFunc nif_funcs[] = {
    {"pixels_nif", 3, mk_pixels},
    {"gif_nif", 4, mk_gif},
    {"pixels_as_gif_nif", 2, pixels_as_gif}
};

ERL_NIF_INIT(ecaptcha, nif_funcs, NULL, NULL, NULL, NULL);
