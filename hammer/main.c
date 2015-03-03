#include <hammer/hammer.h>
#include <hammer/internal.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <math.h>


double get_time() {
    struct timeval t;
    struct timezone tzp;
    gettimeofday(&t, &tzp);
    return t.tv_sec + t.tv_usec*1e-6;
}

struct FileType {
  char*   major_brand;
  char*   major_brand_version;
  char*   compatible_brands;
};

HParsedToken *ftyp_action(const HParseResult *p, void *user_data) {
  //printf("token type: %d\n", p->ast->token_type);
  if( p->ast->token_type == TT_SEQUENCE) {
    char* buffer = h_arena_malloc(p->ast->seq->arena, p->ast->seq->used);
    for (size_t i=0; i<p->ast->seq->used; ++i) {
      buffer[i] = H_CAST_UINT(p->ast->seq->elements[i]);
    }

    HParser* brand    = h_repeat_n(h_choice(h_ch_range('0', '9'), h_ch_range('a', 'z'), NULL), 4);
    HParser* brands   = h_many(brand);
    HParser* ftyp     = h_sequence(brand, h_uint32(), brands, NULL);
    HParseResult *res = h_parse(ftyp, buffer + 8, p->ast->seq->used - 8);

    if(res) {
      //printf("parsed ftyp\n");

      struct FileType* ft         = h_arena_malloc(p->ast->seq->arena, sizeof(struct FileType));
      ft->major_brand             = h_arena_malloc(p->ast->seq->arena, 5);
      ft->major_brand_version     = h_arena_malloc(p->ast->seq->arena, 5);
      ft->compatible_brands       = h_arena_malloc(p->ast->seq->arena, (p->ast->seq->used - 8) + 1);

      for (size_t i=0; i < 4; ++i) {
        ft->major_brand[i] = H_CAST_UINT(p->ast->seq->elements[i]);
      }
      ft->major_brand[4] = 0;
      for (size_t i=0; i < 4; ++i) {
        ft->major_brand_version[i] = H_CAST_UINT(p->ast->seq->elements[i+4]);
      }
      ft->major_brand_version[4] = 0;
      for (size_t i=0; i < (p->ast->seq->used - 8); ++i) {
        ft->compatible_brands[i] = H_CAST_UINT(p->ast->seq->elements[i+8]);
      }
      //printf("major brand: %s\n major version: %04X\ncompatible brands: %s\n",
      //  ft->major_brand, ft->major_brand_version, ft->compatible_brands);

      h_arena_free(p->ast->seq->arena, ft->major_brand);
      h_arena_free(p->ast->seq->arena, ft->major_brand_version);
      h_arena_free(p->ast->seq->arena, ft->compatible_brands);
      h_arena_free(p->ast->seq->arena, ft);
      h_arena_free(p->ast->seq->arena, buffer);
      return res;
    } else {
      printf("did not parse ftyp\n");
      h_arena_free(p->ast->seq->arena, buffer);
    }
  }
  return NULL;
}

HParser* build_parser() {
  HParser *ftyp_tag = h_token("ftyp", 4);
  HParser *moov_tag = h_token("moov", 4);
  HParser *free_tag = h_token("free", 4);
  HParser *skip_tag = h_token("skip", 4);
  HParser *mdra_tag = h_token("mdra", 4);
  HParser *dref_tag = h_token("dref", 4);
  HParser *cmov_tag = h_token("cmov", 4);
  HParser *rmra_tag = h_token("rmra", 4);
  HParser *iods_tag = h_token("iods", 4);
  HParser *mvhd_tag = h_token("mvhd", 4);
  HParser *clip_tag = h_token("clip", 4);
  HParser *trak_tag = h_token("trak", 4);
  HParser *udta_tag = h_token("udta", 4);
  HParser *mdat_tag = h_token("mdat", 4);


  HParser *tag      = h_choice(ftyp_tag, moov_tag, mdat_tag, mdra_tag, dref_tag, cmov_tag, rmra_tag, iods_tag, mvhd_tag, clip_tag, trak_tag, udta_tag, NULL);


  //HParser *ftyp_box  = h_length_value(h_left(h_uint32(), ftyp_tag), h_uint8());
  HParser *ftyp_box  = h_action(h_length_value(h_left(h_uint32(), ftyp_tag), h_uint8()), ftyp_action, NULL);
  HParser *free_box  = h_length_value(h_left(h_uint32(), free_tag), h_uint8());
  HParser *skip_box  = h_length_value(h_left(h_uint32(), skip_tag), h_uint8());
  HParser *moov_box  = h_length_value(h_left(h_uint32(), moov_tag), h_uint8());
  HParser *mdat_box  = h_length_value(h_left(h_uint32(), mdat_tag), h_uint8());
  HParser *mp4_box  = h_choice(ftyp_box, free_box, skip_box, moov_box, mdat_box, NULL);
  //HParser *mp4_box  = h_length_value(h_left(h_uint32(), ftyp), h_uint8());
  //return mp4_box;

  HParser *complete_parser = h_many1(mp4_box);
  return complete_parser;
}

void bench(char* name, char* path) {
  uint8_t input[1024];
  size_t inputsize;

  FILE *fp = fopen ( path , "rb" );
  fseek( fp , 0L , SEEK_END);
  long lSize = ftell( fp );
  rewind( fp );

  const uint8_t* buffer = calloc( 1, lSize+1 );
  if( !buffer ) fclose(fp),fputs("memory alloc fails",stderr),exit(1);

  if( 1!=fread((void*)buffer , lSize, 1 , fp) )
    fclose(fp),free(buffer),fputs("entire read fails",stderr),exit(1);

  //printf("got a buffer of %ld bytes\n", lSize);

  HParser *parser = build_parser();
  //HParser *manyparser = h_many1(parser);
  //printf("built the parser\n");

  int iterations = 100000;
  double measured[100000];
  double acc = 0;

  for(int i = 0; i < iterations; i++) {
    double begin = get_time();
    HParseResult *result = h_parse(parser, buffer, lSize);
    if(result) {
    //    printf("yay!\n");
    } else {
        printf("nay!\n");
    }
    double end = get_time();
    measured[i] = end - begin;
    acc += measured[i];
  }

  double mean = acc / iterations;
  double acc2 = 0;
  for(int t =0; t < iterations; t++) {
    acc2 = pow(fabs(measured[t] - mean), 2);
  }

  double variance = acc2 / iterations;
  printf("\n\nbench %s:\n", name);
  //printf("begin: %f\nend: %f\ndiff: %f\n", begin, end, end - begin);


  printf("%f ns/iter (variance: %f)\n", mean * 1e9, variance * 1e9);
  fclose(fp);
  free(buffer);
}

int main(int argc, char *argv[]) {
  bench("small", "../small.mp4");
  bench("bigbuckbunny", "../bigbuckbunny.mp4");
}
