/*
 * This is a simple driver for the http-parser package.  It is
 * intended to read one HTTP request after another from a file,
 * nothing more.
 *
 * For "feature parity" with the Haskell code in RFC2616.hs, we
 * allocate and populate a simple structure describing each request,
 * since that's the sort of thing that many real applications would
 * themselves do and the library doesn't do this for us.
 *
 * For the http-parser source, see
 * https://github.com/joyent/http-parser/blob/master/http_parser.c
 */

/*
 * Turn off this preprocessor symbol to have the callbacks do nothing
 * at all, which "improves performance" by about 50%.
 */
/*#define LOOK_BUSY*/

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <math.h>

#include "http_parser.h"

void get_time(struct timeval *t) {
    struct timezone tzp;
    gettimeofday(t, &tzp);
    //return t.tv_sec*1e9 + t.tv_usec*1e3;
}

struct http_string {
    size_t len;
    char value[0];
};

struct http_header {
    struct http_string *name;
    struct http_string *value;
    struct http_header *next;
};

struct http_request {
    struct http_string *method;
    struct http_string *uri;
    struct http_header *headers, *last;
};

struct data {
    size_t count;
    struct http_request req;
};

static void *xmalloc(size_t size)
{
    void *ptr;

    if ((ptr = malloc(size)) == NULL) {
  perror("malloc");
  exit(1);
    }

    return ptr;
}

static struct http_string *xstrdup(const char *src, size_t len, size_t extra)
{
    struct http_string *dst = xmalloc(sizeof(*dst) + len + extra);
    memcpy(dst->value, src, len);
    dst->len = len;
    return dst;
}

static void xstrcat(struct http_string **dst, const char *src, size_t len)
{
    struct http_string *p;

    if (*dst == NULL) {
  *dst = xstrdup(src, len, 0);
  return;
    }

    p = xstrdup((*dst)->value, (*dst)->len, len);
    memcpy(p->value + (*dst)->len, src, len);
    p->len += len;
    free(*dst);
    *dst = p;
}

static int begin(http_parser *p)
{
    struct data *data = p->data;

    data->count++;

    return 0;
}

static int url(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;

    xstrcat(&data->req.uri, at, len);
#endif

    return 0;
}

static int header_field(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;

    if (data->req.last && data->req.last->value == NULL) {
  xstrcat(&data->req.last->name, at, len);
    } else {
  struct http_header *hdr = xmalloc(sizeof(*hdr));

  hdr->name = xstrdup(at, len, 0);
  hdr->value = NULL;
  hdr->next = NULL;

  if (data->req.last != NULL)
      data->req.last->next = hdr;
  data->req.last = hdr;
  if (data->req.headers == NULL)
      data->req.headers = hdr;
    }
#endif

    return 0;
}

static int header_value(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;

    xstrcat(&data->req.last->value, at, len);
#endif

    return 0;
}

static int complete(http_parser *p)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;
    struct http_header *hdr, *next;

    free(data->req.method);
    free(data->req.uri);

    for (hdr = data->req.headers; hdr != NULL; hdr = next) {
  next = hdr->next;
  free(hdr->name);
  free(hdr->value);
  free(hdr);
  hdr = next;
    }

    data->req.method = NULL;
    data->req.uri = NULL;
    data->req.headers = NULL;
    data->req.last = NULL;
#endif

    /* Bludgeon http_parser into understanding that we really want to
     * keep parsing after a request that in principle ought to close
     * the "connection". */
    if (!http_should_keep_alive(p)) {
  p->http_major = 1;
  p->http_minor = 1;
  p->flags &= ~6;
    }

    return 0;
}

static void parse(char * buf, long nread)
{
    struct data data;
    http_parser_settings s;
    http_parser p;
    //ssize_t nread;

    memset(&s, 0, sizeof(s));
    s.on_message_begin = begin;
    s.on_url = url;
    s.on_header_field = header_field;
    s.on_header_value = header_value;
    s.on_message_complete = complete;

    p.data = &data;

    http_parser_init(&p, HTTP_REQUEST);

    data.count = 0;
    data.req.method = NULL;
    data.req.uri = NULL;
    data.req.headers = NULL;
    data.req.last = NULL;

    ssize_t np = http_parser_execute(&p, &s, buf, nread);
    //printf("np: %zu\n", np);
    if (np != nread) {
      fprintf(stderr, "parse failed\n");
    }
    //printf("%ld\n", (unsigned long) data.count);
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

  //HParser *parser = build_parser();
  //HParser *manyparser = h_many1(parser);
  //printf("built the parser\n");
  struct data data;
  http_parser_settings s;
  http_parser p;
  //ssize_t nread;

  memset(&s, 0, sizeof(s));
  s.on_message_begin = begin;
  s.on_url = url;
  s.on_header_field = header_field;
  s.on_header_value = header_value;
  s.on_message_complete = complete;

  p.data = &data;

  http_parser_init(&p, HTTP_REQUEST);

  data.count = 0;
  data.req.method = NULL;
  data.req.uri = NULL;
  data.req.headers = NULL;
  data.req.last = NULL;

  int iterations = 1000;
  uint64_t measured[1000];
  uint64_t acc = 0;

  printf("starting iterations:\n");
  //parse(buffer, lSize);

  struct timeval t1, t2;
  for(int i = 0; i < iterations; i++) {
    //printf("i: %d\n", i);
    get_time(&t1);
    ssize_t np = http_parser_execute(&p, &s, buffer, lSize);
    //printf("np: %zu\n", np);
    if (np != lSize) {
      fprintf(stderr, "parse failed\n");
      break;
    }
    //parse(buffer, lSize);
    get_time(&t2);
    uint64_t secs  = t2.tv_sec - t1.tv_sec;
    uint64_t usecs = t2.tv_usec - t1.tv_usec;
    //printf("t1:%d, t2: %d\n", t1.tv_sec, t2.tv_sec);
    //printf("ut1:%d, ut2: %d\n", t1.tv_usec, t2.tv_usec);
    // the seconds are not relevant
    //measured[i] = secs*1e9 + usecs*1e3;
    measured[i] = usecs;
    //printf("usecs: %d, measured: %d\n", usecs, measured[i]);
    acc += measured[i];
  }

  printf("acc: %d, iterations: %d\n", acc, iterations);
  double mean = acc / iterations;
  double acc2 = 0;
  for(int t =0; t < iterations; t++) {
    acc2 = pow(fabs(measured[t] - mean), 2);
  }

  double variance = acc2 / iterations;
  printf("\n\nbench %s:\n", name);
  //printf("begin: %f\nend: %f\ndiff: %f\n", begin, end, end - begin);

  //printf("%f ns/iter (variance: %f)\n", mean * 1e9, variance * 1e9);
  printf("%lf ns/iter (variance: %lf)\n", mean * 1000, variance * 1000);
  fclose(fp);
  free(buffer);
}

int main(int argc, char *argv[]) {
  bench("small", "../http-requests.txt");
  bench("bigger", "../http-requests.txt");
}

