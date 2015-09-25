all: http # stats.html

%.o: %.c
	g++ -pthread -std=c++11 -march=native -O3 -DNDEBUG -o $@ $< -I ~/custom/nonius/include -c

http: main.o http_parser.o
	g++ -pthread $^ -o $@

bench: http
	./http --reporter=standard

stats.html: http
	./http --reporter=html --output=stats.html

*.o: http_parser.h

.PHONY: all bench
