CC=cc

CFLAGS+=-Wall -g -pthread

PROG=emacs-appindicator-helper
SOURCES=cli.c appindicator-helper.c

CFLAGS+=-DWITH_APPINDICATOR
CFLAGS+=`pkg-config --cflags appindicator3-0.1`
LDFLAGS=`pkg-config --libs appindicator3-0.1`

$(PROG): $(SOURCES)
	$(CC) $(CFLAGS) -o $(PROG) $(SOURCES) $(LDFLAGS)

clean:
	@rm -vf $(PROG)

.PHONY: clean
