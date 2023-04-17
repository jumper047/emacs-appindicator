CC=cc

CFLAGS+=-Wall -g -pthread

PROG=emacs-appindicator-helper
SOURCES=cli.c appindicator-helper.c

ifeq ($(shell pkg-config --exists appindicator3-0.1 && echo yes), yes)
CFLAGS+=`pkg-config --cflags appindicator3-0.1`
LDFLAGS=`pkg-config --libs appindicator3-0.1`
else ifeq ($(shell pkg-config --exists ayatana-appindicator3-0.1 && echo yes), yes)
CFLAGS+=-DWITH_AYATANA_APPINDICATOR
CFLAGS+=`pkg-config --cflags ayatana-appindicator3-0.1`
LDFLAGS=`pkg-config --libs ayatana-appindicator3-0.1`
else
$(error package libayatana-appindicator3-dev or appindicator3-dev should be installed!)
endif

$(PROG): $(SOURCES)
	$(CC) $(CFLAGS) -o $(PROG) $(SOURCES) $(LDFLAGS)

clean:
	@rm -vf $(PROG)

.PHONY: clean
