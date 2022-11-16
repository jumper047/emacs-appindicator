/*
 * telega-server.c --- Bridge between Emacs and TDLib.
 *
 * Copyright (C) 2016-2021 by Zajcev Evgeny
 *
 * Author: Zajcev Evgeny <zevlg@yandex.ru>
 *
 * telega is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * telega is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with telega.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdbool.h>
#include <pthread.h>
#include <assert.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include "slack-dat.h"

extern bool telega_appindicator_init(void);
extern void telega_appindicator_send(const char* json);
extern void* telega_appindicator_loop(void* data);
extern void telega_appindicator_stop(void);

/*
 * Input/Output Protocol:
 * ~~~~~~~~~~~~~~
 *   <COMMAND> <SPACE> <PLIST-LEN> <NEWLINE>
 *   <PLIST of PLIST-LEN length> <NEWLINE>
 *
 * COMMAND is one of `send', `event' or `error'
 * `event' and `error' is used for output
 *
 * If VOIP support is compiled in (make WITH_VOIP=true) then also
 * `voip' command available
 *
 * If support for appindicator is compiled in, then `appindicator'
 * command is also available.
 *
 * For example:
 *   event 105
 *   (:@type "updateAuthorizationState" :authorization_state (:@type "authorizationStateWaitTdlibParameters"))
 *
 *   send 109
 *   (:@type "getTextEntities" :text "@telegram /test_command https://telegram.org telegram.me" :@extra ["5" 7.0])
 *
 */

char* logfile = NULL;
size_t logfile_size = 4 * 1024 * 1024;
int verbosity = 5;
const char* version = "0.8.1";

/* true when stdin_loop() is running */
volatile bool server_running;

int parse_mode = 0;
#define PARSE_MODE_JSON 1
#define PARSE_MODE_PLIST 2

void
usage(char* prog)
{
        printf("Version %s", version);
        printf(", with appindicator");
        printf("\n");
        printf("usage: %s [-jp] [-L SIZE] [-l FILE] [-v LVL] [-h]\n", prog);
        printf("\t-L SIZE    Log file size in bytes (default=%zu)\n",
               logfile_size);
        printf("\t-l FILE    Log to FILE (default=stderr)\n");
        printf("\t-v LVL     Verbosity level (default=5)\n");
        printf("\t-j         Parse json from stdin and exit\n");
        printf("\t-p         Parse plist from stdin and exit\n");
        exit(0);
}

void
telega_output(const char* otype, const char* str)
{
        if (verbosity > 4) {
                fprintf(stderr, "[telega-server] "
                        "OUTPUT %s: %s\n", otype, str);
        }

        printf("%s %zu\n%s\n", otype, strlen(str), str);
        fflush(stdout);
}

void
telega_output_json(const char* otype, const char* json)
{
        struct telega_dat json_src = TDAT_INIT;
        struct telega_dat plist_dst = TDAT_INIT;

        if (verbosity > 4) {
                fprintf(stderr, "[telega-server] "
                        "OUTPUT %s: %s\n", otype, json);
        }

        tdat_append(&json_src, json, strlen(json));
        tdat_json_value(&json_src, &plist_dst);
        tdat_append1(&plist_dst, "\0");

        assert(tdat_len(&plist_dst) > 0);
        printf("%s %zu\n%s\n", otype, tdat_len(&plist_dst)-1, plist_dst.data);
        fflush(stdout);

        tdat_drop(&json_src);
        tdat_drop(&plist_dst);
}



/*
 * NOTE: Emacs sends HUP when associated buffer is killed
 * kind of graceful exit
 */
static void
on_sighup(int sig)
{
        close(0);
}

static void
stdin_loop()
{
        struct telega_dat plist_src = TDAT_INIT;
        struct telega_dat json_dst = TDAT_INIT;
        char cmdline[33];

        signal(SIGHUP, on_sighup);
        while (fgets(cmdline, 33, stdin)) {
                cmdline[32] = '\0';

                char cmd[33];
                size_t cmdsz = 0;
                if (2 != sscanf(cmdline, "%s %zu\n", cmd, &cmdsz)) {
                        fprintf(stderr, "[slack-notifier] "
                                "Unexpected cmdline format: %s\n", cmdline);
                        continue;
                }
                if (cmdsz > 10 * 1024 * 1024) {
                        fprintf(stderr, "[slack-notifier] cmd size = %zu is too large",
                                cmdsz);
                        continue;
                }

                tdat_ensure(&plist_src, cmdsz);

                /* read including newline */
                size_t rc = fread(plist_src.data, 1, cmdsz + 1, stdin);
                if (rc != cmdsz + 1) {
                        /* EOF or error */
                        fprintf(stderr, "[slack-notifier] "
                                "fread() error: %d\n", ferror(stdin));
                        break;
                }
                plist_src.end = cmdsz + 1;
                tdat_append1(&plist_src, "\0");
                if (verbosity > 4) {
                        fprintf(stderr, "[slack-notifier] "
                                "INPUT (cmd=%s): %s\n", cmd, plist_src.data);
                }

                tdat_plist_value(&plist_src, &json_dst);
                tdat_append1(&json_dst, "\0");

                if (!strcmp(cmd, "appindicator")) {
                        /* Strip leading/trailing " */
                        if (tdat_len(&json_dst) > 1) {
                                if (tdat_start(&json_dst)[0] == '"')
                                        tdat_drain(&json_dst, 1);
                                json_dst.end -= 2;
                                tdat_append1(&json_dst, "\0");
                        }
                        telega_appindicator_send(tdat_start(&json_dst));
                } else {
                        char error[128];
                        snprintf(error, 128, "\"Unknown cmd `%s'\"", cmd);

                        fprintf(stderr, "[telega-server] "
                                "Unknown command: %s\n", cmd);
                }

                tdat_reset(&plist_src);
                tdat_reset(&json_dst);
        }

        tdat_drop(&plist_src);
        tdat_drop(&json_dst);
}

static void
parse_stdin(void)
{
        struct telega_dat src = TDAT_INIT;

#define RDSIZE 1024
        tdat_ensure(&src, RDSIZE);

        ssize_t rlen;
        while ((rlen = read(0, &src.data[src.end], RDSIZE)) > 0) {
                src.end += rlen;
                tdat_ensure(&src, RDSIZE);
        }
#undef RDSIZE
        tdat_append1(&src, "\0");

        struct telega_dat dst = TDAT_INIT;
        if (parse_mode == PARSE_MODE_JSON)
                tdat_json_value(&src, &dst);
        else
                tdat_plist_value(&src, &dst);
        tdat_append1(&dst, "\0");

        printf("%s\n", dst.data);

        tdat_drop(&src);
        tdat_drop(&dst);
}

int
main(int ac, char** av)
{
        int ch;
        while ((ch = getopt(ac, av, "L:E:R:f:jpl:v:h")) != -1) {
                switch (ch) {
                case 'j':
                        parse_mode = PARSE_MODE_JSON;
                        break;
                case 'p':
                        parse_mode = PARSE_MODE_PLIST;
                        break;
                case 'h':
                case '?':
                default:
                        usage(av[0]);
                        /* NOT REACHED */
                }
        }


        if (parse_mode) {
                parse_stdin();
                return 0;
                /* NOT REACHED */
        }

        server_running = true;

        bool has_appind = telega_appindicator_init();
	if (has_appind == false)
	  return 1;
        pthread_t appind_thread;
	int rc = pthread_create(&appind_thread, NULL,
				telega_appindicator_loop, NULL);
	assert(rc == 0);

        stdin_loop();
        /* Gracefully stop the tdlib_loop */
        server_running = false;

	telega_appindicator_stop();
	rc = pthread_join(appind_thread, NULL);
	assert(rc == 0);
        return 0;
}
