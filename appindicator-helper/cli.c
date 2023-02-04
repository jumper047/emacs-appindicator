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
#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern bool appindicator_init(void);
extern void appindicator_send(const char* json);
extern void* appindicator_loop(void* data);
extern void appindicator_stop(void);

const char *version = "0.0.1";

/* true when stdin_loop() is running */
volatile bool server_running;

void
usage(char* prog)
{
        printf("Version %s", version);
        printf("\n");
        exit(0);
}

void
send_output(const char* data)
{
        printf("%zu\n%s\n", strlen(data), data);
        fflush(stdout);
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
        char cmdline[33];

        signal(SIGHUP, on_sighup);
        while (fgets(cmdline, 33, stdin)) {
                cmdline[32] = '\0';

                size_t cmdsz = 0;
                if (1 != sscanf(cmdline, "%zu\n", &cmdsz)) {
                        fprintf(stderr, "[emacs-appindicator] "
                                "Unexpected cmdline format: %s\n", cmdline);
                        continue;
                }
                if (cmdsz > 10 * 1024 * 1024) {
                        fprintf(stderr, "[emacs-appindicator] cmd size = %zu is too large",
                                cmdsz);
                        continue;
                }
                char data[cmdsz+1];

                size_t rc = fread(data, 1, cmdsz + 1, stdin);
                if (rc != cmdsz + 1) {
                        /* EOF or error */
                        fprintf(stderr, "[emacs-appindicator] "
                                "fread() error: %d\n", ferror(stdin)); 
                        break;
                }
                /* stripping quotes */
                data[cmdsz-1] = 0;
                char* data_ptr = data;
                data_ptr++;
                appindicator_send(data_ptr);
        }
}


int
main(int ac, char** av)
{
        int ch;
        while ((ch = getopt(ac, av, "h")) != -1) {
                switch (ch) {
                case 'h':
                case '?':
                default:
                        usage(av[0]);
                        /* NOT REACHED */
                }
        }

        server_running = true;

        bool appind_initialized = appindicator_init();
        if (appind_initialized == false)
          return 1;
        pthread_t appind_thread;
        int rc = pthread_create(&appind_thread, NULL, appindicator_loop,
                                NULL);
        assert(rc == 0);

        stdin_loop();
        /* Gracefully stop the tdlib_loop */
        server_running = false;

        appindicator_stop();
        rc = pthread_join(appind_thread, NULL);
        assert(rc == 0);
        return 0;
}
