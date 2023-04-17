/*
 * telega-appindicator.c --- Bridge between Emacs and TDLib.
 *
 * Copyright (C) 2016-2020 by Zajcev Evgeny
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
 *
 ** Commentary:
 *
 * Support for telega icon in System Tray using libappindicator.
 *
 * Adds "appindicator" command:
 *  - appindicator setup <svg-icon:str>"
 *  - appindicator status <active|passive>"
 *  - appindicator label <label:str>"
 *
 * Can send "appindicator-event" when menu buttons are clicked.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

#ifdef WITH_AYATANA_APPINDICATOR
#include <libayatana-appindicator/app-indicator.h>
#else
#include <libappindicator/app-indicator.h>
#endif

extern void send_output(const char* data);

static GMainLoop* loop = NULL;
static GtkWidget* menu = NULL;
static AppIndicator* appind = NULL;

static void
appindicator_click_cb(GtkWidget *widget, gpointer data)
{
        const gchar* text = (const gchar*)data;
        if (text) {
                send_output(text);
        }
}

static void
appindicator_create_menu(char* menu_elts)
{
        if (appind) {
                char *menu_elt = strtok(menu_elts, "|");
                menu = gtk_menu_new();
                while(menu_elt) {
                        if (!strcmp(menu_elt, "-")){
                                GtkWidget* item = gtk_separator_menu_item_new();
                                gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
                                gtk_widget_show(item);
                        } else {
                                GtkWidget* item = gtk_menu_item_new_with_label(menu_elt);
                                /* https://stackoverflow.com/questions/43303990/c-gtk-g-signal-connect-using-the-data-field */

                                char *persist_menu_elt = (char *)malloc(strlen(menu_elt) + 1 + 2);

                                sprintf(persist_menu_elt, "\"%s\"", menu_elt);
                                
                                g_signal_connect(item, "activate",
                                                 G_CALLBACK(appindicator_click_cb), persist_menu_elt);

                                gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
                                gtk_widget_show(item);
                        }

                        menu_elt = strtok(NULL, "|");
                }
                
                app_indicator_set_menu(appind, GTK_MENU(menu));
        } else {
                fprintf(stderr, "[emacs-appindicator]: Appindicator already initialized!\n");
        }
}

static int
appindicator_cmd(void* data)
{
        char* cmd = (char*)data;
        char* cmd_args = strchr(cmd, ' ');
        if (cmd_args)
                cmd_args += 1;

        if (cmd_args == NULL) {
                /* All commands requires args */
                fprintf(stderr, "[emacs-appindicator]: Invalid cmd -> %s\n",
                        cmd);
        } else if (!strncmp(cmd, "init ", 5)) {
                appind = app_indicator_new(cmd_args, "invalid",
                                           APP_INDICATOR_CATEGORY_COMMUNICATIONS);
                g_assert(IS_APP_INDICATOR(appind));
        } else if (!strncmp(cmd, "menu ", 5)) {
                /* setup <icon-path:str> */
                appindicator_create_menu(cmd_args);
        } else if (!strncmp(cmd, "status ", 7)) {
                /* status <active|passive> */
                if (appind) {
                        AppIndicatorStatus status = APP_INDICATOR_STATUS_PASSIVE;
                        if (!strcmp(cmd_args, "active"))
                                status = APP_INDICATOR_STATUS_ACTIVE;
                        app_indicator_set_status(appind, status);
                }
        } else if (!strncmp(cmd, "label ", 6)) {
                /* label <label:str> */
                if (appind)
                        app_indicator_set_label(appind, cmd_args, cmd_args);
        } else if (!strncmp(cmd, "icon ", 5)) {
                app_indicator_set_icon_full(appind, cmd_args, "emacs appindicator icon");
        } else {
                fprintf(stderr, "[emacs-appindicator]: unknown cmd -> %s\n",
                        cmd);
        }

        free(data);
        return 0;
}

void
appindicator_send(const char* json)
{
        char* json_copy = strdup(json);
        g_idle_add(appindicator_cmd, json_copy);
}

bool
appindicator_init(void)
{
        return gtk_init_check(NULL, NULL);
}

void*
appindicator_loop(void* data)
{
        loop = g_main_loop_new(NULL, FALSE);
        g_main_loop_run(loop);
        return NULL;
}

void
appindicator_stop(void)
{
        if (loop)
                g_main_loop_quit(loop);
}
