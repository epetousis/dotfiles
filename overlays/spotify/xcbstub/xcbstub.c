// Copyright (c) Sergey Nizovtsev (snizovtsev) 2022. All rights reserved.
// Taken from https://community.spotify.com/t5/Desktop-Linux/Wayland-support/m-p/5425694/highlight/true#M21238

#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

void gtk_init(int *argc, char ***argv) { }
void* gdk_pixbuf_loader_new() { return NULL; }

typedef struct xcb_connection {
    int has_error;
} xcb_connection_t;

xcb_connection_t*
xcb_connect(const char *displayname, int *screenp)
{
    xcb_connection_t *conn = calloc(1, sizeof(xcb_connection_t));
    conn->has_error = 1;
    return conn;
}

int xcb_connection_has_error(xcb_connection_t *c)
{
    return 1;
}

int xcb_flush(xcb_connection_t *c)
{
    return 0;
}

uint32_t xcb_generate_id(xcb_connection_t *c)
{
    return 42;
}

void xcb_disconnect(xcb_connection_t *c)
{
    free(c);
}

void* cef_get_xdisplay() {
    return NULL;
}

void *XOpenDisplay(const char *display_name)
{
    return NULL;
}

typedef unsigned long Atom;
typedef unsigned long Window; /*?*/

int XInternAtoms(void *display, char **names, int count, bool only_if_exists, Atom *atoms_return)
{
    return -1;
}

int XChangeProperty(void *display, Window w, Atom property, Atom type, int format, int mode, const unsigned  char
        *data, int nelements)
{
    return  0;
}
