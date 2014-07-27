// gcc `pkg-config --libs xcb` main.c -o main

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <xcb/xcb.h>

#define HELLO_WORLD "Hello world!"

xcb_connection_t *connection;
xcb_screen_t *screen;

xcb_char2b_t *build_chars(const char *str, size_t length) {
  // taken from http://lists.freedesktop.org/archives/xcb/2009-April/004611.html
  // but https://bitbucket.org/ryanflannery/xtabs/src/4a36b745532d3a75f98ea115ceaabfac839fb8bc/xutil.c
  // juts casts..
  
  int i;
  xcb_char2b_t *ret = malloc(length * sizeof(xcb_char2b_t));
  if (!ret)
    return NULL;
  
  for (i = 0; i < length; i++) {
    ret[i].byte1 = 0;
    ret[i].byte2 = str[i];
  }
  
  return ret;
}

void measure_string(char *string, xcb_font_t font,
		    int *width, int *height, int *origin) {
  // man xcb_query_text_extents
  // http://www.x.org/releases/current/doc/xproto/x11protocol.html#requests:QueryFont
  // http://www.x.org/releases/current/doc/xproto/x11protocol.html#requests:ImageText8
  
  xcb_char2b_t *xcb_str;
  xcb_query_text_extents_cookie_t cookie;
  xcb_query_text_extents_reply_t *reply;
  
  xcb_str = build_chars(string, strlen(string));
  
  cookie = xcb_query_text_extents(connection, font, strlen(string), xcb_str);
  reply = xcb_query_text_extents_reply(connection, cookie, NULL);
  
  if(!reply) puts("STRING ERROR");
  
  *width = reply->overall_width;
  *height = reply->font_ascent + reply->font_descent;
  *origin = reply->font_ascent;
  
  free(xcb_str);
  free(reply);
}

xcb_window_t create_window(int width, int height) {
  // http://www.x.org/releases/current/doc/xproto/x11protocol.html#requests:CreateWindow
  // http://www.x.org/archive/current/doc/man/man3/xcb_create_window.3.xhtml
  //
  // N.B. xcb's order corresponds to the order of the wire.
  // You can look at the protocol encoding: http://www.x.org/releases/current/doc/xproto/x11protocol.html#Encoding::Requests
  
  uint32_t mask;
  uint32_t values[2];
  
  xcb_window_t window;
  xcb_void_cookie_t cookie;
  
  mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  values[0] = screen->white_pixel;
  values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
  
  window = xcb_generate_id(connection);
  cookie = xcb_create_window(connection,
			     XCB_COPY_FROM_PARENT, window, screen->root,
			     0, 0, width, height,
			     0,
			     XCB_WINDOW_CLASS_INPUT_OUTPUT,
			     screen->root_visual,
			     mask, values);
  
  xcb_map_window(connection, window);
  
  return window;
}

xcb_gcontext_t create_graphics_context(xcb_window_t window, xcb_font_t font) {
  xcb_gcontext_t graphics_context;
  
  uint32_t mask;
  uint32_t values[4];
  
  mask = XCB_GC_FOREGROUND | XCB_GC_BACKGROUND | XCB_GC_FONT | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = 0xF778A1;
  values[1] = screen->white_pixel;
  values[2] = font;
  values[3] = 1;
  
  graphics_context = xcb_generate_id(connection);
  xcb_create_gc(connection,
		graphics_context, window,
		mask, values);
  
  return graphics_context;
}

void event_loop(xcb_window_t window, xcb_gcontext_t graphics_context, int origin) {
  xcb_generic_event_t *event;
  
  while(event = xcb_wait_for_event(connection)) {
    switch(event->response_type) {
    case XCB_EXPOSE:
      xcb_image_text_8(connection, strlen(HELLO_WORLD), window, graphics_context, 0, origin, HELLO_WORLD);
      xcb_flush(connection);
      
      break;
    case XCB_KEY_PRESS:
      return;
    }
  }
}

int main(void) {
  int width, height, origin;
  
  xcb_font_t font;
  xcb_window_t window;
  xcb_gcontext_t graphics_context;
  
  connection = xcb_connect(NULL, NULL); // Callers need to use xcb_connection_has_error() to check for failure.
  screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data;
  
  font = xcb_generate_id(connection);
  char * font_name = "-misc-dejavu sans-medium-o-normal--0-0-0-0-p-0-ascii-0";
  xcb_open_font(connection, font, strlen(font_name), font_name);
  measure_string(HELLO_WORLD, font, &width, &height, &origin);
  
  window = create_window(width, height);
  graphics_context = create_graphics_context(window, font);
  
  xcb_flush(connection);
  
  event_loop(window, graphics_context, origin);
  puts("bye!");
  
  return EXIT_SUCCESS;
}

