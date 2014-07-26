// gcc `pkg-config --libs xcb` main.c -o main

#include <stdio.h>
#include <stdlib.h>

#include <xcb/xcb.h>

xcb_connection_t *connection;
xcb_screen_t *screen;

void create_window() {
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
			     0, 0, 640, 480,
			     0,
			     XCB_WINDOW_CLASS_INPUT_OUTPUT,
			     screen->root_visual,
			     mask, values);
  
  xcb_map_window(connection, window);
}

int main(void) {
  connection = xcb_connect(NULL, NULL); // Callers need to use xcb_connection_has_error() to check for failure.
  screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data;
  
  create_window();
  
  xcb_flush(connection);
  sleep(1);
  
  return EXIT_SUCCESS;
}
