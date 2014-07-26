// gcc `pkg-config --libs xcb xcb-image` main.c -o main

#include <stdio.h>
#include <stdlib.h>

#include <xcb/xcb.h>
#include <xcb/xcb_image.h>

#define neko_width 32
#define neko_height 32
static char neko_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x04,
   0x40, 0x10, 0x10, 0x02, 0x80, 0x28, 0x28, 0x01, 0x00, 0x49, 0x24, 0x00,
   0x06, 0x44, 0x44, 0x60, 0x18, 0x84, 0x42, 0x18, 0x60, 0x82, 0x83, 0x06,
   0x00, 0x02, 0x80, 0x00, 0x00, 0x22, 0x88, 0x00, 0x0f, 0x22, 0x88, 0x78,
   0x00, 0x22, 0x88, 0x00, 0x00, 0x02, 0x80, 0x00, 0x00, 0x3a, 0xb9, 0x00,
   0x00, 0x04, 0x40, 0x00, 0x00, 0x08, 0x20, 0x00, 0x00, 0x70, 0x1c, 0x02,
   0x00, 0x40, 0x04, 0x05, 0x00, 0x20, 0x88, 0x04, 0x00, 0x10, 0x50, 0x02,
   0x00, 0x08, 0x20, 0x01, 0x00, 0x0b, 0xa0, 0x01, 0x80, 0x0c, 0x61, 0x02,
   0x40, 0x18, 0x31, 0x04, 0x40, 0x10, 0x11, 0x04, 0xc0, 0x11, 0x11, 0x07,
   0x60, 0x90, 0x13, 0x0c, 0xe0, 0xff, 0xfe, 0x0f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

xcb_connection_t *connection;
xcb_screen_t *screen;

xcb_window_t create_window() {
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
			     0, 0, neko_width, neko_height,
			     0,
			     XCB_WINDOW_CLASS_INPUT_OUTPUT,
			     screen->root_visual,
			     mask, values);
  
  xcb_map_window(connection, window);
  
  return window;
}

xcb_gcontext_t create_graphics_context(xcb_window_t window) {
  xcb_gcontext_t graphics_context;
  
  uint32_t mask;
  uint32_t values[3];
  
  mask = XCB_GC_FOREGROUND | XCB_GC_BACKGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = screen->black_pixel;
  values[1] = screen->white_pixel;
  values[2] = 0;
  
  graphics_context = xcb_generate_id(connection);
  xcb_create_gc(connection,
		graphics_context, window,
		mask, values);
  
  return graphics_context;
}

void event_loop(xcb_window_t window, xcb_gcontext_t graphics_context, xcb_pixmap_t pixmap) {
  xcb_generic_event_t *event;
  
  while(event = xcb_wait_for_event(connection)) {
    switch(event->response_type) {
    case XCB_EXPOSE:
      xcb_copy_area(connection,
		    pixmap, window, graphics_context,
		    0, 0, 0, 0,
		    neko_width, neko_height);
      xcb_flush(connection);
      
      break;
    case XCB_KEY_PRESS:
      return;
    }
  }
}

int main(void) {
  xcb_window_t window;
  xcb_gcontext_t graphics_context;
  xcb_pixmap_t pixmap;
  
  connection = xcb_connect(NULL, NULL); // Callers need to use xcb_connection_has_error() to check for failure.
  screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data;
  
  window = create_window();
  graphics_context = create_graphics_context(window);
  pixmap = xcb_create_pixmap_from_bitmap_data(connection, window,
					      neko_bits, neko_width, neko_height,
					      screen->root_depth, screen->black_pixel, screen->white_pixel,
					      NULL);
  if(!pixmap) puts("NEKO ERROR");
  
  xcb_flush(connection);
  
  event_loop(window, graphics_context, pixmap);
  puts("bye!");
  
  return EXIT_SUCCESS;
}
