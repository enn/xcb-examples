#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <xcb/xcb.h>

int initialized = 0;
xcb_connection_t *connection;
xcb_screen_t *screen;

void x_initialize(void) {
  if(initialized) return;
  
  connection = xcb_connect(NULL, NULL);
  screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data;
  
  initialized = 1;
}

int x_create_window(int x, int y, int w, int h) {
  xcb_window_t window;
  xcb_void_cookie_t cookie;
  
  uint32_t mask;
  uint32_t values[2];
  
  if(!initialized) return;
  
  mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  values[0] = screen->white_pixel;
  values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
  
  window = xcb_generate_id(connection);
  cookie = xcb_create_window(connection,
			     XCB_COPY_FROM_PARENT, window, screen->root,
			     x, y, w, h,
			     0,
			     XCB_WINDOW_CLASS_INPUT_OUTPUT,
			     screen->root_visual,
			     mask, values);
  xcb_map_window(connection, window);
  
  return window;
}

int x_create_graphics_context(int window) {
  xcb_gcontext_t graphics_context;
  
  uint32_t mask;
  uint32_t values[2];
  
  if(!initialized) return;
  
  mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = screen->black_pixel;
  values[1] = 0;
  
  graphics_context = xcb_generate_id(connection);
  xcb_create_gc(connection,
		graphics_context, window,
		mask, values);
  
  return graphics_context;
}

void x_fill_rectangle(int window, int graphics_context, int x, int y, int w, int h) {
  xcb_rectangle_t r = { x, y, w, h };
  
  if(!initialized) return;
  
  xcb_poly_fill_rectangle(connection, window, graphics_context, 1, &r);
}

void x_draw_line(int window, int graphics_context, int x1, int y1, int x2, int y2) {
  xcb_point_t p[2];
  
  if(!initialized) return;
  
  p[0] = (xcb_point_t){ x1, y1 };
  p[1] = (xcb_point_t){ x2, y2 };
  
  xcb_poly_line(connection, XCB_COORD_MODE_ORIGIN, window, graphics_context, 2, p);
}

typedef void (fptr_t)(int);

void x_handle_events(fptr_t ptr) {
  xcb_generic_event_t *event;
  
  if(!initialized) return;
  
  xcb_flush(connection);
  
  while(event = xcb_wait_for_event(connection)) {
    switch(event->response_type) {
    case XCB_EXPOSE:
      
      ptr(17);
      
      xcb_flush(connection);
      break;
    case XCB_KEY_PRESS:
      return;
    }
  }
}
