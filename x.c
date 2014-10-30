#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <xcb/xcb.h>
#include <xcb/xcb_image.h>
#include <xcb/xcb_keysyms.h>

#define XK_Left                          0xff51  /* Move left, left arrow */
#define XK_Up                            0xff52  /* Move up, up arrow */
#define XK_Right                         0xff53  /* Move right, right arrow */
#define XK_Down                          0xff54  /* Move down, down arrow */

#include "borgar/blank.xbm"
#include "borgar/box.xbm"
#include "borgar/box_on_goal.xbm"
#include "borgar/goal.xbm"
#include "borgar/keeper.xbm"
#include "borgar/keeper_on_goal.xbm"
#include "borgar/wall.xbm"

int initialized = 0;
xcb_connection_t *connection;
xcb_screen_t *screen;
xcb_key_symbols_t *syms;

void x_initialize(void) {
  if(initialized) return;
  
  connection = xcb_connect(NULL, NULL);
  screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data;
  syms = xcb_key_symbols_alloc(connection);
  // TODO: free this up with: xcb_key_symbols_free(syms);
  
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
  values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS | XCB_EVENT_MASK_STRUCTURE_NOTIFY;
  
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

void x_paint_pixmap(int window, int graphics_context, int x, int y, int bitmap_id) {
  int w, h;
  uint8_t *bitmap;
  xcb_pixmap_t pixmap;

  w = 20;
  h = 20;
  
  switch(bitmap_id) {
  case 0:
    bitmap = blank_bits;
    break;
  case 1:
    bitmap = wall_bits;
    break;
  case 2:
    bitmap = goal_bits;
    break;
  case 3:
    bitmap = keeper_bits;
    break;
  case 4:
    bitmap = keeper_on_goal_bits;
    break;
  case 5:
    bitmap = box_bits;
    break;
  case 6:
    bitmap = box_on_goal_bits;
    break;
  }
  
  pixmap = xcb_create_pixmap_from_bitmap_data(connection, window,
					      bitmap, w, h,
					      screen->root_depth, screen->black_pixel, screen->white_pixel,
					      NULL);
  
  xcb_copy_area(connection,
                pixmap, window, graphics_context,
                0, 0, x, y,
                w, h);
}

typedef void (fptr_t)(int);
typedef void (fptr_configure_t)(int,int,int);

void x_handle_events(fptr_t ptr_expose, fptr_configure_t ptr_configure, fptr_t ptr_keypress_arrow) {
  xcb_generic_event_t *event;
  
  if(!initialized) return;
  
  xcb_flush(connection);
  

  /* if event = null then expose & block */
  while(event = xcb_wait_for_event(connection)) {
    while(event != NULL) {
      switch(event->response_type) {
      case XCB_EXPOSE:
        /* ptr_expose(17); */
        /* xcb_flush(connection); */
        break;
      case XCB_CONFIGURE_NOTIFY:
        {
          xcb_configure_notify_event_t *ev = (xcb_configure_notify_event_t*)event;
        
          ptr_configure(ev->event, ev->width, ev->height);
        }
      
        break;
      case XCB_KEY_PRESS:
        {
          xcb_key_press_event_t *ev = (xcb_key_press_event_t *)event;
        
          if (ev->detail == 9) return;
        
          xcb_keysym_t sym = xcb_key_press_lookup_keysym(syms, ev, 0);
        
          switch(sym) {
          case XK_Up:
            ptr_keypress_arrow(0);
            xcb_flush(connection);
            break;
          case XK_Down:
            ptr_keypress_arrow(1);
            xcb_flush(connection);
            break;
          case XK_Left:
            ptr_keypress_arrow(2);
            xcb_flush(connection);
            break;
          case XK_Right:
            ptr_keypress_arrow(3);
            xcb_flush(connection);
            break;
          }
        }

      }
      
      event = xcb_poll_for_event(connection);
    }

    ptr_expose(17);
    xcb_flush(connection);
  }
}
