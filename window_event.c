#include <stdlib.h>
#include <stdio.h>

#include <xcb/xcb.h>

xcb_connection_t    *c;
xcb_screen_t        *screen;

/* win */
xcb_drawable_t       win;

/* stard/end point storage for line drawing */
xcb_point_t line_start;
xcb_point_t line_end;


xcb_drawable_t create_sub_window(xcb_drawable_t parent, xcb_gravity_t gravity) {
  
  xcb_drawable_t swin;
  uint32_t mask = 0;
  uint32_t values4[4];

  swin = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXEL | XCB_CW_BORDER_PIXEL | XCB_CW_WIN_GRAVITY | XCB_CW_EVENT_MASK;
  values4[0] = screen->white_pixel;
  values4[1] = screen->black_pixel;
  values4[2] = gravity;
  values4[3] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
  xcb_create_window (c,
                     XCB_COPY_FROM_PARENT,
                     swin,
		     parent,
                     0, 0,
                     50, 50,
                     2,
                     XCB_WINDOW_CLASS_INPUT_OUTPUT,
                     screen->root_visual,
                     mask, values4);
  return swin;
}

void create_window() {
  
  uint32_t             mask = 0;
  uint32_t             values[3];

  win = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  values[0] = screen->white_pixel;
  values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
  xcb_create_window (c,
		     screen->root_depth,
                     win,
                     screen->root,
                     0, 0,
                     54,54, /* border size big enough for the subwindows to fit in */
                     0,
                     XCB_WINDOW_CLASS_INPUT_OUTPUT,
                     screen->root_visual, 
                     mask, values);

  xcb_map_window (c, win);
}


void event_loop() {
  xcb_generic_event_t *e;
  while ((e = xcb_wait_for_event (c))) {
    switch (e->response_type & ~0x80) {

      /* ESC to exit */
    case XCB_KEY_PRESS: {
      xcb_key_press_event_t *ev;
      ev = (xcb_key_press_event_t *)e;
      if (ev->detail == 9) return;
      break;
    }

    }
    free (e);
  }
  
}

int main(void) {
  int i;
  /* Open the connection to the X server */
  c = xcb_connect (NULL, NULL);
  /* Get the first screen */
  screen = xcb_setup_roots_iterator (xcb_get_setup (c)).data;
  /* make the main window */  
  create_window();

  /* Create a subwindow for each gravity type */
  create_sub_window(win, XCB_GRAVITY_NORTH_WEST);
  create_sub_window(win, XCB_GRAVITY_NORTH_EAST);
  create_sub_window(win, XCB_GRAVITY_SOUTH_WEST);
  create_sub_window(win, XCB_GRAVITY_SOUTH_EAST);
  
  /* map the subwindows */
  xcb_map_subwindows(c, win);
  xcb_flush(c);

  event_loop();

  puts("bye!");
  return EXIT_SUCCESS;
}
