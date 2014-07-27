#include <stdlib.h>
#include <stdio.h>

#include <xcb/xcb.h>

xcb_connection_t    *c;
xcb_screen_t        *screen;


/* graphics contexts */
xcb_gcontext_t       foreground;
xcb_gcontext_t       fill;
/* win */
xcb_drawable_t       win;
xcb_drawable_t       pid;

/* stard/end point storage for line drawing */
xcb_point_t line_start;
xcb_point_t line_end;

void create_window() {
  uint32_t             mask = 0;
  uint32_t             values[2];

  
  /* black foreground graphic context */
  foreground = xcb_generate_id (c);
  mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = screen->black_pixel;
  values[1] = 0;
  xcb_create_gc (c, foreground, screen->root, mask, values);

  /* make the background pixmap for the window */
  pid = xcb_generate_id (c);
  xcb_create_pixmap(c,
		    screen->root_depth,
		    pid,
		    screen->root,
		    500, 500);

  /* context for filling with white */
  fill = xcb_generate_id(c);
  mask = XCB_GC_FOREGROUND | XCB_GC_BACKGROUND;
  values[0] = screen->white_pixel;
  values[1] = screen->white_pixel;
  xcb_create_gc(c, fill, pid, mask,values);

  /* Create the window */
  win = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXMAP  | XCB_CW_EVENT_MASK;
  values[0] = pid;
  values[1] = XCB_EVENT_MASK_EXPOSURE       | XCB_EVENT_MASK_BUTTON_PRESS   |
              XCB_EVENT_MASK_BUTTON_RELEASE | XCB_EVENT_MASK_BUTTON_MOTION |
              XCB_EVENT_MASK_KEY_PRESS      | XCB_EVENT_MASK_KEY_RELEASE;
  xcb_create_window (c,                             /* Connection          */
		     screen->root_depth,          /* depth               */
                     win,                           /* window Id           */
                     screen->root,                  /* parent window       */
                     0, 0,                          /* x, y                */
                     150, 150,                      /* width, height       */
                     10,                            /* border_width        */
                     XCB_WINDOW_CLASS_INPUT_OUTPUT, /* class               */
                     screen->root_visual,           /* visual              */
                     mask, values);                 /* masks */

  /* Map the window on the screen */
  xcb_map_window (c, win);

  /* fill the pixmap with white (it starts empty) */
  xcb_poly_fill_rectangle(c, pid, fill, 1, (xcb_rectangle_t[]){{ 0, 0, 500, 500}});
  
}


void event_loop() {
  xcb_generic_event_t *e;
  while ((e = xcb_wait_for_event (c))) {
    switch (e->response_type & ~0x80) {

    case XCB_KEY_PRESS: {
      /* fill pixmap with white */
      /* why isn't this happening */
      xcb_poly_fill_rectangle_checked(c, pid, fill, 1, (xcb_rectangle_t[]){{ 0, 0, 500, 500}});
      
      /* clear win to reveal pixmap */
      xcb_clear_area(c, 1, win, 0, 0, 500,500);
      
      xcb_flush(c);
      break;
    }

    case XCB_MOTION_NOTIFY: {
      xcb_motion_notify_event_t *ev = (xcb_motion_notify_event_t  *)e;
      
      /*
	1. clear the area on the win between line_start and mouse_pos (or whole win)
	2. update mouse_pos
	3. draw line from line_start to mouse_pos
      */
      xcb_clear_area(c, 1, win,0, 0, 500,500);
      xcb_point_t mouse_pos = { (ev->event_x - line_start.x), (ev->event_y - line_start.y)} ;
      xcb_point_t points[] = {line_start, mouse_pos};
      xcb_poly_line (c, XCB_COORD_MODE_PREVIOUS, win, foreground, 2, points);

      xcb_flush(c);
      break;
    }

    case XCB_BUTTON_PRESS: {
      xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;
      line_start = (xcb_point_t){ ev->event_x, ev->event_y };
      xcb_flush (c);
      break;
    }

    case XCB_BUTTON_RELEASE: {
      xcb_button_release_event_t *ev = (xcb_button_release_event_t *)e;
      
      line_end = (xcb_point_t){ (ev->event_x - line_start.x), (ev->event_y - line_start.y) };

      xcb_point_t points[] = {line_start, line_end};
      xcb_poly_line (c, XCB_COORD_MODE_PREVIOUS, pid, foreground, 2, points);
      xcb_poly_line (c, XCB_COORD_MODE_PREVIOUS, win, foreground, 2, points);

      xcb_flush (c);
      break;
    }
    case XCB_EXPOSE: {
      
      xcb_flush(c);
      break;
    }
    default: {
      break;
    }
    }
    free (e);
  }
  
}

int main(void) {
  /* Open the connection to the X server */
  c = xcb_connect (NULL, NULL);
  /* Get the first screen */
  screen = xcb_setup_roots_iterator (xcb_get_setup (c)).data;

  create_window();

  xcb_flush(c);

  event_loop();

  puts("bye!");
  return EXIT_SUCCESS;
}
