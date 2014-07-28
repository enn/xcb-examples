#include <stdlib.h>
#include <stdio.h>
#include <string.h>
 
#include <xcb/xcb.h>


xcb_connection_t    *c;
xcb_screen_t        *screen;

xcb_gcontext_t       gc;

xcb_drawable_t       win;
xcb_drawable_t       swin;

int drag_state = 0;
uint32_t offset[2];
uint32_t origin[2];

void draw_button(xcb_drawable_t d) {
  char string[] = "Drag me";
  uint8_t string_len = strlen(string);
  xcb_image_text_8(c, string_len, d, gc, 6, 12, string);
}

void make_button() {
  uint32_t mask = 0;
  uint32_t values3[3];
  swin = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXEL | XCB_CW_BORDER_PIXEL | XCB_CW_EVENT_MASK;
  values3[0] = screen->white_pixel;
  values3[1] = screen->black_pixel;
  values3[2] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_BUTTON_PRESS |
    XCB_EVENT_MASK_BUTTON_RELEASE |  XCB_EVENT_MASK_BUTTON_MOTION | 
    XCB_EVENT_MASK_ENTER_WINDOW   | XCB_EVENT_MASK_LEAVE_WINDOW   |
    XCB_EVENT_MASK_KEY_PRESS;
  xcb_create_window (c,
		     XCB_COPY_FROM_PARENT,
		     swin,
		     win,
		     0, 0,
		     50, 15,
		     1,
		     XCB_WINDOW_CLASS_INPUT_OUTPUT,
		     screen->root_visual,
		     mask, values3);
	origin[0] = 0;
	origin[1] = 0;
  xcb_map_window(c, swin);
}


void create_window() {
  uint32_t mask = 0;
  uint32_t values[3];

  
  gc = xcb_generate_id (c);
  mask = XCB_GC_FOREGROUND | XCB_GC_BACKGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = screen->black_pixel;
  values[1] = screen->white_pixel;
  values[2] = 0;
  xcb_create_gc (c, gc, screen->root, mask, values);
 
  /* create the window */
  win = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  values[0] = screen->white_pixel;
  values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_BUTTON_PRESS | XCB_EVENT_MASK_STRUCTURE_NOTIFY
    | XCB_EVENT_MASK_BUTTON_RELEASE | XCB_EVENT_MASK_KEY_PRESS | XCB_EVENT_MASK_BUTTON_MOTION;
  xcb_create_window (c,                             /* connection    */
                     screen->root_depth,          /* depth         */
                     win,                           /* window Id     */
                     screen->root,                  /* parent window */
                     0, 0,                          /* x, y          */
		     300,300,                      /* width, height */
                     0,                            /* border_width  */
                     XCB_WINDOW_CLASS_INPUT_OUTPUT, /* class         */
                     screen->root_visual,           /* visual        */
                     mask, values);                 /* masks         */

  xcb_map_window(c, win);
}



void event_loop() {
  xcb_generic_event_t *e;


  while ((e = xcb_wait_for_event (c))) {
    switch (e->response_type & ~0x80) {

    case XCB_MOTION_NOTIFY: {
      xcb_motion_notify_event_t *mev = (xcb_motion_notify_event_t  *)e;
      
      if (mev->event == swin && drag_state) {
	origin[0] += mev->event_x - offset[0];
	origin[1] += mev->event_y - offset[1];

	xcb_configure_window(c,swin,XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,origin);
	
      }

      xcb_flush(c);
      break;
    }

    case XCB_BUTTON_PRESS: {
      xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;
      /* grab the offset from the subwindow's origin */
      if (ev->event == swin) {
        offset[0] = ev->event_x;
        offset[1] = ev->event_y;
	
        drag_state = 1;
      }
      break;
    }

    case XCB_BUTTON_RELEASE: {
      drag_state = 0;


      break;
    }

    case XCB_CONFIGURE_NOTIFY: {
      xcb_configure_notify_event_t *ev = (xcb_configure_notify_event_t*)e;
      if (ev->event == swin) {
	puts("pz");
	//origin[0] = ev->x;
	//origin[1] = ev->y;
      }
      break;
    }

    case XCB_EXPOSE: {
      draw_button(swin);
      xcb_flush(c);
      break;
    }

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


 
int main(void)
{
  c = xcb_connect (NULL, NULL);
  screen = xcb_setup_roots_iterator (xcb_get_setup (c)).data;


  create_window();
  make_button();
  draw_button(swin);

  xcb_flush (c);

  event_loop();

 
  puts("bye!");
  return EXIT_SUCCESS;
}
