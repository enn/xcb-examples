#include <stdlib.h>
#include <stdio.h>
#include <string.h>
 
#include <xcb/xcb.h>


xcb_connection_t    *c;
xcb_screen_t        *screen;
xcb_gcontext_t       gc;
xcb_drawable_t       win;

/*  event callback functin pointer */
typedef void (*fptr_t)(void* o);

typedef struct button_t {
  xcb_drawable_t id;
  int drag_state;
  uint32_t offset[2];
  uint32_t origin[2];

  /* fptr_t *press_callback; */
  /* fptr_t *release_callback; */

} button_t;

#define BUTTON_HEAP_SIZE 20
int button_heap_counter = 0;
button_t button_heap[BUTTON_HEAP_SIZE];

button_t* alloc_button(button_t b) {
  if(button_heap_counter < BUTTON_HEAP_SIZE) {
    button_heap[button_heap_counter] = b;
    printf("registered button: %d to %d\n", b.id, button_heap_counter);
    return &button_heap[button_heap_counter++];
  } else {
    puts("can't allocate button, out of space");
    exit(0);
  }
}

button_t create_button(int x, int y) {
  uint32_t mask = 0;
  uint32_t values3[3];
  xcb_drawable_t window_id;

  window_id = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXEL | XCB_CW_BORDER_PIXEL | XCB_CW_EVENT_MASK;
  values3[0] = screen->white_pixel;
  values3[1] = screen->black_pixel;
  values3[2] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_BUTTON_PRESS |
    XCB_EVENT_MASK_BUTTON_RELEASE |  XCB_EVENT_MASK_BUTTON_MOTION | 
    XCB_EVENT_MASK_ENTER_WINDOW   | XCB_EVENT_MASK_LEAVE_WINDOW   |
    XCB_EVENT_MASK_KEY_PRESS;
  xcb_create_window(c,
		    XCB_COPY_FROM_PARENT,
		    window_id,
		    win,
		    x, y,
		    50, 15,
		    1,
		    XCB_WINDOW_CLASS_INPUT_OUTPUT,
		    screen->root_visual,
		    mask, values3);

  xcb_map_window(c, window_id);
  return (button_t){window_id, 0,{0,0}, {x,y}};
}

/* draws button on given window */
void draw_button_window(xcb_window_t w) {
  char string[] = "Drag me";
  uint8_t string_len = strlen(string);
  xcb_image_text_8(c, string_len, w, gc, 6, 12, string);
}

void create_window() {
  uint32_t mask = 0;
  uint32_t values[3];

  /* create graphics context */
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
      button_t * b;
      int i;
      for (i=0;i<button_heap_counter;i++) {
	b = &button_heap[i];
	if (mev->event == b->id && b->drag_state) {
	  printf("motion: %d; x:%d, y:%d\n", b->id, mev->event_x, mev->event_y);
	  b->origin[0] += mev->event_x - b->offset[0];
	  b->origin[1] += mev->event_y - b->offset[1];
	  xcb_configure_window(c,b->id,XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,b->origin);
	  break;
	}
      }

      xcb_flush(c);
      break;
    }

    case XCB_BUTTON_PRESS: {
      xcb_button_press_event_t *ev = (xcb_button_press_event_t *)e;
      button_t * b;
      int i;

      for (i=0;i<button_heap_counter;i++) {
	b = &button_heap[i];

	/* grab the offset from the subwindow's origin */
	if (ev->event == b->id) {
	  printf("press: %d\n", b->id);
	  b->offset[0] = ev->event_x;
	  b->offset[1] = ev->event_y;
	  b->drag_state = 1;
	  break;
	}
      }

      break;
    }

    case XCB_BUTTON_RELEASE: {
      xcb_button_release_event_t *ev = (xcb_button_release_event_t *)e;
      button_t* b;
      int i;

      for (i=0;i<button_heap_counter;i++) {
	b = &button_heap[i];
	if (ev->event == b->id) {
	  printf("release: %d\n", b->id);
	  b->drag_state = 0;
	  break;
	}
      }


      break;
    }

    case XCB_EXPOSE: {
      xcb_expose_event_t *ev = (xcb_expose_event_t *)e;
      if(ev->window != win) {
	draw_button_window(ev->window);
      }
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

  alloc_button(create_button(50,50));
  alloc_button(create_button(200,50));
  alloc_button(create_button(50,100));
  alloc_button(create_button(200,100));
  xcb_flush (c);

  event_loop();

 
  puts("bye!");
  return EXIT_SUCCESS;
}
