#include <stdlib.h>
#include <stdio.h>
#include <string.h>
 
#include <xcb/xcb.h>

xcb_connection_t    *c;
xcb_screen_t        *screen;
xcb_gcontext_t       gc;
xcb_drawable_t       win;
xcb_drawable_t back_pixmap;
int winwidth;
int winheight;
xcb_gcontext_t fill;
/*  event callback functin pointer
    int -> void
    where int is a button/object id */
typedef void (fptr_t)(int);
typedef enum {DRAGGABLE, DRAGGABLE_X, DRAGGABLE_Y} obj_type;

typedef struct button_t {
  xcb_drawable_t id;
  obj_type obj_type;
  char label[100];
  char label_orig;
  char label_w;
  char label_h;
  int drag_state;
  uint32_t offset[2];
  uint32_t origin[2];
  fptr_t *click_callback;

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
  
  cookie = xcb_query_text_extents(c, font, strlen(string), xcb_str);
  reply = xcb_query_text_extents_reply(c, cookie, NULL);
  
  if(!reply) puts("STRING ERROR");
  
  *width = reply->overall_width;
  *height = reply->font_ascent + reply->font_descent;
  *origin = reply->font_ascent;
  
  free(xcb_str);
  free(reply);
}

button_t create_button(obj_type otype, char *label, int x, int y, fptr_t *click_callback) {
  uint32_t mask = 0;
  uint32_t values3[3];
  xcb_drawable_t window_id;
  
  int width, height, origin;
  xcb_font_t font;

  font = xcb_generate_id(c);
  char * font_name = "7x13";
  xcb_open_font(c, font, strlen(font_name), font_name);
  measure_string(label, font, &width, &height, &origin);

  button_t b;

  window_id = xcb_generate_id(c);

  mask = XCB_CW_BACK_PIXEL | XCB_CW_BORDER_PIXEL | XCB_CW_EVENT_MASK;
  values3[0] = screen->white_pixel;
  values3[1] = screen->black_pixel;
  values3[2] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_BUTTON_PRESS |
    XCB_EVENT_MASK_BUTTON_RELEASE | XCB_EVENT_MASK_BUTTON_MOTION | 
    XCB_EVENT_MASK_ENTER_WINDOW   | XCB_EVENT_MASK_LEAVE_WINDOW   |
    XCB_EVENT_MASK_KEY_PRESS;
  xcb_create_window(c,
		    XCB_COPY_FROM_PARENT,
		    window_id,
		    win,
		    x, y,
		    width, height,
		    1,
		    XCB_WINDOW_CLASS_INPUT_OUTPUT,
		    screen->root_visual,
		    mask, values3);


  b = (button_t){window_id, otype, "", origin, width, height, 0, {0,0},{x,y}, click_callback};

  if(strlen(label) < 100) {
    strcpy(b.label, label);
  } else {
    puts("error: tried to make button with label > 100 chars");
    exit(0);
  }

  xcb_map_window(c, window_id);
  return b;
}

/* draws button on given window */
void draw_button(button_t *b) {
  uint8_t string_len = strlen(b->label);
  xcb_image_text_8(c, string_len, b->id, gc, 0, b->label_orig, b->label);
}

void create_window() {
  uint32_t mask = 0;
  uint32_t values[3];

  back_pixmap = xcb_generate_id (c);
  xcb_create_pixmap(c,
		    screen->root_depth,
		    back_pixmap,
		    screen->root,
		    800, 800);
  /* context for filling with white */
  fill = xcb_generate_id(c);
  mask = XCB_GC_FOREGROUND;
  values[0] = screen->white_pixel;
  xcb_create_gc(c, fill, back_pixmap, mask,values);

  /* create graphics context */
  gc = xcb_generate_id (c);
  mask = XCB_GC_FOREGROUND | XCB_GC_BACKGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = screen->black_pixel;
  values[1] = screen->white_pixel;
  values[2] = 0;
  xcb_create_gc (c, gc, screen->root, mask, values);
 
  /* create the window */
  win = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXMAP  | XCB_CW_EVENT_MASK;
  values[0] = back_pixmap;
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
  xcb_poly_fill_rectangle(c, back_pixmap, fill, 1, (xcb_rectangle_t[]){{ 0, 0, 800, 800}});
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
	  switch(b->obj_type) {
	    
	  case DRAGGABLE: {
	    b->origin[0] += mev->event_x - b->offset[0];
	    b->origin[1] += mev->event_y - b->offset[1];
	    xcb_configure_window(c,b->id,XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,b->origin);
	    b->click_callback(i);
	    break;
	  }

	  case DRAGGABLE_X: {
	    b->origin[0] += mev->event_x - b->offset[0];
	    /* b->origin[1] += mev->event_y - b->offset[1]; */
	    xcb_configure_window(c,b->id,XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,b->origin);
	    b->click_callback(i);
	    break;
	  }

	  case DRAGGABLE_Y: {
	    /* b->origin[0] += mev->event_x - b->offset[0]; */
	    b->origin[1] += mev->event_y - b->offset[1];
	    xcb_configure_window(c,b->id,XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,b->origin);
	    b->click_callback(i);
	    break;
	  }

	    
	  }
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
	  b->drag_state = 0;
	  break;
	}
      }


      break;
    }
      
    case XCB_CONFIGURE_NOTIFY: {
      xcb_configure_notify_event_t *ev = (xcb_configure_notify_event_t *)e;
      if (ev->window == win) {
	winwidth = ev->width;
	winheight = ev->height;
      }
      
    }

    case XCB_EXPOSE: {
      xcb_expose_event_t *ev = (xcb_expose_event_t *)e;
      button_t* b;
      int i;
      
      for (i=0;i<button_heap_counter;i++) {
	b = &button_heap[i];
	if (ev->window == b->id) {
	  draw_button(b);
	}
      }

      xcb_flush(c);
      break;
    }
      
    case XCB_KEY_PRESS: {
      xcb_key_press_event_t *ev;
      ev = (xcb_key_press_event_t *)e;

      

      if (ev->detail == 9)
	return;
      break;
    }
  }
  free (e);
  }
}


void x_y(int b_id) {
  button_t* b = &button_heap[b_id];
  int val = (b->origin[0]*100) / winwidth   ;
  int val2 = (b->origin[1]*100) / winheight  ;
  
  char str[15];
  sprintf(str, "xy:%d,%d", val, val2);

  xcb_poly_fill_rectangle(c,win, fill, 1, (xcb_rectangle_t[]){{ 20, 0, 80, 40}});
  xcb_image_text_8(c, strlen(str), win, gc, 20, 20, str);
  
}


void x(int b_id) {
  button_t* b = &button_heap[b_id];
  int val = (b->origin[0]*100) / winwidth   ;
  char str[15];
  sprintf(str, "x:%d", val);

  xcb_poly_fill_rectangle(c,win, fill, 1, (xcb_rectangle_t[]){{ 100, 0, 80, 40}});
  xcb_image_text_8(c, strlen(str), win, gc, 100, 20, str);
}


void y(int b_id) {
  button_t* b = &button_heap[b_id];
  int val = (b->origin[1]*100) / winheight  ;
  char str[15];
  sprintf(str, "y:%d", val);

  xcb_poly_fill_rectangle(c,win, fill, 1, (xcb_rectangle_t[]){{ 180, 0, 80, 40}});
  xcb_image_text_8(c, strlen(str), win, gc, 180, 20, str);
  
}

int main(void)
{
  c = xcb_connect (NULL, NULL);
  screen = xcb_setup_roots_iterator (xcb_get_setup (c)).data;

  create_window();

  alloc_button(create_button(DRAGGABLE, "xy", 50,50, &x_y));
  alloc_button(create_button(DRAGGABLE_X, "x", 200,50, &x));
  alloc_button(create_button(DRAGGABLE_Y, "y", 50,100, &y));

  xcb_flush (c);

  event_loop();
  puts("bye!");
  return EXIT_SUCCESS;
}
