/* x.c, Marc Feeley (09/94) */
#include <stdio.h>
#include <stdlib.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

/*--------------------------------------------------------------------------*/

#define X_MAX_NB_WINDOWS 10
#define X_MAX_NB_COLORS 256

static int x_initialized = 0;

static Display *x_display;
static int x_screen;
static int x_depth;
static int x_black, x_white;
static Visual *x_visual;
static int x_nb_colors;

static struct {
  int in_use;
  int width, height;
  Window wind;
  Colormap cmap;
  GC color[X_MAX_NB_COLORS];
  XFontStruct *font;
  } x_wind[X_MAX_NB_WINDOWS];


static void x_check_wind( w )
int w;
{
  if ((!x_initialized) ||
      (w < 0) || (w >= X_MAX_NB_WINDOWS) ||
      (!x_wind[w].in_use))
  { fprintf( stderr, "X: Invalid window id (%d)\n", w ); exit(1); }
}


#define PI 3.1415926

static double x_sin(x)
double x;
{
  if (x>PI/2.) x = PI - x;
  else if (x<-PI/2.) x = -PI - x;
  return x*(1.-x*x/2./3.*(1.-x*x/4./5.*(1.-x*x/6./7.)));
}


void x_clear_window( w )
int w;
{
  x_check_wind( w );
  XFillRectangle( x_display, x_wind[w].wind, x_wind[w].color[0],
                  0, 0, x_wind[w].width, x_wind[w].height );
  XFlush( x_display );
}


int x_open_window( title, width, height )
char *title;
int width, height;
{
  int w;
  XEvent report;

  if (!x_initialized)
  {
    int i;

    /* connect to X server */

    if ((x_display = XOpenDisplay(NULL)) == NULL)
    {
      fprintf( stderr, "X: Can't connect to X server %s\n",
                       XDisplayName(NULL) );
      exit(1);
    }

    x_screen = DefaultScreen( x_display );

    x_black = BlackPixel( x_display, x_screen );
    x_white = WhitePixel( x_display, x_screen );

    if ((x_black < 0) || (x_black > 1) ||
        (x_white < 0) || (x_white > 1))
    { fprintf( stderr, "X: Black and white must be 0 or 1\n" ); exit(1); }

    x_visual = DefaultVisual( x_display, x_screen );

    x_depth = DefaultDepth( x_display, x_screen );

    for (i=0; i<X_MAX_NB_WINDOWS; i++) x_wind[i].in_use = 0;

    x_initialized = 1;
  }

  for (w=0; w<X_MAX_NB_WINDOWS; w++)
    if (!x_wind[w].in_use) break;
  if (w==X_MAX_NB_WINDOWS) return -1;

  /* get font */

  if ((x_wind[w].font=XLoadQueryFont(x_display,"6x10")) == NULL)
    if ((x_wind[w].font=XLoadQueryFont(x_display,"fixed")) == NULL)
    { fprintf( stderr, "X: Can't find font 6x10 or fixed\n" ); exit(1); }

  /* create the window */

  x_wind[w].in_use = 1;
  x_wind[w].width  = width;
  x_wind[w].height = height;

  x_wind[w].wind =
    XCreateSimpleWindow( x_display,
                         RootWindow( x_display, x_screen ),
                         0, 0,
                         width, height,
                         0, x_white, x_black );

  if (x_wind[w].wind == NULL)
  { fprintf( stderr, "X: Can't create window\n" ); exit(1); }

  XSetStandardProperties( x_display, x_wind[w].wind, title, title,
                          NULL, NULL, 0, NULL );

  /* set graphic context */

  if (x_depth == 1)
  {
    int i;
    XGCValues values;

    x_nb_colors = 2;

    for (i=0; i<x_nb_colors; i++)
    {
      values.foreground = (i==0) ? x_black : x_white;
      values.background = x_black;

      x_wind[w].color[i] =
        XCreateGC( x_display,
                   RootWindow( x_display, x_screen ),
                   (GCForeground | GCBackground),
                   &values );

      XSetFont( x_display, x_wind[w].color[i], x_wind[w].font->fid );
    }
  }
  else if (x_depth == 8)
  {
    int i;
    XGCValues values;
    XColor colors[X_MAX_NB_COLORS];

    x_nb_colors = X_MAX_NB_COLORS;

    x_wind[w].cmap =
      XCreateColormap( x_display,
                       RootWindow( x_display, x_screen ),
                       x_visual,
                       AllocAll );

    colors[0].pixel = 0;
    colors[0].red   = (x_black == 0) ? 0 : 65535;
    colors[0].green = (x_black == 0) ? 0 : 65535;
    colors[0].blue  = (x_black == 0) ? 0 : 65535;
    colors[0].flags = DoRed | DoGreen | DoBlue;
    colors[1].pixel = 1;
    colors[1].red   = (x_black != 0) ? 0 : 65535;
    colors[1].green = (x_black != 0) ? 0 : 65535;
    colors[1].blue  = (x_black != 0) ? 0 : 65535;
    colors[1].flags = DoRed | DoGreen | DoBlue;
    for (i=2; i<x_nb_colors; i++)
    {
      double x = -PI+2.*PI*(i-2)/(x_nb_colors-2);
      double y = x+2.*PI/3.;
      double z = y+2.*PI/3.;
      colors[i].pixel = i;
      colors[i].red   = (int)(65535*(x_sin(x)+1.)/2.);
      colors[i].green = (int)(65535*(x_sin(y)+1.)/2.);
      colors[i].blue  = (int)(65535*(x_sin(z)+1.)/2.);
      colors[i].flags = DoRed | DoGreen | DoBlue;
    }
    XStoreColors( x_display, x_wind[w].cmap, colors, x_nb_colors );
    XInstallColormap( x_display, x_wind[w].cmap );

    XSetWindowColormap( x_display, x_wind[w].wind, x_wind[w].cmap );

    for (i=0; i<x_nb_colors; i++)
    {
      values.foreground = (i<2) ? ((i==0) ? x_black : x_white) : i;
      values.background = 0;
      values.fill_style = FillSolid;

      x_wind[w].color[i] =
        XCreateGC( x_display,
                   RootWindow( x_display, x_screen ),
                   (GCForeground | GCBackground | GCFillStyle),
                   &values );

      XSetFont( x_display, x_wind[w].color[i], x_wind[w].font->fid );
    }
  }
  else
  { fprintf( stderr, "X: Display depth should be 1 or 8\n" ); exit(1); }

  /* display window */

  XMapWindow( x_display, x_wind[w].wind );
  XFlush( x_display );

  /* wait until window appears */

  XSelectInput( x_display, x_wind[w].wind, ExposureMask );
  XWindowEvent( x_display, x_wind[w].wind, ExposureMask, &report );

  x_clear_window( w );

  return w;
}


void x_close_window( w )
int w;
{
  int i;
  x_check_wind( w );
  XDestroyWindow( x_display, x_wind[w].wind );
  if (x_depth != 1)
    XUninstallColormap( x_display, x_wind[w].cmap );
  x_wind[w].in_use = 0;
  for (i=0; i<X_MAX_NB_WINDOWS; i++)
    if (x_wind[i].in_use) return;
  XCloseDisplay( x_display );
  x_initialized = 0;
}


void x_draw_text( w, col, x, y, str, center )
int w, col, x, y;
char *str;
int center;
{
  int len, width, xx, yy;

  x_check_wind( w );
  len = 0; while (str[len] != '\0') len++;
  width = XTextWidth( x_wind[w].font, str, len );
  yy = x_wind[w].height - (y /* - x_wind[w].font->max_bounds.ascent/2 */);
  switch (center)
  {
    case 0: xx = x;           break;
    case 1: xx = x - width/2; break;
    case 2: xx = x - width;   break;
  }
  XDrawString( x_display, x_wind[w].wind, x_wind[w].color[col],
               xx, yy, str, len );
  XFlush( x_display );
}


void x_draw_line( w, col, x1, y1, x2, y2 )
int w, col, x1, y1, x2, y2;
{
  x_check_wind( w );
  XDrawLine( x_display, x_wind[w].wind, x_wind[w].color[col],
             x1, x_wind[w].height-y1, x2, x_wind[w].height-y2 );
  XFlush( x_display );
}


void x_draw_image( w, x, y, width, height, data )
int w, x, y, width, height;
int *data;
{
  XImage *image;
  int i, j;
  int *p;
  int bytes_per_line;
  char *pixels;

  x_check_wind( w );

  if (x_depth == 1)
    bytes_per_line = (width+8-1)/8;
  else
    bytes_per_line = width;

  pixels = malloc( height * bytes_per_line );
  if (pixels == NULL)
  { fprintf( stderr, "X: Memory overflow\n" ); exit(1); }

  image = XCreateImage( x_display,
                        DefaultVisual( x_display, x_screen ),
                        x_depth,
                        ZPixmap, 0,
                        pixels, width, height,
                        32, bytes_per_line );

  if (image == NULL)
  { fprintf( stderr, "X: Memory overflow\n" ); exit(1); }

  image->byte_order = MSBFirst;
  image->bitmap_bit_order = MSBFirst;

  p = data;
  if (x_depth == 1)
    for (j=height-1; j>=0; j--)
      for (i=0; i<width; i++)
      {
        int pix = *p++;
        if ((pix&1) == x_black)
          pixels[bytes_per_line*j+i/8] &= ~(1 << (7 - i%8));
        else
          pixels[bytes_per_line*j+i/8] |= (1 << (7 - i%8));
      }
  else
    for (j=height-1; j>=0; j--)
      for (i=0; i<width; i++)
      {
        int pix = *p++;
        if (pix < 2)
        { if (pix == 0) pix = x_black; else pix = x_white; }
        else
          pix = 2 + (pix-2)%(x_nb_colors-2);
        pixels[bytes_per_line*j+i] = pix; /* XPutPixel( image, i, j, pix ); */
      }

  XPutImage( x_display, x_wind[w].wind, x_wind[w].color[0], image,
             0, 0, x, x_wind[w].height-(y+height-1), width, height );

  XFlush( x_display );

  XDestroyImage( image );

  free( pixels );
}


/*---------------------------------------------------------------------------*/
