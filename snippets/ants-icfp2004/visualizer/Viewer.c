#include <SDL.h>

#include "Viewer.h"
#include "Data.h"

static SDL_Surface* screen;
static SDL_Surface* tile;
static SDL_Surface* ants;
static int Viewer_x;
static int Viewer_y;
static char title[128];

static int ViewerDrawCell(int x, int y)
{
  SDL_Rect src, dest;
  int i;
  
  int gy = (TILESIZEY - TILEMIDSIZE)*y;
  int gx = x * (TILESIZEX-1) + TILEMIDSIZE*(y%2);
  
  if (gx > WINX) return 1;
  
  x+=Viewer_x;
  y+=Viewer_y;
  if (y>99) y = 99;
  if (x>99) x = 99;
  
  dest.x = gx;
  dest.y = gy;
  
  src.x = Map[y][x].terrain* TILESIZEX;
  src.y = 0;
  src.w = TILESIZEX;
  src.h = TILESIZEY;
  
  SDL_BlitSurface(tile , &src , screen , &dest);
  
  i = Map[y][x].food;
  if (i > 0)
  {
      dest.x = gx + 6;
      dest.y = gy + TILESIZEY - TILEMIDSIZE - TILEMIDSIZE/3 - i;
      dest.w = 12;
      dest.h = i;
      SDL_FillRect(screen, &dest, SDL_MapRGB(screen->format, 0, 190, 0));
  }
  
  char marks = Map[y][x].mark1; 
  for (i=0; i<6; i++)
  {
    if (marks%2)
    {
      dest.x = gx + TILESIZEX - 8;
      dest.y = gy + TILESIZEY - TILEMIDSIZE - TILEMIDSIZE/3 - i*2 ;
      dest.w = 2;
      dest.h = 2;
      SDL_FillRect(screen, &dest, SDL_MapRGB(screen->format, 255*((i+1)%2), 0, 255*(i%2)));
    }
    marks >>= 1;
  }
  
  marks = Map[y][x].mark2; 
  for (i=0; i<6; i++)
  {
    if (marks%2)
    {
      dest.x = gx + TILESIZEX - 5;
      dest.y = gy + TILESIZEY - TILEMIDSIZE - TILEMIDSIZE/3  - i*2 ;
      dest.w = 2;
      dest.h = 2;
      SDL_FillRect(screen, &dest, SDL_MapRGB(screen->format, 255*((i+1)%2), 0, 255*(i%2)));
    }
    marks >>= 1;
  }
  
  if (Map[y][x].ant.color > 0)
  {
    dest.x = gx + TILEMIDSIZE/3;
    dest.y = gy + TILESIZEY/2 - 15/2 - 1;
    dest.w = 14;
    dest.h = 15;
  
    src.x = Map[y][x].ant.dir * 14;
    src.y = ((Map[y][x].ant.color-1)+Map[y][x].ant.food*2) * 15 ;
    src.w = 14;
    src.h = 15;
    SDL_BlitSurface(ants , &src , screen , &dest);
  }
  return 0;
}

void ViewerInit()
{
  SDL_Init(SDL_INIT_VIDEO );
  screen = SDL_SetVideoMode(WINX, WINY , 0 , 0);
  tile = SDL_LoadBMP("hexs.bmp");
  SDL_SetAlpha(tile, SDL_SRCALPHA,255);
  SDL_SetColorKey(tile, SDL_SRCCOLORKEY, SDL_MapRGB(tile->format, 115, 113, 115));
  
  ants = SDL_LoadBMP("ants.bmp");
  SDL_SetAlpha(ants, SDL_SRCALPHA,255);
  SDL_SetColorKey(ants, SDL_SRCCOLORKEY, SDL_MapRGB(ants->format, 115, 113, 115));

  Viewer_x = 0;
  Viewer_y = 0;
}

void ViewerFree()
{
  free(tile);
  free(ants);
  SDL_Quit();
}


void ViewerDraw()
{
  int x; int y;
  sprintf(title, "Round: %i/%i, Red's: %i, Black's: %i", round, roundMax,redScore, blackScore);
  SDL_WM_SetCaption(title, NULL);
  
  for (y=0;y<100;y++)
    for (x=0;x<100;x++)
      if (ViewerDrawCell(x,y)) break;
    
  SDL_Flip(screen);
}

void ViewerMove(int x, int y)
{
  Viewer_x += x;
  Viewer_y += y;
  
  if (Viewer_x < 0) Viewer_x = 0;
  else  if (Viewer_x > 69) Viewer_x = 69;
  
  if (Viewer_y < 0) Viewer_y = 0;
  else  if (Viewer_y > 75) Viewer_y = 75;
}

