#include <SDL.h>

#include "Data.h"
#include "Viewer.h"

int main(int argc, char *argv[])
{
  if (argc<2)
  {
    printf("%s file\n", argv[0]);
    return 1;
  }
  
  DataOpen(argv[1]);
  ViewerInit();
  ViewerDraw();
  SDL_Event event;
  
  SDL_EnableKeyRepeat(100,100);
  
  while(SDL_WaitEvent(&event) && (event.type != SDL_QUIT) 
   && ((event.key.keysym.sym!=SDLK_q) && (event.key.keysym.sym!=SDLK_ESCAPE) || (event.type!=SDL_KEYDOWN)))
  {
    switch (event.type)
    {
      
      case SDL_KEYDOWN :
        switch (event.key.keysym.sym)
        {
          case SDLK_INSERT:
            DataSeek(-100);
          break;
          case SDLK_DELETE:
            DataSeek(100);
          break;
          case SDLK_HOME:
            DataSeek(-10);
          break;
          case SDLK_END :
            DataSeek(10);  
          break;
          case SDLK_PAGEUP:
            DataSeek(-1);  
          break;
          case SDLK_PAGEDOWN :
            DataSeek(1);  
          break;
       
          case SDLK_LEFT:
            ViewerMove(-10,0);
          break;
          case SDLK_RIGHT :
            ViewerMove(10,0);
          break;
          case SDLK_UP:
            ViewerMove(0,-10);
          break;
          case SDLK_DOWN :
            ViewerMove(0,10);
          break;
        }
        ViewerDraw();
    }
  }
  ViewerFree();
  DataClose();
  return 0;
}
