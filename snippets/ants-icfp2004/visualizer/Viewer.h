#define TILESIZEX 26
#define TILEMIDSIZE 10
#define TILESIZEY 32
#ifndef WINX
#define WINX 1024
#endif
#ifndef WINY
#define WINY 768
#endif

void ViewerInit(void);
void ViewerFree(void);
void ViewerDraw(void);
void ViewerMove(int x, int y);
