#define MAXX 100
#define MAXY 100

typedef struct _Ant
{
  char color;
  char dir;
  char food;
} Ant;

typedef enum _Terrain
{
  GRASS = 0,
  ROCKS = 1,
  HILL_RED = 2,
  HILL_BLACK = 3
} Terrain;

typedef struct _Tile
{
  unsigned char mark1;
  unsigned char mark2;
  Ant ant;
  Terrain terrain;
  int food;
} Tile;

Tile Map[MAXY][MAXX];

int round;
int roundMax;
int redScore;
int blackScore;

void DataClean(void);
void DataOpen(char* name);
void DataSeek(int r);
void DataClose();
