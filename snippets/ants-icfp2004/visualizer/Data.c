#include <stdio.h>
#include <string.h>

#include "Data.h"

#define ROUNDS 100000
#define BUFLEN 256

static fpos_t idx[ROUNDS];
static FILE* f; 
static char buf[BUFLEN];


static void DataGetIndex()
{
  int i = 0;
  fseek(f, 0L, SEEK_SET);
  while (fgets(buf, BUFLEN, f) != NULL)
  {
    if (strncmp(buf, "After round ", 12) == 0)
    {
      char *n = buf+12;
      if (i%100 == 0) printf("Index for %i rounds.\n", i);
      fgetpos(f, &idx[i++]);
    }
  }
  roundMax = i-1;
}

static void DataParseLine(char *l)
{
  if (strncmp(buf, "cell (", 6) == 0)
  {
    int x, y, id, dir, food, state, resting;
    sscanf(l, "cell (%i, %i", &x, &y);
    
    if (Map[y][x].terrain == HILL_RED)
      redScore -= Map[y][x].food;
    if (Map[y][x].terrain == HILL_BLACK)
      blackScore -= Map[y][x].food;
    memset(&Map[y][x], 0, sizeof(Tile));
    
    l = strchr(l, ':');

    while (l!=NULL)
    {
      l++; 
      while (*l==' ') l++;
      if (strncmp(l, "red hill", 8) == 0)
      {
        Map[y][x].terrain = HILL_RED;
      } else
      if (strncmp(l, "black hill", 9) == 0)
      {
        Map[y][x].terrain = HILL_BLACK;
      } else
      if (strncmp(l, "rock", 4) == 0)
      {
        Map[y][x].terrain = ROCKS;
      } else
      if (sscanf(l, "%i food", &food) == 1)
      {
        Map[y][x].food = food;
      } else
      if (strncmp(l, "black marks: ", 13) == 0)
      {
        l+=13;
        while ((*l >= '0') && (*l <= '5'))
        {
          Map[y][x].mark1 |=  1 << (*l - '0' + 1 );
          l++;
        }
      } else
      if (strncmp(l, "red marks: ", 11) == 0)
      {
        l+=11;
        while ((*l >= '0') && (*l <= '5'))
        {
          Map[y][x].mark2 |=  1 << (*l - '0' + 1 );
          l++;
        }
      }
      if (sscanf(l, "black ant of id %i, dir %i, food %i, state %i, resting %i", &id, &dir, &food, &state, &resting) == 5)
      {
        Map[y][x].ant.color = 2;
        Map[y][x].ant.dir = dir;
        Map[y][x].ant.food = food;
      } else
      if (sscanf(l, "red ant of id %i, dir %i, food %i, state %i, resting %i", &id, &dir, &food, &state, &resting) == 5)
      {
        Map[y][x].ant.color = 1;
        Map[y][x].ant.dir = dir;
        Map[y][x].ant.food = food;
      } 
      l = strchr(l, ';');
    }
    if (Map[y][x].terrain == HILL_RED)
      redScore += Map[y][x].food;
    if (Map[y][x].terrain == HILL_BLACK)
      blackScore += Map[y][x].food;
  }
}

static void DataApplyRound(int r)
{
  fsetpos(f, &idx[r]);
  while ((fgets(buf, BUFLEN, f) != NULL) && (strncmp(buf, "After round ", 12) != 0))
    DataParseLine(buf);
}

static void DataUpdateToRound(int r)
{
  if (r < 0) r = 0; else
  if (r > roundMax) r = roundMax;
  if (r < round)
  {
    round = 0;
    DataClean();
    DataApplyRound(0);
  }
  while (round < r)
      DataApplyRound(++round);
}

void DataClean()
{
  memset(Map, 0, sizeof(Map));
  redScore = 0;
  blackScore = 0;
}

void DataOpen(char* name)
{
  f = fopen(name, "rb");
  DataGetIndex();
  round = 0;
  DataClean();  
  DataApplyRound(0);
}

void DataSeek(int r)
{
  DataUpdateToRound(round + r);
}

void DataClose()
{
  fclose(f);
}

