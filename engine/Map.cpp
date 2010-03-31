#include "Map.h"
#include "Randomizer.h"
#include "PascalFunctions.h"

TMap::TMap()
{
  filled = false;
  //initialize map tiles
  LongInt i, j, k;
  for (i = 0; i<cMap_X; ++i)
  {
    for (j = 0; j<cMap_Y; ++j)
    {
      for (k = cMin_Nations; k<=cMax_Nations; ++k)
        discovered[i][j][k] = false;
      tiles[i][j] = NULL;
      river[i][j] = cMapRiverNone;
    }//for
  }
}//construc

TMap::~TMap()
{
  Byte i, j;
  for (i=0; i< cMap_X; ++i)
  {
    for (j =0; j<cMap_Y; ++j)
    {
      if (tiles[i][j]!=NULL)
      {
        delete tiles[i][j];
        tiles[i][j] = NULL;
      }
    }
  }
}//destruc

void TMap::Generate(const float Landmass)
{
  LongInt i,j;
  //highest and lowest row are arctic
  for (i =1; i<=cMap_X-2; ++i)
  {
    tiles[i][0] = new TTerrain(ttArctic);
    tiles[i][cMap_Y-1] = new TTerrain(ttArctic);
  }//for
  //first and last column are open sea (as in: goes to Europe)
  for (i=0; i<=cMap_Y-1; ++i)
  {
    tiles[0][i] = new TTerrain(ttOpenSea);
    tiles[cMap_X-1][i] = new TTerrain(ttOpenSea);
  }//for

  //fill inner map
  for (i=1; i<=cMap_X-2; ++i)
  {
    for (j=1; j<=cMap_Y-2; ++j)
    {
      if (Randomizer::GetSingleton().random()<=Landmass)
      {
        tiles[i][j] = new TTerrain(ttGrassland);
      }
      else
        tiles[i][j] = new TTerrain(ttSea);
    }
  }

  //smoothen landmass
  for (i=2; i<=cMap_X-3; ++i)
  {
    for (j=2; j<=cMap_Y-3; ++j)
    {
      if ((tiles[i-1][j]->m_Type==ttGrassland) and (tiles[i][j-1]->m_Type==ttGrassland) and
          (tiles[i+1][j]->m_Type==ttGrassland) and (tiles[i][j+1]->m_Type==ttGrassland))
          tiles[i][j]->m_Type = ttGrassland;
    }//for
  }

  //no rivers yet
  ClearRiverCache();

  //set the flag to indicate that we have data for all tiles
  filled = true;
}//proc

void TMap::GenerateSpecials(const bool LandOnly)
{
  if (!filled) Generate(0.7);
  LongInt i, j;
  for (i = 0; i<cMap_X; ++i)
    for (j = 0; j< cMap_Y; ++j)
    {
      if ((!LandOnly) or (!(tiles[i][j]->IsWater())))
        if (Randomizer::GetSingleton().random()<=0.15) tiles[i][j]->CreateSpecial();
    }
}//proc

void TMap::ClearRiverCache()
{
  Byte i,j;
  for (i=0; i<cMap_X; ++i)
    for (j=0; j<cMap_Y; ++j)
      river[i][j] = cMapRiverNone;
}//proc

void TMap::GenerateRiverCache()
{
  if (!filled) return;
  Byte i,j;
  for (i= 0; i<cMap_X; ++i)
    for (j=0; j<cMap_Y; ++j)
    {
      if (tiles[i][j]->HasRiver())
      {
        SetRiverType(i,j);
      }
      else river[i][j] = cMapRiverNone;
    }//for
}//proc

Byte TMap::GetRiverType(const Byte x, const Byte y) const
{
  if ((x<cMap_X) and (y<cMap_Y))
    return river[x][y];
  return cMapRiverNone;
}//func

bool TMap::SaveToFile(const std::string& FileName) const
{
  std::ofstream fs;
  fs.open(FileName.c_str());
  if (!fs)
    return false;

  if (fs.tellp()>0) fs.seekp(0, std::ios::beg);
  //Header
  fs.write(cMapFileHeader.c_str(), cMapFileHeader.length());
  //The real data
  LongInt i,j;
  Byte Buffer;
  for (i=0; i<cMap_X; ++i)
    for (j=0; j<cMap_Y; ++j)
    {
      Buffer = Ord(tiles[i][j]->m_Type);
      fs.write((char*) &Buffer, 1);
      if (tiles[i][j]->HasRiver()) Buffer = TERRAIN_RIVER_BIT; else Buffer = 0;
      if (tiles[i][j]->HasRoad()) Buffer = Buffer | TERRAIN_ROAD_BIT;
      if (tiles[i][j]->HasSpecial()) Buffer = Buffer | TERRAIN_SPECIAL_BIT;
      if (tiles[i][j]->IsPloughed()) Buffer = Buffer | TERRAIN_PLOUGHED_BIT;
      fs.write((char*) &Buffer,1);
    }//for
  const bool Result = fs.good();
  fs.close();
  return Result;
}//func

bool TMap::LoadFromFile(const std::string& FileName)
{
  std::ifstream fs;
  fs.open(FileName.c_str());
  if (!fs)
    return false;

  char buffer[256];
  //read header
  fs.read(buffer, cMapFileHeader.length());
  if (!fs.good()) return false;
  buffer[cMapFileHeader.length()] = '\0';
  //check header
  if (std::string(buffer)!=cMapFileHeader) return false;

  //now read the real data
  LongInt i,j;
  for (i= 0; i< cMap_X; ++i)
    for (j= 0; j< cMap_Y; ++j)
    {
      fs.read(buffer, 2);
      if (!fs.good()) return false;
      if ((buffer[0]>20) or (buffer[0]<0)) //byte value does not represent a TTerrainType
      {
        return false;
      }//id
      if (tiles[i][j]!=NULL) delete tiles[i][j];
      tiles[i][j] = new TTerrain(TTerrainType(buffer[0]),
                     ((buffer[1] & TERRAIN_RIVER_BIT)!=0),
                     ((buffer[1] & TERRAIN_ROAD_BIT)!=0),
                     ((buffer[1] & TERRAIN_SPECIAL_BIT)!=0),
                     ((buffer[1] & TERRAIN_PLOUGHED_BIT)!=0));
    }//for

  fs.close();
  filled = true;
  GenerateRiverCache();
  return true;
}//func

//used later, for GUI
void TMap::SetRiverType(const Byte x, const Byte y)
{
  if ((x<cMap_X) and (y<cMap_Y) and filled)
  {
    river[x][y] = cMapRiverNone;
    if (tiles[x][y]->HasRiver())
    {
      if (x>0)
        if (tiles[x-1][y]->HasRiver()) river[x][y] = river[x][y] | cMapRiverWest;
      if (y>0)
        if (tiles[x][y-1]->HasRiver()) river[x][y] = river[x][y] | cMapRiverNorth;
      if (x+1<cMap_X)
        if (tiles[x+1][y]->HasRiver()) river[x][y] = river[x][y] | cMapRiverEast;
      if (y+1<cMap_Y)
        if (tiles[x][y+1]->HasRiver()) river[x][y] = river[x][y] | cMapRiverSouth;
    }//if
  }//if
}//proc

bool TMap::IsTouchingLand(const Byte x, const Byte y) const
{
  if (!filled) return false;
  LongInt i,j;
  for (i = x-1; i<= x+1; ++i)
    for (j = y-1; j<= y+1; ++j)
    {
      if ((i>=0) and (j>=0) and (i<cMap_X) and (j<cMap_Y))
      {
        if (!tiles[i][j]->IsWater()) return true;
      }
    }//for
  return false;
  //maybe we should check for tile[x,y] being land and exempt it from examination
  // or at least react in some way...
}//func

void TMap::DiscoverSurroundingTiles(const Byte x, const Byte y, const Byte cNation, const bool two_squares)
{
  if ((cNation>=cMin_Nations) and (cNation<=cMax_Nations))
  {
    LongInt i, j;
    for (i= x-1-Ord(two_squares); i<=x+1+Ord(two_squares); ++i)
      for (j= y-1-Ord(two_squares); j<= y+1+Ord(two_squares); ++j)
        if ((i>=0) and (j>=0) and (i<cMap_X) and (j<cMap_Y))
          discovered[i][j][cNation] = true;
  }//if
}//proc

void TMap::RevealAll(const LongInt num_Nation)
{
  if ((num_Nation<=cMax_Nations) and (num_Nation>=cMin_Nations))
  {
    Byte i, j;
    for (i= 0; i<cMap_X; ++i)
      for (j= 0; j<cMap_Y; ++j)
        discovered[i][j][num_Nation] = true;
  }//if
}//proc

bool TMap::IsDiscovered(const Byte x, const Byte y, const LongInt num_Nation) const
{
  if ((x>=cMap_X) or (y>=cMap_Y) or (num_Nation>cMax_Nations)
      or (num_Nation<cMin_Nations))
    return true;
  return discovered[x][y][num_Nation];
}//func
