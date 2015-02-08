/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011, 2015  Thoronador

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ***************************************************************************
*/

#include "Map.hpp"
#include "DebugWriter.hpp"
#include <cmath> //for sqrt()
#include <fstream>
#include <iostream>
#include "Randomizer.hpp"
#include "PascalFunctions_Files.hpp"
#include "PascalFunctions_TerrainType.hpp"

int sqr(const int x)
{
  return x*x;
}

float h2(const Byte mx, const Byte my, const Byte r, const float h, const Byte x, const Byte y)
{
  const float dist = sqrt(sqr(mx-x)+sqr(my-y))/r;
  return h*(1- sqrt(dist));
} //func

float h4(const Byte mx, const Byte my, const Byte r, const float h, const Byte x, const Byte y)
{
  const float dist = sqrt(sqr(mx-x)+sqr(my-y))/r;
  return h*(1- sqrt(sqrt(dist)));
} //func

TMap::TMap()
{
  filled = false;
  //initialize map tiles
  int i, j, k;
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
    {
      for (k = cMinEuropean; k <= cMaxEuropean; ++k)
        discovered[i][j][k] = false;
      tiles[i][j] = nullptr;
      river[i][j] = cMapRiverNone;
    } //for j
} //constructor

TMap::~TMap()
{
  Byte i;
  Byte j;
  for (i =0; i < cMap_X; ++i)
    for (j =0; j < cMap_Y; ++j)
      if (tiles[i][j]!=nullptr)
        tiles[i][j] = nullptr; //frees object managed by std::unique_ptr
} //destructor

void TMap::Generate(const float Landmass)
{
  //highest and lowest row are arctic
  int i;
  for (i =1; i <= cMap_X-2; ++i)
  {
    tiles[i][0] = std::unique_ptr<TTerrain>(new TTerrain(ttArctic));
    tiles[i][cMap_Y-1] = std::unique_ptr<TTerrain>(new TTerrain(ttArctic));
  }//for
  //first and last column are open sea (as in: goes to Europe)
  for (i = 0; i < cMap_Y; ++i)
  {
    tiles[0][i] = std::unique_ptr<TTerrain>(new TTerrain(ttOpenSea));
    tiles[cMap_X-1][i] = std::unique_ptr<TTerrain>(new TTerrain(ttOpenSea));
  } //for

  //fill inner map
  //Randomize;
  int j;
  for (i = 1; i <= cMap_X-2; ++i)
    for (j = 1; j <= cMap_Y-2; ++j)
      if (Randomizer::get().random() <= Landmass)
        tiles[i][j] = std::unique_ptr<TTerrain>(new TTerrain(ttGrassland));
      else
        tiles[i][j] = std::unique_ptr<TTerrain>(new TTerrain(ttSea));

  //smoothen landmass
  for (i = 2; i <= cMap_X-3; ++i)
    for (j =2; j <= cMap_Y-3; ++j)
    {
      if ((tiles[i-1][j]->m_Type==ttGrassland) and (tiles[i][j-1]->m_Type==ttGrassland) and
          (tiles[i+1][j]->m_Type==ttGrassland) and (tiles[i][j+1]->m_Type==ttGrassland))
          tiles[i][j]->m_Type = ttGrassland;
    } //for

  //no rivers yet
  ClearRiverCache();

  //set the flag to indicate that we have data for all tiles
  filled = true;
}//proc

float TerrainProbability(const int h1, const int h2, const int y)
{
  if (y < h1) return 1.0f;
  else if (y > h2) return 0.0f;
  else return -0.9/(h2-h1) * y +0.95+0.9*h1/static_cast<float>(h2-h1);
} //function

TTerrainType TerrainByH(const int h)
{
  const int cTundraStart = 1;
  const int cBorealStart = 7;
  const int cLeafStart = 14;
  const int cPrairieStart = 21;
  const int cSavannahStart = 28;
  const int cTropicalCenter = (cMap_Y / 2); //integer division
  const int cSavannahEnd = cMap_Y -28;
  const int cPrairieEnd = cMap_Y-21;
  const int cLeafEnd = cMap_Y-14;
  const int cBorealEnd = cMap_Y-7;
  const int cTundraEnd = cMap_Y-1;

  if ((cTundraStart <= h) && (h < cBorealStart))
  {
    if (Randomizer::get().random() < TerrainProbability(cTundraStart, cBorealStart, h))
      return ttTundra;
    else
      return ttBoreal;
  }

  if ((cBorealStart <= h) && (h < cLeafStart))
  {
    if (Randomizer::get().random() < TerrainProbability(cBorealStart, cLeafStart, h))
      return ttBoreal;
    else return ttBroadleaf;
  }

  if ((cLeafStart <= h) && (h < cPrairieStart))
  {
    if (Randomizer::get().random() < TerrainProbability(cLeafStart, cPrairieStart, h))
      return ttBroadleaf;
    else
      return ttPrairie;
  }

  if ((cPrairieStart <= h) && (h < cSavannahStart))
  {
    if (Randomizer::get().random() < TerrainProbability(cPrairieStart, cSavannahStart, h))
      return ttPrairie;
    else
      return ttSavannah;
  }

  if ((cSavannahStart <= h) && (h < cTropicalCenter))
  {
    if (Randomizer::get().random() < TerrainProbability(cSavannahStart, cTropicalCenter, h))
      return ttSavannah;
    else
      return ttRainForest;
  }

  if (cTropicalCenter == h)
    return ttTropicalForest;

  if ((cTropicalCenter < h) && (h <= cSavannahEnd))
  {
    if (Randomizer::get().random() < TerrainProbability(cTropicalCenter, cSavannahEnd, h))
      return ttRainForest;
    else
      return ttSavannah;
  }

  if ((cSavannahEnd < h) && (h <= cPrairieEnd))
  {
    if (Randomizer::get().random() < TerrainProbability(cSavannahEnd, cPrairieEnd, h))
      return ttSavannah;
    else
      return ttPrairie;
  }

  if ((cPrairieEnd < h) && (h <= cLeafEnd))
  {
    if (Randomizer::get().random() < TerrainProbability(cPrairieEnd, cLeafEnd, h))
      return ttPrairie;
    else
      return ttBroadleaf;
  }

  if ((cLeafEnd < h) && (h <= cBorealEnd))
  {
    if (Randomizer::get().random() < TerrainProbability(cLeafEnd, cBorealEnd, h))
      return ttBroadleaf;
    else
      return ttConiferForest;
  }

  if ((cBorealEnd < h) && (h <= cTundraEnd))
  {
    if (Randomizer::get().random() < TerrainProbability(cBorealEnd, cTundraEnd, h))
      return ttConiferForest;
    else
      return ttTundra;
  }

  //should not normally happen
  return ttArctic;
} //func

void TMap::Generate(const float LandMass, const THillFunction f, const TTemperatureType temperature, const TClimateType climate)
{
  if (LandMass >= 0.86)
  {
    std::cout << "Invalid parameter value for LandMass in TMap.Generate!\n"
              << "Using generation function with default parameter instead.\n";
    Generate(0.85, f, temperature, climate);
    return;
  }

  //highest and lowest row are arctic
  int i;
  for (i = 1; i <= cMap_X-2; ++i)
  {
    tiles[i][0] = std::unique_ptr<TTerrain>(new TTerrain(ttArctic));
    tiles[i][cMap_Y-1] = std::unique_ptr<TTerrain>(new TTerrain(ttArctic));
  }//for
  //first and last column are open sea (as in: goes to Europe)
  for (i = 0; i <= cMap_Y-1; ++i)
  {
    tiles[0][i] = std::unique_ptr<TTerrain>(new TTerrain(ttOpenSea));
    tiles[cMap_X-1][i] = std::unique_ptr<TTerrain>(new TTerrain(ttOpenSea));
  }//for

  // ---- generation of height map ----
  std::array<std::array<float, cMap_Y>, cMap_X> heightmap; //array [0..cMap_X-1, 0..cMap_Y-1] of Single;
  //first fill it with default value
  int j;
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
      heightmap[i][j] = -2.0; //randomly chosen

  unsigned int currentLandMass = 0;
  unsigned int count = 0;

  Byte radius;
  Byte h_x, h_y;
  while (static_cast<float>(currentLandMass)/static_cast<float>((cMap_X-1)*(cMap_Y-1)) < LandMass)
  {
    count = count +1;
    #ifdef DEBUG_CODE
    std::cout << "Info: Generation cycle count: " << count << "Land: " << currentLandMass << "\n";
    #endif
    //now set the hill's radius
    radius = 3 + Randomizer::get().random(0, 6);
    //...and its location
    /* Check for x-position is neccessary, because we don't want half an island
       popping in at the eastern or western end of the map.*/
    do
      h_x = Randomizer::get().random(cMap_X);
    while (!((h_x>radius) and (h_x<cMap_X-radius)));

    /* For the y-position: Too much checks will lead to isolated artic lines
       at the bottom and top of the map. Too few checks will lead to no
       (northern or southern) east-west passage to the "Pacific Ocean", because
       it will be blocked by land.

       To get out of this dilemma (sort of, at least we try to get out), we only
       check randomly.
    */
    h_y = Randomizer::get().random(cMap_Y);
    if ((h_y<=radius) and (h_y>=cMap_Y-radius) and (Randomizer::get().random()<0.33))
    {
      do
        h_y = Randomizer::get().random(cMap_Y);
      while ((h_y<=radius) or (h_y>=cMap_Y-radius));
    }//if

    #ifdef DEBUG_CODE
    std::cout << "    x: " << h_x  << ", y: " << h_y << ", r: " << radius << "\n";
    #endif
    for (i = h_x-radius; i <= h_x+radius; ++i)
      for (j = h_y-radius; j <= h_y+radius; ++j)
      {
        if (IsValidMapPosition(i,j))
          if (heightmap[i][j]<0.0)
          {
            heightmap[i][j] = f(h_x, h_y, radius, 2.5, i, j);
            if (heightmap[i][j]>0.0)
              currentLandMass = currentLandMass +1;
          }//if
      }//for j
  } //while

  // ---- generation of rivers ----
  std::array<std::array<bool, cMap_Y>, cMap_X> rivermap; //array [0..cMap_X-1, 0..cMap_Y-1] of Boolean;
  //fill with false (i.e. no river)
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
      rivermap[i][j] = false;

  //generate one river for every 3% of landmasson average, but still seems quite
  // dry there
  int river_count;
  switch (climate)
  {
    case ctDry:
         river_count = static_cast<int>(25*LandMass);
         break;
    case ctNormal:
         river_count = static_cast<int>(33*LandMass);
         break;
    case ctWet:
         river_count = static_cast<int>(50*LandMass);
         break;
    default:
         river_count = 0; //should never happen
         break;
  } //switch
  for (i = 1; i <= river_count; ++i)
  {
    //get a location that is land and has no river yet
    do
    {
      h_x = 3+Randomizer::get().random(cMap_X-6);
      h_y = 3+Randomizer::get().random(cMap_Y-6);
    }
    while ((heightmap[h_x][h_y] <= 0.0) || rivermap[h_x][h_y]);

    do
    {
      rivermap[h_x][h_y] = true;
      Byte new_x = h_x;
      Byte new_y = h_y;
      //search for lowest adjacent tile (only horizontal or vertical)
      if (IsValidMapPosition(h_x-1, h_y))
        if (heightmap[h_x-1][h_y] < heightmap[h_x][h_y])
        {
          new_x = h_x-1;
          new_y = h_y;
        }
      if (IsValidMapPosition(h_x+1, h_y))
        if (heightmap[h_x+1][h_y] < heightmap[new_x][new_y])
        {
          new_x = h_x+1;
          new_y = h_y;
        }
      if (IsValidMapPosition(h_x, h_y-1))
        if (heightmap[h_x][h_y-1] < heightmap[new_x][new_y])
        {
          new_x = h_x;
          new_y = h_y-1;
        }
      if (IsValidMapPosition(h_x, h_y+1))
        if (heightmap[h_x][h_y+1] < heightmap[new_x][new_y])
        {
          new_x = h_x;
          new_y = h_y+1;
        }
      //Now (new_x,new_y) should be coordinates of lowest adjecent tile. If
      // (new_x,new_y) equals (h_x,h_y), then we are already at the lowest.
      //In that case, just create a lake there by making that tile below zero.
      if ((new_x==h_x) && (new_y==h_y))
      {
        heightmap[h_x][h_y] = -0.5;
        rivermap[h_x][h_y] = true;
      }
      else
      {
        rivermap[new_x][new_y] = true;
        h_x = new_x;
        h_y = new_y;
      }//else
    }
    while (heightmap[h_x][h_y]>=0.0);
  } //for
  // ---- end of generation of rivers ----

  //now set the real tiles
  for (i = 1; i <= cMap_X-2; ++i)
    for (j = 1; j <= cMap_Y-2; ++j)
    {
      if (tiles[i][j] != nullptr)
      {
        tiles[i][j] = nullptr;
      }
      if (heightmap[i][j]<=0.0)
        tiles[i][j] = std::unique_ptr<TTerrain>(new TTerrain(ttSea, rivermap[i][j]));
      else if (heightmap[i][j]<=1.0)
        tiles[i][j] = std::unique_ptr<TTerrain>(new TTerrain(TerrainByH(j), rivermap[i][j]));
      else if (heightmap[i][j]<=2.0)
        tiles[i][j] = std::unique_ptr<TTerrain>(new TTerrain(ttHills, rivermap[i][j]));
      else tiles[i][j] = std::unique_ptr<TTerrain>(new TTerrain(ttMountains, rivermap[i][j]));
    }//for

  /*
  //smoothen landmass
  for (i = 2; i <= cMap_X-3; ++i)
    for (j = 2; j <= cMap_Y-3; ++j)
    {
      if ((tiles[i-1][j].m_Type==ttGrassland) and (tiles[i][j-1].m_Type==ttGrassland) and
          (tiles[i+1][j].m_Type==ttGrassland) and (tiles[i][j+1].m_Type==ttGrassland))
          tiles[i][j].m_Type = ttGrassland;
    }//for
  */
  //update river cache
  ClearRiverCache();
  //set the flag to indicate that we have data for all tiles
  filled = true;
  GenerateRiverCache();
}//func Generate (with f)

void TMap::GenerateSpecials(const bool LandOnly)
{
  if (!filled)
    Generate(0.7, h2, ttModerate, ctNormal);
  // else Randomize;
  int i, j;
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
      if ((!LandOnly) or (!tiles[i][j]->IsWater()))
        if (Randomizer::get().random() <= 0.15)
          tiles[i][j]->CreateSpecial();
} //proc

void TMap::ClearRiverCache()
{
  Byte i, j;
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
      river[i][j] = cMapRiverNone;
} //proc

void TMap::GenerateRiverCache()
{
  if (!filled) return;
  Byte i, j;
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
    {
      if (tiles[i][j]->HasRiver())
        SetRiverType(i,j);
      else river[i][j] = cMapRiverNone;
    }//for
}//proc

Byte TMap::GetRiverType(const Byte x, const Byte y) const
{
  if ((x<cMap_X) and (y<cMap_Y))
    return river[x][y];
  else return cMapRiverNone;
}//func

bool TMap::SaveToFile(const std::string& FileName) const
{
  std::ofstream fs(FileName, std::ios_base::trunc | std::ios_base::binary | std::ios_base::out);
  if (!fs.good() || !fs.is_open())
  {
    return false;
  }
  if (fs.tellp()>0)
    fs.seekp(0, std::ios_base::beg);
  //Header
  fs.write(cMapFileHeader.c_str(), cMapFileHeader.size());
  //The real data
  int i, j;
  for (i = 0; i < cMap_X; ++i)
    for (j =0; j < cMap_Y; ++j)
    {
      Byte Buffer = Pascal::TTerrainType::Ord(tiles[i][j]->m_Type);
      fs.write(reinterpret_cast<const char*>(&Buffer), 1);
      if (tiles[i][j]->HasRiver())
        Buffer = TERRAIN_RIVER_BIT;
      else
        Buffer = 0;
      if (tiles[i][j]->HasRoad())
        Buffer = Buffer | TERRAIN_ROAD_BIT;
      if (tiles[i][j]->HasSpecial())
        Buffer = Buffer | TERRAIN_SPECIAL_BIT;
      if (tiles[i][j]->IsPloughed())
        Buffer = Buffer | TERRAIN_PLOUGHED_BIT;
      fs.write(reinterpret_cast<const char*>(&Buffer), 1);
    }//for
  const bool result = fs.good();
  fs.close();
  return result;
}//func

bool TMap::LoadFromFile(const std::string& FileName)
{
  if (!FileExists(FileName)) return false;
  std::ifstream fs(FileName, std::ios_base::in | std::ios_base::binary);
  if (!fs.good() || !fs.is_open())
  {
    fs.close();
    return false;
  }

  //read header
  Byte buffer[256];
  fs.read(reinterpret_cast<char*>(buffer), cMapFileHeader.size());
  int bytes_read = fs.gcount();
  if (!fs.good() || (bytes_read != cMapFileHeader.size()))
    return false;//file is to short to read header
  //check header
  buffer[cMapFileHeader.size()] = '\0';
  if (std::string(reinterpret_cast<char*>(buffer)) != cMapFileHeader)
    return false;

  //now read the real data
  int i, j;
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
    {
      fs.read(reinterpret_cast<char*>(buffer), 2);
      bytes_read = fs.gcount();
      if (bytes_read!=2 || !fs.good())
      {
        //Read error
        fs.close();
        return false;
      }//if
      if (buffer[0]>20) //byte value does not represent a TTerrainType
      {
        fs.close();
        return false;
      }//if
      if (tiles[i][j] != nullptr)
        tiles[i][j] = nullptr;
      tiles[i][j] = std::unique_ptr<TTerrain>(new TTerrain(static_cast<TTerrainType>(buffer[0]),
                     ((buffer[1] | TERRAIN_RIVER_BIT) != 0),
                     ((buffer[1] | TERRAIN_ROAD_BIT) != 0),
                     ((buffer[1] | TERRAIN_SPECIAL_BIT) != 0),
                     ((buffer[1] | TERRAIN_PLOUGHED_BIT) != 0)));
    } //for

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
} //proc

bool TMap::IsTouchingLand(const Byte x, const Byte y) const
{
  if (!filled) return false;
  int i, j;
  for (i = x-1; i<= x+1; ++i)
    for (j = y-1; j <= y+1; ++j)
    {
      if (IsValidMapPosition(i,j))
        if (not tiles[i][j]->IsWater())
        {
          return true;
        }//if
    }//for
  //maybe we should check for tile[x,y] being land and exempt it from examination
  // or at least react in some way...
  return false;
}//func

bool TMap::IsAdjacentToWater(const Byte x, const Byte y) const
{
  //don't go for non-filled maps
  if (not filled) return false;
  //return false for (x,y) being water
  if (IsValidMapPosition(x,y))
    if (tiles[x][y]->IsWater()) return false;
  //loop through surrounding tiles
  int i, j;
  for (i = x-1; i <= x+1; ++i)
    for (j = y-1; j <= y+1; ++j)
      if (IsValidMapPosition(i,j))
        if (tiles[i][j]->IsWater() and ((i!=0) or (j!=0)))
          return true;
  return false;
}//func

bool TMap::IsAdjacentToMountains(const Byte x, const Byte y) const
{
  //don't go for non-filled maps or invalid squares
  if ((not filled) or not IsValidMapPosition(x,y)) return false;
  //loop through surrounding tiles
  int i, j;
  for (i = x-1; i <= x+1; ++i)
    for (j = y-1; j <= y+1; ++j)
      if (IsValidMapPosition(i,j))
        if (((tiles[i][j]->GetType() == ttHills) || (tiles[i][j]->GetType() == ttMountains)) and ((i!=0) or (j!=0)))
          return true;
  return false;
}//func

bool TMap::IsAdjacentToForest(const Byte x, const Byte y) const
{
  //don't go for non-filled maps or invalid squares
  if (not filled or not IsValidMapPosition(x,y)) return false;
  //loop through surrounding tiles
  int i, j;
  for (i = x-1; i <= x+1; ++i)
    for (j = y-1; j <= y+1; ++j)
      if (IsValidMapPosition(i,j))
        if (tiles[i][j]->HasForest() and ((i!=0) or (j!=0)))
          return true;//if
  return false;
}//func

void TMap::DiscoverSurroundingTiles(const Byte x, const Byte y, const Byte cNation, const bool two_squares)
{
  if ((cNation>=cMinEuropean) and (cNation<=cMaxEuropean))
  {
    const int tSqBonus = (two_squares ? 1 : 0);
    int i, j;
    for (i = x-1-tSqBonus; i <= x+1+tSqBonus; ++i)
      for (j = y-1-tSqBonus; j <= y+1+tSqBonus; ++j)
        if (IsValidMapPosition(i,j))
          discovered[i][j][cNation] = true;
  }//if
}//proc

void TMap::RevealAll(const int num_Nation)
{
  if ((num_Nation<=cMaxEuropean) and (num_Nation>=cMinEuropean))
  {
    int i, j;
    for (i = 0; i < cMap_X; ++i)
      for (j = 0; j < cMap_Y; ++j)
        discovered[i][j][num_Nation] = true;
  }//if
}//proc

bool TMap::IsDiscovered(const Byte x, const Byte y, const int num_Nation) const
{
  if ((x>=cMap_X) or (y>=cMap_Y) or (num_Nation>cMaxEuropean)
      or (num_Nation<cMinEuropean))
    return true;
  else return discovered[x][y][num_Nation];
}//func

bool TMap::IsValidMapPosition(const int x, const int y)
{
  return ((x>=0) and (y>=0) and (x<cMap_X) and (y<cMap_Y));
}//func
