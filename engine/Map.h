#ifndef MAP_H
#define MAP_H

#include "PascalTypes.h"
#include "Terrain.h"
#include "Nations.h"
#include <fstream>
#include <string>

//general map size
const Byte cMap_X = 56;
const Byte cMap_Y = 70;

//const for loading/ saving map
const std::string cMapFileHeader = "VMD";

//const for rivers
const Byte cMapRiverNone = 0;
const Byte cMapRiverNorth = 1;
const Byte cMapRiverEast = 2;
const Byte cMapRiverSouth = 4;
const Byte cMapRiverWest = 8;

//combined river consts
const Byte cMapRiverNE = cMapRiverNorth | cMapRiverEast;
const Byte cMapRiverSE = cMapRiverSouth | cMapRiverEast;
const Byte cMapRiverSW = cMapRiverSouth | cMapRiverWest;
const Byte cMapRiverNW = cMapRiverNorth | cMapRiverWest;

const Byte cMapRiverNS = cMapRiverNorth | cMapRiverSouth;
const Byte cMapRiverEW = cMapRiverEast | cMapRiverWest;

const Byte cMapRiverNotN = cMapRiverWest | cMapRiverEast | cMapRiverSouth;
const Byte cMapRiverNotE = cMapRiverWest | cMapRiverNorth | cMapRiverSouth;
const Byte cMapRiverNotS = cMapRiverWest | cMapRiverEast | cMapRiverNorth;
const Byte cMapRiverNotW = cMapRiverNorth | cMapRiverEast | cMapRiverSouth;

const Byte cMapRiverAll = cMapRiverNorth | cMapRiverEast | cMapRiverSouth
                           | cMapRiverWest;

class TMap
{
  private:
    bool filled; //internal value to determine whether map is not only nil
    //discovered: array [0..cMap_X-1, 0..cMap_Y-1] of array [cMin_Nations..cMax_Nations] of Boolean;
    bool discovered[cMap_X][cMap_Y][cMax_Nations+1];
    //river: array [0..cMap_X-1, 0..cMap_Y-1] of Byte;
    Byte river[cMap_X][cMap_Y];
    void GenerateRiverCache();
    void ClearRiverCache();
    void SetRiverType(const Byte x, const Byte y);
  public:
    TTerrain* tiles[cMap_X][cMap_Y]; //should better be private
    TMap();
    virtual ~TMap();
    void Generate(const float Landmass);
    void GenerateSpecials(const bool LandOnly=true);
    bool SaveToFile(const std::string& FileName) const;
    bool LoadFromFile(const std::string& FileName);

    Byte GetRiverType(const Byte x, const Byte y) const;
    //determines, whether a water tile has at least one non-water neighbour
    bool IsTouchingLand(const Byte x, const Byte y) const;
    //proc for reavealing surrounding tiles around a unit
    void DiscoverSurroundingTiles(const Byte x, const Byte y, const Byte cNation, const bool two_squares);
    //proc to reaveal complete map ("cheat")
    void RevealAll(const LongInt num_Nation);
    bool IsDiscovered(const Byte x, const Byte y, const LongInt num_Nation) const;
};//class

#endif

