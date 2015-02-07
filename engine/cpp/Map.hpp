/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011, 2015  Dirk Stolle

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

#ifndef MAP_HPP
#define MAP_HPP

#include <functional>
#include <memory>
#include <string>
#include "Terrain.hpp"
#include "Nation.hpp"
#include "PascalTypes.hpp"

//general map size
const Byte cMap_X = 56;
const Byte cMap_Y = 70;

//const for loading/ saving map
const std::string cMapFileHeader = "VMD";

//const for rivers
/* These constants are used during visualisation of the rivers on the map
   to interact properly with rivers in adjacent fields.
*/
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

/* function type for landscape height generation

   parameters:
       mx, my - location of peak in map coordinates
       r      - radius of hill, distance to zero
       h      - height of peak
       x, y   - current field coordinates

*/
typedef std::function<float(const Byte mx, const Byte my, const Byte r, const float h, const Byte x, const Byte y)> THillFunction;

/* enumeration type for climate setting in map generation function */
enum TClimateType {ctDry, ctNormal, ctWet};

/* enumeration type for temperature setting in map generation function */
enum TTemperatureType {ttCool, ttModerate, ttWarm};

/* ********
   **** TMap class
   ****
   **** purpose: holds the map data for the current game.
   *******

   To Do:
   ======
      - save discovery status info (array discovered) during SaveToFile() and
        load it during LoadFromFile() function
      - write a better function for generation of maps
      - two-parameter version of TMap.Generate():
          - generate rivers
          - generate broader variety of terrain types
*/
class TMap
{
  private:
    bool filled; //internal value to determine whether map is not only NULL
    /* holds information about which fields of the map have already been
       discovered by the four European nations. */
    std::array<std::array<std::array<bool, cMaxEuropean>, cMapY>, cMapX> discovered; //array [0..cMap_X-1, 0..cMap_Y-1] of array [cMinEuropean..cMaxEuropean] of Boolean;
    /* "River Cache": holds information about rivers in adjacent fields of all
       fields on the map */
    std::array<std::array<Byte, cMapY>, cMapX> river;

    /* generates the river cache, i.e. fills the above array with proper values */
    void GenerateRiverCache();
    /* clears the "river cache", i.e. sets all fields to cMapRiverNone */
    void ClearRiverCache();
    /* determines the river cache entry for a single square on the map

       parameters:
           x, y - coordinates of that square
    */
    void SetRiverType(const Byte x, const Byte y);
  public:
    /* holds information about all squares/fields on the map

       remarks:
           This array should be private and only accessible via public
           functions. However, I decided to make it public for faster access,
           because this array will usually be used during every frame (or most
           frames) while a game is running.
    */
    std::array<std::array<std::unique_ptr<TTerrain>, cMapY>, cMapX> tiles; //array [0..cMap_X-1, 0..cMap_Y-1] of TTerrain;

    /* constructor */
    TMap();

    /* destructor */
    ~TMap();

    /* generates a map with the given parameters

       parameters:
           Landmass - the amount of land on the map - 1.0 means all is land,
                      while 0.0 means no land and just water. Usually this
                      value should be between 0.5 and 0.8, I guess.

       remarks:
           Although this function already generates a map, it does not do it
           too well. It just generates a row of arctic terrain at the top and
           bottom of the map, a column of open sea at the left and right side,
           and all other land is either grassland or sea.
           It's quite clear that this function has to be improved in the
           future, if we really want to use it. Things that could be done are
           considering terrain height to generate mountains and hills, too;
           generation of rivers, consideration of climate (given by a new
           function parameter) to generate different terrain types, and a way
           to make the generated map look more like a continent (or a group of
           islands) and not just like randomly thrown squares of land.
    */
    void Generate(const float Landmass);

    /* generates a map with the given parameters

       parameters:
           Landmass    - the amount of land on the map - 1.0 means all is land,
                         while 0.0 means no land and just water. Usually this
                         value should be between 0.3 and 0.8, I guess.
           f           - the function that is used to generate hills/ mountains
           temperature - the temperate setting used to generate terrain tiles
                         (This setting is ignored in the current implementation.)
           climate     - the climate setting used to generate terrain tiles

       remarks:
           Although this function already generates a map, it does not do it
           too well. It just generates a row of arctic terrain at the top and
           bottom of the map, a column of open sea at the left and right side,
           and all other land is a more or less randomly chosen type of
           terrain. Function f is used to generate the height map. The climate
           setting influences the number of rivers generated, the temperature
           setting is currently ignored.
           This is a try to implement a slightly better version of the map
           generation function.
    */
    void Generate(const float LandMass, const THillFunction f, const TTemperatureType temperature, const TClimateType climate);

    /* generates the special ressources for the map

       parameters:
           LandOnly - boolean that indicates whether special ressources should
                      only be generated for land squares (true) or also for
                      watery squares (false)

       remarks:
           Only call this once for a genarates map. For maps loaded from a
           file, never ever call that function, because loaded maps already
           have their special ressources set.
    */
    void GenerateSpecials(const bool LandOnly = true);

    /* tries to save the map to the given file and returns true on success

       parameters:
           FileName - name of the destionation file
    */
    bool SaveToFile(const std::string& FileName) const;

    /* tries to load a map from the given file and returns true on success

       parameters:
           FileName - name of the source file
    */
    bool LoadFromFile(const std::string& FileName);

    /* returns the river type of a certain square

       parameters:
           x, y - coordinates of the square
    */
    Byte GetRiverType(const Byte x, const Byte y) const;

    /* determines, whether a water tile has at least one non-water neighbour
       and returns true in that case

       parameters:
           x, y - coordinates of the square
    */
    bool IsTouchingLand(const Byte x, const Byte y) const;

    /* determines, whether a land tile has at least one water neighbour and
       returns true in that case

       parameters:
           x, y - coordinates of the square
    */
    bool IsAdjacentToWater(const Byte x, const Byte y) const;

    /* determines, whether a land tile has at least one hill/mountain neighbour
       square and returns true in that case

       parameters:
           x, y - coordinates of the square
    */
    bool IsAdjacentToMountains(const Byte x, const Byte y) const;

    /* determines, whether a land tile has at least one neighbour square with
       forest and returns true in that case

       parameters:
           x, y - coordinates of the square
    */
    bool IsAdjacentToForest(const Byte x, const Byte y) const;

    /* this procedure reaveals surrounding tiles around a unit (or that is what
       it's made for)

       parameters:
           x, y       - coordinates of unit's position on the map
           cNation    - integer constant identifying the unit's nation
           twoSquares - usually, this function only reveals the squares
                        directly adjacent to the unit's position, i.e. at
                        maximum nine squares (including the unit's position).
                        However, if twoSquares is true, this function will also
                        reveal the squares adjacent to that, revealing each
                        field that is within two (and not one) square range of
                        the unit. Usually only scout units do that.
    */
    void DiscoverSurroundingTiles(const Byte x, const Byte y, const Byte cNation, const bool two_squares);

    /* This procedure reaveals the complete map for the given nation. Yes,
       it's a cheat.

       parameters:
           num_Nation - integer constant identifying the nation in question
    */
    void RevealAll(const int num_Nation);

    /* returns true, if a certain European nation has discovered a certain
       field of the map

       parameters:
           x, y      - coordinates of the field's position
           numNation - integer constant identifying the nation in question
    */
    bool IsDiscovered(const Byte x, const Byte y, const int num_Nation) const;

    /* returns true, if the passed position is a valid position on the map

       parameters:
           x, y - coordinates of the field's position
    */
    static bool IsValidMapPosition(const int x, const int y);
}; //class Map

//functions for height map generation
/* h2 generates a hill at (mx,my) with radius r and not so heavy slope */
float h2(const Byte mx, const Byte my, const Byte r, const float h, const Byte x, const Byte y);

/* h4 generates a hill at (mx,my) with radius r and faster slope (i.e. less
   hills and mountains, more grassland than h2) */
float h4(const Byte mx, const Byte my, const Byte r, const float h, const Byte x, const Byte y);

#endif // MAP_HPP
