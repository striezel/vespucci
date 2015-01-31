/* ***************************************************************************
    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010  Dirk Stolle

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

#ifndef TERRAIN_HPP
#define TERRAIN_HPP

#include <stdint.h>
#include "Goods.hpp"

const uint8_t TERRAIN_RIVER_BIT = 1;
const uint8_t TERRAIN_ROAD_BIT = 2;
const uint8_t TERRAIN_SPECIAL_BIT = 4;
const uint8_t TERRAIN_PLOUGHED_BIT = 8;

/* enumeration type to represent the terrain type */
enum TTerrainType {
     //open terrain types
     ttArctic, ttSea, ttOpenSea, ttPlains, ttGrassland, ttPrairie,
     ttSavannah, ttMarsh, ttSwamp, ttDesert, ttTundra,
     //forested terrain types
     ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest,
     ttConiferForest, ttRainForest, ttTropicalForest,
     //others
     ttHills, ttMountains};

/*********
    **** TTerrain class
    ****
    **** purpose: holds the information about the terrain, i.e. one square in
    ****          the map, such as type and whether it has a road or a river.
    *******
*/
class TTerrain
{
  private:
    bool m_River;
    bool m_Road;

    /**indicates, whether terrain has a special ressource.
       arctic: none
       Sea, OpenSea: Fish
       Plains: Wheat
       Grassland: Tobacco
       Prairie: Cotton
       Savannah: Sugar
       Marsh: Minerals
       Swamp: Minerals
       Desert: Oasis
       Tundra: None allowed
       Boreal: Wild/ Deer
       Wetland: Minerals
       ScrubForest: Oasis
       Broadleaf: Wild/Deer
       MixedForest: Beaver
       ConiferForest: Best Wood
       RainForest: Minerals
       TropicalForest: BestWood
       Hills: Ore
       Mountains: Silver
      **/
    bool m_Special;
    bool m_Ploughed;

  public:
    /* type of the terrain

       remarks:
           In theory, this member should be private and not public, but I
           decided to have this one public for faster access.
    */
    TTerrainType m_Type;

    /* constructor

       parameters:
           ATerrain - type of that terrain
           River    - boolean to indicate whether or not there shall be a river
           Road     - boolean to indicate whether or not there shall be a road
           Special  - boolean to indicate whether or not there shall be a
                      special ressource on that field
           Ploughed - boolean to indicate whether or not this terrain shall be
                      ploughed
    */
    TTerrain(const TTerrainType ATerrain, const bool River = false,
             const bool Road = false, const bool Special = false,
             const bool Ploughed = false);

    /* destructor */
    ~TTerrain();

    /* returns the type of the terrain */
     TTerrainType GetType() const;

    /* returns the type this terrain would become, if all forest would be
       removed */
    TTerrainType ClearedBecomes() const;

    /* returns true, if there's forest on that terrain */
    bool HasForest() const;

    /* returns true, if there's a river flowing through that square */
    bool HasRiver() const;

    /* returns true, if this terrain has a road */
    bool HasRoad() const;

    /* returns true, if this square has a special ressource */
    bool HasSpecial() const;

    /* returns true, if this terrain was ploughed */
    bool IsPloughed() const;

    /* returns true, if this terrain isn't land but water */
    bool IsWater() const;

    /* returns the amount of a certain good that would be produced in that
       field, if a certain unit was working here

       parameters:
           AGood  - the good that shall be "produced" (i.e. food, cotton,
                    tobacco, wood, ...)
           expert - boolean value that indicates, if the unit is an expert for
                    the production of this good (true) or not (false)
    */
    uint8_t GetGoodProduction(const TGoodType AGood, const bool expert) const;

    //for colony base fields
    /* returns the amount of food that would be produced in that field, if a
       colony was build here
    */
    uint8_t GetColonyFood() const;

    /* returns the type of goods that would be produced in that field, if a
        colony was build here (use GetColonyGoodAmount to get the amount)
    */
    TGoodType GetColonyGoodType() const;

    /* returns the amount of goods that would be produced in that field, if a
       colony was build here (use GetColonyGoodType to get the good type)
    */
    uint8_t GetColonyGoodAmount() const;

    //defense bonus for terrain, in percent. Maximum is 150, so Byte will do.
    uint8_t GetDefenceBonus() const;

    //change terrain state
    /* if this terrain has a forest, this procedure will remove it */
    void ClearForest();

    /* creates a road on that field */
    void CreateRoad();

    /* ploughs that field */
    void Plough();

    //changes that should only take place at initialization
    /* creates a special ressource here */
    void CreateSpecial();
}; //class

const TTerrainType ttFlachland = ttPlains;
const TTerrainType ttGreenland = ttGrassland;
//const TTerrainType ttFeuchtgebiete = ttMarsh;
//const TTerrainType ttGestrueppwald = ttScrub;
//const TTerrainType ttNadelwald = ttConifer;
//const TTerrainType ttLaubwald = ttBroadleafForest;
//const TTerrainType ttFeuchtwald = ttWetland;

#endif // TERRAIN_HPP
