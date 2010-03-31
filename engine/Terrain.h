#ifndef TERRAIN_H
#define TERRAIN_H

#include "Goods.h"
const unsigned char TERRAIN_RIVER_BIT = 1;
const unsigned char TERRAIN_ROAD_BIT = 2;
const unsigned char TERRAIN_SPECIAL_BIT = 4;
const unsigned char TERRAIN_PLOUGHED_BIT = 8;

enum TTerrainType {//open terrain types
                  ttArctic, ttSea, ttOpenSea, ttPlains, ttGrassland, ttPrairie,
                  ttSavannah, ttMarsh, ttSwamp, ttDesert, ttTundra,
                  //forested terrain types
                  ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest,
                  ttConiferForest, ttRainForest, ttTropicalForest,
                  //others
                  ttHills, ttMountains};

//workaround for built-in Pascal functions
int Ord(const TTerrainType tt);
TTerrainType High(const TTerrainType tt);
TTerrainType Low(const TTerrainType tt);
TTerrainType Succ(const TTerrainType tt);


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
    TTerrainType m_Type;

    TTerrain(const TTerrainType ATerrain, const bool River=false,
            const bool Road=false, const bool Special=false,
            const bool Ploughed=false);
    ~TTerrain();


    TTerrainType GetType();
    TTerrainType ClearedBecomes();
    bool HasForest();
    bool HasRiver();
    bool HasRoad();
    bool HasSpecial();
    bool IsPloughed();
    bool IsWater();

    unsigned char GetGoodProduction(const TGoodType AGood, const bool expert);
    //for colony base fields
    unsigned char GetColonyFood();
    TGoodType GetColonyGoodType();
    unsigned char GetColonyGoodAmount();
    //defense bonus for terrain, in percent. Maximum is 150, so Byte will do.
    unsigned char GetDefenceBonus();

    //change terrain state
    void ClearForest();
    void CreateRoad();
    void Plough();
    //changes that should only take place at initialization
    void CreateSpecial();
};//class

const TTerrainType ttFlachland = ttPlains;
const TTerrainType ttGreenland = ttGrassland;
  //ttFeuchtgebiete = ttMarsh;
  //ttGestrueppwald = ttScrub;
  //ttNadelwald = ttConifer;
  //ttLaubwald = ttBroadleafForest;
  //ttFeuchtwald = ttWetland;

#endif

