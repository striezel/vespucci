#ifndef COLONY_H
#define COLONY_H

#include "Settlement.h"
#include "Goods.h"
#include "Units.h"
#include "Map.h"
#include "PascalTypes.h"
#include <string>
#include <fstream>
#include <vector>

enum TBuildingType {btNone, //nichts, dummy
                   btFort, //Einpfählung, Fort, Festung
                   btDock, //Hafenanlagen, Trockendock, Werft
                   btWarehouse, //Lagerhaus, Lagerhauserweiterung
                   btStable, //Ställe
                   btCustomHouse, //Zollhaus
                   btPress, //Druckerei, Verlag
                   btSchool, //Schule, College, Universität
                   btArmory, //Waffenkammer, Waffendepot, Waffenarsenal
                   btTownHall, //Rathaus
                   btWeaver, //Haus d. Webers, Weberei, Textilwerk
                   btTobacconist, //Haus d. Tabakhändlers, Tabakladen, Zigarrenfabrik
                   btDistiller, //Haus d. Rumbrenners, Rumbrennerei, Rumfabrik
                   btFurTrader, //Haus d. Gerbers, Gerberei, Pelzfabrik
                   btCarpenter, //Zimmerei, Sägewerk
                   btChurch, //Kirche, Kathedrale
                   btBlacksmith //Haus d. Schmieds, Schmiede, Eisenhütte
                  };

//workaround for built-in Pascal functions
int Ord(const TBuildingType bt);
TBuildingType High(const TBuildingType bt);
TBuildingType Low(const TBuildingType bt);
TBuildingType Succ(const TBuildingType bt);

class TColony: public TSettlement
{
  public:
    TColony(const LongInt X, const LongInt Y, const LongInt ANation, const std::string& AName);
    virtual ~TColony();
    std::string GetName() const;
    void SetName(const std::string& new_name);
    Word GetStore(const TGoodType AGood) const;
    Word RemoveFromStore(const TGoodType AGood, const Word amount);
    void AddToStore(const TGoodType AGood, const Word amount);
    //try to avoid SetStore, use RemoveFromStore or AddToStore instead
    void SetStore(const TGoodType AGood, const Word new_amount);
    //for buildings
    Byte GetBuildingLevel(const TBuildingType bt) const;
    void SetBuildingLevel(const TBuildingType bt, const Byte new_level);
    TBuildingType GetCurrentConstruction() const;
    void SetCurrentConstruction(const TBuildingType bt);
    void ConstructNextLevel();
    LongInt GetProduction(const TBuildingType bt, const TUnitType ut) const;
    void NewRound(const TMap& AMap);
    TUnit* GetUnitInField(const LongInt x_shift, const LongInt y_shift) const;
    TGoodType GetUnitInFieldGood(const LongInt x_shift, const LongInt y_shift) const;
    void SetUnitInField(const LongInt x_shift, const LongInt y_shift, TUnit* AUnit, const TGoodType AGood=gtFood);
    TUnit* GetUnitInBuilding(const TBuildingType bt, const Byte place) const;
    void SetUnitInBuilding(const TBuildingType bt, const Byte place, TUnit* AUnit);
    void RealignUnitsInBuilding(const TBuildingType bt);
    ShortInt GetFirstFreeBuildingSlot(const TBuildingType bt) const;
    Word GetInhabitants() const;
    bool AdjacentWater(const TMap& AMap) const;
    bool SaveToStream(std::ofstream& fs) const;
  private:
    std::string m_Name;
    Word Store[gtCross+1]; //max. is 300, a word should do it;
    //current level of buildings
    Byte Buildings[btBlacksmith+1];
    //indicates, which building is going to be constructed next
    // hint: value "btTownHall" indicates no construction in progress, since
    //max. town hall level is 1 and this level is set at colony creation
    TBuildingType CurrentConstruction;
    //UnitsInBuilding: array [btArmory..btBlacksmith] of array [0..2] of TUnit;
    TUnit* UnitsInBuilding[btBlacksmith+1][3];
    /*UnitsInFields: array[-1..1] of array [-1..1] of record
                                                      u: TUnit;
                                                      GoesFor: TGoodType;
                                                    end;//rec*/
    struct TFieldRecord
    {
      TUnit* u;
      TGoodType GoesFor;
    };
    TFieldRecord UnitsInFields[3][3];
};//class

typedef std::vector<TColony*> TColonyArr;

Byte GetMaxBuildingLevel(const TBuildingType bt);
TGoodType GetProducedGood(const TBuildingType bt);
void GetBuildingCost(const TBuildingType bt, const Byte level, Word* Hammers, Word* Tools);
#endif

