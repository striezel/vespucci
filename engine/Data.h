#ifndef DATA_H
#define DATA_H

#include "Nations.h"
#include "Language.h"
#include "Units.h"
#include "Colony.h"
#include "Tribe.h"
#include "Map.h"
#include "Goods.h"
#include "Helper.h"
#include "PascalTypes.h"
#include <string>
#include <vector>
#include <fstream>

#ifdef Win32
const std::string path_delimiter = "\\";
#else
const std::string path_delimiter = "/";
#endif
const std::string data_path = "data" + path_delimiter;
const std::string america_map_path = data_path +"america"+path_delimiter+"america.vmd";
const std::string img_path = data_path+"img"+path_delimiter;
const std::string good_img_path = img_path+"goods"+path_delimiter;
const std::string terrain_img_path = img_path+"terrain"+path_delimiter;
const std::string unit_img_path = img_path+"units"+path_delimiter;
const std::string state_img_path = unit_img_path+"state"+path_delimiter;
const std::string colony_img_path = img_path+"colony"+path_delimiter;
const std::string building_img_path = colony_img_path+"building"+path_delimiter;
const std::string tribe_img_path = img_path+"tribe"+path_delimiter;
const std::string save_path = data_path+"saves"+path_delimiter;

const std::string cDataFileHeader = "VDD";
const std::string cColonyFileHeader = "VCD";
const std::string cUnitFileHeader = "VUD";
const std::string cNationFileHeader = "VND";

class TData
{
  private:
    LongInt Year;
    bool Autumn;
    LongInt player_nation;
    TNation* Nations[cMaxIndian+1];
    //the units
    std::vector<TUnit*> m_Units;
    LongInt Unit_max;
    //the colonies
    std::vector<TColony*> m_Colonies;
    LongInt Colony_max;
    //the tribes
    std::vector<TTribe*> m_Tribes;
    LongInt Tribe_max;
    /*//map
    TMap m_Map;
    //language
    lang: TLanguage;*/

    //relative path
    std::string base_dir;
    //loading routines (maybe save routines should be here, too?)
    bool LoadUnitFromStream(TUnit* AUnit, std::ifstream& fs);
    bool LoadColonyFromStream(TColony* AColony, std::ifstream& fs);
    bool LoadNationFromStream(TNation* ANat, std::ifstream& fs);
    void InitializeNations();
    void InitializeMap();
    void InitTribes_America();
    void DeInitColonies();
    void DeInitTribes();
    void DeInitUnits();
    TData(const LongInt NumNation_Player=cNationEngland);
    TData(const TData& op) {}
  public:
    static TData& GetSingleton();
    virtual ~TData();
    LongInt GetYear() const;
    bool IsAutumn() const;
    LongInt PlayerNation() const;
    void SetPlayerNation(const LongInt count);
    TNation* GetNation(const LongInt count) const;
    void AdvanceYear();
    //units
    TUnit* NewUnit(const TUnitType TypeOfUnit, const LongInt ANation, const LongInt X=1, const LongInt Y=1);
    TUnit* GetFirstUnitInXY(const LongInt x, const LongInt y, const bool OnlyAmerica=true) const;
    TUnit* GetFirstLazyUnit(const LongInt num_Nation) const;
    TUnitArr GetAllShips(const LongInt numNation) const;
    TUnitArr GetAllShipsInXY(const LongInt x, const LongInt y, const bool OnlyAmerica=true) const;
    TUnitArr GetAllShipsInEurope(const LongInt num_nation) const;
    TUnitArr GetAllNonShipsInEurope(const LongInt num_nation) const;
    TUnitArr GetAllShipsGoingToEurope(const LongInt num_nation) const;
    TUnitArr GetAllShipsGoingToNewWorld(const LongInt num_nation) const;
    void GetEuropeanQuartett(const LongInt num_nation, TUnitArr& Ships, TUnitArr& People, TUnitArr& ExpectedSoon, TUnitArr& ToNewWorld) const;
    //units in colonies
    TUnitArr GetAllUnitsInColony(const TColony* ACol) const;
    //colonies
    TColony* NewColony(const Byte x, const Byte y, const LongInt num_Nation, const std::string& AName);
    TColony* GetColonyInXY(const Byte x, const Byte y) const;
    TColonyArr GetColonyList(const LongInt num_nation) const;
    bool DeleteColony(const Byte x, const Byte y);
    //tribes
    TTribe* NewTribe(const Byte x, const Byte y, const LongInt num_Nation, const TUnitType Teaches);
    TTribe* GetTribeInXY(const Byte x, const Byte y) const;
    //general (settlements)
    bool FreeForSettlement(const Byte x, const Byte y) const;
    //others
    void NewRound(const LongInt num_Nation);
    bool SaveData(const Word n, std::string& err);
    bool LoadData(const Word n, std::string& err);
    std::string GetSaveInfo(const Word n);
    TStringArr GetSaveSlots();
    std::string GetPathBase();
    /*          function GetLang: TLanguage;
              function GetMap: TMap;*/
    TStringArr GetJobList(const ShortInt x_shift, const ShortInt y_shift, const TUnitType UnitType, const TColony* ACol) const;
};//class

#endif //DATA_H

