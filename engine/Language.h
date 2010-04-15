#ifndef LANGUAGE_H
#define LANGUAGE_H

#include "Goods.h"
#include "Units.h"
#include "Terrain.h"
#include "Nations.h"
#include "Colony.h"
#include "PascalTypes.h"
#include <string>
#include <vector>

enum TMenuCategory {mcNone, mcGame, mcView, mcOrders, mcReports, mcTrade};

int Ord(const TMenuCategory mcc);
TMenuCategory High(const TMenuCategory mcc);
TMenuCategory Low(const TMenuCategory mcc);
TMenuCategory Succ(const TMenuCategory mcc);
TMenuCategory Pred(const TMenuCategory mcc);

enum TSaveLoadString {slsLoadChoose, slsLoadError, slsLoadSuccess, slsSaveChoose, slsSaveError, slsSaveSuccess, slsNoGameLoaded};

enum TTransferString {tsBoycotted, tsOutOfGold, tsOutOfSpace};

enum TOtherString {osLocation, osDestination, osFreight, osShip, osHighSea,
                  osNewWorld, osMoves, osEmpty, osNothing, osNoChanges, osTax,
                  osGold, osCost, osSaving, osEarnings, osUndefined};

enum TEuroPortString {epsManageHeading, epsNotOnShip, epsGoOnShip, epsArm, epsDisarm, epsGiveHorses, epsNoHorses, epsGiveTools, epsNoTools, epsTrainHeading, epsBuyHeading};

enum TReportType {rtNone, rtEconomy, rtColony, rtFleet};

enum TColonyString {csRenameQuestion, csRenameLabel, csAbandonYes, csAbandonNo, csAbandonQuestion};
enum TColonyUnitString {cusOptions, cusCancelOrders, cusOnBoard, cusFortify};
enum TBuildingString {bsUnderConstruction, bsSelectNext, bsNotify, bsMaxThree};

int Ord(const TBuildingString bss);
TBuildingString Low(const TBuildingString bss);
TBuildingString High(const TBuildingString bss);
TBuildingString Succ(const TBuildingString bss);

enum TPioneerString {psNoTools, psHasRoad, psIsPloughed, psIsCleared, psNeedsClearing, psWrongUnit};


class TLanguage
{
  private:
    std::string Menu[mcTrade+1];
    std::string MenuOptions[mcTrade+1][11];
    struct mh_rec
    {
      LongInt max_len;
      LongInt count;
    };
    mh_rec menu_helpers[mcTrade+1];//rec
    std::string GoodNames[gtCross+1];
    std::string NationNames[cMaxIndian+1];
    std::string PortNames[cMaxEuropean+1];
    std::string TerrainNames[ttMountains+1];
    std::string UnitNames[utBraveOnHorse+1];
    std::string Seasons[2];
    std::string Transfer[tsOutOfSpace+1];
    //others
    std::string Others[osUndefined+1];
    //for messages after saving/loading the game
    std::string SaveLoad[slsNoGameLoaded+1];
    //for landfall message box
    std::string Landfall[3];
    //for building new colonies
    std::string BuildColony[5];
    //Renaming colonies & Abandon colony
    std::string ColonyStrings[csAbandonQuestion+1];
    //proposal for colony names
    std::vector<std::string> ColonyNames[cMaxEuropean+1]; //two-dimensional vector
    //managing units outside of colonies (but within colony square)
    std::string ColonyUnit[cusFortify+1];
    //for names of buildings on different levels
    std::string Buildings[btBlacksmith+1][4];
    //for units in buildings, moving them in and out, etc.
    std::string BuildingStrings[bsMaxThree+1];
    //for managing units in european port
    std::string EuroPortManage[epsBuyHeading+1];
    //for pioneer actions
    std::string Pioneers[psWrongUnit+1];
    void InitialValues();
    void InitialColonyNames();
    void InitialBuildingNames();
    void SetMenuHelpers();
    LongInt privGetOptionCount(const TMenuCategory categ) const;
    TLanguage();
    TLanguage(const TLanguage& op) {}
  public:
    virtual ~TLanguage();
    static TLanguage& GetSingleton();
    //menu related
    LongInt GetOptionCount(const TMenuCategory categ) const;
    std::string GetMenuLabel(const TMenuCategory categ) const;
    std::string GetMenuOption(const TMenuCategory categ, const Byte option) const;
    //menu helper
    LongInt GetMaxLen(const TMenuCategory categ) const;
    //general stuff
    std::string GetGoodName(const TGoodType AGood) const;
    std::string GetNationName(const LongInt NationNum) const;
    std::string GetPortName(const LongInt NationNum) const;
    std::string GetTerrainName(const TTerrainType ATerrain) const;
    std::string GetUnitName(const TUnitType AUnit) const;
    std::string GetSeason(const bool autumn) const;
    std::string GetTransfer(const TTransferString which_string) const;
    //others
    std::string GetOthers(const TOtherString which_one) const;
    std::string GetSaveLoad(const TSaveLoadString which) const;
    std::string GetLandfall(const Byte which) const;
    std::string GetBuildColony(const Byte which) const;
    std::string GetColonyString(const TColonyString which) const;
    std::string GetColonyNames(const LongInt num_nation, const Byte col_count) const;
    std::string GetColonyUnit(const TColonyUnitString which) const;
    std::string GetBuildingName(const TBuildingType which, const Byte level) const;
    std::string GetBuildingString(const TBuildingString which) const;
    std::string GetEuroPort(const TEuroPortString which) const;
    std::string GetPioneer(const TPioneerString which) const;
    bool SaveToFile(const std::string& FileName) const;
    bool LoadFromFile(const std::string& FileName);
};//class

#endif

