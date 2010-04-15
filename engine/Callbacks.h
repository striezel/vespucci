#ifndef CALLBACKS_H
#define CALLBACKS_H

#include "Units.h"
#include "Map.h"
#include "Data.h"
#include "Colony.h"
#include "Goods.h"
#include "Nations.h"
#include "PascalTypes.h"

const LongInt CBT_ANY = 0;
const LongInt CBT_EXIT = 1;
const LongInt CBT_LANDFALL = 2;
const LongInt CBT_BUILD_COLONY = 3;
const LongInt CBT_SAVE_GAME = 4;
const LongInt CBT_LOAD_GAME = 5;
const LongInt CBT_JOB_CHANGE = 6;
const LongInt CBT_EURO_PORT_UNIT = 7;
const LongInt CBT_EURO_PORT_BUY = 8;
const LongInt CBT_EURO_PORT_TRAIN = 9;
const LongInt CBT_RENAME_COLONY = 10;
const LongInt CBT_ABANDON_COLONY = 11;
const LongInt CBT_COLONY_UNIT = 12;
const LongInt CBT_GOTO_SHIP = 13;
const LongInt CBT_CONSTRUCTION = 14;


//  TExitCallback = procedure (const option: Integer);
typedef void (*TExitCallback)(const LongInt option);
typedef bool (*TLandfallCallback)(const LongInt option, TUnit* AShip, const TUnitType AType, const Byte x, const Byte y, TMap& AMap);

struct TLandfallData
{
  TLandfallCallback cbLandfall;
  TUnit* Ship;
  TUnitType UType;
  Byte x, y;
  TMap& AMap;
};

struct TBuildColonyData
{
  Byte x,y;
  //ColName: ShortString; //delivered through input text
  Byte num_nation;
  TUnit* founder;
  TData& AData;
};//rec

struct TSaveGameData
{
  TData& AData;
};//rec

struct TLoadGameData
{
  TData& AData;
};//rec
struct TJobChangeData
{
  ShortInt x_shift, y_shift;
  TColony* AColony;
};//rec
struct TEuroPortUnitData
{
  TUnit* AUnit;
  TEuropeanNation* EuroNat;
};//rec
struct TEuroBuyData
{
  TData& AData;
  TEuropeanNation* EuroNat;
};//rec
struct TEuroTrainData
{
  TData& AData;
  TEuropeanNation* EuroNat;
};//rec
struct TRenameColonyData
{
  TColony* AColony;
};//rec
struct TAbandonColonyData
{
  TColony* AColony;
  TData& AData;
};//rec
struct TColonyUnitData
{
  TUnit* AUnit;
};//rec
struct TGotoShipData
{
  TUnit* Ship;
  TData& AData;
};//rec
struct TConstructionData
{
  TColony* AColony;
};//rec

struct TCallbackRec
{
  LongInt option;
  std::string inputText;
  LongInt _type;
  union {
    void* Data;
    TExitCallback cbExit;
    TLandfallData* Landfall;
    TBuildColonyData* BuildColony;
    TSaveGameData* SaveGame;
    TLoadGameData* LoadGame;
    TJobChangeData* JobChange;
    TEuroPortUnitData* EuroPort;
    TEuroBuyData* EuroBuy;
    TEuroTrainData* EuroTrain;
    TRenameColonyData* RenameColony;
    TAbandonColonyData* AbandonColony;
    TColonyUnitData* ColonyUnit;
    TGotoShipData* GotoShip;
    TConstructionData* Construction;
  };
};//rec


TCallbackRec cEmptyCallback();

void CBF_Exit(const LongInt option);
bool CBF_Landfall(const LongInt option, TUnit* AShip, const TUnitType AType, const Byte x, const Byte y, TMap& AMap);

bool HandleCallback(const TCallbackRec cbRec);

#endif

