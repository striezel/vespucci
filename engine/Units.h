#ifndef UNITS_H
#define UNITS_H

#include "Goods.h"
#include "Map.h"
#include "Terrain.h"
#include "PathFinder.h"
#include "PascalTypes.h"
#include <vector>

const Byte UNIT_ITEM_NONE = 0;
const Byte UNIT_TOOL_MASK = 7;
const Byte UNIT_HORSE_BIT = 8;
const Byte UNIT_MUSKET_BIT = 16;

enum TUnitType {utCriminal, utServant, utColonist,
               utFarmer, utFisher, utFurHunter, utSilverMiner, utWoodcutter,
               utOreMiner, utSugarplanter, utCottonplanter, utTobaccoplanter,

               utPreacher, utStatesman,

               utCarpenter, utDistiller, utWeaver, utTobacconist, utFurTrader,
               utSmith, utWeaponSmith,

               utScout, utPioneer, utMissionary,

               utRegular, utDragoon, utArtillery,
               utConvoy,
               utCaravel, utTradingShip, utGalleon, utPrivateer, utFrigate,
               utMan_o_War,

               utBrave, utBraveOnHorse};

//workaround for built-in Pascal functions
int Ord(const TUnitType ut);
TUnitType High(const TUnitType ut);
TUnitType Low(const TUnitType ut);
TUnitType Succ(const TUnitType ut);

enum TUnitLocation {ulAmerica, ulInColony, ulGoToEurope, ulGoToNewWorld, ulEurope, ulEmbarked};

//workaround for built-in Pascal functions
int Ord(const TUnitLocation ul);
TUnitLocation High(const TUnitLocation ul);
TUnitLocation Low(const TUnitLocation ul);
TUnitLocation Succ(const TUnitLocation ul);

enum TUnitState {usNormal/*-*/, usFortified/*F*/, usWaitingForShip/*S*/, usGoTo/*G*/, usPloughing/*P*/, usCreateRoad/*R*/};

//workaround for built-in Pascal functions
int Ord(const TUnitState us);
TUnitState High(const TUnitState us);
TUnitState Low(const TUnitState us);
TUnitState Succ(const TUnitState us);

enum TDirection {dirSW, dirS, dirSE, dirE, dirNE, dirN, dirNW, dirW, dirNone};

//workaround for built-in Pascal functions
int Ord(const TDirection dir);

LongInt cShipPrices(const TUnitType ut); //look-up "table" for ship prices in Europe

LongInt cUnitPrices(const TUnitType ut); //look-up "table" for unit prices in Europe




class TTask; //forward declaration - to be resolved later

class TUnit
{
  public:
    LongInt MovesLeft;
    TUnit(const TUnitType TypeOfUnit, const LongInt ANation, const LongInt X=1, const LongInt Y=1);
    ~TUnit();
    void NewRound();
    bool Move(const TDirection direction, TMap& AMap);
    bool WarpToXY(const Byte x, Byte y, TMap& AMap);
    LongInt GetPosX() const;
    LongInt GetPosY() const;
    LongInt GetNation() const;
    void ChangeNation(const LongInt new_nation);
    TUnitType GetType() const;
    void ChangeType(const TUnitType newType);
    TUnitLocation GetLocation() const;
    void SetLocation(const TUnitLocation loc);
    TUnitState GetState() const;
    void SetState(const TUnitState state);
    Byte GetRoundsInOpenSea() const;
    void SetRoundsInOpenSea(const Byte rounds_left);
    bool IsShip() const;
    LongInt MovesPerRound() const;
    LongInt AttackStrength() const;
    TTask* GetTask() const;
    void SetTask(TTask* new_task, const bool ImmediateExecute=true);
    //go across the big pond
    bool SendToEurope();
    bool SendToNewWorld();
    bool CallBackToEurope();
    bool CallBackToNewWorld();
    //functions for loading/ unloading freigth or passengers and checking freigth status
    Byte FreightCapacity() const;
    Byte FreeCapacity() const;
    Byte EmbarkedPassengers() const;
    TUnit* GetFirstEmbarkedPassenger() const;
    TUnit* GetPassengerBySlot(const Byte slot) const;
    bool LoadGood(const TGoodType AGood, const Byte num);
    Byte UnloadGood(const TGoodType AGood, const Byte num);
    bool LoadUnit(TUnit* AUnit);
    bool UnloadUnit(const TUnitType AType, const Byte x, const Byte y, TMap& AMap);
    void DropAllPassengers();
    //item functions
    Byte GetToolAmount() const;
    void GiveTools(const Byte amount);
    bool HasHorses() const;
    void GiveHorses(const bool has=true);
    bool HasMuskets() const;
    void GiveMuskets(const bool has=true);
    void ChangeAllItems(const Byte new_items);
    bool SaveToStream(std::ofstream& fs) const;
    void SetCargo(const Byte slot, const Byte new_amount, const TGoodType AGood);
    Byte GetCargoAmountBySlot(const Byte slot) const;
    TGoodType GetCargoGoodBySlot(const Byte slot) const;
  private:
    LongInt PosX, PosY;
    TUnitType UnitType;
    TUnitLocation m_location;
    TUnitState m_State;
    Byte m_RoundsInOpenSea;
    LongInt m_Nation;
    //stores items like horses, muskets, tools
    Byte items;
    //stores passengers (on ships)
    TUnit* passengers[6];
    struct TCargoRec
    {
      Byte amount;
      TGoodType which;
    };
    //stores cargo (on ships an convoys)
    /*cargo_load: array [0..5] of record
                                  amount: Byte;
                                  which: TGoodType;
                                end;//rec*/
    TCargoRec cargo_load[6];
    TTask* AI_Task;
};//class TUnit

typedef std::vector<TUnit*> TUnitArr;

//the AI stuff

enum TTaskType {ttGeneric, ttPlough, ttRoad, ttClear, ttGoTo};

class TTask
{
  protected:
      TUnit* target;
  public:
    virtual bool Done() const =0;
    virtual bool Execute() =0;
    virtual TTaskType GetType() const;
    TTask(TUnit* target_unit);
    virtual ~TTask();
};//class

//for the pioneers
class TPloughTask: public TTask
{
  private:
    Byte m_X, m_Y;
    Byte RoundsLeft;
  public:
    TPloughTask(TUnit* target_unit, const Byte X, const Byte Y);
    virtual bool Done() const;
    virtual bool Execute();
    virtual TTaskType GetType() const;
};//class

class TRoadTask: public TTask
{
  private:
    Byte m_X, m_Y;
    Byte RoundsLeft;
  public:
    TRoadTask(TUnit* target_unit, const Byte X, const Byte Y);
    virtual bool Done() const;
    virtual bool Execute();
    virtual TTaskType GetType() const;
};//class

class TClearTask: public TTask
{
  private:
    Byte m_X, m_Y;
    Byte RoundsLeft;
  public:
    TClearTask(TUnit* target_unit, const Byte X, const Byte Y);
    virtual bool Done() const;
    virtual bool Execute();
    virtual TTaskType GetType() const;
};//class

class TGoToTask: public TTask
{
  protected:
    Byte m_X, m_Y;//destination location
    Byte spec_X, spec_Y; //location of special field
    TCoordArr m_Path;
  public:
    TGoToTask(TUnit* target_unit, const Byte ToX, const Byte ToY, const Byte SpecialX=250, const Byte SpecialY=250);
    virtual ~TGoToTask();
    virtual bool Done() const;
    virtual bool Execute();
    Byte DestinationX() const;
    Byte DestinationY() const;
};//class

void ApplyDir(Byte* x, Byte* y, const TDirection dir);
TDirection GetApplyingDirection(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y);
TUnitType GetUnitForGood(const TGoodType AGood);
bool HasExpertStatus(const TGoodType AGood, const TUnitType ut);

#endif  //UNITS_H

