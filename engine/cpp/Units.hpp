/* **************************************************************************

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

#ifndef UNITS_HPP
#define UNITS_HPP

#include <array>
#include <fstream>
#include <map>
#include <memory>
#include <vector>
#include "PascalTypes.hpp"

#include "Goods.hpp"
#include "Map.hpp"
#include "PathFinder.hpp"

const Byte UNIT_ITEM_NONE = 0;
const Byte UNIT_TOOL_MASK = 7;
const Byte UNIT_HORSE_BIT = 8;
const Byte UNIT_MUSKET_BIT = 16;

/* enumeration type for unit type */
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

/* enumeration type for units' location */
enum TUnitLocation {ulAmerica, ulInColony, ulGoToEurope, ulGoToNewWorld, ulEurope, ulEmbarked};

/* state of units, i.e. fortified, waiting, going to,... */
enum TUnitState {usNormal,/*-*/ usFortified,/*F*/ usWaitingForShip/*S*/, usGoTo/*G*/, usPloughing/*P*/, usCreateRoad/*R*/};

/* enumeration type for directions */
enum TDirection {dirSW, dirS, dirSE, dirE, dirNE, dirN, dirNW, dirW, dirNone};

/* constant map that holds the prices for ships in Europe
*/
const std::map<TUnitType, int> cShipPrices
                               = {
                               {utArtillery, 500},
                               {utConvoy, 16000000/*Convoy, can't be bought*/},
                               {utCaravel, 1000},
                               {utTradingShip, 2000},
                               {utGalleon, 3000},
                               {utPrivateer, 2000},
                               {utFrigate, 5000}
                               };

/* constant array that holds the prices for recruitung units in Europe. A value
    of -1 indicates that this type of unit cannot be recruited.
  */
const std::map<TUnitType, LongInt> cUnitPrices
                ={
                  {utFarmer, 1100}, //utFarmer
                  {utFisher, 1000}, //utFisher
                  {utFurHunter, -1}, //utFurHunter
                  {utSilverMiner, 900}, //utSilverMiner
                  {utWoodcutter, 700}, //utWoodcutter
                  {utOreMiner, 600}, //utOreMiner
                  {utSugarplanter, -1}, //utSugarplanter
                  {utCottonplanter, -1}, //utCottonplanter
                  {utTobaccoplanter, -1}, //utTobaccoplanter
                  {utPreacher, 1500}, //utPreacher
                  {utStatesman, 1900}, //utStatesman
                  {utCarpenter, 1000}, //utCarpenter
                  {utDistiller, 1100}, //utDistiller
                  {utWeaver, 1300}, //utWeaver
                  {utTobacconist, 1200}, //utTobacconist
                  {utFurTrader, 950}, //utFurTrader
                  {utSmith, 1050}, //utSmith
                  {utWeaponSmith, 850}, //utWeaponSmith
                  {utScout, -1}, //utScout
                  {utPioneer, 1200}, //utPioneer
                  {utMissionary, 1400}, //utMissionary
                  {utRegular, 2000} //utRegular
                 };

/* forward declaration of TTask (see below TUnit for full declaration) */
class TTask;

/* ********
   **** TUnit class
   ****
   **** purpose: represents a single unit within the game, i.e. a colonist,
   ****          an Indian warrior, a ship,...
   ********

     -TO-DO: function LoadGood() still uses a new slot for every good, even if there already is
      ****** an amount of the given good loaded (e.g. trying to load 20 food and
             then again less then 80 food will occupy two slots, even though a
             slot is able to store up to 100 units of a good.
*/
class TUnit
{
  public:
    /* holds the number of moves that the unit still can perform within the
       current turn

       remarks:
           This value should usually be private and not public.
    */
    int MovesLeft;

    /* constructor

       parameters:
           TypeOfUnit - the unit's type
           ANation    - integer constant identifying the unit's nation
           x, y       - initial position of the unit
    */
    TUnit(const TUnitType TypeOfUnit, const int ANation, int X = 1, int Y = 1);

    /* destructor */
    ~TUnit();

    /* starts a new round for the unit */
    void NewRound();

    /* tries to moves the unit into a given direction and returns true, if the
       unit could be moved

       parameters:
           direction - the direction into which the unit should be moving
           AMap      - the current map
           dat       - data structure
    */
    bool Move(const TDirection direction, const std::shared_ptr<TMap> AMap, const void * dat);

    /* tries to "teleport" a unit to the given coordinates and returns true on
       success

       parameters:
           x, y - destination position of the unit
           AMap - the current map

       remarks:
           This function always succeeds, except for positions which are not
           on the map. If a map is specified (i.e. not nil), the surrounding
           tiles of the new position will be discovered by the unit.
    */
    bool WarpToXY(const Byte x, const Byte y, const std::shared_ptr<TMap> AMap);

    /* returns the x-coordinate of the unit's position */
    LongInt GetPosX() const;

    /* returns the y-coordinate of the unit's position */
    LongInt GetPosY() const;

    /* returns the unit's nation identifier */
    LongInt GetNation() const;

    /* changes the nation of the unit

       parameters:
           new_nation - the integer identifying the unit's new nation
    */
    void ChangeNation(const LongInt new_nation);

    /* returns the unit's type */
    TUnitType GetType() const;

    /* changes the type of the unit

       parameters:
           newType - the unit's new type
    */
    void ChangeType(const TUnitType newType);

    /* returns the unit's location as a symbolic enumeration value

       remarks:
           This is not to be confused with the "real" position, i.e. the map
           position of a unit. If you want to get that position, use
           GetPosX() and GetPosY().
    */
    TUnitLocation GetLocation() const;

    /* sets the unit's location

       parameters:
           loc - the new location
    */
    void SetLocation(const TUnitLocation loc);

    /* returns the unit's status */
    TUnitState GetState() const;

    /* sets a new unit status

       parameters:
           state - the new unit state
    */
    void SetState(const TUnitState state);

    /* returns the rounds the unit still is in open sea before it will arrive
       in Europe or the new World

       remarks:
           This is only used for ships, as you might have guessed.
    */
    Byte GetRoundsInOpenSea() const;

    /* sets the number of rounds that the unit still is in open sea before it
       arrives in Europe or the new World

       parameters:
           rounds_left - the amount of rounds left until arrival
    */
    void SetRoundsInOpenSea(const Byte rounds_left);

    /* returns true, if the unit is a ship */
    bool IsShip() const;

    /* returns the number of moves that the unit has per round */
    int MovesPerRound() const;

    /* returns the combat strength of the unit */
    int AttackStrength() const;

    /* returns the current AI task that is assigned to this unit, or nil if
       there is no task at the moment
    */
    std::shared_ptr<TTask> GetTask() const;

    /* assigns a new AI task to the unit

       parameters:
           new_task         - the AI task
           ImmediateExecute - If set to true, the unit will immediately start
                              to work according to the assigned task. Otherwise
                              it will wait until the next round begins.
    */
    void SetTask(const std::shared_ptr<TTask> new_task, const bool ImmediateExecute = true);

    // ---- go across the big pond ----
    /* tries to send a unit on travel to Europe and returns true on success

       remarks:
           This function can only succeed for ships, and only if the ship is
           in the new world. Calling this function for a unit that already is
           sailing to Europe will return false, too.
    */
    bool SendToEurope();

    /* tries to send a unit on travel to the new world and returns true on
       success

       remarks:
           This function can only succeed for ships, and only if the ship is
           in Europe. Calling this function for a unit that already is sailing
           to the new world will return false, too.
    */
    bool SendToNewWorld();

    /* tries to send a unit which already sails to the new world back to Europe
       and will return true on success

       remarks:
           This function can only succeed for ships, and only if the ship is
           already sailing to the new world.
    */
    bool CallBackToEurope();

    /* tries to send a unit which already sails to Europe back to the new world
       and will return true on success

       remarks:
           This function can only succeed for ships, and only if the ship is
           already sailing to Europe.
    */
    bool CallBackToNewWorld();

    // ---- functions for loading/ unloading freight or passengers and
    //      checking freight status ----
    /* returns the total number of slots the unit has

       remarks:
           For units that are not ships or caravans, this will always return
           zero.
    */
    Byte FreightCapacity() const;

    /* returns the number of free slots that the unit has

       remarks:
           For units that are not ships or caravans, this will always return
           zero.
    */
    Byte FreeCapacity() const;

    /* returns the number of passengers that the ship carries */
    Byte EmbarkedPassengers() const;

    /* returns the first embarked unit in the ship, or nil if there are no
       passengers
    */
    std::shared_ptr<TUnit> GetFirstEmbarkedPassenger() const;

    /* returns the ship's passenger within a certain slot

       parameters:
           slot - the slot (0-based, has to be less than six)
    */
    std::shared_ptr<TUnit> GetPassengerBySlot(const Byte slot) const;


    /* tries to load num units of good 'AGood', maximum is 100, and returns
       true, if the goods could be loaded onto the ship/caravan

       parameters:
           AGood - the type of good that will be loaded
           num   - the amount of that good
    */
    bool LoadGood(const TGoodType AGood, const Byte num);

    /* tries to unload num units of good AGood and returns the amount that
       could be unloaded

       parameters:
           AGood - the type of good that has to be unloaded
           num   - the corresponding amount
    */
    Byte UnloadGood(const TGoodType AGood, const Byte num);

    /* tries to load a unit onto the ship and returns true on success

       parameters:
           AUnit - the unit that will be loaded onto the ship

       remarks:
           Only ships can load units, and the loaded unit must not be a ship
           or caravan itself.
    */
    bool LoadUnit(std::shared_ptr<TUnit> AUnit);

    /* tries to unload a certain unit from a ship and returns true on success

       parameters:
           AType - the type of the unit that has to be unloaded
           x,y   - position of the unit after unloading it
           AMap  - the current map

       remarks:
           The position (x;y) has to be adjacent to the ship's position or it
           should be the ship's position itself. Otherwise, the function will
           return false and not unload the unit.
    */
    bool UnloadUnit(const TUnitType AType, const Byte x, const Byte y, TMap& AMap);

    /* drops all passengers, i.e. unloads all possible units

       remarks:
           The unloaded units will be placed at the ship's map position.
    */
    void DropAllPassengers();

    // ---- item-related functions ----
    /* returns the number of tools that the unit carries */
    Byte GetToolAmount() const;

    /* sets the amount of tools that the unit has

       parameters:
           amount - the new amount of tools

       remarks:
           Any value above 100 will be capped to 100.
           The amount of tools will also be a multiple of 20; other values
           will be reduced to the next lower multiple of 20.
    */
    void GiveTools(const Byte amount);

    /* returns true, if the unit has horses */
    bool HasHorses() const;

    /* sets whether or not the unit has horses

       parameters:
           has - true, if the unit shall be given horses, false otherwise
    */
    void GiveHorses(const bool has = true);

    /* returns true, if the unit is equipped with muskets */
    bool HasMuskets() const;

    /* sets whether or not the unit shall be equipped with muskets

       parameters:
           has - if set to true, the unit will have muskets. Otherwise, it won't.
    */
    void GiveMuskets(const bool has = true);

    /* sets the interal item byte

       parameters:
           new_items - value indicating the unit's items

       remarks:
           Do NOT use this function directly, it's only used during the loading
           process. If you want to change the items of a unit, you should use
           the functions GiveTools(), GiveHorses() and GiveMuskets() instead.
    */
    void ChangeAllItems(const Byte new_items);

    /* tries to save the unit to the given file stream and returns true in case
       of success

       parameters:
           fs - the file stream the unit shall be saved to
    */
    bool SaveToStream(std::ofstream& fs) const;

    /* sets the good amount that the unit carries in a certain slot

       parameters:
           slot       - the internal slot (0-based, less than six)
           new_amount - the amount of the good that has to be stored in the slot
           AGood      - the type of good that has to be stored in the slot

       remarks:
           Do not use this function directly, it's only used during the loading
           process. You should use function LoadGood() instead.
    */
    void SetCargo(const Byte slot, const Byte new_amount, const TGoodType AGood);

    /* returns the amount of goods in a certain slot

       parameters:
           slot - the slot index (has to be in [0;5])
    */
    Byte GetCargoAmountBySlot(const Byte slot) const;

    /* returns the type of good in a certain slot

       parameters:
           slot - the slot index (has to be in [0;5])

       remarks:
           The return value of that function is meaningless, unless the
           function GetCargoAmountBySlot() returns a value larger than zero
           for the same slot.
    */
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
      std::array<std::shared_ptr<TUnit>, 6> passengers;
      //stores cargo (on ships an convoys)
      struct cls
      {
          Byte amount;
          TGoodType which;
      };
      std::array<cls, 6> cargo_load;
      std::shared_ptr<TTask> AI_Task;
};//class TUnit

/* type for an array of units */
typedef std::vector<std::shared_ptr<TUnit>> TUnitArr;


// ---- the AI stuff ----
/* enumeration type to indicate the type of an AI task */
enum TTaskType {ttGeneric, ttPlough, ttRoad, ttClear, ttGoTo, ttGoToEurope, ttFindLand};

/* ********
   **** TTask abstract class
   ****
   **** purpose: represents a task that a unit has to do, usually assigned by
   ****          AI or similar. This class is abstract and mainly serves as an
   ****          interface for derived classes.
   *******
*/
class TTask
{
  protected:
    std::shared_ptr<TUnit> target;
  public:
    /* returns true, if the task is done

       remarks:
           Derived classes have to implement their own version of that function.
    */
    virtual bool Done() const = 0;

    /* executes the next step of the task and returns true, if something was
       done. Usually, a return value of false indicates that the task is
       already done.

       remarks:
           Derived classes have to implement their own version of that function.
    */
    virtual bool Execute() = 0;

    /* returns the type of the task (something like a RTTI)

       remarks:
           Derived classes have to implement their own version of that function.
    */
    virtual TTaskType GetType() const;

    /* constructor

       parameters:
           target_unit - the unit that has to fulfill the task

       remarks:
           Derived classes will most likely have to implement their own
           constructor.
    */
    TTask(const std::shared_ptr<TUnit> target_unit);

    /* destructor

       remarks:
           Derived classes might have to implement their own destructor.
    */
    virtual ~TTask();
};//class

// ---- tasks for the pioneers ----
/* ********
   **** TPloughTask class
   ****
   **** purpose: represents a task where a unit has to plough a certain map
   ****          square. Usually that unit is a pioneer.
   ****          Derived from TTask.
   *******
*/
class TPloughTask: public TTask
{
  private:
    Byte m_X, m_Y;
    std::shared_ptr<TMap> m_Map;
    Byte RoundsLeft;
  public:
    /* constructor

       parameters:
           target_unit - the unit that has to fulfill the task
           X,Y         - position where the unit does the task
           AMap        - the current map
    */
    TPloughTask(const std::shared_ptr<TUnit> target_unit, Byte X, Byte Y, const std::shared_ptr<TMap> AMap);

    /* returns true, if the ploughing is done */
    virtual bool Done() const override;

    /* executes the next step of the task and returns true, if something was
       done. Usually, a return value of false indicates that the task is
       already done.
    */
    virtual bool Execute() override;

    /* returns the type of the task (something like a RTTI) */
    virtual TTaskType GetType() const override;
};//class

/* ********
   **** TRoadTask class
   ****
   **** purpose: represents a task where a unit has to construct a road in a
   ****          certain map square. Usually that unit is a pioneer.
   ****          Derived from TTask.
   *******
*/
class TRoadTask: public TTask
{
  private:
    Byte m_X, m_Y;
    std::shared_ptr<TMap> m_Map;
    Byte RoundsLeft;
  public:
    /* constructor

       parameters:
           target_unit - the unit that has to fulfill the task
           X,Y         - position where the unit does the task
           AMap        - the current map
    */
    TRoadTask(const std::shared_ptr<TUnit> target_unit, Byte X, Byte Y, const std::shared_ptr<TMap> AMap);

    /* returns true, if the road is constructed */
    virtual bool Done() const override;

    /* executes the next step of the task and returns true, if something was
       done. Usually, a return value of false indicates that the task is
       already done.
    */
    virtual bool Execute() override;

    /* returns the type of the task (something like a RTTI) */
    virtual TTaskType GetType() const override;
};//class

/* ********
   **** TClearTask class
   ****
   **** purpose: represents a task where a unit has to clear the forest in a
   ****          certain map square. Usually that unit is a pioneer.
   ****          Derived from TTask.
   *******
*/
class TClearTask: public TTask
{
  private:
    Byte m_X, m_Y;
    std::shared_ptr<TMap> m_Map;
    Byte RoundsLeft;
  public:
    /* constructor

       parameters:
           target_unit - the unit that has to fulfill the task
           X,Y         - position where the unit does the task
           AMap        - the current map
    */
    TClearTask(const std::shared_ptr<TUnit> target_unit, Byte X, Byte Y, const std::shared_ptr<TMap> AMap);

    /* returns true, if the forest is cleared */
    virtual bool Done() const override;

    /* executes the next step of the task and returns true, if something was
       done. Usually, a return value of false indicates that the task is
       already done.
    */
    virtual bool Execute() override;

    /* returns the type of the task (something like a RTTI) */
    virtual TTaskType GetType() const override;
};//class

// ---- task for pathfinding ----
/* ********
   **** TGoToTask class
   ****
   **** purpose: represents a task where a unit has to travel to a certain map
   ****          square. This task uses A* for path finding.
   ****          Derived from TTask.
   *******
*/
class TGoToTask: public TTask
{
  protected:
    Byte m_X, m_Y;//destination location
    Byte spec_X, spec_Y; //location of special field
    std::shared_ptr<TMap> m_Map;
    TCoordArr m_Path; //path that the unit will travel
  public:
    /* constructor

       parameters:
           target_unit - the unit that will travel to the given destination
           ToX,ToY     - destination position
           AMap        - the current map (must not be nil)
           SpecialX    - x-coordinate of the "special" field
           SpecialY    - y-coordinate of the "special" field
    */
    TGoToTask(const std::shared_ptr<TUnit> target_unit, Byte ToX, Byte ToY, const std::shared_ptr<TMap> AMap, const Byte SpecialX = 250, const Byte SpecialY = 250);

    /* destructor */
    virtual ~TGoToTask();

    /* returns true, if the unit has arrived at its destination or if there is
       no path to the desired destination */
    virtual bool Done() const override;

    /* executes the next step of the task and returns true, if something was
       done. Usually, a return value of false indicates that the task is
       already done (see Done()).
    */
    virtual bool Execute() override;

    /* returns the x-coordinate of the destination field */
    Byte DestinationX() const;

    /* returns the y-coordinate of the destination field */
    Byte DestinationY() const;

    /* returns the type of the task (something like a RTTI) */
    virtual TTaskType GetType() const override;
};//class


/* ********
    **** TGoToEuropeTask class
    ****
    **** purpose: represents a task where a unit has to travel to a high sea map
    ****          square and then will be sent to Europe. This task uses A* for
    ****          path finding.
    ****          Derived from TGoToTask.
    *******
  */
class TGoToEuropeTask: public TGoToTask
{
  public:
    /* constructor

       parameters:
           target_unit - the unit that shall travel to Europe
           ToX,ToY     - destination position, i.e. position of a high sea map square
           AMap        - the current map (must not be nil)
     */
     TGoToEuropeTask(const std::shared_ptr<TUnit> target_unit, Byte ToX, Byte ToY, const std::shared_ptr<TMap> AMap);

    /* destructor */
    virtual ~TGoToEuropeTask();

    /* returns true, if the unit has arrived at its destination or if there is
       no path to the desired destination */
    bool Done() const override;

    /* executes the next step of the task and returns true, if something was
       done. Usually, a return value of false indicates that the task is
       already done (see Done()).
    */
    virtual bool Execute() override;

    /* returns the type of the task (something like a RTTI) */
    virtual TTaskType GetType() const override;
};//class

/* ********
   **** TFindLandForColonyTask class
   ****
   **** purpose: represents a task where a unit tries to find a land square for
   ****          building a colony there. Might not always succeed yet.
   ****          Derived from TTask.
   ****
   **** TODO: make sure it always succeeds
   *******
*/
class TFindLandForColonyTask: public TGoToTask
{
  protected:
    void * m_Data;
    bool m_BuildWhenDone;
  public:
    /* constructor

       parameters:
           target_unit - the unit that will travel to the given destination
           AMap        - the current map (must not be nil)
           dat         - Pointer to TData object (must not be nil)
    */
    TFindLandForColonyTask(const std::shared_ptr<TUnit> target_unit, const std::shared_ptr<TMap> AMap, const void * dat);

    /* destructor */
    ~TFindLandForColonyTask();

    /* returns true, if the unit has finished its task, i.e. it has found a
       land square and dropped a colonist to build a colony
    */
    virtual bool Done() const override;

    /* executes the next step of the task and returns true, if something was
       done. Usually, a return value of false indicates that the task is
       already done (see Done()).
    */
    virtual bool Execute() override;

    /* returns the type of the task (something like a RTTI) */
    virtual TTaskType GetType() const override;
};//class TFindLandTask


/* applies a given direction to the position, i.e. returns (in x and y) the
   position that (x;y) would be, if it moved into the given direction

   parameters:
       x,y - position (before the call: initial position; after the call:
              new position after the coordinates were moved into the direction)
       dir - the direction
*/
void ApplyDir(Byte& x, Byte& y, const TDirection dir);

/* returns the direction that has to be applied to get from one square to
   another, adjacent square. If the two given squares are not directly
   adjacent, dirNone will be returned.

   parameters:
       from_x,from_y - position of the first field
       to_x, to_y    - position of the second/destination field
*/
TDirection GetApplyingDirection(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y);

/* returns the best unit type for the production of a certain good. If no unit
   is found, utCriminal is returned.

   parameters:
       AGood - the type of good
*/
TUnitType GetUnitForGood(const TGoodType AGood);

/* returns true, if the given unit type is an expert for producing the given
   good type

   parameters:
       AGood - the type of the good
       ut    - the unit's type
*/
bool HasExpertStatus(const TGoodType AGood, const TUnitType ut);

#endif // UNITS_HPP
