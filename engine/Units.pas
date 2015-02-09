{ ***************************************************************************

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
}

unit Units;

interface

uses
  Goods, Map, Classes, Terrain, PathFinder;

const
  UNIT_ITEM_NONE: Byte = 0;
  UNIT_TOOL_MASK: Byte = 7;
  UNIT_HORSE_BIT: Byte = 8;
  UNIT_MUSKET_BIT: Byte = 16;

type
  { enumeration type for unit type }
  TUnitType = (utCriminal, utServant, utColonist,
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

               utBrave, utBraveOnHorse);

  { enumeration type for units' location }
  TUnitLocation = (ulAmerica, ulInColony, ulGoToEurope, ulGoToNewWorld, ulEurope, ulEmbarked);

  { state of units, i.e. fortified, waiting, going to,... }
  TUnitState = (usNormal,{-} usFortified,{F} usWaitingForShip{S}, usGoTo{G}, usPloughing{P}, usCreateRoad{R});

  { enumeration type for directions }
  TDirection = (dirSW, dirS, dirSE, dirE, dirNE, dirN, dirNW, dirW, dirNone);

const
  { constant array that holds the prices for ships in Europe }
  cShipPrices: array [utArtillery..utFrigate] of Integer
                =(500{artillery}, 16000000{Convoy, can't be bought},
                  1000{caravel}, 2000{trading ship}, 3000{galleon},
                  2000{privateer}, 5000{frigate});

  { constant array that holds the prices for recruitung units in Europe. A value
    of -1 indicates that this type of unit cannot be recruited.
  }
  cUnitPrices: array [utFarmer..utRegular] of LongInt =(
                 1100, //utFarmer
                 1000, //utFisher
                 -1, //utFurHunter
                 900, //utSilverMiner
                 700, //utWoodcutter
                 600, //utOreMiner
                 -1, //utSugarplanter
                 -1, //utCottonplanter
                 -1, //utTobaccoplanter
                 1500, //utPreacher
                 1900, //utStatesman
                 1000, //utCarpenter
                 1100, //utDistiller
                 1300, //utWeaver
                 1200, //utTobacconist
                 950, //utFurTrader
                 1050, //utSmith
                 850, //utWeaponSmith
                 -1, //utScout
                 1200, //utPioneer
                 1400, //utMissionary
                 2000 //utRegular
               );

type
  { forward declaration of TTask (see below TUnit for full declaration) }
  TTask = class;

  { ********
    **** TUnit class
    ****
    **** purpose: represents a single unit within the game, i.e. a colonist,
    ****          an Indian warrior, a ship,...
    ********

      -TO-DO: function LoadGood() still uses a new slot for every good, even if there already is
       ****** an amount of the given good loaded (e.g. trying to load 20 food and
              then again less then 80 food will occupy two slots, even though a
              slot is able to store up to 100 units of a good.
  }
  TUnit = class
    public
      { holds the number of moves that the unit still can perform within the
        current turn

        remarks:
            This value should usually be private and not public.
      }
      MovesLeft: Integer;

      { constructor

        parameters:
            TypeOfUnit - the unit's type
            ANation    - integer constant identifying the unit's nation
            x, y       - initial position of the unit
      }
      constructor Create(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1);

      { destructor }
      destructor Destroy; override;

      { starts a new round for the unit }
      procedure NewRound;

      { tries to moves the unit into a given direction and returns true, if the
        unit could be moved

        parameters:
            direction - the direction into which the unit should be moving
            AMap      - the current map
            dat       - data structure
      }
      function Move(const direction: TDirection; const AMap: TMap; const dat: Pointer): Boolean;

      { tries to "teleport" a unit to the given coordinates and returns true on
        success

        parameters:
            x, y - destination position of the unit
            AMap - the current map

        remarks:
            This function always succeeds, except for positions which are not
            on the map. If a map is specified (i.e. not nil), the surrounding
            tiles of the new position will be discovered by the unit.
      }
      function WarpToXY(const x, y: Byte; AMap: TMap): Boolean;

      { returns the x-coordinate of the unit's position }
      function GetPosX: LongInt;

      { returns the y-coordinate of the unit's position }
      function GetPosY: LongInt;

      { returns the unit's nation identifier }
      function GetNation: LongInt;

      { changes the nation of the unit

        parameters:
            new_nation - the integer identifying the unit's new nation
      }
      procedure ChangeNation(const new_nation: LongInt);

      { returns the unit's type }
      function GetType: TUnitType;

      { changes the type of the unit

        parameters:
            newType - the unit's new type
      }
      procedure ChangeType(const newType: TUnitType);

      { returns the unit's location as a symbolic enumeration value

        remarks:
            This is not to be confused with the "real" position, i.e. the map
            position of a unit. If you want to get that position, use
            GetPosX() and GetPosY().
      }
      function GetLocation: TUnitLocation;

      { sets the unit's location

        parameters:
            loc - the new location
      }
      procedure SetLocation(const loc: TUnitLocation);

      { returns the unit's status }
      function GetState: TUnitState;

      { sets a new unit status

        parameters:
            state - the new unit state
      }
      procedure SetState(const state: TUnitState);

      { returns the rounds the unit still is in open sea before it will arrive
        in Europe or the new World

        remarks:
            This is only used for ships, as you might have guessed.
      }
      function GetRoundsInOpenSea: Byte;

      { sets the number of rounds that the unit still is in open sea before it
        arrives in Europe or the new World

        parameters:
            rounds_left - the amount of rounds left until arrival
      }
      procedure SetRoundsInOpenSea(const rounds_left: Byte);

      { returns true, if the unit is a ship }
      function IsShip: Boolean;

      { returns the number of moves that the unit has per round }
      function MovesPerRound: Integer;

      { returns the combat strength of the unit }
      function AttackStrength: Integer;

      { returns the current AI task that is assigned to this unit, or nil if
        there is no task at the moment
      }
      function GetTask: TTask;

      { assigns a new AI task to the unit

        parameters:
            new_task         - the AI task
            ImmediateExecute - If set to true, the unit will immediately start
                               to work according to the assigned task. Otherwise
                               it will wait until the next round begins.
      }
      procedure SetTask(const new_task: TTask; const ImmediateExecute: Boolean=True);

      // ---- go across the big pond ----
      { tries to send a unit on travel to Europe and returns true on success

        remarks:
            This function can only succeed for ships, and only if the ship is
            in the new world. Calling this function for a unit that already is
            sailing to Europe will return false, too.
      }
      function SendToEurope: Boolean;

      { tries to send a unit on travel to the new world and returns true on
        success

        remarks:
            This function can only succeed for ships, and only if the ship is
            in Europe. Calling this function for a unit that already is sailing
            to the new world will return false, too.
      }
      function SendToNewWorld: Boolean;

      { tries to send a unit which already sails to the new world back to Europe
        and will return true on success

        remarks:
            This function can only succeed for ships, and only if the ship is
            already sailing to the new world.
      }
      function CallBackToEurope: Boolean;

      { tries to send a unit which already sails to Europe back to the new world
        and will return true on success

        remarks:
            This function can only succeed for ships, and only if the ship is
            already sailing to Europe.
      }
      function CallBackToNewWorld: Boolean;

      // ---- functions for loading/ unloading freight or passengers and
      //      checking freight status ----
      { returns the total number of slots the unit has

        remarks:
            For units that are not ships or caravans, this will always return
            zero.
      }
      function FreightCapacity: Byte;

      { returns the number of free slots that the unit has

        remarks:
            For units that are not ships or caravans, this will always return
            zero.
      }
      function FreeCapacity: Byte;

      { returns the number of passengers that the ship carries }
      function EmbarkedPassengers: Byte;

      { returns the first embarked unit in the ship, or nil if there are no
        passengers
      }
      function GetFirstEmbarkedPassenger: TUnit;

      { returns the ship's passenger within a certain slot

        parameters:
            slot - the slot (0-based, has to be less than six)
      }
      function GetPassengerBySlot(const slot: Byte): TUnit;


      { tries to load num units of good 'AGood', maximum is 100, and returns
        true, if the goods could be loaded onto the ship/caravan

        parameters:
            AGood - the type of good that will be loaded
            num   - the amount of that good
      }
      function LoadGood(const AGood: TGoodType; const num: Byte): Boolean;

      { tries to unload num units of good AGood and returns the amount that
        could be unloaded

        parameters:
            AGood - the type of good that has to be unloaded
            num   - the corresponding amount
      }
      function UnloadGood(const AGood: TGoodType; const num: Byte): Byte;

      { tries to load a unit onto the ship and returns true on success

        parameters:
            AUnit - the unit that will be loaded onto the ship

        remarks:
            Only ships can load units, and the loaded unit must not be a ship
            or caravan itself.
      }
      function LoadUnit(AUnit: TUnit): Boolean;

      { tries to unload a certain unit from a ship and returns true on success

        parameters:
            AType - the type of the unit that has to be unloaded
            x,y   - position of the unit after unloading it
            AMap  - the current map

        remarks:
            The position (x;y) has to be adjacent to the ship's position or it
            should be the ship's position itself. Otherwise, the function will
            return false and not unload the unit.
      }
      function UnloadUnit(const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;

      { drops all passengers, i.e. unloads all possible units

        remarks:
            The unloaded units will be placed at the ship's map position.
      }
      procedure DropAllPassengers;

      // ---- item-related functions ----
      { returns the number of tools that the unit carries }
      function GetToolAmount: Byte;

      { sets the amount of tools that the unit has

        parameters:
            amount - the new amount of tools

        remarks:
            Any value above 100 will be capped to 100.
            The amount of tools will also be a multiple of 20; other values
            will be reduced to the next lower multiple of 20.
      }
      procedure GiveTools(const amount: Byte);

      { returns true, if the unit has horses }
      function HasHorses: Boolean;

      { sets whether or not the unit has horses

        parameters:
            has - true, if the unit shall be given horses, false otherwise
      }
      procedure GiveHorses(const has: Boolean = True);

      { returns true, if the unit is equipped with muskets }
      function HasMuskets: Boolean;

      { sets whether or not the unit shall be equipped with muskets

        parameters:
            has - if set to true, the unit will have muskets. Otherwise, it won't.
      }
      procedure GiveMuskets(const has: Boolean = True);

      { sets the interal item byte

        parameters:
            new_items - value indicating the unit's items

        remarks:
            Do NOT use this function directly, it's only used during the loading
            process. If you want to change the items of a unit, you should use
            the functions GiveTools(), GiveHorses() and GiveMuskets() instead.
      }
      procedure ChangeAllItems(const new_items: Byte);

      { tries to save the unit to the given file stream and returns true in case
        of success

        parameters:
            fs - the file stream the unit shall be saved to
      }
      function SaveToStream(var fs: TFileStream): Boolean;

      { sets the good amount that the unit carries in a certain slot

        parameters:
            slot       - the internal slot (0-based, less than six)
            new_amount - the amount of the good that has to be stored in the slot
            AGood      - the type of good that has to be stored in the slot

        remarks:
            Do not use this function directly, it's only used during the loading
            process. You should use function LoadGood() instead.
      }
      procedure SetCargo(const slot: Byte; const new_amount: Byte; const AGood: TGoodType);

      { returns the amount of goods in a certain slot

        parameters:
            slot - the slot index (has to be in [0;5])
      }
      function GetCargoAmountBySlot(const slot: Byte): Byte;

      { returns the type of good in a certain slot

        parameters:
            slot - the slot index (has to be in [0;5])

        remarks:
            The return value of that function is meaningless, unless the
            function GetCargoAmountBySlot() returns a value larger than zero
            for the same slot.
      }
      function GetCargoGoodBySlot(const slot: Byte): TGoodType;
    private
      PosX, PosY: LongInt;
      UnitType: TUnitType;
      m_location: TUnitLocation;
      m_State: TUnitState;
      m_RoundsInOpenSea: Byte;
      m_Nation: LongInt;
      //stores items like horses, muskets, tools
      items: Byte;
      //stores passengers (on ships)
      passengers: array [0..5] of TUnit;
      //stores cargo (on ships an convoys)
      cargo_load: array [0..5] of record
                                    amount: Byte;
                                    which: TGoodType;
                                  end;//rec
      AI_Task: TTask;
  end;//class TUnit

  { type for an array of units }
  TUnitArr = array of TUnit;


  // ---- the AI stuff ----
  { enumeration type to indicate the type of an AI task }
  TTaskType = (ttGeneric, ttPlough, ttRoad, ttClear, ttGoTo, ttGoToEurope, ttFindLand);

  { ********
    **** TTask abstract class
    ****
    **** purpose: represents a task that a unit has to do, usually assigned by
    ****          AI or similar. This class is abstract and mainly serves as an
    ****          interface for derived classes.
    *******
  }
  TTask = class
    protected
      target: TUnit;
    public
      { returns true, if the task is done

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Done: Boolean; virtual; abstract;

      { executes the next step of the task and returns true, if something was
        done. Usually, a return value of false indicates that the task is
        already done.

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Execute: Boolean; virtual; abstract;

      { returns the type of the task (something like a RTTI)

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function GetType: TTaskType; virtual;

      { constructor

        parameters:
            target_unit - the unit that has to fulfill the task

        remarks:
            Derived classes will most likely have to implement their own
            constructor.
      }
      constructor Create(const target_unit: TUnit);

      { destructor

        remarks:
            Derived classes might have to implement their own destructor.
      }
      destructor Destroy; override;
  end;//class

  // ---- tasks for the pioneers ----
  { ********
    **** TPloughTask class
    ****
    **** purpose: represents a task where a unit has to plough a certain map
    ****          square. Usually that unit is a pioneer.
    ****          Derived from TTask.
    *******
  }
  TPloughTask = class(TTask)
    private
      m_X, m_Y: Byte;
      m_Map: TMap;
      RoundsLeft: Byte;
    public
      { constructor

        parameters:
            target_unit - the unit that has to fulfill the task
            X,Y         - position where the unit does the task
            AMap        - the current map
      }
      constructor Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);

      { returns true, if the ploughing is done }
      function Done: Boolean; override;

      { executes the next step of the task and returns true, if something was
        done. Usually, a return value of false indicates that the task is
        already done.
      }
      function Execute: Boolean; override;

      { returns the type of the task (something like a RTTI) }
      function GetType: TTaskType; override;
  end;//class

  { ********
    **** TRoadTask class
    ****
    **** purpose: represents a task where a unit has to construct a road in a
    ****          certain map square. Usually that unit is a pioneer.
    ****          Derived from TTask.
    *******
  }
  TRoadTask = class(TTask)
    private
      m_X, m_Y: Byte;
      m_Map: TMap;
      RoundsLeft: Byte;
    public
      { constructor

        parameters:
            target_unit - the unit that has to fulfill the task
            X,Y         - position where the unit does the task
            AMap        - the current map
      }
      constructor Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);

      { returns true, if the road is constructed }
      function Done: Boolean; override;

      { executes the next step of the task and returns true, if something was
        done. Usually, a return value of false indicates that the task is
        already done.
      }
      function Execute: Boolean; override;

      { returns the type of the task (something like a RTTI) }
      function GetType: TTaskType; override;
  end;//class

  { ********
    **** TClearTask class
    ****
    **** purpose: represents a task where a unit has to clear the forest in a
    ****          certain map square. Usually that unit is a pioneer.
    ****          Derived from TTask.
    *******
  }
  TClearTask = class(TTask)
    private
      m_X, m_Y: Byte;
      m_Map: TMap;
      RoundsLeft: Byte;
    public
      { constructor

        parameters:
            target_unit - the unit that has to fulfill the task
            X,Y         - position where the unit does the task
            AMap        - the current map
      }
      constructor Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);

      { returns true, if the forest is cleared }
      function Done: Boolean; override;

      { executes the next step of the task and returns true, if something was
        done. Usually, a return value of false indicates that the task is
        already done.
      }
      function Execute: Boolean; override;

      { returns the type of the task (something like a RTTI) }
      function GetType: TTaskType; override;
  end;//class

  // ---- task for pathfinding ----
  { ********
    **** TGoToTask class
    ****
    **** purpose: represents a task where a unit has to travel to a certain map
    ****          square. This task uses A* for path finding.
    ****          Derived from TTask.
    *******
  }
  TGoToTask = class(TTask)
    protected
      m_X, m_Y: Byte;//destination location
      spec_X, spec_Y: Byte; //location of special field
      m_Map: TMap;
      m_Path: TCoordArr; //path that the unit will travel
    public
      { constructor

        parameters:
            target_unit - the unit that will travel to the given destination
            ToX,ToY     - destination position
            AMap        - the current map (must not be nil)
            SpecialX    - x-coordinate of the "special" field
            SpecialY    - y-coordinate of the "special" field
      }
      constructor Create(const target_unit: TUnit; ToX, ToY: Byte; const AMap: TMap; const SpecialX: Byte=250; const SpecialY: Byte=250);

      { destructor }
      destructor Destroy; override;

      { returns true, if the unit has arrived at its destination or if there is
        no path to the desired destination }
      function Done: Boolean; override;

      { executes the next step of the task and returns true, if something was
        done. Usually, a return value of false indicates that the task is
        already done (see Done()).
      }
      function Execute: Boolean; override;

      { returns the x-coordinate of the destination field }
      function DestinationX: Byte;

      { returns the y-coordinate of the destination field }
      function DestinationY: Byte;

      { returns the type of the task (something like a RTTI) }
      function GetType: TTaskType; override;
  end;//class


  { ********
    **** TGoToEuropeTask class
    ****
    **** purpose: represents a task where a unit has to travel to a high sea map
    ****          square and then will be sent to Europe. This task uses A* for
    ****          path finding.
    ****          Derived from TGoToTask.
    *******
  }
  TGoToEuropeTask = class(TGoToTask)
    public
      { constructor

        parameters:
            target_unit - the unit that shall travel to Europe
            ToX,ToY     - destination position, i.e. position of a high sea map square
            AMap        - the current map (must not be nil)
      }
      constructor Create(const target_unit: TUnit; ToX, ToY: Byte; const AMap: TMap);

      { destructor }
      destructor Destroy; override;

      { returns true, if the unit has arrived at its destination or if there is
        no path to the desired destination }
      function Done: Boolean; override;

      { executes the next step of the task and returns true, if something was
        done. Usually, a return value of false indicates that the task is
        already done (see Done()).
      }
      function Execute: Boolean; override;

      { returns the type of the task (something like a RTTI) }
      function GetType: TTaskType; override;
  end;//class

  { ********
    **** TFindLandForColonyTask class
    ****
    **** purpose: represents a task where a unit tries to find a land square for
    ****          building a colony there. Might not always succeed yet.
    ****          Derived from TTask.
    ****
    **** TODO: make sure it always succeeds
    *******
  }
  TFindLandForColonyTask = class(TGoToTask)
    protected
      m_Data: Pointer;
      m_BuildWhenDone: Boolean;
    public
      { constructor

        parameters:
            target_unit - the unit that will travel to the given destination
            AMap        - the current map (must not be nil)
            dat         - Pointer to TData object (must not be nil)
      }
      constructor Create(const target_unit: TUnit; const AMap: TMap; const dat: Pointer);

      { destructor }
      destructor Destroy; override;

      { returns true, if the unit has finished its task, i.e. it has found a
        land square and dropped a colonist to build a colony
      }
      function Done: Boolean; override;

      { executes the next step of the task and returns true, if something was
        done. Usually, a return value of false indicates that the task is
        already done (see Done()).
      }
      function Execute: Boolean; override;

      { returns the type of the task (something like a RTTI) }
      function GetType: TTaskType; override;
  end;//class TFindLandTask


  { applies a given direction to the position, i.e. returns (in x and y) the
    position that (x;y) would be, if it moved into the given direction

    parameters:
        x,y - position (before the call: initial position; after the call:
               new position after the coordinates were moved into the direction)
        dir - the direction
  }
  procedure ApplyDir(var x,y: Byte; const dir: TDirection);

  { returns the direction that has to be applied to get from one square to
    another, adjacent square. If the two given squares are not directly
    adjacent, dirNone will be returned.

    parameters:
        from_x,from_y - position of the first field
        to_x, to_y    - position of the second/destination field
  }
  function GetApplyingDirection(const from_x, from_y, to_x, to_y: Byte): TDirection;

  { returns the best unit type for the production of a certain good. If no unit
    is found, utCriminal is returned.

    parameters:
        AGood - the type of good
  }
  function GetUnitForGood(const AGood: TGoodType): TUnitType;

  { returns true, if the given unit type is an expert for producing the given
    good type

    parameters:
        AGood - the type of the good
        ut    - the unit's type
  }
  function HasExpertStatus(const AGood: TGoodType; const ut: TUnitType): Boolean;

implementation

uses Colony, Data, DebugWriter;

//helper procedure
procedure ApplyDir(var x,y: Byte; const dir: TDirection);
begin
  case dir of
    dirW, dirSW, dirNW: if x>0 then x:= x-1;
    dirE, dirNE, dirSE: if x<cMap_X-1 then x:= x+1;
  end;//case
  case dir of
    dirNW, dirN, dirNE: if y>0 then y:= y-1;
    dirSW, dirS, dirSE: if y<cMap_Y-1 then y:= y+1;
  end;//case
end;//proc

function GetApplyingDirection(const from_x, from_y, to_x, to_y: Byte): TDirection;
begin
  if (abs(from_x-to_x)>1) or (abs(from_y-to_y)>1) then Result:= dirNone
  else begin
    case to_x-from_x of
       1: case to_y-from_y of
           -1: Result:= dirNE;
            0: Result:= dirE;
            1: Result:= dirSE;
          end;//case
       0: case to_y-from_y of
           -1: Result:= dirN;
            0: Result:= dirNone;
            1: Result:= dirS;
          end;//case
      -1: case to_y-from_y of
           -1: Result:= dirNW;
            0: Result:= dirW;
            1: Result:= dirSW;
          end;//case
    end;//case
  end;//else
end;//func

// ***************
// *TUnit methods*
// ***************

constructor TUnit.Create(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1);
var i: Integer;
begin
  UnitType:= TypeOfUnit;
  PosX:= X;
  PosY:= Y;
  m_location:= ulAmerica;
  m_State:= usNormal;
  m_RoundsInOpenSea:= 0;
  MovesLeft:= MovesPerRound;
  m_Nation:= ANation;
  items:= 0;
  AI_Task:= nil;
  if TypeOfUnit = utPioneer then GiveTools(100)
  else if (TypeOfUnit in [utRegular, utDragoon]) then GiveMuskets(True);
  if (TypeOfUnit in [utScout, utDragoon, utBraveOnHorse]) then GiveHorses(True);
  for i:= 0 to 5 do
  begin
    passengers[i]:= nil;
    cargo_load[i].amount:= 0;
    cargo_load[i].which:= gtFood;
  end;//for
end;//construc

destructor TUnit.Destroy;
begin
  inherited Destroy;
end;//destruc

procedure TUnit.NewRound;
begin
  //regain moves
  MovesLeft:= MovesPerRound;
  //check for passage over the pond
  if m_RoundsInOpenSea>0 then
  begin
    m_RoundsInOpenSea:= m_RoundsInOpenSea-1;
    if m_RoundsInOpenSea=0 then
    begin
      case m_Location of
        ulGoToNewWorld: m_Location:= ulAmerica;
        ulGoToEurope: begin
                        m_Location:= ulEurope;
                        DropAllPassengers;
                      end;//case GoToEurope
      end;//case
    end;//if
  end;//if

  //check for task and execute, if present
  if AI_Task<>nil then
  begin
    WriteDebugLn('New Round: Exec calling');
    AI_Task.Execute;

    if AI_Task.Done then
    begin
      AI_Task.Destroy;
      AI_Task:= nil;
    end;
  end;
end;//proc

function TUnit.Move(const direction: TDirection; const AMap: TMap; const dat: Pointer): Boolean;
var newX, newY, i: Integer;
    allow: Boolean;
    tempCol: TColony;
    u_arr: TUnitArr;
begin
  if MovesLeft <= 0 then
    Result:= False
  else begin
    case direction of
      dirW, dirSW, dirNW: newX:= PosX-1;
      dirE, dirNE, dirSE: newX:= PosX+1;
    else
      newX:= PosX;
    end;//case
    case direction of
      dirNW, dirN, dirNE: newY:= PosY-1;
      dirSW, dirS, dirSE: newY:= PosY+1;
    else
      newY:= PosY;
    end;//case
    allow:= True;
    //check if we are out of map
    if ((newX<0) or (newY<0) or (newX>=cMap_X) or (newY>=cMap_Y)) then
      allow:= False
    else begin
      if AMap<>nil then
        if AMap.tiles[newX,newY]<>nil then
        begin
          allow:= (IsShip=AMap.tiles[newX,newY].IsWater);
        end;//if
    end;//else

    if allow then
    begin
      //check ships for european route
      if IsShip and (AMap<>nil) and (AI_Task=nil) then //no european route for non-ships or AI tasks
        if ((AMap.tiles[PosX, PosY].GetType=ttOpenSea) and (AMap.tiles[newX, newY].GetType=ttOpenSea)) then
        begin
          SendToEurope;
          Result:= True;
          Exit;
        end;//if
      if direction<>dirNone then
      begin
        //check ships
        if IsShip and (AMap<>nil) then
        begin
          if (not AMap.tiles[PosX, PosY].IsWater) and (dat<>nil) then
          begin
            tempCol:= TData(dat).GetColonyInXY(PosX, PosY);
            if tempCol<>nil then
            begin
              u_arr:= TData(dat).GetAllUnitsInColony(tempCol);
              i:= 0;
              while (FreeCapacity>0) and (i<=High(u_arr)) do
              begin
                if u_arr[i].GetState=usWaitingForShip then LoadUnit(u_arr[i]);
                i:= i+1;
              end;//while
            end;
          end;//if
        end;//if ship and map present
        //do the actual move
        MovesLeft:= MovesLeft -1;
        PosX:= newX;
        PosY:= newY;
      end;//if
      if ((AMap<>nil) and (m_Nation<>0)) then AMap.DiscoverSurroundingTiles(newX, newX, m_Nation, UnitType=utScout);
      Result:= True;
    end//if
    else Result:= False;
  end;//else
end;//func

function TUnit.WarpToXY(const x, y: Byte; AMap: TMap): Boolean;
begin
  if ((x>=cMap_X) or (y>=cMap_Y)) then Result:= False
  else begin
    PosX:= x;
    PosY:= y;
    Result:= True;
    if ((AMap<>nil) and (m_Nation<>0)) then
      AMap.DiscoverSurroundingTiles(x,y, m_Nation, UnitType=utScout);
  end;
end;//func

function TUnit.GetPosX: LongInt;
begin
  Result:= PosX;
end;

function TUnit.GetPosY: LongInt;
begin
  Result:= PosY;
end;

function TUnit.GetNation: LongInt;
begin
  Result:= m_Nation;
end;//func

procedure TUnit.ChangeNation(const new_nation: LongInt);
begin
  m_Nation:= new_nation;
end;//proc

function TUnit.GetType: TUnitType;
begin
  Result:= UnitType;
end;//func

procedure TUnit.ChangeType(const newType: TUnitType);
begin
  //we don't wanna change ships' type or convoy
  if ((not IsShip) and (UnitType<>utConvoy)) then
    UnitType:= newType;
end;//proc

function TUnit.GetLocation: TUnitLocation;
begin
  Result:= m_location;
end;//func

procedure TUnit.SetLocation(const loc: TUnitLocation);
begin
  m_location:= loc;
end;//proc

function TUnit.GetState: TUnitState;
begin
  Result:= m_State;
end;//func

procedure TUnit.SetState(const state: TUnitState);
begin
  m_State:= state;
end;//proc

function TUnit.GetRoundsInOpenSea: Byte;
begin
  Result:= m_RoundsInOpenSea;
end;//func

procedure TUnit.SetRoundsInOpenSea(const rounds_left: Byte);
begin
  m_RoundsInOpenSea:= rounds_left;
end;//proc

function TUnit.IsShip: Boolean;
begin
  Result:= (UnitType in [utCaravel, utTradingShip, utGalleon, utPrivateer,
                        utFrigate, utMan_o_War]);
end;//func

function TUnit.MovesPerRound: Integer;
begin
  case UnitType of
    utScout, utDragoon: Result:= 4;
    utMissionary, utConvoy: Result:= 2;
    utCaravel: Result:= 4;
    utTradingShip: Result:= 5;
    utGalleon: Result:= 6;
    utPrivateer: Result:= 8;
    utFrigate: Result:= 6;
    utMan_o_War: Result:= 5;
    utBraveOnHorse: Result:= 4;
  else Result:= 1;
  end;//case
end;//func

function TUnit.AttackStrength: Integer;
begin
  case UnitType of
    utRegular: Result:= 2;
    utDragoon: Result:= 3;
    utCaravel: Result:= 2;
    utTradingShip: Result:= 6;
    utGalleon: Result:= 10;
    utPrivateer: Result:= 8;
    utFrigate: Result:= 16;
    utMan_o_War: Result:= 24;
    //utBrave: Result:= 1;
    utBraveOnHorse: Result:= 2;
  else Result:= 1;
  end;//case
  if m_State=usFortified then Result:= (Result*3) div 2;
end;//func

function TUnit.GetTask: TTask;
begin
  Result:= AI_Task;
end;//func

procedure TUnit.SetTask(const new_task: TTask; const ImmediateExecute: Boolean=True);
begin
  if AI_Task<>nil then AI_Task.Destroy;
  AI_Task:= new_task;
  if (AI_Task<>nil) and ImmediateExecute then AI_Task.Execute;
end;//proc

function TUnit.SendToEurope: Boolean;
begin
  if (m_location<>ulAmerica) or not IsShip then Result:= False
  else begin
    m_RoundsInOpenSea:= 2;
    MovesLeft:= 0;
    m_location:= ulGoToEurope;
    Result:= True;
  end;//else
end;//func

function TUnit.SendToNewWorld: Boolean;
begin
  if (m_location<>ulEurope) or not IsShip then Result:= False
  else begin
    m_RoundsInOpenSea:= 2;
    MovesLeft:= 0;
    m_location:= ulGoToNewWorld;
    Result:= True;
  end;//else
end;//func

function TUnit.CallBackToEurope: Boolean;
begin
  if (m_location<>ulGoToNewWorld) or not IsShip then Result:= False
  else begin
    m_RoundsInOpenSea:= 2-m_RoundsInOpenSea;
    MovesLeft:= 0;
    m_location:= ulGoToEurope;
    Result:= True;
    if m_RoundsInOpenSea<=0 then
    begin
      m_location:= ulEurope;
      DropAllPassengers;
    end;//if
  end;//else
end;//func

function TUnit.CallBackToNewWorld: Boolean;
begin
  if (m_location<>ulGoToEurope) or not IsShip then Result:= False
  else begin
    m_RoundsInOpenSea:= 2-m_RoundsInOpenSea;
    MovesLeft:= 0;
    m_location:= ulGoToNewWorld;
    Result:= True;
    if m_RoundsInOpenSea<=0 then m_location:= ulAmerica;
  end;//else
end;//func

function TUnit.FreightCapacity: Byte;
begin
  case UnitType of
    utConvoy, utCaravel, utPrivateer: Result:= 2;
    utTradingShip, utFrigate: Result:= 4;
    utGalleon, utMan_o_War: Result:= 6;
  else Result:= 0;
  end;//case
end;//func

function TUnit.FreeCapacity: Byte;
var i, occupied: Byte;
begin
  if FreightCapacity=0 then Result:= 0
  else begin
    occupied:= 0;
    for i:= 0 to 5 do
    begin
      if passengers[i]<>nil then occupied:= occupied+1;
      if cargo_load[i].amount>0 then occupied:= occupied+1;
    end;//for
    if FreightCapacity<=occupied then Result:= 0
    else Result:= FreightCapacity - occupied;
  end//else
end;//func

function TUnit.EmbarkedPassengers: Byte;
var i: Integer;
begin
  Result:= 0;
  for i:= 0 to 5 do
    if passengers[i]<>nil then Result:= Result+1;
end;//func

function TUnit.GetFirstEmbarkedPassenger: TUnit;
var i: Integer;
begin
  Result:= nil;
  for i:= 5 downto 0 do
  begin
    if passengers[i]<>nil then Result:= passengers[i];
  end;//for
end;//func

function TUnit.GetPassengerBySlot(const slot: Byte): TUnit;
begin
  if slot>5 then Result:= nil
  else Result:= passengers[slot];
end;//func

{*tries to load num units of good 'AGood'; maximum is 100
 TO-DO: function still uses a new slot for every good, even if there already is
 ****** an amount of the given good loaded (e.g. trying to load 20 food and
        then again less then 80 food will occupy two slots, even though a slot
        is able to store up to 100 units of a good.
}
function TUnit.LoadGood(const AGood: TGoodType; const num: Byte): Boolean;
var slot: Byte;
begin
  if ((num>100) or (FreeCapacity=0)) then Result:= False
  else begin
    slot:=0;
    while ((cargo_load[slot].amount<>0) and (slot<5)) do
      slot:= slot+1;
    cargo_load[slot].amount:= num;
    cargo_load[slot].which:= AGood;
    Result:= True;
  end;//else
end;//func

//tries to unload 'num' units of good 'AGood' and returns number of unloaded units
function TUnit.UnloadGood(const AGood: TGoodType; const num: Byte): Byte;
var cap: Byte;
    slot: Integer;//needs to be signed type, because it can get negative (-1)
begin
  Result:=0;
  if FreightCapacity>0 then
  begin
    slot:=5;
    while (slot>=0) and (Result<num) do
    begin
      if cargo_load[slot].which=AGood then
      begin
        if cargo_load[slot].amount<num-Result then cap:= cargo_load[slot].amount
        else cap:= num-Result;
        Result:= Result+cap;
        cargo_load[slot].amount:= cargo_load[slot].amount - cap;
      end;//if
      slot:= slot-1;
    end;//while
  end;//if
end;//func

//tries to load unit 'AUnit' and returns True on success
function TUnit.LoadUnit(AUnit: TUnit): Boolean;
var slot: Byte;
begin
  if (FreeCapacity=0) or (AUnit=nil) or (UnitType=utConvoy) then Result:= False
  else if (AUnit.FreightCapacity>0) then Result:= False //no ships or convoys
  else begin
    slot:= 0;
    while (passengers[slot]<>nil) and (slot<5) do
      slot:= slot+1;
    passengers[slot]:= AUnit;
    AUnit.SetLocation(ulEmbarked);
    AUnit.SetState(usWaitingForShip);
    Result:= True;
  end;//else
end;//func

{tries to unload a unit and place it at the given coordinates
 -Return value: true on success, false otherwise
 -TODO: unloads first unit of given type, so if there are two ore more units of
  ===== the same type loaded, then it migth unload the wrong one}
function TUnit.UnloadUnit(const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;
var i: Integer;
begin
  Result:= False;
  if FreightCapacity<=0 then exit;
  if ((sqr(x-GetPosX)>1) or (sqr(y-GetPosY)>1)) then Exit;
  for i:= 5 downto 0 do
  begin
    if passengers[i]<>nil then
      if passengers[i].GetType=AType then
        if passengers[i].WarpToXY(x,y,AMap) then
        begin
          passengers[i].SetLocation(self.GetLocation);
          passengers[i].SetState(usNormal);
          passengers[i]:= nil;
          Result:= True;
          break;
        end;//if
  end;//for
end;//func

procedure TUnit.DropAllPassengers;
var slot: Byte;
begin
  for slot:= 0 to 5 do
  begin
    if passengers[slot]<>nil then
    begin
      if m_Location = ulEurope then
        passengers[slot].SetLocation(ulEurope)
      else begin
        passengers[slot].SetLocation(ulAmerica);
        passengers[slot].WarpToXY(PosX, PosY, nil);
      end;//else
      passengers[slot].SetState(usNormal);
      passengers[slot]:= nil;
    end;//if
  end;//for
end;//proc

function TUnit.GetToolAmount: Byte;
begin
  Result:= (items and UNIT_TOOL_MASK)*20;
end;//func

procedure TUnit.GiveTools(const amount: Byte);
var temp: Byte;
begin
  temp:= amount div 20;
  if temp>5 then temp:=5;
  items:= temp or (items and (UNIT_HORSE_BIT or UNIT_MUSKET_BIT));
end;//proc

function TUnit.HasHorses: Boolean;
begin
  Result:= (items and UNIT_HORSE_BIT)>0;
end;//func

procedure TUnit.GiveHorses(const has: Boolean = True);
begin
  if has then
  begin
    items:= (items or UNIT_HORSE_BIT);
    if UnitType=utRegular then UnitType:= utDragoon;
    if UnitType=utBrave then UnitType:= utBraveOnHorse;
  end//if
  else begin
    items:= (items and (not UNIT_HORSE_BIT));
    if UnitType=utDragoon then UnitType:= utRegular;
    if UnitType=utBraveOnHorse then UnitType:= utBrave;
  end;//else
end;//proc

function Tunit.HasMuskets: Boolean;
begin
  Result:= (items and UNIT_MUSKET_BIT)>0;
end;//func

procedure TUnit.GiveMuskets(const has: Boolean = True);
begin
  if has then items:= (items or UNIT_MUSKET_BIT)
  else items:= (items and (not UNIT_MUSKET_BIT));
end;//proc

procedure TUnit.ChangeAllItems(const new_items: Byte);
begin
  items:= new_items;
end;//proc

{ saving function, returns true on success.
  Loading function is part of TData (to keep data integrity).}
function TUnit.SaveToStream(var fs: TFileStream): Boolean;
var i: Integer;
    pass_count: Byte;
begin
  if fs=nil then
  begin
    Result:= False;
    Exit;
  end;//if
  Result:= fs.Write(MovesLeft, sizeof(MovesLeft))=sizeof(MovesLeft);
  Result:= Result and (fs.Write(PosX, sizeof(PosX))=sizeof(PosX));
  Result:= Result and (fs.Write(PosY, sizeof(PosY))=sizeof(PosY));
  Result:= Result and (fs.Write(UnitType, sizeof(TUnitType))=sizeof(TUnitType));
  Result:= Result and (fs.Write(m_location, sizeof(TUnitLocation))=sizeof(TUnitLocation));
  Result:= Result and (fs.Write(m_State, sizeof(TUnitState))=sizeof(TUnitState));
  Result:= Result and (fs.Write(m_RoundsInOpenSea, sizeof(Byte))=sizeof(Byte));
  Result:= Result and (fs.Write(m_Nation, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(items, sizeof(items))=sizeof(items));
  //save cargo
  for i:= 0 to 5 do
  begin
    Result:= Result and (fs.Write(cargo_load[i].amount, sizeof(Byte))=sizeof(Byte));
    Result:= Result and (fs.Write(cargo_load[i].which, sizeof(TGoodType))=sizeof(TGoodType));
  end;//func
  //save passengers
  pass_count:= EmbarkedPassengers;
  Result:= Result and (fs.Write(pass_count, sizeof(Byte))=sizeof(Byte));
  for i:= 0 to 5 do
    if passengers[i]<>nil then
      Result:= Result and passengers[i].SaveToStream(fs);
  //********* save tasks needs to be done yet! *********
end;//func

function TUnit.GetCargoAmountBySlot(const slot: Byte): Byte;
begin
  if slot<=5 then Result:= cargo_load[slot].amount
  else Result:= 0;
end;//func

function TUnit.GetCargoGoodBySlot(const slot: Byte): TGoodType;
begin
  if slot<=5 then Result:= cargo_load[slot].which
  else Result:= gtCross;
end;//func


//only used during loading routine
procedure TUnit.SetCargo(const slot: Byte; const new_amount: Byte; const AGood: TGoodType);
begin
  if ((FreightCapacity=0) or (slot>5) or (new_amount>100)) then Exit
  else begin
    cargo_load[slot].amount:= new_amount;
    cargo_load[slot].which:= AGood;
  end;//else
end;//func

//**** AI-related functions ****

constructor TTask.Create(const target_unit: TUnit);
begin
  inherited Create;
  target:= target_unit;
end;//construc

destructor TTask.Destroy;
begin
  if target<>nil then target.SetState(usNormal);
  inherited Destroy;
end;//destruc

function TTask.GetType: TTaskType;
begin
  Result:= ttGeneric;
end;//func

//**** TPloughTask methods ****

constructor TPloughTask.Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  m_Map:= AMap;
  RoundsLeft:= 4;
  if target<>nil then
  begin
    target_unit.SetState(usPloughing);
    if target.GetType = utPioneer then RoundsLeft:= 2;
  end;
  if AMap.tiles[m_X, m_Y].IsPloughed then RoundsLeft:= 0;
end;//func

function TPloughTask.Done: Boolean;
begin
  Result:= (RoundsLeft<=0);
end;//func

function TPloughTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target.GiveTools(target.GetToolAmount-20);
      m_Map.tiles[m_X, m_Y].Plough;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

function TPloughTask.GetType: TTaskType;
begin
  Result:= ttPlough;
end;//func

// **** TRoadTask methods ****
constructor TRoadTask.Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  m_Map:= AMap;
  RoundsLeft:= 2;
  if target<>nil then
  begin
    target.SetState(usCreateRoad);
    if target.GetType = utPioneer then RoundsLeft:= 1;
  end;
  if AMap.tiles[m_X, m_Y].HasRoad then RoundsLeft:= 0;
end;//func

function TRoadTask.Done: Boolean;
begin
  Result:= (RoundsLeft<=0);
end;//func

function TRoadTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target.GiveTools(target.GetToolAmount-20);
      m_Map.tiles[m_X, m_Y].CreateRoad;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

function TRoadTask.GetType: TTaskType;
begin
  Result:= ttRoad;
end;//func

// **** TClearTask methods ****
constructor TClearTask.Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  m_Map:= AMap;
  RoundsLeft:= 6;
  if target<>nil then
  begin
    target.SetState(usPloughing);
    if target.GetType = utPioneer then RoundsLeft:= 3;
  end;
  if not (AMap.tiles[m_X, m_Y].HasForest) then RoundsLeft:= 0;
end;//func

function TClearTask.Done: Boolean;
begin
  Result:= (RoundsLeft<=0);
end;//func

function TClearTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target.GiveTools(target.GetToolAmount-20);
      m_Map.tiles[m_X, m_Y].ClearForest;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

function TClearTask.GetType: TTaskType;
begin
  Result:= ttClear;
end;//func

// go to task (pathfinding)
constructor TGoToTask.Create(const target_unit: TUnit; ToX, ToY: Byte; const AMap: TMap; const SpecialX: Byte=250; const SpecialY: Byte=250);
begin
  inherited Create(target_unit);
  m_X:= ToX;
  m_Y:= ToY;
  spec_X:= SpecialX;
  spec_Y:= SpecialY;
  m_Map:= AMap;
  SetLength(m_Path, 0);
  if FindPath(target_unit.GetPosX, target_unit.GetPosY, ToX, ToY, target_unit.IsShip, AMap, m_Path, SpecialX, SpecialY) then
  begin
    target_unit.SetState(usGoTo);
  end
  else SetLength(m_Path, 0);
end;//construc

function TGoToTask.Done: Boolean;
begin
  Result:= ((target.GetPosX=m_X) and (target.GetPosY=m_Y)) or (length(m_Path)<1);
end;//func

function TGoToTask.Execute: Boolean;
var direc: TDirection;
    x,y: Byte;
begin
  {$IFDEF DEBUG_CODE}
  WriteLn('GoTo.Execute called. Path len: ', length(m_Path));
  {$ENDIF}
  Result:= True;
  while (target.MovesLeft>0) and (length(m_Path)>0) do
  begin
    x:= m_Path[High(m_Path)].x;
    y:= m_Path[High(m_Path)].y;
    direc:= GetApplyingDirection(target.GetPosX, target.GetPosY, x,y);

    //debug only
    {$IFDEF DEBUG_CODE}
    WriteDebugLn('-GoTo.Execute:');
    WriteLn('-- from: ',target.GetPosX,',',target.GetPosY,'  to: ',x,',',y);
    WriteLn('-- apply dir.: ', Ord(direc));
    {$ENDIF}
    //end debug

    target.Move(direc, m_Map, nil);
    if (target.GetPosX<>x) or (target.GetPosY<>y) then
    begin
      //check for special location
      if ((x=spec_X) and (y=spec_Y)) then
      begin
        target.WarpToXY(spec_X, spec_Y, m_Map);
        target.MovesLeft:= target.MovesLeft-1;
        if target.MovesLeft<0 then target.MovesLeft:=0;
      end
      else begin
        //something went wrong here, abort the whole task
        SetLength(m_Path, 0);
        Result:= False;
        //debug only
        WriteLn('-- direction application failed!');
        //end debug
        Exit;
      end;//else
    end//if
    else SetLength(m_Path, length(m_Path)-1); //remove last waypoint
  end;//while
end;//func

function TGoToTask.DestinationX: Byte;
begin
  Result:= m_X;
end;//func

function TGoToTask.DestinationY: Byte;
begin
  Result:= m_Y;
end;//func

destructor TGoToTask.Destroy;
begin
  SetLength(m_Path, 0);
  target.SetState(usNormal);
  inherited Destroy;
end;//destruc

function TGoToTask.GetType: TTaskType;
begin
  Result:= ttGoTo;
end;//func

// **** TGoToEuropeTask methods ****

constructor TGoToEuropeTask.Create(const target_unit: TUnit; ToX, ToY: Byte; const AMap: TMap);
begin
  inherited Create(target_unit, ToX, ToY, AMap);
end;//construc

destructor TGoToEuropeTask.Destroy;
begin
  inherited Destroy;
end;//destruc

function TGoToEuropeTask.Done: Boolean;
begin
  Result:= (inherited Done) and (target.GetLocation=ulEurope);
end;//func

function TGoToEuropeTask.Execute: Boolean;
begin
  if (not inherited Done) then
  begin
    inherited Execute;
    Result:= true;
  end
  else begin
    if target.GetLocation=ulAmerica then
    begin
      if (m_Map.tiles[target.GetPosX, target.GetPosY].GetType=ttOpenSea) then
        target.SendToEurope;
      Result:= true;
    end//if
    else Result:= target.GetLocation=ulGoToEurope;
  end;//else
end;//func

function TGoToEuropeTask.GetType: TTaskType;
begin
  Result:= ttGoToEurope;
end;//func

// **** TFindLandForColonyTask methods ****

constructor TFindLandForColonyTask.Create(const target_unit: TUnit; const AMap: TMap; const dat: Pointer);
var grid: TColonySiteEvaluationGrid;
    i, step: Integer;
    found: Boolean;
    target_coords_x, target_coords_y: Integer;
    path: TCoordArr;
begin
  target:= target_unit;
  m_Map:= AMap;
  m_Data:= dat;
  //get grid
  grid:= TData(m_Data).GetColonySiteEvaluation(true);
  //find best tile
  found:= false;
  step:= 1;
  while not found and (step<cMap_Y) do begin
    //check top row
    for i:= target_unit.GetPosX-step to target_unit.GetPosX+step do
      if m_Map.IsValidMapPosition(i, target_unit.GetPosY-step) then
        //check, if it has a high enough value
        if grid[i, target_unit.GetPosY-step]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, i, target_unit.GetPosY-step,
                      true, m_Map, path, i, target_unit.GetPosY-step) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= i;
            target_coords_y:= target_unit.GetPosY-step;
            break;
          end;//if
    if found then break;
    //check bottom row
    for i:= target_unit.GetPosX-step to target_unit.GetPosX+step do
      if m_Map.IsValidMapPosition(i, target_unit.GetPosY+step) then
        //check, if it has a high enough value
        if grid[i, target_unit.GetPosY+step]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, i, target_unit.GetPosY+step,
                      true, m_Map, path, i, target_unit.GetPosY+step) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= i;
            target_coords_y:= target_unit.GetPosY+step;
            break;
          end;//if
    if found then break;
    //check left row
    for i:= target_unit.GetPosY-step+1 to target_unit.GetPosY+step-1 do
      if m_Map.IsValidMapPosition(target_unit.GetPosX-step, i) then
        //check, if it has a high enough value
        if grid[target_unit.GetPosX-step, i]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, target_unit.GetPosX-step, i,
                      true, m_Map, path, target_unit.GetPosX-step, i) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= target_unit.GetPosX-step;
            target_coords_y:= i;
            break;
          end;//if
    if found then break;
    //check right row
    for i:= target_unit.GetPosY-step+1 to target_unit.GetPosY+step-1 do
      if m_Map.IsValidMapPosition(target_unit.GetPosX+step, i) then
        //check, if it has a high enough value
        if grid[target_unit.GetPosX+step, i]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, target_unit.GetPosX+step, i,
                      true, m_Map, path, target_unit.GetPosX+step, i) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= target_unit.GetPosX+step;
            target_coords_y:= i;
            break;
          end;//if
    step:= step+1;
  end;//while

  // -- set data for goto task
  //found something?
  if found then
  begin
    m_X:= target_coords_x;
    m_Y:= target_coords_y;
    m_Path:= path;
    m_BuildWhenDone:= true;
  end//if
  else begin
    //nothing found
    m_X:= target_unit.GetPosX;
    m_Y:= target_unit.GetPosY;
    SetLength(m_Path, 0);
    m_BuildWhenDone:= false;
  end;//else
  spec_X:= target_coords_x;
  spec_Y:= target_coords_y;
end;//construc

destructor TFindLandForColonyTask.Destroy;
begin
  m_Map:= nil;
  inherited Destroy;
end;

function TFindLandForColonyTask.Done: Boolean;
begin
  if ((not target.IsShip) or (target.EmbarkedPassengers=0)) then Result:= true
  else begin
      Result:= (inherited Done) or not m_BuildWhenDone; //We are done, if the
      //GoToTask is done or no colony will be build anyway.
  end;//else
end;//func

function TFindLandForColonyTask.Execute: Boolean;
var tries: LongInt;
    founder: TUnit;
begin
  if (target.EmbarkedPassengers=0) then
  begin
    Result:= false;
    exit;
  end;//if
  //execute the GoToTask
  Result:= inherited Execute;
  //Are we at the destination?
  if (target.GetPosX=m_X) and (target.GetPosY=m_Y) then
  begin
    founder:= target.GetFirstEmbarkedPassenger;
    if (target.UnloadUnit(founder.GetType, m_X, m_Y, m_Map)) then
    begin
      //build new colony and set the founder into the upper, left field
      (TData(m_Data).NewColony(m_X, m_Y, founder.GetNation,
        TData(m_Data).GetLang.GetColonyNames(founder.GetNation,
        length(TData(m_Data).GetColonyList(founder.GetNation))))).SetUnitInField(-1, -1, founder);
      //create road in colony square
      m_Map.tiles[m_X,m_Y].CreateRoad;
      //try to unload other units, too
      tries:= 0;
      while (target.GetFirstEmbarkedPassenger<>nil) and (tries<6) do
      begin
        target.UnloadUnit(target.GetFirstEmbarkedPassenger.GetType, m_X, m_Y, m_Map);
        tries:= tries +1;
      end;//while
      Result:= true;
      exit;
    end;//if
  end;//if at destination location
  Result:= true;
end;//func

function TFindLandForColonyTask.GetType: TTaskType;
begin
  Result:= ttFindLand;
end;//func


//general
function GetUnitForGood(const AGood: TGoodType): TUnitType;
begin
  case AGood of
    gtFood: Result:= utFarmer;
    gtSugar: Result:= utSugarplanter;
    gtTobacco: Result:= utTobaccoplanter;
    gtCotton: Result:= utCottonplanter;
    gtFur: Result:= utFurHunter;
    gtWood: Result:= utWoodcutter;
    gtOre: Result:= utOreMiner;
    gtSilver: Result:= utSilverMiner;
    //gtHorses: none
    gtRum: Result:= utDistiller;
    gtCigar: Result:= utTobacconist;
    gtCloth: Result:= utWeaver;
    gtCoat: Result:= utFurTrader;
    //gtTradegoods: none
    gtTool: Result:= utSmith;
    gtMusket: Result:= utWeaponSmith;
    gtHammer: Result:= utCarpenter;
    gtLibertyBell: Result:= utStatesman;
    gtCross: Result:= utPreacher;
  else
    Result:= utCriminal;
  end;//case
end;//func

function HasExpertStatus(const AGood: TGoodType; const ut: TUnitType): Boolean;
begin
  Result:= ((GetUnitForGood(AGood)=ut) and (ut<>utCriminal));
end;//func

end.
