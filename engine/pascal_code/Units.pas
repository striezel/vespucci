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
  TUnitLocation = (ulAmerica, ulInColony, ulGoToEurope, ulGoToNewWorld, ulEurope, ulEmbarked);
  TUnitState = (usNormal,{-} usFortified,{F} usWaitingForShip{S}, usGoTo{G}, usPloughing{P}, usCreateRoad{R});
  TDirection = (dirSW, dirS, dirSE, dirE, dirNE, dirN, dirNW, dirW, dirNone);

const
  cShipPrices: array [utCaravel..utFrigate] of Integer
                =( 1000, 2000, 3000, 2000, 5000);
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
  TTask = class;
  TUnit = class
    public
      MovesLeft: Integer;
      constructor Create(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1);
      destructor Destroy; override;
      procedure NewRound;
      function Move(const direction: TDirection; const AMap: TMap): Boolean;
      function WarpToXY(const x, y: Byte; AMap: TMap): Boolean;
      function GetPosX: LongInt;
      function GetPosY: LongInt;
      function GetNation: LongInt;
      procedure ChangeNation(const new_nation: LongInt);
      function GetType: TUnitType;
      procedure ChangeType(const newType: TUnitType);
      function GetLocation: TUnitLocation;
      procedure SetLocation(const loc: TUnitLocation);
      function GetState: TUnitState;
      procedure SetState(const state: TUnitState);
      function GetRoundsInOpenSea: Byte;
      procedure SetRoundsInOpenSea(const rounds_left: Byte);
      function IsShip: Boolean;
      function MovesPerRound: Integer;
      function AttackStrength: Integer;
      function GetTask: TTask;
      procedure SetTask(const new_task: TTask; const ImmediateExecute: Boolean=True);
      //go across the big pond
      function SendToEurope: Boolean;
      function SendToNewWorld: Boolean;
      function CallBackToEurope: Boolean;
      function CallBackToNewWorld: Boolean;
      //functions for loading/ unloading freigth or passengers and checking freigth status
      function FreightCapacity: Byte;
      function FreeCapacity: Byte;
      function EmbarkedPassengers: Byte;
      function GetFirstEmbarkedPassenger: TUnit;
      function GetPassengerBySlot(const slot: Byte): TUnit;
      function LoadGood(const AGood: TGoodType; const num: Byte): Boolean;
      function UnloadGood(const AGood: TGoodType; const num: Byte): Byte;
      function LoadUnit(AUnit: TUnit): Boolean;
      function UnloadUnit(const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;
      procedure DropAllPassengers;
      //item functions
      function GetToolAmount: Byte;
      procedure GiveTools(const amount: Byte);
      function HasHorses: Boolean;
      procedure GiveHorses(const has: Boolean = True);
      function HasMuskets: Boolean;
      procedure GiveMuskets(const has: Boolean = True);
      procedure ChangeAllItems(const new_items: Byte);
      function SaveToStream(var fs: TFileStream): Boolean;
      procedure SetCargo(const slot: Byte; const new_amount: Byte; const AGood: TGoodType);
      function GetCargoAmountBySlot(const slot: Byte): Byte;
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

  TUnitArr = array of TUnit;


  //the AI stuff

  TTaskType = (ttGeneric, ttPlough, ttRoad, ttClear, ttGoTo);
  TTask = class
    protected
      target: TUnit;
    public
      function Done: Boolean; virtual; abstract;
      function Execute: Boolean; virtual; abstract;
      function GetType: TTaskType; virtual;
      constructor Create(const target_unit: TUnit);
      destructor Destroy; override;
  end;//class

  //for the pioneers
  TPloughTask = class(TTask)
    private
      m_X, m_Y: Byte;
      m_Map: TMap;
      RoundsLeft: Byte;
    public
      constructor Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
      function Done: Boolean; override;
      function Execute: Boolean; override;
      function GetType: TTaskType; override;
  end;//class

  TRoadTask = class(TTask)
    private
      m_X, m_Y: Byte;
      m_Map: TMap;
      RoundsLeft: Byte;
    public
      constructor Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
      function Done: Boolean; override;
      function Execute: Boolean; override;
      function GetType: TTaskType; override;
  end;//class

  TClearTask = class(TTask)
    private
      m_X, m_Y: Byte;
      m_Map: TMap;
      RoundsLeft: Byte;
    public
      constructor Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
      function Done: Boolean; override;
      function Execute: Boolean; override;
      function GetType: TTaskType; override;
  end;//class

  TGoToTask = class(TTask)
    protected
      m_X, m_Y: Byte;//destination location
      spec_X, spec_Y: Byte; //location of special field
      m_Map: TMap;
      m_Path: TCoordArr;
    public
      constructor Create(const target_unit: TUnit; ToX, ToY: Byte; const AMap: TMap; const SpecialX: Byte=250; const SpecialY: Byte=250);
      destructor Destroy; override;
      function Done: Boolean; override;
      function Execute: Boolean; override;
      function DestinationX: Byte;
      function DestinationY: Byte;
  end;//class

  procedure ApplyDir(var x,y: Byte; const dir: TDirection);
  function GetApplyingDirection(const from_x, from_y, to_x, to_y: Byte): TDirection;
  function GetUnitForGood(const AGood: TGoodType): TUnitType;
  function HasExpertStatus(const AGood: TGoodType; const ut: TUnitType): Boolean;

implementation

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
    WriteLn('New Round: Exec calling');
    AI_Task.Execute;

    if AI_Task.Done then
    begin
      AI_Task.Destroy;
      AI_Task:= nil;
    end;
  end;
end;//proc

function TUnit.Move(const direction: TDirection; const AMap: TMap): Boolean;
var newX, newY: Integer;
    allow: Boolean;
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
  end//if
  else begin
    items:= (items and (not UNIT_HORSE_BIT));
    if UnitType=utDragoon then UnitType:= utRegular;
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
  WriteLn('GoTo.Execute called. Path len: ', length(m_Path));
  Result:= True;
  while (target.MovesLeft>0) and (length(m_Path)>0) do
  begin
    x:= m_Path[High(m_Path)].x;
    y:= m_Path[High(m_Path)].y;
    direc:= GetApplyingDirection(target.GetPosX, target.GetPosY, x,y);

    //debug only
    WriteLn('-GoTo.Execute:');
    WriteLn('-- from: ',target.GetPosX,',',target.GetPosY,'  to: ',x,',',y);
    WriteLn('-- apply dir.: ', Ord(direc));
    //end debug

    target.Move(direc, m_Map);
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