unit Units;

interface

uses
  Nation, Goods, Map;

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
  TDirection = (dirSW, dirS, dirSE, dirE, dirNE, dirN, dirNW, dirW, dirNone);
  
  TTask = class;

  PUnit = ^TUnit;
  TUnit = class
    public
      MovesLeft: Integer;
      constructor Create(const TypeOfUnit: TUnitType; X: Integer=1; Y: Integer=1);
      destructor Destroy;
      function Move(const direction: TDirection): Boolean;
      function GetPosX: Integer;
      function GetPosY: Integer;
      function GetNation: PNation;
      function GetType: TUnitType;
      procedure ChangeType(const newType: TUnitType);
      function IsShip: Boolean;
      function MovesPerRound: Integer;
      function AttackStrength: Integer;
      //functions for loading/ unloading freigth or passengers and checking freigth status
      function FreightCapacity: Byte;
      function FreeCapacity: Byte;
      function LoadGood(const AGood: TGoodType; const num: Byte): Boolean;
      function UnloadGood(const AGood: TGoodType; const num: Byte): Byte;
      function LoadUnit(AUnit: PUnit): Boolean;
      //item functions
      function GetToolAmount: Byte;
      procedure GiveTools(const amount: Byte);
      function HasHorses: Boolean;
      procedure GiveHorses(const has: Boolean = True);
      function HasMuskets: Boolean;
      procedure GiveMuskets(const has: Boolean = True);
    private
      PosX, PosY: Integer;
      UnitType: TUnitType;
      Nation: PNation;
      //stores items like horses, muskets, tools
      items: Byte;
      //stores passengers (on ships)
      passengers: array [0..5] of PUnit;
      //stores cargo (on ships an convoys)
      cargo_load: array [0..5] of record
                                    amount: Byte;
                                    which: TGoodType;
                                  end;//rec
      AI_Task: TTask;
  end;//class TUnit
  
  //the AI stuff
  TTask = class
    protected
      target: PUnit;
    public
      function Done: Boolean; virtual; abstract;
      function Execute: Boolean; virtual; abstract;
      constructor Create(const target_unit: PUnit);
      destructor Destroy;
  end;//class
  PTask = ^TTask;
  
  //for the pioneers
  TPloughTask = class(TTask)
    private
      m_X, m_Y: Byte;
      ptrMap: PMap;
      RoundsLeft: Byte;
    public
      constructor Create(const target_unit: PUnit; X, Y: Byte; const AMap: PMap);
      function Done: Boolean;
      function Execute: Boolean;
  end;//class
  
  TRoadTask = class(TTask)
    private
      m_X, m_Y: Byte;
      ptrMap: PMap;
      RoundsLeft: Byte;
    public
      constructor Create(const target_unit: PUnit; X, Y: Byte; const AMap: PMap);
      function Done: Boolean;
      function Execute: Boolean;
  end;//class
  
  TClearTask = class(TTask)
    private
      m_X, m_Y: Byte;
      ptrMap: PMap;
      RoundsLeft: Byte;
    public
      constructor Create(const target_unit: PUnit; X, Y: Byte; const AMap: PMap);
      function Done: Boolean;
      function Execute: Boolean;
  end;//class

implementation

constructor TUnit.Create(const TypeOfUnit: TUnitType; X: Integer=1; Y: Integer=1);
var i: Integer;
begin
  UnitType:= TypeOfUnit;
  PosX:= X;
  PosY:= Y;
  MovesLeft:= MovesPerRound;
  Nation:= nil;
  items:= 0;
  AI_Task:= nil;
  if TypeOfUnit = utPioneer then GiveTools(100)
  else if (TypeOfUnit in [utRegular, utDragoon]) then GiveMuskets(True)
  else if (TypeOfUnit in [utScout, utDragoon, utBraveOnHorse]) then GiveHorses(True);
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

function TUnit.Move(const direction: TDirection): Boolean;
var newX, newY: Integer;
begin
  if MovesLeft = 0 then
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
    //check if we are out of map
    if (newX<0) or (newY<0) then
      Result:= False
    else begin
      if direction<>dirNone then
      begin
        MovesLeft:= MovesLeft -1;
        PosX:= newX;
        PosY:= newY;
      end;//if
      Result:= True;
    end;//else
  end;//else
end;//func

function TUnit.GetPosX: Integer;
begin
  Result:= PosX;
end;

function TUnit.GetPosY: Integer;
begin
  Result:= PosY;
end;

function TUnit.GetNation: PNation;
begin
  Result:= Nation;
end;//func

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
    if FreightCapacity<occupied then Result:= 0
    else Result:= FreightCapacity - occupied;
  end//else
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
        if cargo_load[slot].amount<num then cap:= cargo_load[slot].amount
        else cap:= num;
        Result:= Result+cap;
        cargo_load[slot].amount:= cargo_load[slot].amount - cap;
      end;//if
      slot:= slot-1;
    end;//while
  end;//if
end;//func

//tries to load unit 'AUnit' and returns True on success
function TUnit.LoadUnit(AUnit: PUnit): Boolean;
var slot: Byte;
begin
  if (FreeCapacity=0) or (AUnit=nil) or (UnitType=utConvoy) then Result:= False
  else if (AUnit^.FreightCapacity>0) then Result:= False //no ships or convoys
  else begin
    slot:= 0;
    while (passengers[slot]<>nil) and (slot<5) do
      slot:= slot+1;
    passengers[slot]:= AUnit;
    Result:= True;
  end;//else
end;//func

function TUnit.GetToolAmount: Byte;
begin
  Result:= (items and UNIT_TOOL_MASK)*5;
end;//func

procedure TUnit.GiveTools(const amount: Byte);
var temp: Byte;
begin
  temp:= amount div 20;
  if temp>5 then temp:=5;
  items:= (items or temp);
end;//proc

function TUnit.HasHorses: Boolean;
begin
  Result:= (items and UNIT_HORSE_BIT)>0;
end;//func

procedure TUnit.GiveHorses(const has: Boolean = True);
begin
  if has then items:= (items or UNIT_HORSE_BIT)
  else items:= (items and (not UNIT_HORSE_BIT));
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

//**** AI-related functions ****

constructor TTask.Create(const target_unit: PUnit);
begin
  inherited Create;
  target:= target_unit;
end;//construc

destructor TTask.Destroy;
begin
  inherited Destroy;
end;//destruc

//**** TPloughTask methods ****

constructor TPloughTask.Create(const target_unit: PUnit; X, Y: Byte; const AMap: PMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  ptrMap:= AMap;
  RoundsLeft:= 4;
  if target<>nil then
  begin
    if target^.GetType = utPioneer then RoundsLeft:= 2;
  end;
  if AMap^.tiles[m_X, m_Y].IsPloughed then RoundsLeft:= 0;
end;//func

function TPloughTask.Done: Boolean;
begin
  Result:= (RoundsLeft=0);
end;//func

function TPloughTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target^.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target^.GiveTools(target^.GetToolAmount-20);
      ptrMap^.tiles[m_X, m_Y].Plough;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

// **** TRoadTask methods ****
constructor TRoadTask.Create(const target_unit: PUnit; X, Y: Byte; const AMap: PMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  ptrMap:= AMap;
  RoundsLeft:= 2;
  if target<>nil then
  begin
    if target^.GetType = utPioneer then RoundsLeft:= 1;
  end;
  if AMap^.tiles[m_X, m_Y].HasRoad then RoundsLeft:= 0;
end;//func

function TRoadTask.Done: Boolean;
begin
  Result:= (RoundsLeft=0);
end;//func

function TRoadTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target^.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target^.GiveTools(target^.GetToolAmount-20);
      ptrMap^.tiles[m_X, m_Y].CreateRoad;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

// **** TClearTask methods ****
constructor TClearTask.Create(const target_unit: PUnit; X, Y: Byte; const AMap: PMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  ptrMap:= AMap;
  RoundsLeft:= 6;
  if target<>nil then
  begin
    if target^.GetType = utPioneer then RoundsLeft:= 3;
  end;
  if not (AMap^.tiles[m_X, m_Y].HasForest) then RoundsLeft:= 0;
end;//func

function TClearTask.Done: Boolean;
begin
  Result:= (RoundsLeft=0);
end;//func

function TClearTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target^.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target^.GiveTools(target^.GetToolAmount-20);
      ptrMap^.tiles[m_X, m_Y].ClearForest;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

end.