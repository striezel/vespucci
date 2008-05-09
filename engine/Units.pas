unit Units;

interface

uses
  Nation;

type
  TUnitType = (utCriminal, utColonist,
               utFarmer, utFisher, utFurHunter, utSilverMiner, utWoodcutter,
               utOreMiner, utSugarplanter, utCottonplanter, utTobaccoplanter,

               utPreacher, utStatesman,

               utCarpenter, utDistiller, utWeaver, utTobacconist, utFurTrader,
               utSmith, utWeaponSmith,

               utScout, utPioneer, utMissionary,

               utRegular, utDragoon,
               utCaravel, utTradingShip, utGalleon, utPrivateer, utFrigate,
               utMan_o_War,

               utBrave, utBraveOnHorse);
  TDirection = (dirSW, dirS, dirSE, dirE, dirNE, dirN, dirNW, dirW, dirNone);

  TUnit = class
    public
      MovesLeft: Integer;
      constructor Create(const TypeOfUnit: TUnitType; X: Integer=1; Y: Integer=1);
      destructor Destroy;
      function Move(const direction: TDirection): Boolean;
      function GetPosX: Integer;
      function GetPosY: Integer;
      function IsShip: Boolean;
      function MovesPerRound: Integer;
    private
      PosX, PosY: Integer;
      UnitType: TUnitType;
      Nation: PNation;
      //AI_Task: TAI_Task;
  end;//class TUnit
  PUnit = ^TUnit;

implementation

constructor TUnit.Create(const TypeOfUnit: TUnitType; X: Integer=1; Y: Integer=1);
begin
  UnitType:= TypeOfUnit;
  PosX:= X;
  PosY:= Y;
  MovesLeft:= MovesPerRound;
end;//construc

destructor TUnit.Destroy;
begin

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

function TUnit.IsShip: Boolean;
begin
  Result:= (UnitType in [utCaravel, utTradingShip, utGalleon, utPrivateer,
                        utFrigate, utMan_o_War]);
end;//func

function TUnit.MovesPerRound: Integer;
begin
  case UnitType of
    utScout, utDragoon: Result:= 4;
    utMissionary: Result:= 2;
    utCaravel: Result:= 4;
    utTradingShip: Result:= 5;
    utGalleaon: Result:= 6;
    utPrivateer: Result:= 8;
    utFrigate: Result:= 6;
    utMan_o_War: Result:= 5;
    utBraveOnHorse: Result:= 4;
  else Result:= 1;
  end;//case
end;//func

end.