unit Settlement;

interface

uses
  Nation;

type
  TSettlement = class
    public
      constructor Create(const X, Y: Integer; const ANation: PNation);
      destructor Destroy;
      function GetPosX: Integer;
      function GetPosY: Integer;
    private
      Nation: PNation;
      PosX, PosY: Integer;
  end;//class
  PSettlement = ^TSettlement;

implementation

// **** TSettlement functions ****

constructor TSettlement.Create(const X, Y: Integer; const ANation: PNation);
begin
  if X>=0 then PosX:= X else PosX:= 0;
  if Y>=0 then PosY:= Y else PosY:= 0;
  Nation:= ANation;
end;

destructor TSettlement.Destroy;
begin
  inherited Destroy;
end;//destruc

function TSettlement.GetPosX: Integer;
begin
  Result:= PosX;
end;//func

function TSettlement.GetPosY: Integer;
begin
  Result:= PosY;
end;//func

end.