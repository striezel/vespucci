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
  //no settlements outside of range or at border row/column (index: 0) allowed
  if X>0 then PosX:= X else PosX:= 1;
  if Y>0 then PosY:= Y else PosY:= 1;
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