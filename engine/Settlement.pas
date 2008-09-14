unit Settlement;

interface

uses
  Nation;

type
  TSettlement = class
    public
      constructor Create(const X, Y: Integer; const ANation: TNation);
      destructor Destroy;
      function GetNation: TNation;
      function GetPosX: Integer;
      function GetPosY: Integer;
    protected
      m_Nation: TNation;
      PosX, PosY: Integer;
  end;//class
  PSettlement = ^TSettlement;

implementation

// **** TSettlement functions ****

constructor TSettlement.Create(const X, Y: Integer; const ANation: TNation);
begin
  //no settlements outside of range or at border row/column (index: 0) allowed
  if X>0 then PosX:= X else PosX:= 1;
  if Y>0 then PosY:= Y else PosY:= 1;
  m_Nation:= ANation;
end;

destructor TSettlement.Destroy;
begin
  inherited Destroy;
end;//destruc

function TSettlement.GetNation: TNation;
begin
  Result:= m_Nation;
end;//func

function TSettlement.GetPosX: Integer;
begin
  Result:= PosX;
end;//func

function TSettlement.GetPosY: Integer;
begin
  Result:= PosY;
end;//func

end.