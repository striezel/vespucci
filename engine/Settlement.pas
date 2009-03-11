unit Settlement;

interface

type
  TSettlement = class
    public
      constructor Create(const X, Y: Integer; const ANation: LongInt);
      destructor Destroy;
      function GetNation: LongInt;
      procedure ChangeNation(const new_nation: LongInt);
      function GetPosX: Integer;
      function GetPosY: Integer;
      procedure SetPosition(const x,y: LongInt);
    protected
      m_Nation: LongInt;
      PosX, PosY: LongInt;
  end;//class
  PSettlement = ^TSettlement;

implementation

// **** TSettlement functions ****

constructor TSettlement.Create(const X, Y: Integer; const ANation: LongInt);
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

function TSettlement.GetNation: LongInt;
begin
  Result:= m_Nation;
end;//func

procedure TSettlement.ChangeNation(const new_nation: LongInt);
begin
  if new_nation>=0 then m_Nation:= new_nation;
end;//proc

function TSettlement.GetPosX: LongInt;
begin
  Result:= PosX;
end;//func

function TSettlement.GetPosY: LongInt;
begin
  Result:= PosY;
end;//func

procedure TSettlement.SetPosition(const x,y: LongInt);
begin
  if x>0 then PosX:= x else PosX:= 1;
  if y>0 then PosY:= y else PosY:= 1;
end;//proc

end.