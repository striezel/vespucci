unit Nation;

interface

uses
  Goods;

const
  cMin_Nations = 1;
  cMax_Nations = 4;

  cNationEngland = 1;
  cNationFrance = 2;
  cNationSpain = 3;
  cNationHolland = 4;

type
  TNation = class
    private
      //Name of nation's leader
      m_Leader: string;
      //amount of gold owned by this nation
      m_Gold: Integer;
      //tax rate (in %) for this nation
      m_TaxRate: Byte; //can't be more than 100% ;) so a Byte will do
      m_Boycotted: array [TGoodType] of Boolean;
      m_Prices: array [TGoodType] of Byte;
    public
      constructor Create(const NameOfLeader: string);
      destructor Destroy;
      //tax rate for this nation in percent
      function GetTaxRate: Byte;
      procedure IncreaseTax(const AddedPercentage: Byte);

      function IsBoycotted(const AGood: TGoodType): Boolean;
      procedure DoBoycott(const AGood: TGoodType);
      procedure UndoBoycott(const AGood: TGoodType);

      function GetPrice(const AGood: TGoodType; low: Boolean): Byte;
      //increases price if good, if below upper limit
      procedure AdvancePrice(const AGood: TGoodType);
      //decrease price, if above lower limit
      procedure DropPrice(const AGood: TGoodType);
  end;//class
  PNation = ^TNation;

implementation

constructor TNation.Create(const NameOfLeader: string);
var gt: TGoodType;
begin
  inherited Create;
  m_Leader:= NameOfLeader;
  m_Gold:= 1000;
  m_TaxRate:= 0;

  Randomize;

  gt:= Low(gtFood);
  while gt<High(TGoodType) do
  begin
    m_Boycotted[gt]:= False;//no boycotts at beginning
    m_Prices[gt]:= cGoodPrices[gt].start_min+ Random(cGoodPrices[gt].start_max-cGoodPrices[gt].start_min);
    gt:= Succ(gt);
  end;
  m_Boycotted[High(TGoodType)]:= False;
end;//construc

destructor TNation.Destroy;
begin
  inherited Destroy;
end;//destruc

function TNation.GetTaxRate: Byte;
begin
  Result:= m_TaxRate;
end;//func

procedure TNation.IncreaseTax(const AddedPercentage: Byte);
begin
  if (AddedPercentage<100) and (m_TaxRate+AddedPercentage<100) then
    m_TaxRate:= m_TaxRate + AddedPercentage;
end;//proc

function TNation.IsBoycotted(const AGood: TGoodType): Boolean;
begin
  Result:= m_Boycotted[AGood];
end;//func

procedure TNation.DoBoycott(const AGood: TGoodType);
begin
  if not(AGood in [gtLibertyBell, gtHammer]) then
    m_Boycotted[AGood]:= True;
end;//proc

procedure TNation.UndoBoycott(const AGood: TGoodType);
begin
  m_Boycotted[AGood]:= False;
end;//proc

function TNation.GetPrice(const AGood: TGoodType; low: Boolean): Byte;
begin
  if low then Result:= m_Prices[AGood]
    else Result:= m_Prices[AGood]+cGoodPrices[AGood].diff;
end;//func

procedure TNation.AdvancePrice(const AGood: TGoodType);
begin
  if m_Prices[AGood]+cGoodPrices[AGood].diff<cGoodPrices[AGood].max then
  begin
    m_Prices[AGood]:= m_Prices[AGood]+1;
    //should display message to player
  end;//if
end;//proc

procedure TNation.DropPrice(const AGood: TGoodType);
begin
  if m_Prices[AGood]>cGoodPrices[AGood].min then
  begin
    m_Prices[AGood]:= m_Prices[AGood]-1;
    //should display a message to the player
  end;//if
end;//proc



end.