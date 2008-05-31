unit Nation;

interface

uses
  Goods;

const
  cMin_Nations = 1;
  cMax_Nations = 4;
  cMaxEuropean = 4;
  cMinIndian = 5;
  cMaxIndian = 12;

  //Europeans
  cNationEngland = 1;
  cNationFrance = 2;
  cNationSpain = 3;
  cNationHolland = 4;
  //Indians
  cNationArawak = 5;
  cNationAztec = 6;
  cNationInca = 7;
  cNationTupi = 8;
  cNationCherokee = 9;
  cNationIroquois = 10;
  cNationSioux = 11;
  cNationApache = 12;

type

  TNation = class
    private
      m_count: Integer;//not really sure, if we will need this later :|
      m_NameStr: string;
    public
      constructor Create(const num: Integer; const NameStr: string);
      destructor Destroy;
      function IsIndian: Boolean; virtual; abstract;
      function IsEuropean: Boolean; virtual; abstract;
      function GetCount: Integer;
      function GetName: string;
  end;//class
  PNation = ^TNation;

  TIndianNation = class(TNation)
    public
      constructor Create(const num: Integer; const NameStr: string);
      function IsIndian: Boolean; override;
      function IsEuropean: Boolean; override;
  end;//class
  PIndianNation = ^TIndianNation;

  TEuropeanNation = class(TNation)
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
      constructor Create(const num: Integer; const NameStr: string;
                         const NameOfLeader: string);
      destructor Destroy;

      function IsIndian: Boolean; override;
      function IsEuropean: Boolean; override;

      //tax rate for this nation in percent
      function GetTaxRate: Byte;
      procedure IncreaseTax(const AddedPercentage: Byte);

      function IsBoycotted(const AGood: TGoodType): Boolean;
      procedure DoBoycott(const AGood: TGoodType);
      procedure UndoBoycott(const AGood: TGoodType);

      function GetGold: Integer;
      function GetPrice(const AGood: TGoodType; low: Boolean): Byte;
      //increases price if good, if below upper limit
      procedure AdvancePrice(const AGood: TGoodType);
      //decrease price, if above lower limit
      procedure DropPrice(const AGood: TGoodType);
  end;//class
  PEuropeanNation = ^TEuropeanNation;

implementation

//**** functions of TNation ****

constructor TNation.Create(const num: Integer; const NameStr: string);
begin
  m_count:= num;
  m_NameStr:= NameStr;
end;//construc

destructor TNation.Destroy;
begin
  inherited Destroy;
end;//destructor

function TNation.GetName: string;
begin
  Result:= m_NameStr;
end;//func

function TNation.GetCount: Integer;
begin
  Result:= m_count;
end;//func


// **** functions of TIndianNation ****

constructor TIndianNation.Create(const num: Integer; const NameStr: string);
begin
  inherited Create(num, NameStr);
  //check number and pick a default in case of invalidity
  if ((m_count<cMinIndian) or (m_count>cMaxIndian)) then
    m_count:= cNationArawak;
end;//construc

function TIndianNation.IsIndian: Boolean;
begin
  Result:= True;
end;//func

function TIndianNation.IsEuropean: Boolean;
begin
  Result:= False;
end;//func


//**** functions of TEuropeanNation ****

constructor TEuropeanNation.Create(const num: Integer; const NameStr: string;
                                   const NameOfLeader: string);
var gt: TGoodType;
begin
  inherited Create(num, NameStr);
  //check number and pick some default value to make sure it's European
  if ((num<cMin_Nations) or (num>cMaxEuropean)) then m_Count:= cNationEngland;
  m_Leader:= NameOfLeader;
  m_Gold:= 1000;
  m_TaxRate:= 0;
  Randomize;
  gt:= Low(gtFood);
  while gt<High(TGoodType) do
  begin
    m_Boycotted[gt]:= False;//no boycotts at beginning
    m_Prices[gt]:= cGoodPrices[gt].start_min+ Random(cGoodPrices[gt].start_max-cGoodPrices[gt].start_min+1);
    gt:= Succ(gt);
  end;
  m_Boycotted[High(TGoodType)]:= False;
end;//construc

destructor TEuropeanNation.Destroy;
begin
  inherited Destroy;
end;//destruc

function TEuropeanNation.IsIndian: Boolean;
begin
  Result:= False;
end;//func

function TEuropeanNation.IsEuropean: Boolean;
begin
  Result:= True;
end;//func

function TEuropeanNation.GetTaxRate: Byte;
begin
  Result:= m_TaxRate;
end;//func

procedure TEuropeanNation.IncreaseTax(const AddedPercentage: Byte);
begin
  if (AddedPercentage<100) and (m_TaxRate+AddedPercentage<100) then
    m_TaxRate:= m_TaxRate + AddedPercentage;
end;//proc

function TEuropeanNation.IsBoycotted(const AGood: TGoodType): Boolean;
begin
  Result:= m_Boycotted[AGood];
end;//func

procedure TEuropeanNation.DoBoycott(const AGood: TGoodType);
begin
  if not(AGood in [gtLibertyBell, gtHammer]) then
    m_Boycotted[AGood]:= True;
end;//proc

procedure TEuropeanNation.UndoBoycott(const AGood: TGoodType);
begin
  m_Boycotted[AGood]:= False;
end;//proc

function TEuropeanNation.GetGold: Integer;
begin
  Result:= m_Gold;
end;//func

function TEuropeanNation.GetPrice(const AGood: TGoodType; low: Boolean): Byte;
begin
  if low then Result:= m_Prices[AGood]
    else Result:= m_Prices[AGood]+cGoodPrices[AGood].diff;
end;//func

procedure TEuropeanNation.AdvancePrice(const AGood: TGoodType);
begin
  if m_Prices[AGood]+cGoodPrices[AGood].diff<cGoodPrices[AGood].max then
  begin
    m_Prices[AGood]:= m_Prices[AGood]+1;
    //should display message to player
  end;//if
end;//proc

procedure TEuropeanNation.DropPrice(const AGood: TGoodType);
begin
  if m_Prices[AGood]>cGoodPrices[AGood].min then
  begin
    m_Prices[AGood]:= m_Prices[AGood]-1;
    //should display a message to the player
  end;//if
end;//proc

end.