unit Nation;

interface

uses
  Goods, Classes;

const
  cMin_Nations = 1;
  cMinEuropean = 1;
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

  //the colours
  cNationColours: array[cMin_Nations..cMaxIndian] of array[0..2] of Byte
                =( //europeans
                   (255, 0, 0), //England
                   (50, 50, 255), //France
                   (255, 255, 0), //Spain
                   (255, 128, 0), //Holland
                   //indians
                   (100, 140, 190), //Arawak
                   (200, 160, 30), //Aztec
                   (240, 240, 200), //Inca
                   (0, 100, 0), //Tupi
                   (120, 160, 80), //Cherokee
                   (110, 60, 25), //Iroquois
                   (140, 0, 0), //Sioux
                   (190, 170, 130) //Apache
                 );

type

  TNation = class
    private
      m_count: LongInt;
      m_NameStr: string;
    public
      constructor Create(const num: LongInt; const NameStr: string);
      destructor Destroy;
      function IsIndian: Boolean; virtual; abstract;
      function IsEuropean: Boolean; virtual; abstract;
      function GetCount: LongInt;
      procedure ChangeCount(const new_num: LongInt);
      function GetName: string;
      procedure ChangeName(var new_name: string);
      function SaveToStream(var fs: TFileStream): Boolean; virtual;
  end;//class
  PNation = ^TNation;

  TIndianNation = class(TNation)
    public
      constructor Create(const num: LongInt; const NameStr: string);
      function IsIndian: Boolean; override;
      function IsEuropean: Boolean; override;
  end;//class
  PIndianNation = ^TIndianNation;

  TEuropeanNation = class(TNation)
    private
      //Name of nation's leader
      m_Leader: string;
      //amount of gold owned by this nation
      m_Gold: LongInt;
      //tax rate (in %) for this nation
      m_TaxRate: Byte; //can't be more than 100% ;) so a Byte will do
      m_Boycotted: array [TGoodType] of Boolean;
      m_Prices: array [TGoodType] of Byte;
    public
      constructor Create(const num: LongInt; const NameStr: string;
                         const NameOfLeader: string);
      destructor Destroy;

      function IsIndian: Boolean; override;
      function IsEuropean: Boolean; override;
      function GetLeaderName: string;
      procedure ChangeLeaderName(const NameOfLeader: string);

      //tax rate for this nation in percent
      function GetTaxRate: Byte;
      procedure IncreaseTax(const AddedPercentage: Byte);
      //ChangeTaxRate only used during loading; use IncreaseTax on other occassions
      procedure ChangeTaxRate(const NewPercentage: Byte);

      function IsBoycotted(const AGood: TGoodType): Boolean;
      procedure DoBoycott(const AGood: TGoodType);
      procedure UndoBoycott(const AGood: TGoodType);

      function GetGold: LongInt;
      procedure DecreaseGold(const amount: LongInt);
      procedure IncreaseGold(const amount: LongInt);
      function GetPrice(const AGood: TGoodType; low: Boolean): Byte;
      //increases price if good, if below upper limit
      procedure AdvancePrice(const AGood: TGoodType);
      //decrease price, if above lower limit
      procedure DropPrice(const AGood: TGoodType);
      //set new price, if within limits
      procedure ChangePrice(const AGood: TGoodType; const NewPrice: Byte);
      //functions to buy and sell goods, just does the gold-related stuff
      function BuyGood(const AGood: TGoodType; const num: Byte): Boolean;
      function SellGood(const AGood: TGoodType; const num: Byte): Boolean;

      function SaveToStream(var fs: TFileStream): Boolean; override;
  end;//class
  PEuropeanNation = ^TEuropeanNation;

implementation

//**** functions of TNation ****

constructor TNation.Create(const num: LongInt; const NameStr: string);
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

procedure TNation.ChangeName(var new_name: string);
begin
  if new_name<>'' then m_NameStr:= new_name;
end;//proc

function TNation.GetCount: LongInt;
begin
  Result:= m_count;
end;//func

procedure TNation.ChangeCount(const new_num: LongInt);
begin
  m_count:= new_num;
end;//proc

function TNation.SaveToStream(var fs: TFileStream): Boolean;
var i: LongInt;
begin
  Result:= False;
  if fs=nil then Exit;
  Result:= (fs.Write(m_Count, sizeof(LongInt))=sizeof(LongInt));
  i:= length(m_NameStr);
  Result:= Result and (fs.Write(i, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(m_NameStr[1], length(m_NameStr))=length(m_NameStr));
end;//func


// **** functions of TIndianNation ****

constructor TIndianNation.Create(const num: LongInt; const NameStr: string);
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

constructor TEuropeanNation.Create(const num: LongInt; const NameStr: string;
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

function TEuropeanNation.GetLeaderName: string;
begin
  Result:= m_Leader;
end;//func

procedure TEuropeanNation.ChangeLeaderName(const NameOfLeader: string);
begin
  if NameOfLeader<>'' then m_Leader:= NameOfLeader;
end;//proc

function TEuropeanNation.GetTaxRate: Byte;
begin
  Result:= m_TaxRate;
end;//func

procedure TEuropeanNation.IncreaseTax(const AddedPercentage: Byte);
begin
  if (AddedPercentage<100) and (m_TaxRate+AddedPercentage<100) then
    m_TaxRate:= m_TaxRate + AddedPercentage;
end;//proc

procedure TEuropeanNation.ChangeTaxRate(const NewPercentage: Byte);
begin
  if NewPercentage<100 then m_TaxRate:= NewPercentage;
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

procedure TEuropeanNation.DecreaseGold(const amount: LongInt);
begin
  if m_Gold>amount then m_Gold:= m_Gold - amount
  else m_Gold:= 0;
end;//proc

procedure TEuropeanNation.IncreaseGold(const amount: LongInt);
begin
  if amount>=0 then m_Gold:= m_Gold + amount
  else DecreaseGold(-amount);
end;//proc

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

procedure TEuropeanNation.ChangePrice(const AGood: TGoodType; const NewPrice: Byte);
begin
  if NewPrice<cGoodPrices[AGood].min then m_Prices[AGood]:= cGoodPrices[AGood].min
  else if NewPrice+cGoodPrices[AGood].diff>cGoodPrices[AGood].max then m_Prices[AGood]:= cGoodPrices[AGood].max
  else m_Prices[AGood]:= NewPrice;
end;//proc

//tries to "buy" goods, but only does the money related stuff; returns true on success
function TEuropeanNation.BuyGood(const AGood: TGoodType; const num: Byte): Boolean;
begin
  if IsBoycotted(AGood) or (m_Gold<num*GetPrice(AGood, False)) then Result:= False
  else begin
    m_Gold:= m_Gold - GetPrice(AGood, False)*num;
    //should display message about cost to the player -> GUI
    Result:= True;
  end;//else
end;//func

//tries to "sell" goods, but only does the money related stuff; returns true on success
function TEuropeanNation.SellGood(const AGood: TGoodType; const num: Byte): Boolean;
var tax_amount: Integer;
begin
  if IsBoycotted(AGood) then Result:= False
  else begin
    tax_amount:= (GetPrice(AGood, True)*num*GetTaxRate) div 100;
    m_Gold:= m_Gold + GetPrice(AGood, True)*num - tax_amount;
    //should display message about gain & tax to the player -> GUI
    Result:= True;
  end;//else
end;//func

function TEuropeanNation.SaveToStream(var fs: TFileStream): Boolean;
var i: LongInt;
begin
  Result:= False;
  if fs=nil then Exit;
  Result:= (fs.Write(m_Count, sizeof(LongInt))=sizeof(LongInt));
  i:= length(m_NameStr);
  Result:= Result and (fs.Write(i, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(m_NameStr[1], length(m_NameStr))=length(m_NameStr));
  i:= length(m_Leader);
  Result:= Result and (fs.Write(i, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(m_Leader[1], length(m_Leader))=length(m_Leader));
  Result:= Result and (fs.Write(m_Gold, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(m_TaxRate, sizeof(Byte))=sizeof(Byte));
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
  begin
    Result:= Result and (fs.Write(m_Boycotted[TGoodType(i)], sizeof(Boolean))=sizeof(Boolean));
    Result:= Result and (fs.Write(m_Prices[TGoodType(i)], sizeof(Byte))=sizeof(Byte));
  end;//for
end;//func

end.