{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2010, 2011  Thoronador

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ***************************************************************************
}

unit EuropeanNation;

interface

uses
  Nation, Goods, FoundingFathers, Classes, Helper;

type
  { enumeration type to describe the relation between two European nations }
  TDiplomaticStatus = (dsUndefined, dsPeace, dsWar);

  { ********
    **** TEuropeanNation class
    ****
    **** purpose: represents an European nation within the game. This class is
    ****          a more specialised version of TNation.
    *******
  }

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
      //number of destroyed indian villages
      m_VillagesBurned: LongInt;
      //list of current founding fathers
      m_FoundingFathers: array[TFoundingFathers] of Boolean;
      //next founding father
      m_NextFoundingFather: TFoundingFathers;
      //number of collected liberty bells since last founding father joined
      m_LibertyBells: Word;
      //contains relationship to other nations
      m_Diplomatic: array [cMinEuropean..cMaxEuropean] of TDiplomaticStatus;
      //location of spawnpoint for units on map
      m_SpawnX, m_SpawnY: LongInt;
    public
      { constructor

        parameters:
            num          - integer that identifies that nation
            NameStr      - name of the nation
            NameOfLeader - name of the nation's leader
      }
      constructor Create(const num: LongInt; const NameStr: string;
                         const NameOfLeader: string);

      { destructor }
      destructor Destroy; override;

      { returns true, if this nation is an IndianNation

        remarks:
            Will always return false.
      }
      function IsIndian: Boolean; override;

      { returns true, if this nation is an EuropeanNation

        remarks:
            Will always return true.
      }
      function IsEuropean: Boolean; override;

      { returns the name of the nation's leader }
      function GetLeaderName: string;

      { changes the name of the nation's leader

        parameters:
            NameOfLeader - new name of the nation's leader

        remarks:
            You should not use this procedure directly; the leader's name can
            be set during creation/ via constructor parameters. This function
            is only used during loading process.
      }
      procedure ChangeLeaderName(const NameOfLeader: string);

      { sets the map square where new units (ships) of that nation will spawn
        after creation

        parameters:
            x, y - coordinates of the map square
      }
      procedure SetSpawnpoint(const x, y: LongInt);

      { returns x-coordinate of the spawnpoint }
      function GetSpawnpointX: LongInt;

      { returns y-coordinate of the spawnpoint }
      function GetSpawnpointY: LongInt;

      { returns true, if this nation has valid spawnpoint coordinates }
      function HasValidSpawnpoint: Boolean;

      //returns the tax rate for this nation in percent
      function GetTaxRate: Byte;

      { increases the nation's tax rate

        parameters:
            AddedPercentage - amount that should be added to the current tax rate
      }
      procedure IncreaseTax(const AddedPercentage: Byte);

      { sets the nation's tax rate directly

        parameters:
            NewPercentage - new tax rate in percent

        remarks:
            ChangeTaxRate is only used during loading; use IncreaseTax on other
            occassions.
      }
      procedure ChangeTaxRate(const NewPercentage: Byte);

      { returns true, if a certain good is boycotted by parliament

        parameters:
            AGood - the good to check for boycott
      }
      function IsBoycotted(const AGood: TGoodType): Boolean;

      { boycotts a certain good, i.e. it cannot be traded in European harbour
        any more, until the boycott is removed

        parameters:
            AGood - the good that will be boycotted
      }
      procedure DoBoycott(const AGood: TGoodType);

      { removes boycott from a certain good, i.e. it can be traded in European
        harbour again

        parameters:
            AGood - the good that will not be boycotted any more
      }
      procedure UndoBoycott(const AGood: TGoodType);

      { removes boycott from all goods, i.e. all goods can be traded in European
        harbour again

        remarks:
            This procedure is only used when Jakob Fugger joins the nation's
            continental congress, because his effect/power is to remove all
            boycotts.
      }
      procedure UndoAllBoycotts;

      { returns the current amount of gold that this nation has }
      function GetGold: LongInt;

      { decreases the current amount of gold that this nation has

        parameters:
            amount - amount of gold pieces that should be removed

        remarks:
            This procedure cannot reduce the nation's gold amount to
            less than zero.
      }
      procedure DecreaseGold(const amount: LongInt);

      { increases the current amount of gold that this nation has

        parameters:
            amount - amount of gold pieces that should be added
      }
      procedure IncreaseGold(const amount: LongInt);

      { returns the current price of a certain good in Europe

        parameters:
            AGood - the good to check for
            low   - boolean that indicates whether you want the good's low
                    price (true), i.e. the price you get when selling this good,
                    or the high price (false), i.e. the price you have to buy
                    when buying this good.
      }
      function GetPrice(const AGood: TGoodType; low: Boolean): Byte;

      { increases the price of a certain good, if the price is still below the
        upper price limit set in the game rules (see Goods.pas for limits)

        parameters:
            AGood - the good whose price has to be raised (by one gold piece)
      }
      procedure AdvancePrice(const AGood: TGoodType);

      { decreases the price of a certain good, if the current price is still
        above the lower limit defined in the game rules (see Goods.pas for limits)

        parameters:
            AGood - the good whose price has to be lowered (by one gold piece)
      }
      procedure DropPrice(const AGood: TGoodType);

      { sets the price of a certain good directly, if the new price is within
        the limits defined in the game rules (see Goods.pas for limits)

        parameters:
            AGood    - the good whose price has to be set
            NewPrice - the new price of that good

       remarks:
           If the new price exceeds the given limits, it will be set to the
           upper or lower limit, respectively.
           Do not call this function directly - it's only used during the
           loading process.
      }
      procedure ChangePrice(const AGood: TGoodType; const NewPrice: Byte);
      //functions to buy and sell goods, just does the gold-related stuff
      { buys a certain amount of a good in Europe and returns true on success

        parameters:
            AGood - the good that will be bought
            num   - the amount of that good that will be bought
      }
      function BuyGood(const AGood: TGoodType; const num: Byte): Boolean;

      { sells a certain amount of a good in Europe and returns true on success

        parameters:
            AGood - the good that will be sold
            num   - the amount of that good that will be sold
      }
      function SellGood(const AGood: TGoodType; const num: Byte): Boolean;

      { returns the number of Indian villages that have been destroyed by this
        European Nation
      }
      function GetVillagesBurned: LongInt;

      { returns true, if the given founding father is present at the nation's
        congress

        parameters:
            ff - the founding father which has to be checked for
      }
      function HasFoundingFather(const ff: TFoundingFathers): Boolean;

      { sets the presence state of a certain founding father

        parameters:
            ff      - the founding father whose state is set
            present - true, if the founding father shall be present, false
                      otherwise
      }
      procedure SetFoundingFather(const ff: TFoundingFathers; const present: Boolean);

      { returns the number of founding fathers that are present in this nation's
        congress
      }
      function GetPresentFoundingFathers: Byte;

      { returns the number of liberty bells that this nation has produced yet }
      function GetLibertyBells: Word;

      { adds to the number of produced liberty bells

        parameters:
            lb - the number of new liberty bells, i.e. that amount that will be
                 added to the number of current liberty bells
      }
      procedure AddLibertyBells(const lb: Word);

      { returns the next founding father that will join this nation's congress }
      function GetNextFoundingFather: TFoundingFathers;

      { sets the next founding father that will join this nation's congress

        parameters:
            ff     - enumeration value that indicates the next founding father
      }
      procedure SetNextFoundingFather(const ff: TFoundingFathers);

      { returns an array of founding fathers the player can choose from }
      function GetFoundingFatherSelection: TFoundingFatherArray;

      { sets the number of Indian villages that have been destroyed by this
        European Nation

        parameters:
            villages - the new amount of villages

        remarks:
           Do not call this function directly - it's only used during the
           loading process.
      }
      procedure SetVillagesBurned(const villages: LongInt);

      { returns the diplomatic status of the relations between this nation and
        the specified other European nation

        parameters:
            other_nation - integer constant identifying the other nation

        remarks:
            If other_nation does not identify a European nation, dsUndefined
            will be returned. However, dsUndefined is also a valid return value,
            if this nation and the other nation have not met yet.
      }
      function GetDiplomatic(const other_nation: LongInt): TDiplomaticStatus;

      { sets the diplomatic status of the relations between this nation and
        the specified other European nation

        parameters:
            other_nation - integer constant identifying the other nation
            new_status   - the status that has to be set

        remarks:
            If other_nation does not identify a European nation, then no changes
            are made to any diplomatic status.
      }
      procedure SetDiplomatic(const other_nation: LongInt; const new_status: TDiplomaticStatus);

      { tries to save this nation's data to the given stream and returns true
        in case of success, or false if an error occured

        parameters:
            fs - the file stream the nation shall be saved in

        remarks:
            The file stream already has to be opened and be ready for writing.
      }
      function SaveToStream(var fs: TFileStream): Boolean; override;

      { loads the European nation from the stream and returns true on success

        parameters:
            fs   - the file stream the nation will be loaded from
      }
      function LoadFromStream(var fs: TFileStream): Boolean; override;
  end;//class TEuropeanNation


implementation

//**** functions of TEuropeanNation ****

constructor TEuropeanNation.Create(const num: LongInt; const NameStr: string;
                                   const NameOfLeader: string);
var gt: TGoodType;
    ff: TFoundingFathers;
    i: Integer;
begin
  inherited Create(num, NameStr);
  //check number and pick some default value to make sure it's European
  if ((num<cMinEuropean) or (num>cMaxEuropean)) then m_Count:= cNationEngland;
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
  m_VillagesBurned:= 0;
  //no founding fathers at beginning
  ff:= Low(TFoundingFathers);
  while ff<High(TFoundingFathers) do
  begin
    m_FoundingFathers[ff]:= false;
    ff:= Succ(ff);
  end;//while
  m_FoundingFathers[High(TFoundingFathers)]:= false;
  //next founding father
  m_NextFoundingFather:= ffNone; //none selected yet
  m_LibertyBells:=0; //new nation has no liberty bells yet
  //initialize diplomatic state as undefinded for all European nations
  for i:= cMinEuropean to cMaxEuropean do
  begin
    m_Diplomatic[i]:= dsUndefined;
  end;//for
  //set spawnpoint to (-1;-1) to indicate that it is not set yet
  m_SpawnX:= -1;
  m_SpawnY:= -1;
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

procedure TEuropeanNation.SetSpawnpoint(const x, y: LongInt);
begin
  if (x>=0) and (y>=0) then
  begin
    //location seems to be valid, set it
    m_SpawnX:= x;
    m_SpawnY:= y;
  end
  else begin
    //invalid location given, set to (-1; -1)
    m_SpawnX:= -1;
    m_SpawnY:= -1;
  end;
end;//proc

function TEuropeanNation.GetSpawnpointX: LongInt;
begin
  Result:= m_SpawnX;
end;//func

function TEuropeanNation.GetSpawnpointY: LongInt;
begin
  Result:= m_SpawnY;
end;//func

function TEuropeanNation.HasValidSpawnpoint: Boolean;
begin
  Result:= ((m_SpawnX>=0) and (m_SpawnY>=0));
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

procedure TEuropeanNation.UndoAllBoycotts;
var gt: TGoodType;
begin
  gt:= Low(TGoodType);
  while gt<High(TGoodType) do
  begin
    m_Boycotted[gt]:= False;
    gt:= Succ(gt);
  end;
  m_Boycotted[High(TGoodType)]:= False;
end;//proc

function TEuropeanNation.GetGold: LongInt;
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
    //TODO: should display message to player
  end;//if
end;//proc

procedure TEuropeanNation.DropPrice(const AGood: TGoodType);
begin
  if m_Prices[AGood]>cGoodPrices[AGood].min then
  begin
    m_Prices[AGood]:= m_Prices[AGood]-1;
    //TODO: should display a message to the player
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
    //TODO: should display message about cost to the player -> GUI
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
    //TODO: should display message about gain & tax to the player -> GUI
    Result:= True;
  end;//else
end;//func

function TEuropeanNation.GetVillagesBurned: LongInt;
begin
  Result:= m_VillagesBurned;
end;//func

procedure TEuropeanNation.SetVillagesBurned(const villages: LongInt);
begin
  if (villages>=0) then m_VillagesBurned:= villages;
end;//proc

function TEuropeanNation.HasFoundingFather(const ff: TFoundingFathers): Boolean;
begin
  Result:= m_FoundingFathers[ff];
end;//func

procedure TEuropeanNation.SetFoundingFather(const ff: TFoundingFathers; const present: Boolean);
begin
  //none is not an acceptable value and will always be false
  if ff=ffNone then
  begin
    m_FoundingFathers[ffNone]:= false;
    Exit;
  end;//if none
  //Will a new founding father join, and is this the same one as the one that is
  // expected to be the next?
  if (present and (ff=m_NextFoundingFather) and (m_NextFoundingFather<>ffNone) and
      not m_FoundingFathers[ff]) then
  begin
    //Then set the new ff and adjust the amount of bells accordingly.
    m_FoundingFathers[ff]:= true;
    if m_LibertyBells>=GetRequiredLibertyBells(GetPresentFoundingFathers) then
      m_LibertyBells:= m_LibertyBells-GetRequiredLibertyBells(GetPresentFoundingFathers)
    else m_LibertyBells:= 0;
    //...and set the next ff to none. (Player or AI should choose a new one.)
    m_NextFoundingFather:= ffNone;
  end
  //otherwise just set the value
  else m_FoundingFathers[ff]:= present;
end;//proc

function TEuropeanNation.GetPresentFoundingFathers: Byte;
var i: Integer;
begin
  Result:= 0;
  for i:=Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
    if m_FoundingFathers[TFoundingFathers(i)] then Result:= Result+1;
end;//func

function TEuropeanNation.GetLibertyBells: Word;
begin
  Result:= m_LibertyBells;
end;//func

procedure TEuropeanNation.AddLibertyBells(const lb: Word);
begin
  m_LibertyBells:= m_LibertyBells+lb;
end;//func

{procedure TEuropeanNation.SetLibertyBells(const total_lb: Word);
begin
  m_LibertyBells:= total_lb;
end;//proc }

function TEuropeanNation.GetNextFoundingFather: TFoundingFathers;
begin
  Result:= m_NextFoundingFather;
end;//func

procedure TEuropeanNation.SetNextFoundingFather(const ff: TFoundingFathers);
begin
  if ff<>ffNone then
  begin
    if not HasFoundingFather(ff) then m_NextFoundingFather:= ff;
  end
  else m_NextFoundingFather:= ffNone;
end;//proc

function TEuropeanNation.GetFoundingFatherSelection: TFoundingFatherArray;
var i, next_index: Integer;
begin
  for i:= 0 to 4 do Result[i]:= ffNone;
  Randomize;
  next_index:= 0;
  //trade
  i:= Ord(ffSmith)+Random(5);
  if not HasFoundingFather(TFoundingFathers(i)) then
  begin
    Result[0]:= TFoundingFathers(i);
    next_index:= 1;
  end
  else begin
    for i:= Ord(ffSmith) to Ord(ffDeWitt) do
      if not HasFoundingFather(TFoundingFathers(i)) then
      begin
        Result[0]:= TFoundingFathers(i);
        next_index:= 1;
        break;
      end;//if
  end;//else
  //exploration
  i:= Ord(ffCoronado)+Random(5);
  if not HasFoundingFather(TFoundingFathers(i)) then
  begin
    Result[next_index]:= TFoundingFathers(i);
    next_index:= next_index+1;
  end
  else begin
    for i:= Ord(ffCoronado) to Ord(ffDeSoto) do
      if not HasFoundingFather(TFoundingFathers(i)) then
      begin
        Result[next_index]:= TFoundingFathers(i);
        next_index:= next_index+1;
        break;
      end;//if
  end;//else
  //military
  i:= Ord(ffCortes)+Random(5);
  if not HasFoundingFather(TFoundingFathers(i)) then
  begin
    Result[next_index]:= TFoundingFathers(i);
    next_index:= next_index+1;
  end
  else begin
    for i:= Ord(ffCortes) to Ord(ffWashington) do
      if not HasFoundingFather(TFoundingFathers(i)) then
      begin
        Result[next_index]:= TFoundingFathers(i);
        next_index:= next_index+1;
        break;
      end;//if
  end;//else
  //political
  i:= Ord(ffBolivar)+Random(5);
  if not HasFoundingFather(TFoundingFathers(i)) then
  begin
    Result[next_index]:= TFoundingFathers(i);
    next_index:= next_index+1;
  end
  else begin
    for i:= Ord(ffBolivar) to Ord(ffPocahontas) do
      if not HasFoundingFather(TFoundingFathers(i)) then
      begin
        Result[next_index]:= TFoundingFathers(i);
        next_index:= next_index+1;
        break;
      end;//if
  end;//else
  //religious
  i:= Ord(ffBrebeuf)+Random(5);
  if not HasFoundingFather(TFoundingFathers(i)) then
  begin
    Result[next_index]:= TFoundingFathers(i);
    next_index:= next_index+1;
  end
  else begin
    for i:= Ord(ffBrebeuf) to Ord(ffSepulveda) do
      if not HasFoundingFather(TFoundingFathers(i)) then
      begin
        Result[next_index]:= TFoundingFathers(i);
        next_index:= next_index+1;
        break;
      end;//if
  end;//else
end;//func

function TEuropeanNation.GetDiplomatic(const other_nation: LongInt): TDiplomaticStatus;
begin
  if (other_nation in [cMinEuropean..cMaxEuropean]) then Result:= m_Diplomatic[other_nation]
  else Result:= dsUndefined;
end;//func

procedure TEuropeanNation.SetDiplomatic(const other_nation: LongInt; const new_status: TDiplomaticStatus);
begin
  if (other_nation in [cMinEuropean..cMaxEuropean]) then
  begin
    if (other_nation<>GetCount) then m_Diplomatic[other_nation]:= new_status
    else m_Diplomatic[other_nation]:= dsUndefined;
  end;//if
end;//proc

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
  //burned villages
  Result:= Result and (fs.Write(m_VillagesBurned, sizeof(LongInt))=sizeof(LongInt));
  //founding fathers
  for i:= Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
  begin
    Result:= Result and (fs.Write(m_FoundingFathers[TFoundingFathers(i)],
                                  sizeof(Boolean))=sizeof(Boolean));
  end;//for
  //next founding father
  Result:= Result and (fs.Write(m_NextFoundingFather, sizeof(TFoundingFathers))
                                                     =sizeof(TFoundingFathers));
  //liberty bells produced
  Result:= Result and (fs.Write(m_LibertyBells, sizeof(Word))=sizeof(Word));
  //diplomatic status
  for i:= cMinEuropean to cMaxEuropean do
    Result:= Result and (fs.Write(m_Diplomatic[i], sizeof(TDiplomaticStatus))
                         =sizeof(TDiplomaticStatus));
  //spawnpoint's coordinates
  Result:= Result and (fs.Write(m_SpawnX, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(m_SpawnY, sizeof(LongInt))=sizeof(LongInt));
end;//func

function TEuropeanNation.LoadFromStream(var fs: TFileStream): Boolean;
var i, j: LongInt;
    temp_str: string;
    tr: Byte;
    boycott: Boolean;
    diplomatic: TDiplomaticStatus;
begin
  Result:= False;
  if (fs=nil) then  Exit;
  i:= 0;
  Result:= (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if (not Result) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading count.');
    Exit;
  end;//if
  self.ChangeCount(i);
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if (i<=0) or (i>255) then
  begin
    Result:= False; //string to short or to long
    WriteLn('TEuropeanNation.LoadFromStream: Error: name string does not meet '
            +'the length requirements.');
    Exit;
  end;//if
  temp_str:= SpaceString(i);
  Result:= Result and (fs.Read(temp_str[1], i)=i);
  if (not Result) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading name.');
    Exit;
  end;//if
  temp_str:= Trim(temp_str);
  ChangeName(temp_str);
  //--- European part starts here ----
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if (i<=0) or (i>255) then
  begin
    Result:= False; //string to short or to long
    WriteLn('TEuropeanNation.LoadFromStream: Error: leader name string does '
            +'not meet the length requirements.');
    Exit;
  end;//if
  temp_str:= SpaceString(i);
  Result:= Result and (fs.Read(temp_str[1], i)=i);
  if (not Result) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading leader''s name.');
    Exit;
  end;//if
  ChangeLeaderName(Trim(temp_str));
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if ((not Result) or (i<0)) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading gold amount.');
    Exit;
  end;//if
  m_Gold:= i;
  //tax rate
  tr := 255;
  Result:= Result and (fs.Read(tr, sizeof(Byte))=sizeof(Byte));
  if ((not Result) or (tr>100)) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading tax rate.');
    Exit;
  end;//if
  ChangeTaxRate(tr);
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
  begin
    //boycott status
    Result:= Result and (fs.Read(Boycott, sizeof(Boolean))=sizeof(Boolean));
    m_Boycotted[TGoodtype(i)]:= Boycott;
    //price of good
    Result:= Result and (fs.Read(tr, sizeof(Byte))=sizeof(Byte));
    ChangePrice(TGoodType(i), tr);
  end;//for
  //burned villages
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if ((not Result) or (i<0)) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading number of '
            +'villages burned.');
    Exit;
  end;//if
  SetVillagesBurned(i);
  //founding fathers
  for i:= Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
  begin
    Result:= Result and (fs.Read(boycott, sizeof(Boolean))=sizeof(Boolean));
    m_FoundingFathers[TFoundingFathers(i)]:= boycott;
  end;//for
  m_FoundingFathers[ffNone]:= false;
  //next founding father
  Result:= Result and (fs.Read(m_NextFoundingFather, sizeof(TFoundingFathers))
                                                     =sizeof(TFoundingFathers));
  //liberty bells produced
  Result:= Result and (fs.Read(m_LibertyBells, sizeof(Word))=sizeof(Word));
  if (not Result) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading information '
            +'about founding fathers.');
    Exit;
  end;//if
  //diplomatic status
  diplomatic:= dsUndefined;
  for i:= cMinEuropean to cMaxEuropean do
  begin
    Result:= Result and (fs.Read(diplomatic, sizeof(TDiplomaticStatus))
                         =sizeof(TDiplomaticStatus));
    SetDiplomatic(i, diplomatic);
  end;//for
  if (not Result) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading diplomatic '
            +'status.');
    Exit;
  end;//if
  //spawnpoint's coordinates
  i:= -1;
  j:= -1;
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Read(j, sizeof(LongInt))=sizeof(LongInt));
  SetSpawnpoint(i, j);
  if (not Result) then
  begin
    WriteLn('TEuropeanNation.LoadFromStream: Error while reading spawnpoint.');
    SetSpawnpoint(-1, -1); //reset spawnpoint
  end;//if
end;//func

end.

