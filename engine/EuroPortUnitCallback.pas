{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010, 2011  Thoronador

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

unit EuroPortUnitCallback;

interface

uses
  BasicCallback, EuropeanNation, Units;

const
  { integer constant that identifies the type of a callback record }
  CBT_EURO_PORT_UNIT = 7;

type

  TEuroPortUnitCallback = class(TBasicCallback)
    public
      AUnit: TUnit;
      EuroNat: TEuropeanNation;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const u: TUnit; const eNat: TEuropeanNation);
  end;//class
  PEuroPortUnitCallback = ^TEuroPortUnitCallback;

implementation

uses
  Goods;

function TEuroPortUnitCallback.Handle: Boolean;
begin
  Result:= False;
  if ((AUnit=nil) or (EuroNat=nil)) then Exit;
  case option of
    0: begin //ship
         if (AUnit.GetState=usWaitingForShip) then AUnit.SetState(usNormal)
         else AUnit.SetState(usWaitingForShip);
       end;
    1: //muskets
       if not EuroNat.IsBoycotted(gtMusket) then
       begin
         if (AUnit.HasMuskets) then
         begin
           EuroNat.SellGood(gtMusket, 50);
           AUnit.GiveMuskets(False);
         end//if
         else begin
           if EuroNat.GetGold>=EuroNat.GetPrice(gtMusket, False)*50 then
           begin
             EuroNat.BuyGood(gtMusket, 50);
             AUnit.GiveMuskets(True);
           end;//if
         end;//if
       end; //if
    2: //horses
       if not EuroNat.IsBoycotted(gtHorses) then
       begin
         if (AUnit.HasHorses) then
         begin
           EuroNat.SellGood(gtHorses, 50);
           AUnit.GiveHorses(False);
         end//if
         else begin
           if EuroNat.GetGold>=EuroNat.GetPrice(gtHorses, False)*50 then
           begin
             EuroNat.BuyGood(gtHorses, 50);
             AUnit.GiveHorses(True);
           end;//if
         end;//if
       end; //if
    3: begin//tools
         if not EuroNat.IsBoycotted(gtTool) then
         begin
           if (AUnit.GetToolAmount>0) then
           begin
             EuroNat.SellGood(gtTool, AUnit.GetToolAmount);
             AUnit.GiveTools(0);
           end//if
           else begin
             if (EuroNat.GetGold>=EuroNat.GetPrice(gtTool, False)*(100-AUnit.GetToolAmount)) then
             begin
               EuroNat.BuyGood(gtTool, 100-AUnit.GetToolAmount);
               AUnit.GiveTools(100);
             end;//if
           end;//if
         end;//if no boycott
       end;//3 (tools)
    4: ;//no changes at all
  end;//case
  Result:= True;
end;//func

constructor TEuroPortUnitCallback.Create(const u: TUnit; const eNat: TEuropeanNation);
begin
  _type:= CBT_EURO_PORT_UNIT;
  AUnit:= u;
  EuroNat:= eNat;
end;//construc

end.