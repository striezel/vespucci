{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010  Thoronador

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

unit Tribe;

interface

uses
  Settlement, Nation, Units;

const
  { Constant array that holds the lociations of indian tribes that are present
    in America at the start of a new game.

    remarks/ to do:
        still needs to be extended
  }
  cTribeLocationsAmerica: array [0..12] of record
                     Nation: LongInt;
                     x,y: Byte;
                   end //rec
    =(
      (Nation: cNationAztec ; x: 11; y: 23;),
      (Nation: cNationAztec ; x: 16; y: 26;),
      (Nation: cNationAztec ; x: 23; y: 27;),
      (Nation: cNationAztec ; x: 25; y: 33;),

      (Nation: cNationInca ; x: 26; y: 42;),
      (Nation: cNationInca ; x: 34; y: 59;),
      (Nation: cNationInca ; x: 34; y: 65;),
      (Nation: cNationInca ; x: 35; y: 51;),
      (Nation: cNationInca ; x: 36; y: 55;),

      (Nation: cNationCherokee ; x: 20; y: 20;),
      (Nation: cNationCherokee ; x: 21; y: 17;),
      (Nation: cNationCherokee ; x: 24; y: 19;),
      (Nation: cNationCherokee ; x: 27; y: 22;)
    );


type
  { ********
    **** TTribe class
    ****
    **** purpose: represents a Indian settlement within the game.
    ****          Specialised version of TSettlement for Indians.
    *******
  }
  TTribe = class(TSettlement)
    public
      { constructor

        parameters:
            x, y     - position of the tribe
            ANation  - integer identifier of the Indian nation
            KnownFor - the skill/job the Indians of that tribe can teach to
                       unskilled colonists
      }
      constructor Create(const X, Y: Integer; const ANation: LongInt; const KnownFor: TUnitType);

      { teaches the given unit the special skill of that tribe

        parameters:
            AUnit - the unit that shall learn the skill

        remarks:
            Only unskilled units, i.e. servants or colonists, can learn a skill
            from a tribe. Every European nation can only learn once from the
            same tribe.
      }
      procedure Teach(var AUnit: TUnit);
    private
      m_KnownFor: TUnitType;
      m_HasTought: array[cMinEuropean..cMaxEuropean] of Boolean;
  end;//class

implementation

// **** TTribe functions ****

constructor TTribe.Create(const X, Y: Integer; const ANation: LongInt; const KnownFor: TUnitType);
var i: LongInt;
begin
  //sets Nation and position
  inherited Create(X, Y, ANation);
  for i:= cMinEuropean to cMaxEuropean do
    m_HasTought[i]:= False;
  //set the unit type the tribe is known for
  m_KnownFor:= KnownFor;
end;//construc

procedure TTribe.Teach(var AUnit: TUnit);
begin
  //only servants or free colonists can learn something
  if ((AUnit.GetType<>utServant) and (AUnit.GetType<>utColonist)) then
  begin
    if AUnit.GetType in [utFarmer..utPioneer] then
    begin
      //Show message: "It's a pleasure to see a skilled <whatever>..."
      // Still ToDo();
    end
    else begin
      //Show message, informing player about inappropriate unit type
      // Still ToDo();
    end;//else
  end
  //only European Units can learn from Indians
  else if (AUnit.GetNation in [cMinEuropean..cMaxEuropean]) then
  begin
    //check, if Indians already did teach that nation's units a new skill
    if m_HasTought[AUnit.GetNation] then
    begin
      //Show message, something like "Ihr habt schon etwas von uns gelernt"
      // Still ToDo();
    end//if
    else begin
      //actually teach the unit something
      AUnit.ChangeType(m_KnownFor);
      m_HasTought[AUnit.GetNation]:= True;
    end;//else
  end;//if
end;//proc

end.