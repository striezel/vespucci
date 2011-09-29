{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011  Thoronador

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
  Settlement, Nation, Units, Classes;

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

      { returns true, if this tribe represents a capital of the Indians }
      function IsCapital: Boolean;

      { returns an integer representing this tribe's attitude towards the
        given European Nation. Lower values mean friendlier attitude.

        parameters:
            NumNation - ID of the European Nation
      }
      function GetAttitudeLevel(const NumNation: LongInt): Byte;

      { sets an integer representing this tribe's attitude towards the
        given European Nation. Lower values mean friendlier attitude.

        parameters:
            NumNation - ID of the European Nation
            new_level - the new value for the attitude level
      }
      procedure SetAttitudeLevel(const NumNation: LongInt; const new_level: Byte);

      { tries to save the tribe to a stream and returns true in case of success

        parameters:
            fs - the file stream the tribe has to be saved to

        remarks:
            The file stream already has to be openend and has to be ready for
            writing.
      }
      function SaveToStream(var fs: TFileStream): Boolean; override;

      { tries to load a tribe from a stream and returns true in case of success

        parameters:
            fs - the file stream the tribe has to be loaded from

        remarks:
            The file stream already has to be openend and has to be ready for
            reading.
      }
      function LoadFromStream(var fs: TFileStream): Boolean; override;
    private
      m_KnownFor: TUnitType;
      m_HasTought: array[cMinEuropean..cMaxEuropean] of Boolean;
      //capital flag
      m_Capital: Boolean;
      //attitude level - basically used for internal calculations
      m_AttitudeLevel: array[cMinEuropean..cMaxEuropean] of Byte;
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

function TTribe.IsCapital: Boolean;
begin
  Result:= m_Capital;
end;//func

function TTribe.GetAttitudeLevel(const NumNation: LongInt): Byte;
begin
  if NumNation in [cMinEuropean..cMaxEuropean] then
    Result:= m_AttitudeLevel[NumNation]
  else Result:= 0; //assume best
end;//func

procedure TTribe.SetAttitudeLevel(const NumNation: LongInt; const new_level: Byte);
begin
  if NumNation in [cMinEuropean..cMaxEuropean] then
    m_AttitudeLevel[NumNation]:= new_level;
end;//proc

function TTribe.SaveToStream(var fs: TFileStream): Boolean;
var i: Integer;
begin
  //write inherited data
  Result:= inherited SaveToStream(fs);
  if not Result then
  begin
    WriteLn('TTribe.SaveToStream: Error: could not write inherited data.');
    Exit;
  end;//if
  //write TTribe stuff
  //write special unit type
  Result:= Result and (fs.Write(m_KnownFor, sizeof(TUnitType))=sizeof(TUnitType));
  //write teaching status
  for i:= cMinEuropean to cMaxEuropean do
    Result:= Result and (fs.Write(m_HasTought[i], sizeof(Boolean))=sizeof(Boolean));
  //write capital status
  Result:= Result and (fs.Write(m_Capital, sizeof(Boolean))=sizeof(Boolean));
  //write attitude levels
  for i:= cMinEuropean to cMaxEuropean do
    Result:= Result and (fs.Write(m_AttitudeLevel[i], sizeof(Byte))=sizeof(Byte));
end;//func

function TTribe.LoadFromStream(var fs: TFileStream): Boolean;
var i: LongInt;
begin
  //read inherited data
  Result:= inherited LoadFromStream(fs);
  if not Result then
  begin
    WriteLn('TTribe.LoadFromStream: Error: could not read inherited data.');
    Exit;
  end;//if
  //check nation index
  if not (m_Nation in [cMinIndian..cMaxIndian]) then
  begin
    Result:= False;
    WriteLn('TTribe.LoadFromStream: Error: got invalid nation index.');
    Exit;
  end;//if
  //read TTribe stuff
  //read special unit type
  Result:= Result and (fs.Read(m_KnownFor, sizeof(TUnitType))=sizeof(TUnitType));
  //write teaching status
  for i:= cMinEuropean to cMaxEuropean do
    Result:= Result and (fs.Read(m_HasTought[i], sizeof(Boolean))=sizeof(Boolean));
  //read capital status
  Result:= Result and (fs.Read(m_Capital, sizeof(Boolean))=sizeof(Boolean));
  //read attitude levels
  for i:= cMinEuropean to cMaxEuropean do
    Result:= Result and (fs.Read(m_AttitudeLevel[i], sizeof(Byte))=sizeof(Byte));
end;//func

end.
