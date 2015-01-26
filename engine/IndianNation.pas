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

unit IndianNation;

interface

uses
  Nation, Classes;

type
  { enumeration type for tech level of an Indian nation }
  TTechLevel = (tlNomadic, tlAgricultural, tlDeveloped, tlCivilised);
  { enumeration type for attitude of Indians }
  TIndianAttitude = (iaPleased, iaWorried, iaAnxious, iaAngry, iaBelligerent);

  { ********
    **** TIndianNation class
    ****
    **** purpose: represents an Indian nation within the game. This class is a
    ****          more specialised version of TNation.
    *******
  }
  TIndianNation = class(TNation)
    protected
      //tech level of that nation
      m_TechLevel: TTechLevel;
      //contact with European Nations
      m_Contact: array[cMinEuropean..cMaxEuropean] of Boolean;
      //attitude towards Europeans
      m_Attitude: array[cMinEuropean..cMaxEuropean] of TIndianAttitude;
      //boolean that indicates whether this nation was present at the start of
      // the game (not necessarily all Indian Nations are spawned each game)
      m_Spawned: Boolean;
    public
      { constructor

        parameters:
            num     - integer that identifies that nation
            NameStr - name of the nation
      }
      constructor Create(const num: LongInt; const NameStr: string);

      { returns true, if this nation is an IndianNation

        remarks:
            Will always return true.
      }
      function IsIndian: Boolean; override;

      { returns true, if this nation is an EuropeanNation

        remarks:
            Will always return false.
      }
      function IsEuropean: Boolean; override;

      { returns the nation's tech level }
      function GetTechLevel: TTechLevel;

      { returns whether this nation had already contact with an European nation

        parameters:
            num_EuroNat - the European nation's ID
      }
      function HasContactWith(const num_EuroNat: LongInt): Boolean;

      { sets the contact status with an European Nation

        parameters:
            num_EuroNat - the European nation's ID
            newContact  - the new contact state
      }
      procedure SetContactWith(const num_EuroNat: LongInt; const newContact: Boolean=True);

      { returns this nation's attitude towards an European nation

        parameters:
            num_EuroNat - the European nation's ID
      }
      function GetAttitude(const num_EuroNat: LongInt): TIndianAttitude;

      { sets the nation's attitude towards an European Nation

        parameters:
            num_EuroNat - the European nation's ID
            newAttitude - the new attitude
      }
      procedure SetAttitude(const num_EuroNat: LongInt; const newAttitude: TIndianAttitude);

      { returns true, if this nation spawned at the start of the current game.
        The default value right after contructor call is false. }
      function GetSpawnStatus: Boolean;

      { sets the nation's spawn status

        parameters:
            new_stat - the new status value
      }
      procedure SetSpawnStatus(const new_stat: Boolean);

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
  end;//class

implementation

// **** functions of TIndianNation ****

constructor TIndianNation.Create(const num: LongInt; const NameStr: string);
var i: Integer;
begin
  inherited Create(num, NameStr);
  //check number and pick a default in case of invalidity
  if ((m_count<cMinIndian) or (m_count>cMaxIndian)) then
    m_count:= cNationArawak;
  for i:=cMinEuropean to cMaxEuropean do
  begin
    m_Contact[i]:= false;
    m_Attitude[i]:= iaPleased;
  end;//for
  case m_count of
    cNationArawak, cNationCherokee, cNationIroquois: m_TechLevel:= tlAgricultural;
    cNationAztec: m_TechLevel:= tlDeveloped;
    cNationInca: m_TechLevel:= tlCivilised;
  else m_TechLevel:= tlNomadic; //Tupi, Sioux, and Apache
  end;//case
  m_Spawned:= false;
end;//construc

function TIndianNation.IsIndian: Boolean;
begin
  Result:= True;
end;//func

function TIndianNation.IsEuropean: Boolean;
begin
  Result:= False;
end;//func

function TIndianNation.GetTechLevel: TTechLevel;
begin
  Result:= m_TechLevel;
end;//func

function TIndianNation.HasContactWith(const num_EuroNat: LongInt): Boolean;
begin
  if num_EuroNat in [cMinEuropean..cMaxEuropean] then
    Result:= m_Contact[num_EuroNat]
  else Result:= False;
end;//func

procedure TIndianNation.SetContactWith(const num_EuroNat: LongInt; const newContact: Boolean=True);
begin
  if num_EuroNat in [cMinEuropean..cMaxEuropean] then
    m_Contact[num_EuroNat]:= newContact;
end;//proc

function TIndianNation.GetAttitude(const num_EuroNat: LongInt): TIndianAttitude;
begin
  if num_EuroNat in [cMinEuropean..cMaxEuropean] then
    Result:= m_Attitude[num_EuroNat]
  else Result:= iaPleased;
end;//func

procedure TIndianNation.SetAttitude(const num_EuroNat: LongInt; const newAttitude: TIndianAttitude);
begin
  if num_EuroNat in [cMinEuropean..cMaxEuropean] then
    m_Attitude[num_EuroNat]:= newAttitude;
end;//proc

function TIndianNation.GetSpawnStatus: Boolean;
begin
  Result:= m_Spawned;
end;//func

procedure TIndianNation.SetSpawnStatus(const new_stat: Boolean);
begin
  m_Spawned:= new_stat;
end;//proc

function TIndianNation.SaveToStream(var fs: TFileStream): Boolean;
var i: LongInt;
begin
  //try to save inherited data from TNation
  Result:= inherited SaveToStream(fs);
  //If that failed, exit.
  if (not Result) then Exit;
  //save bits from Indian nation
  // -- tech level
  Result:= Result and (fs.Write(m_TechLevel, sizeof(TTechLevel))=sizeof(TTechLevel));
  // -- contacts and attitudes
  for i:= cMinEuropean to cMaxEuropean do
  begin
    Result:= Result and (fs.Write(m_Contact[i], sizeof(Boolean))=sizeof(Boolean));
    Result:= Result and (fs.Write(m_Attitude[i], sizeof(TIndianAttitude))=sizeof(TIndianAttitude));
  end;//for
  // -- spawning status
  Result:= Result and (fs.Write(m_Spawned, sizeof(Boolean))=sizeof(Boolean));
end;//func

function TIndianNation.LoadFromStream(var fs: TFileStream): Boolean;
var i: LongInt;
begin
  //try to load inherited data from TNation
  Result:= inherited LoadFromStream(fs);
  //If that failed, exit.
  if (not Result) then Exit;
  //load bits from Indian nation
  // -- tech level
  Result:= Result and (fs.Read(m_TechLevel, sizeof(TTechLevel))=sizeof(TTechLevel));
  // -- contacts and attitudes
  for i:= cMinEuropean to cMaxEuropean do
  begin
    Result:= Result and (fs.Read(m_Contact[i], sizeof(Boolean))=sizeof(Boolean));
    Result:= Result and (fs.Read(m_Attitude[i], sizeof(TIndianAttitude))=sizeof(TIndianAttitude));
  end;//for
  // -- spawning status
  Result:= Result and (fs.Read(m_Spawned, sizeof(Boolean))=sizeof(Boolean));
end;//func

end.

