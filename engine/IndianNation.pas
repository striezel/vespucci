{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2010  Thoronador

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
  Nation;

type
  { ********
    **** TIndianNation class
    ****
    **** purpose: represents an Indian nation within the game. This class is a
    ****          more specialised version of TNation.
    *******
  }
  TIndianNation = class(TNation)
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
  end;//class

implementation

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

end.

