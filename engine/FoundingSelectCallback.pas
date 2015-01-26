 { ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2010, 2011, 2012  Thoronador

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

unit FoundingSelectCallback;

interface

uses
  BasicCallback, EuropeanNation, FoundingFathers;

const
  { integer constant that identifies the type of a callback record }
  CBT_SELECT_FOUNDING_FATHER = 15;

type
  TFoundingSelectCallback = class(TBasicCallback)
    protected
      ENat: TEuropeanNation;
      Choices: TFoundingFatherArray;

    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      { function to return the callback's type }
      function GetType: Integer; override;

      constructor Create(const EuroNat: TEuropeanNation; const who: TFoundingFatherArray);
  end;//class
  PFoundingSelectCallback = ^TFoundingSelectCallback;

implementation

function TFoundingSelectCallback.Handle: Boolean;
begin
  Result:= False;
  if ((ENat=nil) or (option<0) or (option>High(Choices))) then Exit;
  //check if Founding father is already present (should never happen)
  if ENat.HasFoundingFather(Choices[option]) then Exit;
  //none is no acceptable selection
  if Choices[option]=ffNone then Exit;
  ENat.SetNextFoundingFather(Choices[option]);
  Result:= True;
end;//func

function TFoundingSelectCallback.GetType: Integer;
begin
  Result:= CBT_SELECT_FOUNDING_FATHER;
end;//func

constructor TFoundingSelectCallback.Create(const EuroNat: TEuropeanNation; const who: TFoundingFatherArray);
begin
  ENat:= EuroNat;
  Choices:= who;
end;//construc

end.
