{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2011, 2012  Dirk Stolle

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

unit JobChangeCallback;

interface

uses
  BasicCallback, Colony;

const
  { integer constant that identifies the type of a callback record }
  CBT_JOB_CHANGE = 6;

type

  TJobChangeCallback = class(TBasicCallback)
    protected
      x_shift, y_shift: ShortInt;
      AColony: TColony;

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

      constructor Create(const x_delta, y_delta: ShortInt; const col: TColony);
  end;//class
  PJobChangeCallback = ^TJobChangeCallback;

implementation

uses
  Goods;

function TJobChangeCallback.Handle: Boolean;
var new_good: TGoodType;
begin
  Result:= False;
  if ((abs(x_shift)>1) or (abs(y_shift)>1) or (AColony=nil)) then Exit;
  if AColony.GetUnitInField(x_shift, y_shift)=nil then Exit;
  case option of
    0: new_good:= gtFood;
    1: new_good:= gtSugar;
    2: new_good:= gtTobacco;
    3: new_good:= gtCotton;
    4: new_good:= gtFur;
    5: new_good:= gtWood;
    6: new_good:= gtOre;
    7: new_good:= gtSilver;
  else //should not happen
    new_good:= gtFood;
  end;//case
  AColony.SetUnitInField(x_shift, y_shift, AColony.GetUnitInField(x_shift, y_shift), new_good);
  Result:= True;
end;//func

function TJobChangeCallback.GetType: Integer;
begin
  Result:= CBT_JOB_CHANGE;
end; //func

constructor TJobChangeCallback.Create(const x_delta, y_delta: ShortInt; const col: TColony);
begin
  x_shift:= x_delta;
  y_shift:= y_delta;
  AColony:= col;
end;//construc

end.
