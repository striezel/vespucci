{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2011  Dirk Stolle

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

unit GotoShipCallback;

interface

uses
  BasicCallback, Data, Units;

const
  { integer constant that identifies the type of a callback record }
  CBT_GOTO_SHIP = 13;

type

  TGotoShipCallback = class(TBasicCallback)
    public
      Ship: TUnit;
      AData: TData;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const vessel: TUnit; const dat: TData);
  end;//class
  PGotoShipCallback = ^TGotoShipCallback;

implementation

uses
  Colony;

function TGotoShipCallback.Handle: Boolean;
var col_arr: TColonyArr;
    goTask: TGoToTask;
begin
  Result:= False;
  if ((Ship=nil) or (AData=nil) or (option<0)) then Exit;
  if option=0 then Result:= True
  else begin
    col_arr:= AData.GetColonyList(Ship.GetNation);
    if length(col_arr)<option then Exit;
    goTask:= TGoToTask.Create(Ship, col_arr[option-1].GetPosX, col_arr[option-1].GetPosY, AData.GetMap,
               col_arr[option-1].GetPosX, col_arr[option-1].GetPosY);//path with destination as special node
    Ship.SetTask(goTask, True);
    Result:= True;
  end;//else
end;//func

constructor TGotoShipCallback.Create(const vessel: TUnit; const dat: TData);
begin
  _type:= CBT_GOTO_SHIP;
  Ship:= vessel;
  AData:= dat;
end;//construc

end.