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

unit SaveLoadCallbacks;

interface

uses
  BasicCallback, Data;

const
  { integer constant that identifies the type of a callback record }
  CBT_SAVE_GAME = 4;
  CBT_LOAD_GAME = 5;

type
  TSaveCallback = class(TBasicCallback)
    public
      AData: TData;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData);
  end;//class
  PSaveCallback = ^TSaveCallback;

  TLoadCallback = class(TBasicCallback)
    public
      AData: TData;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData);
  end;//class
  PLoadCallback = ^TLoadCallback;

implementation

function TSaveCallback.Handle: Boolean;
  var err_str: string;
begin
  err_str:= 'not saved.';
  if (option>0) and (option<65536) then
  begin
    if (AData<>nil) then
    begin
      Result:= AData.SaveData(option, err_str);
    end//if
    else Result:= False;
  end//if
  else Result:= False;
  WriteLn('SaveGame errors: '+err_str);
end;//func

constructor TSaveCallback.Create(const dat: TData);
begin
  _type:= CBT_SAVE_GAME;
  AData:= dat;
end;//construc

function TLoadCallback.Handle: Boolean;
var err_str: string;
begin
  err_str:= 'not loaded.';
  if ((option>0) and (option<65536) and (AData<>nil)) then
  begin
    Result:= AData.LoadData(option, err_str);
  end//if
  else Result:= False;
  WriteLn('LoadGame errors: '+err_str);
end;//func

constructor TLoadCallback.Create(const dat: TData);
begin
  _type:= CBT_LOAD_GAME;
  AData:= dat;
end;//construc

end.