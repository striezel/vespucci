{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2011  Thoronador

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

unit RenameColonyCallback;

interface

uses
  BasicCallback, Colony;

const
  { integer constant that identifies the type of a callback record }
  CBT_RENAME_COLONY = 10;

type

  TRenameColonyCallback = class(TBasicCallback)
    public
      AColony: TColony;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const col: TColony);
  end;//class
  PRenameColonyCallback = ^TRenameColonyCallback;

implementation

function TRenameColonyCallback.Handle: Boolean;
begin
  if (AColony=nil) then
  begin
    Result:= False;
    Exit;
  end;//if
  AColony.SetName(inputText);
  Result:= True;
end;//func

constructor TRenameColonyCallback.Create(const col: TColony);
begin
  _type:= CBT_RENAME_COLONY;
  AColony:= col;
end;//construc

end.