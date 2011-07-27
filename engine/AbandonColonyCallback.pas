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

unit AbandonColonyCallback;

interface

uses
  BasicCallback, Colony, Data;

const
  { integer constant that identifies the type of a callback record }
  CBT_ABANDON_COLONY = 11;

type

  TAbandonColonyCallback = class(TBasicCallback)
    public
      AColony: TColony;
      AData: TData;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const col: TColony; dat: TData);
  end;//class
  PAbandonColonyCallback = ^TAbandonColonyCallback;

implementation

function TAbandonColonyCallback.Handle: Boolean;
begin
  Result:= False;
  if ((AColony=nil) or (AData=nil)) then Exit;
  if option=1 then
  begin
    Result:= AData.DeleteColony(AColony.GetPosX, AColony.GetPosY);
  end;//if
end;//func

constructor TAbandonColonyCallback.Create(const col: TColony; dat: TData);
begin
  _type:= CBT_ABANDON_COLONY;
  AColony:= col;
  AData:= dat;
end;//construc

end.