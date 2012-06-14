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

unit ExitCallback;

interface

uses
  BasicCallback;

const
  { integer constant that identifies the type of a callback record }
  CBT_EXIT = 1;

type

  TExitCallback = class(TBasicCallback)
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

      constructor Create;
  end;//class
  PExitCallback = ^TExitCallback;

implementation

function TExitCallback.Handle: Boolean;
begin
  Result:= True;
  if option=1 then halt;
end;//func

function TExitCallback.GetType: Integer;
begin
  Result:= CBT_EXIT;
end;

constructor TExitCallback.Create;
begin
  //empty
end;//construc

end.
