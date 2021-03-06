{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2015  Dirk Stolle

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

unit DebugWriter;

interface

  { writes the given message to standard output, but only if DEBUG_CODE is
    defined.

    parameters:
        message - the debug message
  }
  procedure WriteDebugLn(message: string);

{PlaceDollarSignHereDEFINE DEBUG_CODE 1}

implementation

procedure WriteDebugLn(message: string);
begin
  {$IFDEF DEBUG_CODE}
  WriteLn(message);
  {$ENDIF}
end;

end.
