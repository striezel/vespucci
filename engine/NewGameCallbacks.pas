{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2011  Thoronador

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

unit NewGameCallbacks;

interface

uses
  BasicCallback, Data;

const
  { integer constant that identifies the type of a callback record }
  CBT_NEW_GAME = 16;
  CBT_LANDMASS_SELECTION = 17;
  CBT_TEMPERATURE_SELECTION = 18;
  CBT_CLIMATE_SELECTION = 19;

type
  TNewGameCallback = class(TBasicCallback)
    protected
      AData: TData;
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData);
  end;//class
  PNewGameCallback = ^TNewGameCallback;


  TLandmassSelectionCallback = class(TNewGameCallback)
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData);
  end;//class


  TTemperatureSelectionCallback = class(TLandmassSelectionCallback)
    protected
      m_Landmass: Single;
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData; const Landmass: Single);
  end;//class


  TClimateSelectionCallback = class(TTemperatureSelectionCallback)
    protected
      m_Temperature: Integer;
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData; const Landmass: Single; const temp: Integer);
  end;//class

implementation

uses
  Helper, Language, MessageSystem, Nation;

function TNewGameCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
begin
  Result:= true; //set result to true, always
  if option=1 then
  begin
    //America map was chosen
    AData.StartNewGame(true, cNationEngland, 1.0);
    Result:= true;
  end
  else begin
    //random map was chosen
    Result:= false;
    temp_cb:= TLandmassSelectionCallback.Create(AData);
    msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsLandmass),
            ToShortStrArr(AData.GetLang.GetNewGameString(ngsLandmassSmall),
            AData.GetLang.GetNewGameString(ngsLandmassMedium),
            AData.GetLang.GetNewGameString(ngsLandmassLarge)), temp_cb);
    //TODO: not properly implemented yet
  end;//else branch
end;//func

constructor TNewGameCallback.Create(const dat: TData);
begin
  _type:= CBT_NEW_GAME;
  AData:= dat;
end;//construc

{ **** TLandmassSelectionCallback functions **** }

constructor TLandmassSelectionCallback.Create(const dat: TData);
begin
  _type:= CBT_LANDMASS_SELECTION;
  AData:= dat;
end;//construc

function TLandmassSelectionCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
    landmass: Single;
begin
  Result:= true; //always successful
  case option of
    0: landmass:= 0.25; //small landmass chosen
    1: landmass:= 0.5; //medium landmass chosen
    2: landmass:= 0.75; //large landmass chosen
  else
    landmass:= 0.75;
  end;//case
  temp_cb:= TTemperatureSelectionCallback.Create(AData, landmass);
  msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsTemperature),
          ToShortStrArr(AData.GetLang.GetNewGameString(ngsTemperatureCool),
          AData.GetLang.GetNewGameString(ngsTemperatureModerate),
          AData.GetLang.GetNewGameString(ngsTemperatureWarm)), temp_cb);
end;//func

{ **** TTemperatureSelectionCallback functions **** }

constructor TTemperatureSelectionCallback.Create(const dat: TData; const Landmass: Single);
begin
  _type:= CBT_TEMPERATURE_SELECTION;
  AData:= dat;
  m_Landmass:= Landmass;
end;//construc

function TTemperatureSelectionCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
    temperature: Integer;
begin
  Result:= true; //always successful
  temperature:= option; //not complete/ used yet
  temp_cb:= TClimateSelectionCallback.Create(AData, m_Landmass, temperature);
  msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsClimate),
          ToShortStrArr(AData.GetLang.GetNewGameString(ngsClimateDry),
          AData.GetLang.GetNewGameString(ngsClimateNormal),
          AData.GetLang.GetNewGameString(ngsClimateWet)), temp_cb);
end;//func

{ **** TClimateSelectionCallback functions **** }

constructor TClimateSelectionCallback.Create(const dat: TData; const Landmass: Single; const temp: Integer);
begin
  _type:= CBT_CLIMATE_SELECTION;
  AData:= dat;
  m_Landmass:= Landmass;
  m_Temperature:= temp;
end;//construc

function TClimateSelectionCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
    climate: Integer;
begin
  climate:= option; //not used yet
  //TODO: make use of climate and temperature selection and pass it to the map
  // generation function

  //finally start the new game
  AData.StartNewGame(false, cNationEngland, m_Landmass);
  Result:= true; //always successful
end;//func

end.