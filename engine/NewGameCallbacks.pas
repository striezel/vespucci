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
  BasicCallback, Data, Map;

const
  { integer constant that identifies the type of a callback record }
  CBT_NEW_GAME = 16;
  CBT_LANDMASS_SELECTION = 17;
  CBT_TEMPERATURE_SELECTION = 18;
  CBT_CLIMATE_SELECTION = 19;
  CBT_NATION_SELECTION = 20;
  CBT_PLAYER_NAME_SELECTION = 21;

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


  TLandmassSelectionCallback = class(TNewGameCallback)
    protected
      m_America: Boolean;
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData; const America: Boolean);
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

      constructor Create(const dat: TData; const America: Boolean; const Landmass: Single);
  end;//class


  TClimateSelectionCallback = class(TTemperatureSelectionCallback)
    protected
      m_Temperature: TTemperatureType;
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData; const America: Boolean; const Landmass: Single; const temp: TTemperatureType);
  end;//class


  TNationSelectionCallback = class(TClimateSelectionCallback)
    protected
      m_Climate: TClimateType;
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData; const America: Boolean; const Landmass: Single; const temp: TTemperatureType; const Climate: TClimateType);
  end;//class


  TPlayerNameSelectionCallback = class(TNationSelectionCallback)
    protected
      m_Nation: Integer;
    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData; const America: Boolean; const Landmass: Single; const temp: TTemperatureType; const Climate: TClimateType; const Nat: Integer);
  end;//class

implementation

uses
  Helper, Language, MessageSystem, Nation, EuropeanNation;

function TNewGameCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
begin
  Result:= true; //set result to true, always
  if option=1 then
  begin
    //America map was chosen, go on with nation selection
    temp_cb:= TNationSelectionCallback.Create(AData, true, 0.75, ttModerate, ctNormal);
    msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsEuropeanPower),
          ToShortStrArr(AData.GetLang.GetNewGameString(ngsEuropeanPowerEngland),
          AData.GetLang.GetNewGameString(ngsEuropeanPowerFrance),
          AData.GetLang.GetNewGameString(ngsEuropeanPowerSpain),
          AData.GetLang.GetNewGameString(ngsEuropeanPowerHolland)), temp_cb);
  end
  else begin
    //random map was chosen, let player set parameters
    Result:= false;
    temp_cb:= TLandmassSelectionCallback.Create(AData, false);
    msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsLandmass),
            ToShortStrArr(AData.GetLang.GetNewGameString(ngsLandmassSmall),
            AData.GetLang.GetNewGameString(ngsLandmassMedium),
            AData.GetLang.GetNewGameString(ngsLandmassLarge)), temp_cb);
  end;//else branch
end;//func

constructor TNewGameCallback.Create(const dat: TData);
begin
  _type:= CBT_NEW_GAME;
  AData:= dat;
end;//construc

{ **** TLandmassSelectionCallback functions **** }

constructor TLandmassSelectionCallback.Create(const dat: TData; const America: Boolean);
begin
  _type:= CBT_LANDMASS_SELECTION;
  AData:= dat;
  m_America:= America;
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
  temp_cb:= TTemperatureSelectionCallback.Create(AData, m_America, landmass);
  msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsTemperature),
          ToShortStrArr(AData.GetLang.GetNewGameString(ngsTemperatureCool),
          AData.GetLang.GetNewGameString(ngsTemperatureModerate),
          AData.GetLang.GetNewGameString(ngsTemperatureWarm)), temp_cb);
end;//func

{ **** TTemperatureSelectionCallback functions **** }

constructor TTemperatureSelectionCallback.Create(const dat: TData; const America: Boolean; const Landmass: Single);
begin
  _type:= CBT_TEMPERATURE_SELECTION;
  AData:= dat;
  m_America:= America;
  m_Landmass:= Landmass;
end;//construc

function TTemperatureSelectionCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
    temperature: TTemperatureType;
begin
  Result:= true; //always successful
  case option of
    0: temperature:= ttCool;
    1: temperature:= ttModerate;
    2: temperature:= ttWarm;
  else
    temperature:= ttModerate; //should never get here
  end;//case
  temp_cb:= TClimateSelectionCallback.Create(AData, m_America, m_Landmass, temperature);
  msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsClimate),
          ToShortStrArr(AData.GetLang.GetNewGameString(ngsClimateDry),
          AData.GetLang.GetNewGameString(ngsClimateNormal),
          AData.GetLang.GetNewGameString(ngsClimateWet)), temp_cb);
end;//func

{ **** TClimateSelectionCallback functions **** }

constructor TClimateSelectionCallback.Create(const dat: TData; const America: Boolean; const Landmass: Single; const temp: TTemperatureType);
begin
  _type:= CBT_CLIMATE_SELECTION;
  AData:= dat;
  m_America:= America;
  m_Landmass:= Landmass;
  m_Temperature:= temp;
end;//construc

function TClimateSelectionCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
    climate: TClimateType;
begin
  case option of
    0: climate:= ctDry;
    1: climate:= ctNormal;
    2: climate:= ctWet;
  else
    climate:= ctNormal; //should never get here
  end;//case
  temp_cb:= TNationSelectionCallback.Create(AData, m_America, m_Landmass, m_Temperature, climate);
  msg.AddMessageOptions(AData.GetLang.GetNewGameString(ngsEuropeanPower),
          ToShortStrArr(AData.GetLang.GetNewGameString(ngsEuropeanPowerEngland),
          AData.GetLang.GetNewGameString(ngsEuropeanPowerFrance),
          AData.GetLang.GetNewGameString(ngsEuropeanPowerSpain),
          AData.GetLang.GetNewGameString(ngsEuropeanPowerHolland)), temp_cb);
  Result:= true; //always successful
end;//func

{ **** TNationSelectionCallback functions **** }

constructor TNationSelectionCallback.Create(const dat: TData; const America: Boolean; const Landmass: Single; const temp: TTemperatureType; const Climate: TClimateType);
begin
  _type:= CBT_NATION_SELECTION;
  AData:= dat;
  m_America:= America;
  m_Landmass:= Landmass;
  m_Temperature:= temp;
  m_Climate:= Climate;
end;//construc

function TNationSelectionCallback.Handle: Boolean;
var temp_cb: TBasicCallback;
    nat: Integer;
begin
  Result:= true; //always successful
  case option of
    0: nat:= cNationEngland;
    1: nat:= cNationFrance;
    2: nat:= cNationSpain;
    3: nat:= cNationHolland;
  else
    //should never happen, but better be save than sorry
    nat:= cNationEngland;
  end;//case
  temp_cb:= TPlayerNameSelectionCallback.Create(AData, m_America, m_Landmass, m_Temperature, m_Climate, nat);
  msg.AddMessageInput('Enter your name:', 'Name:', AData.GetLang.GetDefaultLeaderName(nat), temp_cb);
end;//func

{ **** TPlayerNameSelectionCallback functions **** }

constructor TPlayerNameSelectionCallback.Create(const dat: TData; const America: Boolean; const Landmass: Single; const temp: TTemperatureType; const Climate: TClimateType; const Nat: Integer);
begin
  _type:= CBT_PLAYER_NAME_SELECTION;
  AData:= dat;
  m_America:= America;
  m_Landmass:= Landmass;
  m_Temperature:= temp;
  m_Climate:= Climate;
  m_Nation:= Nat;
end;//construc

function TPlayerNameSelectionCallback.Handle: Boolean;
begin
  inputText:= trim(inputText);
  if (inputText='') then inputText:= AData.GetLang.GetDefaultLeaderName(m_Nation);
  //TODO: make use of climate and temperature selection and pass it to the map
  // generation function

  //finally start the new game
  AData.StartNewGame(m_America, m_Nation, m_Landmass, m_Temperature, m_Climate);
  (AData.GetNation(m_Nation) as TEuropeanNation).ChangeLeaderName(inputText);
  Result:= true; //always successful
end;//func

end.