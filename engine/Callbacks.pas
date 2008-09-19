unit Callbacks;

interface

uses
  Units, Map, Data;

const
  CBT_ANY = 0;
  CBT_EXIT = 1;
  CBT_LANDFALL = 2;
  CBT_BUILD_COLONY = 3;
  CBT_SAVE_GAME = 4;
  CBT_LOAD_GAME = 5;

type
  TExitCallback = procedure (const option: Integer);
  TLandfallCallback = function (const option: Integer; AShip: TUnit; const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;

  TLandfallData = record
                    cbLandfall: TLandfallCallback;
                    Ship: TUnit;
                    UType: TUnitType;
                    x,y: Byte;
                    AMap: TMap;
                  end;//rec
  TBuildColonyData = record
                        x,y: Byte;
                        //ColName: ShortString; //delivered through input text
                        num_nation: Byte;
                        founder: TUnit;
                        AMap: TMap;
                        AData: TData;
                      end;//rec
  TSaveGameData = record
                    AData: TData;
                    AMap: TMap;
                  end;//rec
  TLoadGameData = record
                    AData: TData;
                    AMap: TMap;
                  end;//rec

  TCallbackRec = record
                   option: Integer;
                   inputText: ShortString;
                   _type: LongInt;
                   case LongInt of
                     0: (Data: Pointer);
                     1: (cbExit: TExitCallback);
                     2: (Landfall: TLandfallData);
                     3: (BuildColony: TBuildColonyData);
                     4: (SaveGame: TSaveGameData);
                     5: (LoadGame: TLoadGameData);
                 end;//rec

const
  cEmptyCallback: TCallbackRec =(option: 0; inputText: ''; _type: CBT_ANY; Data: nil);

  procedure CBF_Exit(const option: Integer);
  function CBF_Landfall(const option: Integer; AShip: TUnit; const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;

  procedure HandleCallback(const cbRec: TCallbackRec);

implementation

procedure CBF_Exit(const option: Integer);
begin
  if option=1 then halt;
end;//func

function CBF_Landfall(const option: Integer; AShip: TUnit; const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;
begin
  if ((option=1) and (AShip<>nil)) then Result:= AShip.UnloadUnit(AType, x,y, AMap)
  else Result:= False;
end;//func

function CBF_BuildColony(const x,y: Byte; const num_nation: Byte; ColName: ShortString; founder: TUnit; AMap: TMap; AData: TData): Boolean;
begin
  Result:= False;
  if ((ColName='') or (founder=nil) or (AMap=nil) or (AData=nil) or (x>=cMap_X-1)
      or (y>=cMap_Y-1) or (x=0) or (y=0)) then Exit;
  if founder.WarpToXY(x,y, AMap) then
  begin
    AData.NewColony(x,y, num_nation, ColName);
    founder.SetLocation(ulInColony);
    AMap.tiles[x,y].CreateRoad;
    Result:= True;
  end;//if
end;//func

function CBF_SaveGame(const option: Integer; AMap: TMap; AData: TData): Boolean;
var err_str: string;
begin
  err_str:= 'not saved.';
  if (option>0) and (option<65536) then
  begin
    if ((AMap<>nil) and (AData<>nil)) then
    begin
      Result:= AData.SaveData(option, AMap, err_str);
    end//if
    else Result:= False;
  end//if
  else Result:= False;
  WriteLn('SaveGame errors: '+err_str);
end;//func

procedure HandleCallback(const cbRec: TCallbackRec);
begin
  case cbRec._type of
    CBT_ANY: ; //do nothing here
    CBT_EXIT: cbRec.cbExit(cbRec.option);
    CBT_LANDFALL: cbRec.Landfall.cbLandfall(cbRec.option, cbRec.Landfall.Ship,
                    cbRec.Landfall.UType, cbRec.Landfall.x, cbRec.Landfall.y,
                    cbRec.Landfall.AMap);
    CBT_BUILD_COLONY: CBF_BuildColony(cbRec.BuildColony.x, cbRec.BuildColony.y,
                        cbRec.BuildColony.num_nation, cbRec.inputText,
                        cbRec.BuildColony.founder, cbRec.BuildColony.AMap,
                        cbRec.BuildColony.AData);
    CBT_SAVE_GAME: CBF_SaveGame(cbRec.option, cbRec.SaveGame.AMap, cbRec.SaveGame.AData);
    CBT_LOAD_GAME: ; //not implemented yet
  end;//case
end;//proc

end.
