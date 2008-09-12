unit Callbacks;

interface

uses
  Units, Map;

const
  CBT_ANY = 0;
  CBT_EXIT = 1;
  CBT_LANDFALL = 2;

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

  TCallbackRec = record
                   option: Integer;
                   _type: LongInt;
                   case LongInt of
                     0: (Data: Pointer);
                     1: (cbExit: TExitCallback);
                     2: (Landfall: TLandfallData);
                 end;//rec

const
  cEmptyCallback: TCallbackRec =(option: 0; _type: CBT_ANY; Data: nil);

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

procedure HandleCallback(const cbRec: TCallbackRec);
begin
  case cbRec._type of
    CBT_ANY: ; //do nothing here
    CBT_EXIT: cbRec.cbExit(cbRec.option);
    CBT_LANDFALL: cbRec.Landfall.cbLandfall(cbRec.option, cbRec.Landfall.Ship,
                    cbRec.Landfall.UType, cbRec.Landfall.x, cbRec.Landfall.y,
                    cbRec.Landfall.AMap);
  end;//case
end;//proc

end.
