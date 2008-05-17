unit Colony;

interface

uses
  Nation, Settlement, Goods;

type
  TBuildingType = (btFort, //Einpfählung, Fort, Festung
                   btArmory, //Waffenkammer, Waffendepot, Waffenarsenal
                   btDock, //Hafenanlagen, Trockendock, Werft
                   btTownHall, //Rathaus
                   btSchool, //Schule, College, Universität
                   btWarehouse, //Lagerhaus, Lagerhauserweiterung
                   btStable, //Ställe
                   btCustomHouse, //Zollhaus
                   btPress, //Druckerei, Verlag
                   btWeaver, //Haus d. Webers, Weberei, Textilwerk
                   btTobacconist, //Haus d. Tabakhändlers, Tabakladen, Zigarrenfabrik
                   btDistiller, //Haus d. Rumbrenners, Rumbrennerei, Rumfabrik
                   btFurTrader, //Haus d. Gerbers, Gerberei, Pelzfabrik
                   btCarpenter, //Zimmerei, Sägewerk
                   btChurch, //Kirche, Kathedrale
                   btBlacksmith //Haus d. Schmieds, Schmiede, Eisenhütte
                  );

  TColony = class(TSettlement)
    public
      constructor Create(const X, Y: Integer; const ANation: PNation; const AName: string);
      destructor Destroy;
      function GetMaxLevel(const bt: TBuildingType): Byte;
      procedure ConstructNextLevel(const bt: TBuildingType);
    private
      m_Name: string;
      Store: array[TGoodType] of Word; //max. is 300, a word should do it;
      Buildings: array[TBuildingType] of Byte;
  end;//class
  PColony = ^TColony;

implementation

// **** TColony functions ****

constructor TColony.Create(const X, Y: Integer; const ANation: PNation; const AName: string);
var gt: TGoodType;
    bt: TBuildingType;
begin
  //sets position and nation
  inherited Create(X, Y, ANation);
  //set name
  m_Name:= AName;
  //set all goods to zero
  gt:= gtFood;
  while gt<High(TGoodType) do
  begin
    Store[gt]:= 0;
    gt:= Succ(gt);
  end;//while
  Store[High(TGoodType)]:= 0;
  //clear all buildings
  bt:= btFort;
  while bt<High(TBuildingType) do
  begin
    Buildings[bt]:= 0;
    bt:= Succ(bt);
  end;//while
  Buildings[High(TBuildingType)]:= 0;
  //set initial buildins
  Buildings[btTownHall]:= 1;
  Buildings[btCarpenter]:= 1;
  Buildings[btBlacksmith]:= 1;
  Buildings[btTobacconist]:= 1;
  Buildings[btWeaver]:= 1;
  Buildings[btDistiller]:= 1;
  Buildings[btFurTrader]:= 1;
end;//create

destructor TColony.Destroy;
begin
  //what shall we do?
  inherited Destroy;
end;//destruc

function TColony.GetMaxLevel(const bt: TBuildingType): Byte;
begin
  case bt of
    btTownHall, btStable, btCustomHouse: Result:= 1;
    btWarehouse, btPress, btCarpenter, btChurch: Result:= 2;
  else Result:= 3;
  end;//case
end;//func

procedure TColony.ConstructNextLevel(const bt: TBuildingType);
begin
  if (Buildings[bt]<GetMaxLevel(bt)) then
    Buildings[bt]:= Buildings[bt]+1;
end;//proc

end.