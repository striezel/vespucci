unit Tribe;

interface

uses
  Settlement, Nation, Units;

const
  cTribeLocationsAmerica: array [0..12] of record
                     Nation: LongInt;
                     x,y: Byte;
                   end //rec
    =(
      (Nation: cNationAztec ; x: 11; y: 23;),
      (Nation: cNationAztec ; x: 16; y: 26;),
      (Nation: cNationAztec ; x: 23; y: 27;),
      (Nation: cNationAztec ; x: 25; y: 33;),
      
      (Nation: cNationInca ; x: 26; y: 42;),
      (Nation: cNationInca ; x: 34; y: 59;),
      (Nation: cNationInca ; x: 34; y: 65;),
      (Nation: cNationInca ; x: 35; y: 51;),
      (Nation: cNationInca ; x: 36; y: 55;),
      
      (Nation: cNationCherokee ; x: 20; y: 20;),
      (Nation: cNationCherokee ; x: 21; y: 17;),
      (Nation: cNationCherokee ; x: 24; y: 19;),
      (Nation: cNationCherokee ; x: 27; y: 22;)
    );
    

type
  TTribe = class(TSettlement)
    public
      constructor Create(const X, Y: Integer; const ANation: LongInt; KnownFor: TUnitType);
      procedure Teach(var AUnit: TUnit);
    private
      m_KnownFor: TUnitType;
      m_HasTought: array[cMin_Nations..cMaxEuropean] of Boolean;
  end;//class
  PTribe = ^TTribe;

implementation

// **** TTribe functions ****

constructor TTribe.Create(const X, Y: Integer; const ANation: LongInt; KnownFor: TUnitType);
var i: LongInt;
begin
  //sets Nation and position
  inherited Create(X, Y, ANation);
  for i:= cMin_Nations to cMaxEuropean do
    m_HasTought[i]:= False;
  //set the unit type the tribe is known for
  m_KnownFor:= KnownFor;
end;//construc

procedure TTribe.Teach(var AUnit: TUnit);
begin
  //only servants or free colonists can learn something
  if ((AUnit.GetType<>utServant) and (AUnit.GetType<>utColonist)) then
  begin
    if AUnit.GetType in [utFarmer..utPioneer] then
    begin
      //Show message: "It's a pleasure to see a skilled <whatever>..."
      // Still ToDo();
    end
    else begin
      //Show message, informing player about inappropriate unit type
      // Still ToDo();
    end;//else
  end
  //only European Units can learn from Indians
  else if (AUnit.GetNation in [cMin_Nations..cMaxEuropean]) then
  begin
    //check, if Indians already did teach that nation's units a new skill
    if m_HasTought[AUnit.GetNation] then
    begin
      //Show message, something like "Ihr habt schon etwas von uns gelernt"
      // Still ToDo();
    end//if
    else begin
      //actually teach the unit something
      AUnit.ChangeType(m_KnownFor);
      m_HasTought[AUnit.GetNation]:= True;
    end;//else
  end;//if
end;//proc

end.