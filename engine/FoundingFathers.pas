unit FoundingFathers;

interface

type
  { enumeration type for founding fathers }
  TFoundingFathers = (//trade
                      ffSmith, ffFugger, ffMinuit, ffStuyvesant, ffDeWitt,
                      //exploration
                      ffCoronado, ffHudson, ffLaSalle, ffMagellan, ffDeSoto,
                      //military
                      ffCortes, ffDrake, ffJones, ffRevere, ffWashington,
                      //political
                      ffBolivar, ffFranklin, ffJefferson, ffPaine, ffPocahontas,
                      //religious
                      ffBrebeuf, ffBrewster, ffLasCasas, ffPenn, ffSepulveda,
                      //none
                      ffNone);

  //array type
  TFoundingFatherArray = array[0..4] of TFoundingFathers;
  
  { enumeration type for area of founding fathers }
  TFoundingFatherType = (fftTrade, fftExploration, fftMilitary, fftPolitical,
                         fftReligious);
  TFoundingType = TFoundingFatherType; //alias to ease coding a bit

  { returns the type/ area a founding father belongs to

    parameters:
        ff - enumeration value that identifies the founding father
  }
  function GetFoundingFatherType(const ff: TFoundingFathers): TFoundingType;

  { returns the amount of liberty bells that are required for the n-th founding
    father to join the congress

    parameters:
        n - number of the next founding father (1-based, that is)
  }
  function GetRequiredLibertyBells(const n: Byte): Word;

implementation

function GetFoundingFatherType(const ff: TFoundingFathers): TFoundingType;
begin
  case ff of
    ffSmith..ffDeWitt: Result:= fftTrade;
    ffCoronado..ffDeSoto: Result:= fftExploration;
    ffCortes..ffWashington: Result:= fftMilitary;
    ffBolivar..ffPocahontas: Result:= fftPolitical;
  else Result:= fftReligious;
  end;//case
end;//func

function GetRequiredLibertyBells(const n: Byte): Word;
begin
  if n>0 then Result:= 80*n-50 //usual formula
  else Result:= 0; // prevents negative result in case of invalid parameter
end;//func

end.
