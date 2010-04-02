#include "Tribe.h"

// **** TTribe functions ****

TTribe::TTribe(const LongInt X, const LongInt Y, const LongInt ANation, const TUnitType KnownFor)
  : TSettlement(X, Y, ANation)
{
  //sets Nation and position
  //inherited Create(X, Y, ANation);
  LongInt i;
  for (i= cMin_Nations; i<=cMaxEuropean; ++i)
    m_HasTought[i] = false;
  //set the unit type the tribe is known for
  m_KnownFor = KnownFor;
}//construc

void TTribe::Teach(TUnit* AUnit)
{
  //only servants or free colonists can learn something
  if ((AUnit->GetType()!=utServant) and (AUnit->GetType()!=utColonist))
  {
    if ((AUnit->GetType()>=utFarmer) and (AUnit->GetType()<=utPioneer))
    {
      //Show message: "It's a pleasure to see a skilled <whatever>..."
      // Still ToDo();
    }
    else
    {
      //Show message, informing player about inappropriate unit type
      // Still ToDo();
    }//else
  }
  //only European Units can learn from Indians
  else if ((AUnit->GetNation()>=cMin_Nations) and (AUnit->GetNation()<=cMaxEuropean))
  {
    //check, if Indians already did teach that nation's units a new skill
    if (m_HasTought[AUnit->GetNation()])
    {
      //Show message, something like "Ihr habt schon etwas von uns gelernt"
      // Still ToDo();
    }//if
    else
    {
      //actually teach the unit something
      AUnit->ChangeType(m_KnownFor);
      m_HasTought[AUnit->GetNation()] = true;
    }//else
  }//if
}//proc
