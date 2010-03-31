# include "Randomizer.h"
#include <ctime>
#include <cstdlib>

Randomizer::Randomizer()
{
  srand(time(NULL));
}

Randomizer::~Randomizer()
{
  //empty
}

Randomizer& Randomizer::GetSingleton()
{
  static Randomizer Instance;
  return Instance;
}

unsigned int Randomizer::d6()
{
  const float res = rand()/float(RAND_MAX);
  return 1+ int(6*res);
}

Byte Randomizer::random(const Byte a, const Byte b)
{
  if (a==b) return a;
  const float res = rand()/float(RAND_MAX);
  if (a<b)
    return a+ Byte((b-a+1)*res);
  //else ---> b<a
  return b+ Byte((a-b+1)*res);
}

LongInt Randomizer::random(const LongInt a, const LongInt b)
{
  if (a==b) return a;
  const float res = rand()/float(RAND_MAX);
  if (a<b)
    return a+ int((b-a+1)*res);
  //else ---> b<a
  return b+ int((a-b+1)*res);
}
