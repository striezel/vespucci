#ifndef RANDOMIZER_H
#define RANDOMIZER_H

#include "PascalTypes.h"

class Randomizer
{
  public:
    /* virtual destructor */
    virtual ~Randomizer();

    /* Singleton access function */
    static Randomizer& GetSingleton();

    /* "Throws" a six-sided die, returns a random number between 1 and 6 */
    unsigned int d6();

    /* generates a random number between a and b (including a and b) */
    Byte random(const Byte a, const Byte b);
    LongInt random(const LongInt a, const LongInt b);
  private:
    //constructor
    Randomizer();
    //empty copy constructor
    Randomizer(const Randomizer& op) {}
};//class

#endif // RANDOMIZER_H
