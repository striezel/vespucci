#ifndef HELPER_H
#define HELPER_H

#include <string>
#include <vector>
#include "PascalTypes.h"

const std::string cSpace60 = "                                                            ";

typedef std::vector<std::string> TStringArr;

std::string IntToStr(const LongInt i);
TStringArr ToStringArr(const std::string& s1, const std::string& s2);
TStringArr ToStringArr(const std::string& s1, const std::string& s2, const std::string& s3);
TStringArr ToStringArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4);
TStringArr ToStringArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4, const std::string& s5);
TStringArr ToStringArr(const std::string& s1, const TStringArr& arr);
std::string SpaceString(const Byte len);
std::string Trim(const std::string& str1);
std::string StretchTo59(const std::string& s1, const std::string& s2);
std::string StretchTo60(const std::string& s1, const std::string& s2);


LongInt Min(const LongInt a, const LongInt b);
double Min(const double a, const double b);

#endif //HELPER_H

