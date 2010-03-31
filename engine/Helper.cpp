#include "Helper.h"
#include <sstream>

//helper functions
std::string IntToStr(const LongInt i)
{
  std::stringstream s_str;
  s_str << i;
  return s_str.str();
}//func

TStringArr ToStringArr(const std::string& s1, const std::string& s2)
{
  TStringArr temp;
  temp.push_back(s1);
  temp.push_back(s2);
  return temp;
}//func

TStringArr ToStringArr(const std::string& s1, const std::string& s2, const std::string& s3)
{
  TStringArr temp;
  temp.push_back(s1);
  temp.push_back(s2);
  temp.push_back(s3);
  return temp;
}//func

TStringArr ToStringArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4)
{
  TStringArr temp;
  temp.push_back(s1);
  temp.push_back(s2);
  temp.push_back(s3);
  temp.push_back(s4);
  return temp;
}//func

TStringArr ToStringArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4, const std::string& s5)
{
  TStringArr temp;
  temp.push_back(s1);
  temp.push_back(s2);
  temp.push_back(s3);
  temp.push_back(s4);
  temp.push_back(s5);
  return temp;
}//func

TStringArr ToStringArr(const std::string& s1, const TStringArr& arr)
{
  TStringArr temp;
  temp.push_back(s1);
  unsigned int i;
  for (i=0; i<arr.size(); ++i)
  {
    temp.push_back(arr.at(i));
  }
  return temp;
}//func

std::string SpaceString(const Byte len)
{
  return std::string(len, ' ');
}//func

std::string Trim(const std::string& str1)
{
  if (str1.empty()) return "";

  int i, len;
  len = str1.length();
  i = 0;
  while (i<len)
  {
    if (str1.at(i)<= ' ') ++i;
    else break;
  }//while

  if (i>=len) return "";

  --len;
  while (len>=0)
  {
    if (str1.at(i)<= ' ') --len;
    else break;
  }
  return str1.substr(i, len-i+1);
}//func

std::string StretchTo59(const std::string& s1, const std::string &s2)
{
  const std::string s1_trim = Trim(s1);
  const std::string s2_trim = Trim(s2);
  if (s1_trim.length()+s2_trim.length()<59)
    return s1_trim+SpaceString(59-s1_trim.length()-s2_trim.length())+s2_trim;
  return s1_trim+" "+s2_trim;
}//func

std::string StretchTo60(const std::string& s1, const std::string &s2)
{
  const std::string s1_trim = Trim(s1);
  const std::string s2_trim = Trim(s2);
  if (s1_trim.length()+s2_trim.length()<60)
    return s1_trim+SpaceString(60-s1_trim.length()-s2_trim.length())+s2_trim;
  return s1_trim+" "+s2_trim;
}//func

LongInt Min(const LongInt a, const LongInt b)
{
  if (a<=b) return a;
  return b;
}//func

double Min(const double a, const double b)
{
  if (a<=b) return a;
  return b;
}//func
