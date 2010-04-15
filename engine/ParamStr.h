#ifndef PARAMSTR_H
#define PARAMSTR_H

#include <string>
#include <vector>

class ParamBuffer
{
  private:
    ParamBuffer();
    ParamBuffer(const ParamBuffer& op) {}
    std::vector<std::string> params;
  public:
    static ParamBuffer& GetSingleton();
    ~ParamBuffer();
    std::string ParamString(const unsigned int index) const;
    void push(const std::string& str);
}; //class

inline std::string ParamStr(const unsigned int index)
{
  return ParamBuffer::GetSingleton().ParamString(index);
}

#endif // PARAMSTR_H
