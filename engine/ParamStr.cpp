#include "ParamStr.h"

ParamBuffer::ParamBuffer()
{
  params.clear();
}

ParamBuffer& ParamBuffer::GetSingleton()
{
  static ParamBuffer Instance;
  return Instance;
}

ParamBuffer::~ParamBuffer()
{
  params.clear();
}

std::string ParamBuffer::ParamString(const unsigned int index) const
{
  if (index<params.size()) return params[index];
  return "";
}

void ParamBuffer::push(const std::string& str)
{
  params.push_back(str);
}
