#include "PascalFunctions.h"
#include <fstream>
#include <sys/stat.h>

int Ord_bool(const bool b)
{
  if (b) return 1;
  return 0;
}

int Ord_char(const char c)
{
  return static_cast<int> (c);
}

LongInt sqr(const LongInt i)
{
  return i*i;
}

bool DirectoryExists(const std::string& Dir)
{
  struct stat buffer;
  if (stat(Dir.c_str(), &buffer)==0)
  {
    return (buffer.st_mode==S_IFDIR);
  };
  return false;
}

bool CreateDirectory(const std::string& Dir)
{
  return (mkdir(Dir.c_str(), S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)==0);
}

bool ForceDirectories(const std::string& Dir)
{
  if (DirectoryExists(Dir)) return true;
  if (Dir=="") return false;

  #ifdef Win32
  const char path_delim = '\\';
  #else
  const char path_delim = '/';
  #endif

  int i=Dir.size();
  while (i>=0)
  {
    if (Dir[i]==path_delim) break;
    --i;
  }
  if (i<0) return CreateDirectory(Dir);
  if (i==0) return true; //assume existence of root directory ;)
  if (ForceDirectories(Dir.substr(0,i)))
  {
    return CreateDirectory(Dir);
  }
  return false;
}

bool FileExists(const std::string& FileName)
{
  std::ifstream fs;
  fs.open(FileName.c_str());
  if (!fs) return false;
  fs.close();
  return true;
}
