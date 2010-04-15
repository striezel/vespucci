#ifndef PASCALFUNCTIONS_H
#define PASCALFUNCTIONS_H

#include "PascalTypes.h"
#include <string>

int Ord_bool(const bool b);

int Ord_char(const char c);

LongInt sqr(const LongInt i);

bool DirectoryExists(const std::string& Dir);

bool ForceDirectories(const std::string& Dir);

bool FileExists(const std::string& FileName);

#endif

