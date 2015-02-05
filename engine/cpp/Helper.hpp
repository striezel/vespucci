/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010  Dirk Stolle

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ***************************************************************************
*/

#ifndef HELPER_HPP
#define HELPER_HPP

#include "PascalTypes.hpp"
#include <string>
#include <vector>

/* *******
  **** unit Helper
  ****
  **** purpose: holds some utility functions, constants and types that are used
  ****          by different other units
  *******
*/

/* constants for quicker construction of strings in function SpaceString() */
const std::string cSpace01 = " ";
const std::string cSpace02 = "  ";
const std::string cSpace04 = "    ";
const std::string cSpace08 = "        ";
const std::string cSpace16 = cSpace08+cSpace08;
const std::string cSpace32 = cSpace16+cSpace16;
const std::string cSpace60 = "                                                            ";
const std::string cSpace64 = cSpace32+cSpace32;
const std::string cSpace128 = cSpace64+cSpace64;


/* type to represent an array of (short, i.e. <255 characters) strings */
typedef std::vector<std::string> TShortStrArr;

/* returns the string representation of an integer value

   parameters:
       i - the integer value

   remarks:
       There is a function with the same name and for the same purpose within
       a certain Pascal system unit, but I use that function to avoid that unit
       in the uses-clause of other units (because IntToStr() is the only function
       from that system unit that is needed).
*/
std::string IntToStr(const LongInt i);

/* returns the length that the string representation of the given integer
   would have

   parameters:
       i - the integer value
*/
Byte IntegerLength(const int i);

/* returns a string array with the two specified elements

   parameters:
       see overloaded version of function below
*/
TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2);

/* returns a string array with the three specified elements

   parameters:
       see overloaded version of function below
*/
TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2, const std::string& s3);

/* returns a string array with the four specified elements

   parameters:
       see overloaded version of function below
*/
TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4);

/* returns a string array with the five specified elements

   parameters:
       s1 - first string in the returned array
       s2 - second string in the returned array
       s3 - third string in the returned array
       s4 - fourth string in the returned array
       s5 - fifth string in the returned array
*/
TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4, const std::string& s5);

/* returns a string array that is constructed by the given string and the array

   parameters:
       s1  - first string in the returned array
       arr - array that holds the rest of the returned array's elements
*/
TShortStrArr ToShortStrArr(const std::string& s1, const TShortStrArr& arr);

/* returns a string that entirely consists of spaces

    parameters:
        len - length of the string (=number of spaces)
*/
std::string SpaceString(const Byte len);

/* "trims" a string, i.e. deletes all leading and trailing spaces, and returns
   the trimmed string

   parameters:
       str1 - the string that has to be trimmed

   remarks:
       See remarks for IntToStr(), these apply here, too.
*/
std::string Trim(const std::string& str1);

/* "stretches" the concatenation of two strings to 59 characters, i.e.
   concatenates the first and second string, and puts enough spaces between
   them so that the result is exactly 59 characters long

   parameters:
       s1 - first part of that string
       s2 - last part of that string

   remarks:
       If the concatenation of s1 and s2 is already longer than 59 characters,
       then only one single space character is put between them.
*/
std::string StretchTo59(const std::string& s1, const std::string& s2);

/* "stretches" the concatenation of two strings to 60 characters, i.e.
   concatenates the first and second string, and puts enough spaces between
   them so that the result is exactly 60 characters long

   parameters:
       s1 - first part of that string
       s2 - last part of that string

   remarks:
       If the concatenation of s1 and s2 is already longer than 60 characters,
       then only one single space character is put between them.
*/
std::string StretchTo60(const std::string& s1, const std::string& s2);

/* returns the smaller value of the given parameters */
int Min(const int a, const int b);
double Min(const double a, const double b);

#endif // HELPER_HPP
