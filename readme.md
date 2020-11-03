# Vespucci

Vespucci is an open-source remake of the old Colonization classic, a game
similar to Civilization. It is still in an early stage of development.

## Building from source

### Prerequisites

To build Vespucci from source you need an Object Pascal / Delphi compiler like
[FreePascal](https://www.freepascal.org/), and the GLUT or freeglut library.

It also helps to have Git, a distributed version control system, on your build
system to get the latest source code directly from the Git repository.

All of that can usually be installed be typing

    apt-get install fp-compiler fp-units-gfx freeglut3 git 

or

    yum install epel-release # needed for fpc (Free Pascal compiler) on CentOS 8
                             # ... and maybe on other Red Hat-based distros, too
    yum install fpc freeglut git

into a root terminal.

### Getting the source code

Get the source directly from Git by cloning the Git repository and change to
the directory after the repository is completely cloned:

    git clone https://gitlab.com/striezel/vespucci.git
    cd vespucci

That's it, you should now have the current source code on your machine.

### Build process

The build process is relatively easy, because you just have to invoke the
FreePascal compiler. Starting in the root directory of the source, you can do
the following steps:

    cd engine
    fpc -S2 vespucci.dpr

## Usage

To start the compiled program, type `./vespucci` into the terminal.

Note that the game currently only has a German GUI, so you might want to wait
for the English translation, or - even better - help with that.

## Copyright and Licensing

Copyright 2008-2020  Dirk Stolle

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
