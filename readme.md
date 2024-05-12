# Vespucci

Vespucci is an open-source remake of the old Colonization classic, a game
similar to Civilization. It is still in an early stage of development.

## Available releases

Check out the [release page](https://github.com/striezel/vespucci/releases) to
download the latest available release.

Currently, only binaries for Windows are provided. However, the Windows binary
also works on Linux and macOS with the help of the
[Wine compatibility layer](https://www.winehq.org/).

## Build status

* GitLab:
[![GitLab pipeline status](https://gitlab.com/striezel/vespucci/badges/master/pipeline.svg)](https://gitlab.com/striezel/vespucci/-/pipelines)
* GitHub:
[![GitHub CI status](https://github.com/striezel/vespucci/workflows/GitHub-CI/badge.svg)](https://github.com/striezel/vespucci/actions)

## Building from source

### Prerequisites

To build Vespucci from source you need an Object Pascal / Delphi compiler like
[Free Pascal](https://www.freepascal.org/), and the GLUT or freeglut library.

It also helps to have Git, a distributed version control system, on your build
system to get the latest source code directly from the Git repository.

All of that can usually be installed by typing

    # Debian 11 and earlier versions
    apt-get install fp-compiler fp-units-gfx freeglut3 git

or

    # Debian 12
    apt-get install fp-compiler fp-units-gfx libglut3.12 git

or

    yum install epel-release # needed for fpc (Free Pascal compiler) on Rocky Linux 9
                             # ... and maybe on other Red Hat-based distros, too
    yum install fpc freeglut git glibc-devel

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

## History of changes

A changelog is provided as [separate file](./changelog.md).

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
