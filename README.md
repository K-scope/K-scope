# README

K-scope is a Fortran source code analysis tool with graphical user interface
that visualizes program structures for Fortran 90 and FORTRAN 77 source code.
This tool simply visualizes call tree from AST based on compiler's static
analysis. The call tree consists of loops, branches, and procedure-calls as
candidates of hot-spot in an application. Such a source code visualization
is suitable for source code reading for engineers who improve application
performance in high performance computing. This tool will be expected to
understand program structure before performance tuning.

K-scope is developed by RIKEN AICS Software Development Team, and is distributed
as open-source software. The latest version and documents are available in the
following download site.

http://www.aics.riken.jp/ungi/soft/kscope/

## Preliminaries

K-scope requires Java version 1.8 or later. If you build a binary package
from the K-scope source codes, please download JDK from the Oracle site and
install it on your environment. 

Except for the above case (for example, developer), JRE is enough for ordinary use. 

http://www.oracle.com/technetwork/java/javase/downloads/index.html

In addition, K-scope uses intermediate codes (= AST) created by front end of
the Omni XcalableMP compiler. Please download the compiler suite from
following URL.

http://omni-compiler.org/index.html

If you can access a remote server installed with Omni XcalableMP (XMP)
compiler (For example, front-end of the K computer is suitable environment
to make intermediate codes with XMP), you can build intermediate codes
on the server remotely via SSH connection by SSHconnect.jar. This software
is an utility for remote command execution with automatic transfer.
It is developed by RIKEN AICS HPC Usability Team in collaboration to enhance
the K-scope's feature, and is bundled with the binary package of K-scope.

## Compile and Run

This software is written by pure Java to improve portability.
We provide two-type packages: jar-executable package and source code package
in our site. Especially, the source cord package includes all source codes
necessary for modify and compiling. (The source codes includes Japanese
comments by UTF-8.)

The source code package includes build.xml to compiler with Apache ant.
Also compiling is simple and easy.

  $ ant

After the compiling, you may obtain jar-executable including classes.
Run is simple as follow.

  $ java -jar kscope.jar

At the time of startup, K-scope requires specific folders for properties.
If K-scope cannot find those folders, it may terminate abnormally.

Tips on usage) "-Duser.language" VM-option is language selector, English(en) and Japanese(ja).

  $ java -jar -Duser.language=en kscope.jar ## for english mode

  $ java -jar -Duser.language=ja kscope.jar ## for japanese mode

## License

 K-scope
 Copyright 2012-2015 RIKEN, Japan

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
