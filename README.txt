README
======

K-scope is a source code analysis tool with graphical user interface that
visualizes program structures of Fortran 90 and FORTRAN 77 source code.
This software is build by pure java.

The latest version and documents are available in the following download site.
http://www.aics.riken.jp/ungi/soft/kscope/

Preliminaries
========

First of all, K-scope requires JDK7 or later. Please download from Oracle site.
http://www.oracle.com/technetwork/java/javase/downloads/index.html

In addition, this tool uses intermediate codes by front end of the XcalableMP compiler.
Please download and compile the F_Front program. 
http://www.hpcs.cs.tsukuba.ac.jp/omni-compiler/xcalablemp/download.html

Compile and Run
===========

We provides two-type packages: jar-executable package and all source codes in our site.
Therefore you can easily modify and compiling from source codes.
In that case, we recommend IDE enviroments such as Eclipse or NetBeans.
NOTICE) The source codes includes Japanese comments by UTF-8.

After the compiling, you may obtain jar-executable or classes.
Run is simple as follows.

  $ java -jar kscope.jar

K-scope must require folders for properties and resources.
If the program cannot find the folders, it may terminate abnormally.

In the normally process, you may obtained start screen after you enter the commnad.

Tips on usage) "-Duser.language" VM-option is language selecter, English(en) and Japanese(ja).

License
=====
 K-scope
 Copyright 2012-2013 RIKEN, Japan

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

