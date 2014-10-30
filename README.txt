README
======

K-scope is a source code analysis tool with graphical user interface that
visualizes program structures of Fortran 90 and FORTRAN 77 source code.
It is suitable for source code reading for engineers who work the performance
improvement for an application.

K-scope is developed by RIKEN AICS Software Development Team, and is distributed
as Open Source Software. The latest version and documents are available in the
following download site.
http://www.aics.riken.jp/ungi/soft/kscope/

Preliminaries
=============

First of all, K-scope requires JDK7 or later. Please download from Oracle site.
http://www.oracle.com/technetwork/java/javase/downloads/index.html

In addition, this tool uses intermediate codes created by front end of
the Omni XcalableMP compiler. There exists a download site as follow.
http://www.hpcs.cs.tsukuba.ac.jp/omni-compiler/xcalablemp/download.html

If you can access a server installed with Omni XcalableMP compiler, 
you can build intermediate codes on the server remotely via SSH service by
SSHconnect.jar. This software is an utility for remote command execution with
automatic transfer. It is developed by RIKEN AICS HPC Usability Team in
collaboration to enhance the K-scope's feature, and is bundled with the binary
package of the K-scope.

This setup is easy. Firstly, SSHconnect.jar is placed in the same directory as
kscope.jar. After starting the K-scope, new option is enabled in new project
wizard.

Compile and Run
===============

This software is written by pure Java to improve the portability.
We provide two-type packages: jar-executable package and source code package
in our site. Especially this source cord packages includes all source codes
necessary for modify and compiling. In that case, we recommend IDE enviroments
such as Eclipse or NetBeans.
NOTICE) The source codes includes Japanese comments by UTF-8.

We provide build.xml to compile.

  $ ant

After the compiling, you may obtain jar-executable or classes.
Run is simple as follows.

  $ java -jar kscope.jar

K-scope requires specific folders for properties.
If the program cannot find that folders, it may terminate abnormally.
In the normally process, you may obtained start screen.

Tips on usage) "-Duser.language" VM-option is language selector, English(en) and Japanese(ja).

License
=======
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
