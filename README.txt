README
======

K-scope is a source code analysis tool with graphical user interface that
visualizes program structures of Fortran 90 and FORTRAN 77 source code.
It is suitable for source code reading for engineers who work the performance
improvement for an applicaiton.

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


K-scope with Docker IaaS tools
==============================

If you have a computer with Docker installed, you can use Docker IaaS Tools to quickly 
make a setup for building code inside a Docker container.
K-scope uses makeRemote.sh to build source code in a Docker container. If makeRemote.sh
is in the same directory as kcope.jar, additional options are enabled in new 
project wizard.


Server-side setup for using K-scope with Docker IaaS Tools
----------------------------------------------------------

Clone Docker IaaS Tools.
Make new directory on your server machine, cd into it and run:
 
git clone git@github.com:pyotr777/dockerIaaSTools.git .
 
If you don't have image with Omni XMP compiler in you docker registry, run in directory
with Docker IaaS Tools:
 
docker load < atool.tar.gz
 

Prepare K-scope user public SSH-key, copy it to the directory with Docker IaaS Tools 
on the server. cd into Docker IaaS Tools directory and run:
 
sudo ./createuser.sh <user name> <image with Omni XMP> <public key file>
 

Local computer setup
--------------------

Make sure you have makeRemote.sh file in your K-scope directory.
In new project vizard or in Project/Server settings menu set up:
server address, K-scope user name on the server, local path to SSH private key for K-scope
user. 


Demonstration http://youtu.be/86ybJdnNvUc

Docker IaaS Tools and makeRemote.sh are developed by RIKEN AICS HPC Usability Research Team
http://github.com/pyotr777/dockerIaaSTools

Docker http://docker.com


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

  $ java -jar -Xmx1024m kscope.jar

K-scope requires specific folders for properties.
If the program cannot find that folders, it may terminate abnormally.
In the normally process, you may obtained start screen.

Tips on usage) "-Duser.language" VM-option is language selecter, English(en) and Japanese(ja).

Build jar file
==============
Run in bin directory:
jar cfe ../kscope.jar jp.riken.kscope.Kscope *



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
