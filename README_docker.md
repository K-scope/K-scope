# About K-scope with Docker-SSHconnect

If you can access a remote server with installed Omni XcalableMP (XMP) compiler 
(for example, front-end of the K computer is suitable environment to make intermediate 
code with XMP), you can build intermediate code on the server remotely via SSH connection 
using SSHconnect.jar. This software is an utility for remote command execution with automatic 
file transfer.

It is developed in coolaboration with RIKEN AICS HPC Usability Research Team to enhance 
K-scope usability, and is bundled with the binary package of K-scope.

# Remote code building

As an alternative to installing Omni XcalableMP compiler on local computer it is possible:

1. to use K-scope installed in a Docker container.
2. to use the compiler installed on a remote server machine,
  1. to use the compiler installed in a Docker container,
3. to use Docker IaaS tools for easy access to the compiler installed in a Docker container 
either on a remote server or on user local computer. 


See below for usage instructions for each alternative. 

K-scope uses connect.sh script or SSHconnect.jar to build source code on a remote server or in a Docker container. 
If connect.sh or SSHconnect.jar are found in "utils" subdirectory, additional options are enabled in new 
project wizard and project settings dialogs.

All settings for accessing Omni XMP compiler on a remote location are stored in Remote settings files. 

Remote settings files for using Omni XMP installed on a remote server machine (2. and 2i. above) 
work with SSHconnect.jar program and are stored under "sshconnect" remote service folder. 
Remote settings files for using Docker IaaS tools for accessing the compiler in a Docker container (3.)
work with connect.sh script and are stored under "dockeriaas" remote service folder.   

Whether intermediate code is built on a remote server or on local computer 
"Build command" for building intermediate code and "Clean command" for removing
intermediate code do not change.  Intermediate code is built automatically
after a New Project wizard dialog is complete and can be rebuilt later with 
"Project > Rebuild intermediate code" menu.


## Why use Docker? 

When using Omni XMP installed on a remote server, project files are copied to a temporary location
with paths different from their paths on local computer. Because of that all 
absolute paths in project files must be taken care of and replaced with a placeholder 
`#[remote_path]` beforehand.

When using Docker container with Docker IaaS tools project files are placed in container with the same
paths as on local computer, so replacing absolute paths is not necessary.

If you have Docker on the same machine with you source code, you can use K-scope installed in 
a Docker containt (case 1. above). It is the easiest use case in that you don't have to install 
or configure anything: cd to source code directory and run Docker command. See details below.


## 1. Use K-scope installed in a Docker container

To simplify installation even more we have created Docker image with installed K-scope along with Omni XMP 
compiler. Provided you have Docker installed on your machine, you can start using K-scope without need 
to install anything. 

### Start K-scope in a Docker container

On your machine cd to the directory with your Fortran source code. Start container with:

```
docker run -d -p 22 -v $(pwd):$(pwd) pyotr777/kscope:0.7.0
```
You can copy-paste the above command.
Make sure container has started and find it's host-side port number with
```
docker ps
```

Login to the container using the host-side SSH port number with
```
ssh -Y -p <host-side SSH port number> root@localhost
```

root password is "docker".

To start K-scope inside container change directory to /kscope and run
./kscope.sh

Source code will be inside container with the same path as on the host machine.

## 2. Use Omni XMP compiler on remote server machine

Install Omni XcalableMP compiler on remote machine as described here: 
http://www.hpcs.cs.tsukuba.ac.jp/omni-compiler/doc/Install.html
To be able to connect to the server machine from local computer sshd must be 
configured and running on the server. Consult sshd manual for your server OS.

Substitute all absolute paths in your project source files with `#[remote_path]`
and include these files into "Absolute paths in" list in Project Settings. Create 
a new Remote settings file with URL, port number, user name etc. 
necessary to login to the server with SSH.

## 2i. Use Docker container with Omni XMP compiler

Start Docker container with command: 

```
docker run -d -p 22 pyotr777/omnixmp:0.7.0 /usr/sbin/sshd -D
```

You can login into container with SSH as user "root" with password "docker".
Compiler front-end is installed inside the container in /opt/omnixmp/bin directory. 
Make sure to set "add_path" parameter in K-scope Project settings > Remote settings file.  

Don't forget to substitute all absolute paths in your project source files with `#[remote_path]`
and include these files into "Absolute paths in" list in Project Settings. Create 
a new Remote settings file with URL, port number, user name etc. 
necessary to login to the Docker container with SSH. 


## 3. Use K-scope with Docker IaaS Tools

If you have a server machine with Docker installed, you can use Docker IaaS Tools to quickly 
make a setup for building code inside a Docker container.


### Server-side setup for using K-scope with Docker IaaS Tools (DIT) and Omni XMP Compiler (OmniXMP)

Make new directory on your server machine for DIT, cd into it and clone git repository:

```
git clone https://github.com/pyotr777/dockerIaaSTools.git .
```

Download OmniXMP Docker image:

```
docker pull pyotr777/omnixmp:0.7.0
```

Prepare K-scope user public SSH-key, copy it to the directory with DIT 
on the server. cd into DIT directory and run:
```
sudo ./createuser.sh <user name> pyotr777/omnixmp <public key file>
```

### K-scope Project setup 

Make sure you have connect.sh file in your K-scope directory.
In new project wizard or in Project settings > Remote settings file press "Manage remote settings" button,
then press "+" button to create a new remote settings file or select existing settings file and 
press "copy" button. Select dockeriaas service and setup parameters values as described 
below:
For server_address use you server machine address,
for port use 22 (or whatever port number is used by sshd on your server machine),
for key enter path to you public ssh key on your local computer,
for user name use the same name that was used in createuser.sh command on the server machine.


Demonstration: http://youtu.be/86ybJdnNvUc


*Docker IaaS Tools and connect.sh are developed by RIKEN AICS HPC Usability Research Team
http://github.com/pyotr777/dockerIaaSTools*
Docker IaaS Tools require Ubuntu. Their work on other OSs is not guaranteed. 

*Docker http://docker.com*

