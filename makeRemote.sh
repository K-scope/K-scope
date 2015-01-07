#!/bin/bash

# Mount local folder into container on a server.
# To be executed on user local computer.
# Parameters:
#  remote user
#  server address
#  server port number (must be free)
#  path to directory to mount
#
# Ex.:
# makeSetup.sh user@server:port /path
#
# Created by Bryzgalov Peter
# Copyright (c) 2015 Japan Riken AICS. All rights reserved

usage="Usage:\nmakeRemote.sh -u <username> -h <server address> -p <local path to code> -k <path to ssh-key> -m <build command>"

remote_port=0
local_user=$USER


# Defauts
make_command="make"
add_path="/opt/omnixmp/bin"

echo "Called with $@"

while getopts "u:h:p:k:m:a:" opt; do
  case $opt in
    u)
      remoteuser=$OPTARG
      ;;
    h)
      server=$OPTARG
      ;;
    p)
      path=$OPTARG
      ;;
    m)
      make_command=$OPTARG
      ;;
    k)
      ssh_key=$OPTARG
      ;;
    a)
      add_path=$OPTARG
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      echo -e "$usage"
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      echo -e "$usage"
      exit 1
      ;;
  esac
done


# Check that necessary arguments provided

if [ -z "$remoteuser" ]
then
	echo "Need user name."
	echo -e "$usage"
	exit 1
fi

if [ -z "$server" ]
then
	echo "Need user name."
	echo -e "$usage"
	exit 1
fi

if [ -z "$path" ]
then
	echo "Need user name."
	echo -e "$usage"
	exit 1
fi

if [ -n "$ssh_key" ]
then
	ssh-add $ssh_key
	keyoption="-i $ssh_key"
fi

container_port=$(ssh $remoteuser@$server port 2>/dev/null)

if [ $container_port = "null" ]
then
	echo "Container is not running"
	ssh $keyoption $remoteuser@$server "daemon" 2>/dev/null
	container_started="true"
fi


free_port=$(ssh $remoteuser@$server freeport 2>/dev/null)

echo "SSH server port: $free_port, container port:$container_port"

command="ssh $remoteuser@$server -R $free_port:localhost:22 -N"
echo $command
$command &
ssh_tunnel=$!
echo "tunnel PID=$ssh_tunnel"

# ssh
remote_commands="mkdir -p $path\nsshfs -p $free_port $local_user@172.17.42.1:$path $path\ncd $path\necho \"ver \$version\";pwd;ls -l;export PATH=\$PATH:$add_path;$make_command"
echo $remote_commands

cmd_file="rcom.sh"
echo "#!/bin/bash" > $cmd_file
echo "version=0.1" >> $cmd_file
echo -e $remote_commands >> $cmd_file
chmod +x $cmd_file
cp_command="scp $keyoption -P $container_port $cmd_file root@$server:/"
echo $cp_command
$cp_command
command="ssh -A -o StrictHostKeyChecking=no $remoteuser@$server '/$cmd_file'"
echo $command
$command
rm $cmd_file
if [ -n "$container_started" ]
then
	ssh $remoteuser@$server "nodaemon" 2>/dev/null
	echo "Container will stop"
fi
kill "$ssh_tunnel"



