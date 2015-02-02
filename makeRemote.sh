#!/bin/bash

# Mount local folder into container on a server.
# To be executed on user local computer.
# Parameters:
#  remote user
#  server address
#  local path to mount inside container
#  path to ssh-key (optional)
#  commands to be executed in container (optional)
#
# Created by Bryzgalov Peter
# Copyright (c) 2015 RIKEN AICS. All rights reserved

version="0.12quotes"

usage="Usage:\nmakeRemote.sh -u <username> -h <server address> \
-p <local directory to mount> -k <path to ssh-key> -m <remote command>"

remote_port=0
local_user=$USER
hostIP="172.17.42.1"  # server IP as seen from inside containers


# Defauts
remote_commands=""
add_path="/opt/omnixmp/bin"

echo "$0 ver.$version"
echo "Called with parameters: $@"

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
      remote_commands=$OPTARG
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
	echo "Starting container in daemon mode"
	ssh $keyoption $remoteuser@$server "daemon" 2>/dev/null
	container_started="true"
	container_port=$(ssh $remoteuser@$server port 2>/dev/null)
fi


free_port=$(ssh $remoteuser@$server freeport 2>/dev/null)

echo "SSH server port: $free_port, container port:$container_port"

command="ssh $remoteuser@$server -R $free_port:localhost:22 -N"
echo $command
$command &
ssh_tunnel=$!
echo "tunnel PID=$ssh_tunnel"

# ssh
if [ -z $remote_commands ]
then  # No commands -- interactive shell login
    remote_commands="mkdir -p \"$path\"\nsshfs -p $free_port $local_user@$hostIP:\"$path\" \"$path\"\ncd \"$path\"\necho \"ver \$version\";pwd;ls -l;export PATH=\$PATH:$add_path;"
    echo -e $remote_commands

    # Save remote commands to a file. Execute it in container. 
    cmd_file="rcom.sh"
    echo "#!/bin/bash" > $cmd_file
    echo "version=$version" >> $cmd_file
    echo -e $remote_commands >> $cmd_file
    chmod +x $cmd_file
    # Copy command file into container using container SSH port number as seen from server-side.
    cp_command="scp $keyoption -P $container_port $cmd_file root@$server:/"
    echo $cp_command
    $cp_command
    command="ssh -A -o StrictHostKeyChecking=no $remoteuser@$server '/$cmd_file'"
    $command
    command="ssh -A -o StrictHostKeyChecking=no $remoteuser@$server"
    $command
else # Execute remote commands. No interactive shell login.
    remote_commands="mkdir -p \"$path\"\nsshfs -p $free_port $local_user@$hostIP:\"$path\" \"$path\"\ncd \"$path\"\necho \"ver \$version\";pwd;ls -l;export PATH=\$PATH:$add_path;$remote_commands"
    echo -e $remote_commands

    # Save remote commands to a file. Execute it in container. 
    cmd_file="rcom.sh"
    echo "#!/bin/bash" > $cmd_file
    echo "version=$version" >> $cmd_file
    echo -e $remote_commands >> $cmd_file
    chmod +x $cmd_file
    # Copy command file into container using container SSH port number as seen from server-side.
    cp_command="scp $keyoption -P $container_port $cmd_file root@$server:/"
    echo $cp_command
    $cp_command
    command="ssh -A -o StrictHostKeyChecking=no $remoteuser@$server '/$cmd_file'"
    echo $command
    $command
fi

# Remove file with commands
rm $cmd_file

# Stop container if it was started or unmount local folder
if [ -n "$container_started" ]
then
	ssh $remoteuser@$server "stopnow" 2>/dev/null
	echo "Container will stop"
else
	# Unmount SSHFS mount
	echo "Unmount SSHFS"
	ssh $remoteuser@$server "umount $path" 2>/dev/null
fi
kill "$ssh_tunnel"