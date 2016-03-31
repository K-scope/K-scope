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

version="0.28"
debug="1"

usage="Usage:\nconnect.sh -u <username> -h <server address> -p <server port number> \
-l <local directory to mount> -i <path to ssh-key> \
-a <add to PATH> -m <remote command>"

remote_port=0
local_user=$USER
hostIP="172.17.42.1"  # server IP as seen from inside containers


# Defauts
remote_commands=""
add_path="/opt/omnixmp/bin"

echo "$0 ver.$version"
if [ $debug ]; then 
	echo "Called with parameters: $@"
fi

# Trim quotes around a string
trimQuotes() {
	v=$1
	val=$(echo $v | sed 's/^\"//' | sed 's/\"$//')
	echo "$val"
}

while getopts "u:h:l:i:k:m:a:p:" opt; do
	case $opt in
	u)
		remoteuser=$(trimQuotes "$OPTARG")
		;;
	h)
		server=$(trimQuotes "$OPTARG")
		;;
	l)
		path=$(trimQuotes "$OPTARG")
		;;
	m)
		remote_commands=$(trimQuotes "$OPTARG")
		;;
	k)
		ssh_key=$(trimQuotes "$OPTARG")
		;;
	i)
		ssh_key=$(trimQuotes "$OPTARG")
		;;
	a)
		add_path=$(trimQuotes "$OPTARG")
		;;
	p)
		server_port="-p $(trimQuotes $OPTARG)"
		;;
	\?)
		echo "Invalid option: -$OPTARG" >&2      
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
	echo "Need server address."
	echo -e "$usage"
	exit 1
fi

if [ -z "$path" ]
	then
	path=$(pwd)
	echo "Use current folder: $path"
fi

if [ -n "$ssh_key" ]
	then
	key_in_ssh_add=$(ssh-add -l | grep $ssh_key | wc -l)
	if [ $key_in_ssh_add -le 0 ] 
		then
		echo "Add SSH_KEY $ssh_key to agent."
		ssh-add $ssh_key
		ssh_key_added=1
	fi
	keyoption="-i $ssh_key"	
fi

SSH_PARAMETERS="-A $server_port $remoteuser@$server"

container_port=$(ssh $SSH_PARAMETERS port 2>/dev/null)

if [ $container_port="null" ]
	then
	if [ $debug ]; then
		echo "Starting container in daemon mode ($SSH_PARAMETERS)"
	fi
	ssh $SSH_PARAMETERS "daemon" 2>/dev/null
	container_started="true"
	container_port=$(ssh $SSH_PARAMETERS port 2>/dev/null)
	if [ -z $container_port ]
		then
		echo "Could not reach container. Check the address, user name, connection, ..."
		exit 1
	fi
fi


free_port=$(ssh $SSH_PARAMETERS freeport 2>/dev/null)

echo "SSH server port: $free_port, container port:$container_port"

command="ssh $SSH_PARAMETERS -R $free_port:localhost:22 -N"
if [ $debug ]; then 
	echo $command
fi
$command & 
ssh_tunnel=$!
echo "tunnel PID=$ssh_tunnel"

# ssh
if [ -z "$remote_commands" ]
then  # No commands -- interactive shell login
	remote_commands="mkdir -p \"$path\"\nsshfs -o StrictHostKeyChecking=no,UserKnownHostsFile=/dev/null,nonempty -p $free_port $local_user@$hostIP:$path $path\ncd \"$path\"\necho \"ver \$version\";pwd;ls -l;export PATH=\$PATH:$add_path;"
	#echo -e $remote_commands

	# Save remote commands to a file. Execute it in container. 
	cmd_file="rcom.sh"
	if [ $debug ]; then
		echo "#!/bin/bash -x" > $cmd_file
	else 
		echo "#!/bin/bash" > $cmd_file
	fi
	echo "version=$version" >> $cmd_file
	echo -e $remote_commands >> $cmd_file
	chmod +x $cmd_file
	if [ $debug ]
		then
		echo "Command file:"
		cat $cmd_file
		echo "---"
	fi
	# Copy command file into container using container SSH port number as seen from server-side.
	cp_command="scp $keyoption -P $container_port $cmd_file root@$server:/"
	$cp_command 
	command="ssh -o StrictHostKeyChecking=no $SSH_PARAMETERS '/$cmd_file'"    
	$command 2>&1 | grep -i "error"
	#command="ssh -A -Y -o StrictHostKeyChecking=no $remoteuser@$server"
	command="ssh -Y -o StrictHostKeyChecking=no $keyoption -p $container_port root@$server"
	if [ $debug ]; then
		echo $command
	fi
	$command 
else # Execute remote commands. No interactive shell login.
	# testcommand ="ssh -vv -p $free_port $local_user@$hostIP;echo "'$HOST'";exit;\n"
	setup_commands="mkdir -p \"$path\"\nsshfs -o StrictHostKeyChecking=no -p $free_port $local_user@$hostIP:\"$path\" \"$path\"\ncd \"$path\"\necho \"ver \$version\";pwd;ls -l | wc -l;export PATH=\$PATH:$add_path"
	setup_commands="$setup_commands\n$remote_commands"
	echo -e $setup_commands

	# Save remote commands to a file. Execute it in container. 
	cmd_file="rcom.sh"
	echo "#!/bin/bash" > $cmd_file
	echo "version=$version" >> $cmd_file    
	echo -e $setup_commands >> $cmd_file    
	chmod +x $cmd_file
	if [ $debug ]; then
		echo "Command file:"
		cat $cmd_file
		echo "---"
	fi
	# Copy command file into container using container SSH port number as seen from server-side.
	cp_command="scp $keyoption -P $container_port $cmd_file root@$server:/"   
	if [ $debug ]; then
		echo $cp_command
	fi
	$cp_command 
	command="ssh $SSH_PARAMETERS '/$cmd_file'"
	if [ $debug ]; then
		echo $command
	fi
	$command 
fi



# Remove file with commands
rm $cmd_file

# Stop container if it was started or unmount local folder
if [ -n "$container_started" ]
then
	ssh $SSH_PARAMETERS "stopnow" 2>/dev/null
	echo "Container will stop now"
else
	# Unmount SSHFS mount
	echo "Unmount SSHFS"
	ssh $SSH_PARAMETERS "umount \"$path\"" 2>/dev/null
fi
kill "$ssh_tunnel"
if [ $ssh_key_added ] 
	then
	echo "Remove SSH_KEY $ssh_key from agent."
	ssh-add -d $ssh_key
fi
