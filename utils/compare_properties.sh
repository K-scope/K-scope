#!/bin/bash

# Compare properties in two files
# formatted as Java .properties file.
# Format:
# property.name=Property value

leftfile=$1
rightfile=$2

notfoundproperties=""

# Get property name from string of format
# Filename:propery.name=property value
property_name () {
    IFS='=' read -a arr1 <<< "$1"
    IFS=':' read -a arr2 <<< "${arr1[0]}"
    echo "${arr2[1]}"    
}

# Get property value from string of format
# Filename:propery.name=property value
property_value () {
    IFS='=' read -a arr1 <<< "$1"
    echo "${arr1[1]}"        
}

grep -vr "^#" $leftfile | while read line; do    
    property=$(property_name $line)
    if [ "$property" ]
        then
        # echo "---$property---"
        en_value=$(property_value "$line")
        search=$(grep "$property" $2)
        if [ "$search" ] 
            then
            jp_value=$(property_value "$search")
            if [ ${#jp_value} -lt 2 ]
                then 
                echo "j: $property = $jp_value"
            fi
            if [ ${#en_value} -lt 2 ]
                then 
                echo "e: $property = $en_value (${#en_value})"
            fi            
        else
            echo "$property = $en_value"
        fi
    fi
done
