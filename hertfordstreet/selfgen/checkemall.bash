#!/bin/bash

for file in *.py; do
    echo $file;
    python $file | diff $file -;
    retval=$?; 
    if [ "$retval" -eq 0 ]; then
        echo "cool!";
    else
        echo "not cool";
    fi
done;
