#!/bin/bash

# Loop from 1 to 12
for i in $(seq 1 12); do
    # Format the number with leading zero if it's less than 10
    formatted_i=$(printf "%02d" $i)
    
    # Run the dune exec command with the formatted file name
    dune exec interp01 examples/$formatted_i.ml
done
