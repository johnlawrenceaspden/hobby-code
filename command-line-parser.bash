#!/bin/bash

# Robert Siemer's answer to:
# http://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash

# myscript -vfd ./foo/bar/someFile -o /fizz/someOtherFile
# myscript -v -f -d -o/fizz/someOtherFile -- ./foo/bar/someFile 
# myscript --verbose --force --debug ./foo/bar/someFile -o/fizz/someOtherFile
# myscript --output=/fizz/someOtherFile ./foo/bar/someFile -vfd
# myscript ./foo/bar/someFile -df -v --output /fizz/someOtherFile

getopt --test > /dev/null
if [[ $? -ne 4 ]]; then
    echo "I’m sorry, `getopt --test` failed in this environment."
    exit 1
fi

SHORT=dfo:v
LONG=debug,force,output:,verbose

# -temporarily store output to be able to check for errors
# -activate advanced mode getopt quoting e.g. via “--options”
# -pass arguments only via   -- "$@"   to separate them correctly
PARSED=`getopt --options $SHORT --longoptions $LONG --name "$0" -- "$@"`
if [[ $? -ne 0 ]]; then
    # e.g. $? == 1
    #  then getopt has complained about wrong arguments to stdout
    exit 2
fi

echo command-line is magically transformed to:
echo $PARSED
echo which we can then parse 

# use eval with "$PARSED" to properly handle the quoting
eval set -- "$PARSED"

# now enjoy the options in order and nicely split until we see --
while true; do
    case "$1" in
        -d|--debug)
            d=y
            shift
            ;;
        -f|--force)
            f=y
            shift
            ;;
        -v|--verbose)
            v=y
            shift
            ;;
        -o|--output)
            outFile="$2"
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Programming error"
            exit 3
            ;;
    esac
done

# handle non-option arguments
if [[ $# -ne 1 ]]; then
    echo "$0: A single input file is required."
    exit 4
fi

echo "verbose: $v, force: $f, debug: $d, in: $1, out: $outFile"
