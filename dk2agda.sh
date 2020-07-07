#!/bin/bash

usage="Usage: $(basename $0) [-f FILE.(dk|lp)] [-d DIR] [-o OUT]"

while getopts 'f:d:o:h' arg
do
    case "$arg" in
        f) infile="$OPTARG" 
            if [[ $infile != *.dk && $infile != *.lp ]] 
            then
                echo $usage 
                echo "Input files must end in .dk or .lp"
                exit 1 
            fi
            ;;
        d) indir="$OPTARG" ;;
        o) outdir="$OPTARG"
           [[ ! -e $outdir ]] && mkdir $outdir
           [[ ! -d $outdir ]] && echo "Output is not a directory" && exit 1
           ;;
        h) echo "$usage"
           exit 0
           ;;
        *) echo "Invalid argument"
           echo "$usage"
           exit 1
           ;;
    esac
done

if [[ -z ${indir} && -z ${infile} ]]
then
    echo "Input not specified"
    echo "${usage}"
    exit 1
fi

if [[ -n ${indir} && -n ${infile} ]]
then
    echo "Options -f and -d cannot be used together"
    echo "${usage}"
    exit 1
fi

[[ -n ${indir} ]] && find ${indir} \( -name "*.dk" -o -name "*.lp" \) -exec echo dk2agda {} ${outdir:-"out"} \;

[[ -n ${infile} ]] && echo dk2agda ${infile} ${outdir:-"out"}

