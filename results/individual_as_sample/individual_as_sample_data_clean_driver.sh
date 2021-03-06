## This script should be run from the projects root directory

## move to the directory that will contain the individual worm data (also must
contain the .zip folders)
cd data/plate_as_sample

## run choreography to extract ID (D), speed(s), bias(b), morphwidth(M), midline(m) and area (e) on all files in a directory
for zipfolder in *; do java -Xmx8g -jar /Users/michelleroux/Documents/Tiffany/MWT_analysis/Chore.jar --shadowless -p 0.27 -M 2 -t 20 -S -N all -o DsbMmexy --plugin Reoutline::despike --plugin Respine $zipfolder; done

## make one file containing all the data, the first column contains the file path info
## from which we can later extract the strain name
for filename in $(find . -name '*.dat'); do grep -r '[0-9]' $filename >> merged.file; done

## move to the projects root directory
cd ../..

## parse the first column into strain, date, etc and save it as merged.file.parsed
rscript bin/parse.R data/plate_as_sample/merged.file