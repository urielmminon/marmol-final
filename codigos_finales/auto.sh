#!/bin/bash

mkdir Datos
cd Datos

curl -L -o test.csv.zip "https://drive.google.com/uc?export=download&id=144qWTDn64Lf2-SIIQwxLnwWu3ubIxIPu"
curl -L -o train.csv.zip "https://drive.google.com/uc?export=download&id=1mV-LmKfbyIi209YVBlGBkZiVqwB4un2e"

unzip train.csv.zip
unzip test.csv.zip

cd ..

Rscript PipeLine.R
