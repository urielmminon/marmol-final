#!/bin/bash

mkdir Datos
cd Datos

curl -L -o test.csv.zip "https://drive.google.com/uc?export=download&id=1IbIfNdpBJf2R9l_bgWkENes3-FWyWZ86"
curl -L -o train.csv.zip "https://drive.google.com/uc?export=download&id=1Vrr4TvX0DoPEJzgbCjcUGnHi0hueiGgt"

unzip train.csv.zip
unzip test.csv.zip

cd ../..

Rscript PipeLine.R
