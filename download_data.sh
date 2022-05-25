#!/bin/bash

mkdir data

curl http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/2021/09/datos_abiertos_covid19_01.09.2021.zip --output data/data.zip

unzip data/data.zip
