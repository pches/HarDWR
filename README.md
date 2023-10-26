# HarDWR - Harmonized Database of Western U.S. Water Rights
## Description
This repository contains the code which was used to create the HarDWR datasets. In summary, raw water rights data was collected from 11 states in the Western United States. The water right records when through several quality control tests, and then were harmonized into a single dataset to be consistent across all states. More specific details on data collection, quality control, and harmonization are described in Lisk et al. (*in review*). These data were instrumental in conducting a study of the multi-sector dynamics of intersectoral water allocation changes through water markets (Grogan et al., *in review*). The datasets are available here:

HarDWR Raw Data: https://data.msdlive.org/records/8y9nq-bdt52

HarDWR Spatial Boundaries: https://data.msdlive.org/records/v5ree-qj344

HarDWR: https://data.msdlive.org/records/br8a2-jam42

HarDWR Cumulative Curves: https://data.msdlive.org/records/y76hr-dvn55


The code available here are divided into subdirectories based on which dataset they were used to create. The below headers are the subdirectories with their files listed beneath them.

### container

`waterRightAnalysis.def` - A Singularity definition file used to create the run environment for the scrips described below. This file is provided for two main reasons. The first reason is to make available to others the exact processing environment in which the code was originally ran. The second reason is that is serves as an archive of the same processing environment. This is particularly important as some of the original R packages used have become difficult to install, like the `raster` package once the `terra` package became available.

### dataHarmonization

`wrDataHarmonization.R` - This script reads in the raw water rights data and preforms the harmonization of the records. It is organized into sections by state, as the data each state had their own unique challenges to be harmonized. See Lisk et al. (*in review*) for more details. It can be noted that a user may change the number and which states to run the harmonization for by editing a single variable, *states*. The reason for this is to make the processing easier in case a user may not want to run all 11 states.

`wrDataHarmonization_CustomFunctions.R` - A supporting file for `wrDataHarmonization.R` which contains all the required custom written functions for the main script.

### dataCumulationCurves

`wrCumulation.R` - This script reads in the harmonized water rights data and preforms the calculations used to create the cumulative curves. First, the script extracts the years available from within the given harmonized data records, to provide information on the year ranges that can be found within said data. A user then has the option to use the extracted date range or may input their own dates. The default data values begin at the minimum year extracted from the harmonized data, and end at 2100, with the date boundaries being inclusive. The year extremes pulled from the harmonized data at the time of writing are 217 and 9146. Should a user provide years outside of the data minimum and maximum then the code should run without issue. However, in the case of a less than 217 minimum this would fill the output with rows of 0's and in the case of a greater than 9146 the rows will copy the values those in year 9146.
Once the user has selected the desired date range, the script will then preform the cumulative curve calculations. Once the calculations are finished, there is some clean up done to make the resulting cumulative curves easier to access, a version of the output is saved as a .RDate file, and then individual .csv's are written out for each Water Management Area (WMA) and water source (surface or groundwater).
The final section of the script creates a series of summary images from the resulting data. These images include WMA boundaries with the associated Points of Diversion (PoD) and/or Places of Use (PoU), a maximum cumulative line graph of Cubic Feet Per Second (CFS) by year, and a stacked graph with the proportion of CFS assigned to each sector by year.

`wrCumulation_CustomFunctionsR` - A supporting file for `wrCumulation.R` which contains all the required custom written functions for the main script.

`checkWMA_tables.py` - A script used to validate the calculated cumulative curve values.

### WMAs

`simplifyWMAs.R` - The script which reads in the raw downloads of the various state's WMAs as spatial data, harmonizes them, and merges them into two spatial layers. The reason two layers were required is due to Arizona managing its surface water assets with different boundaries than its ground water assets, where each other state manages their surface and ground water assets with one set of boundaries. As we proceeded with using the Arizona surface water boundaries in our follow-up analysis (Grogan et al., *in review*), the Arizona surface water boundaries are included with the boundary sets from the other states, and the Arizona groundwater boundaries are included in a second layer.



## Citations

Grogan, D. *et al*. Bringing hydrologic realism to water markets. (in review).

Lisk, M. *et al*. Harmonized Database of Western U.S Water Rights (HarDWR) v.1. (in review).

