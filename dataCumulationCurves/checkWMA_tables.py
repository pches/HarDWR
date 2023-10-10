##########################################################################
##########################################################################
## Script Name: checkWMA_tables.py
## Purpose of Script: Checks to make sure cumulative curve output files
##                   formatted correctly and that there are no unexpected
##                   or invalid values.
##
## Special Requirements: Should only work on cumulative curve output
##                      files.
##
## Author: Shan Zuidema
## Email: Shan.Zuidema@unh.edu
## Date Created: 7/27/2023
##
## Last Moddified By: Matthew D. Lisk
## Editors Email: mdl5548@psu.edu
## Date Last Edited: 10/10/2023
##
## Copyright (c) 2023 The University of New Hampshire
##
##########################################################################
##########################################################################

#!/usr/bin/env python3

def main(fldr):
    import pandas as pd
    import numpy as np
    checkDecliners={}
    for i in range(1,909):
        # Are both sources present?
        SW=os.path.exists(fldr+"/WMA_{}_SW.csv".format(i))
        GW=os.path.exists(fldr+"/WMA_{}_GW.csv".format(i))
        if (not SW) & (not GW):
            print("WMA {}: Not present for either SW or GW".format(i))
            continue
        if not (SW & GW):
            print("WMA {}: Only one source present: SW: {}  GW: {}".format(i,SW,GW))


        # if any of these variables are changed by the following code, there are bad values in the data
        SWnegatives=False
        SWallocOver=False
        SWdecline=False
        SWnans=False
        GWnegatives=False
        GWallocOver=False
        GWdecline=False
        GWnans=False

        if (SW):
            SW=pd.read_csv(fldr+"/WMA_{}_SW.csv".format(i),index_col="Year")
            # any negatives?
            SWnegatives = (SW.min() < 0).any()
            # any allocations add to more than 100%
            sectors=list(set(SW.columns) - set(['CUML']))
            SWallocOver = (SW[sectors].sum(axis=1) > 100.01).any() # Allow a little rounding error
            # any cumulative values decline?
            SWdecline  = ((SW['CUML'].diff(1)/SW['CUML']).loc["1800":"2020"] < -0.001).any() # Allow a little rounding error
            # Any NaNs?
            SWnans   = np.isnan(SW.values).sum() > 0

        if (GW):
            GW=pd.read_csv(fldr+"/WMA_{}_GW.csv".format(i),index_col="Year")
            # any negatives?
            GWnegatives = (GW.min() < 0).any()
            # any allocations add to more than 100%
            sectors=list(set(GW.columns) - set(['CUML']))
            GWallocOver = (GW[sectors].sum(axis=1) > 100.01).any() # Allow a little rounding error
            # any cumulative values decline?
            GWdecline  = ((GW['CUML'].diff(1)/GW['CUML']).loc["1800":"2020"] < -0.001).any() # Allow a little rounding error (a very small percent)
            # Any NaNs?
            GWnans   = np.isnan(GW.values).sum() > 0


        if any([SWnegatives,GWnegatives,SWallocOver,GWallocOver,SWdecline,GWdecline,SWnans,GWnans]):
            print("WMA {} : Negatives SW: {} GW: {} AllocOver100  SW: {}  GW: {}  DeclinCUML  SW: {}  GW: {} NANs SW: {} GW: {}".format(
                i,SWnegatives,GWnegatives,SWallocOver,GWallocOver,SWdecline,GWdecline,SWnans,GWnans))
        if any([SWdecline,GWdecline]):
            if (SW):
                sdec=SW['CUML'].loc[(SW['CUML'].diff(1)/SW['CUML'] < -0.001)].loc["1800":"2020"]
            if (GW):
                gdec=GW['CUML'].loc[(GW['CUML'].diff(1)/GW['CUML'] < -0.001)].loc["1800":"2020"]
            checkDecliners[i]=(sdec,gdec)
    for v,d in checkDecliners.items():
        if len(d[0]) > 0:
            print("WMA {} SW: ".format(v))
            print(d[0])
        if len(d[1]) > 0:
            print("WMA {} GW: ".format(v))
            print(d[1])

if __name__ == "__main__":

    import sys
    import os
    Fldr=sys.argv[1]
    assert os.path.exists(Fldr)
    main(Fldr)
