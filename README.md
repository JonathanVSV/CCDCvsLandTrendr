# CCDCvsLandTrendr

Google Earth Engine codes for running the comparison of CCDC and LandTrendr. The repo has two folders.

## GEE scripts

Google Earth Engine scripts used to run CCDC and LandTrendr. 

### CCDC

CCDC is run in three steps to avoid hitting memory limits. The first step (temporal segmentation) is done in six different grids, before passing on to the second step. Each step exports the output as an asset to be read in the next step.

### LandTrendr

LandTrendr script to run the LandTrendR temporal segmentation. This is just a modification of the original LandTrendr script.

## Rscripts

R scripts to make additional analyses.
These are five scripts that make additional analyses with the results from CCDC and LandTrendr. 

1. Analysis. Has the main analyses for calculating the accuracy metrics, intersection over union, difference in date of detection.
2. verif_pts. Has the script used to generate the validation points.
3-5. Have the scripts used to create the time series plots with CCDC and LandTrendr, and join them in a single plot. These scripts were created with the aid of Claude Code.

