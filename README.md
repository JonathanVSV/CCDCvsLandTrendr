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

1. Analysis. The main analyses for calculating the accuracy metrics, intersection over union, difference in date of detection.
2. verif_pts. The script used to generate the validation points.
3. plot_verifpts_197. The script to build the LandTrendr time series plot.
4. ST_CCDC_plot. The script to build the LandTrendr time series plot. 
5. jointPlot. The script to join the two previous plots in a single plot, as well as to join the date of disturbane detection plots (created in Analysis). These scripts (3-5) were created with the aid of Claude Code.

