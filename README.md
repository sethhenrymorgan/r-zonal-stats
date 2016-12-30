# r-zonal-stats
R script to calculate zonal statistics within polygons based on raster data. Meant to replicate ArcMap's Zonal Statistics and Buffer tools. 

The zonal_stat.R file can be used and modified to calculate zonal statistics over a list of raster files based on buffers around a set of points. This replicates the results generated using ArcMap's Buffer and Zonal Statistics tools, and significantly reduces the time used to calculate these statistics compared to ArcMap's GUI.

The original use for this script was to replicate a Propensity Score Matching process used to select villages for a survey. This is demonstrated in vilmatch_howto.R.
