#  Quantifying regional-scale impacts of hemlock woolly adelgid invasion on North American forest bird communities

### Bruna R. Amaral, Andrew M. Wilson, Julie Blum and David A. Miller


### Global Change Biology [](https://onlinelibrary.wiley.com/doi/10.1111/gcb.16349) ADD MY LINK

### Code/Data DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5597600.svg)](https://doi.org/10.5281/zenodo.5597600) ADD MY LINK

Amaral, BR; Wilson, AM; Blum, J; Miller, DA. Quantifying regional-scale impacts of hemlock woolly adelgid invasion on North American forest bird communities. Global Change Biology xx(xx): xx - xx.

_______________________________________________________________________________________________________________________________________

## Abstract:
Humans significantly influence geographic patterns of biological invasions, creating conditions for species to overcome biogeographic barriers and colonize new areas. In the eastern United States, forested landscapes containing eastern hemlock (*Tsuga canadensis*) are under threat by the invasive hemlock woolly adelgid (*Adelges tsugae*). Although several studies have shown the negative effects of adelgid invasion in local bird communities, its regional impacts have not yet been quantified. Using broad-scale spatial (entire eastern US range of hemlock) and temporal (>40 years of bird monitoring data) databases, we built spatial auto-regressive generalized mixed linear models to estimate immediate and long-term population effects of adelgid infestation on population trends of fourteen bird species. We also determined how winter temperature interacted with adelgid infestation to affect population trends. We selected the best models using WAIC, and validated model performance and power using Monte Carlo simulation, permutation tests, and sensitivity analysis. For all but one species, the best model included the effects of adelgid infestation on abundance trends. We observed a > 30% decline for two hemlock-associates species after infestation: the blackburnian warbler, and the hermit thrush. Declines were greater in the warmest part of their ranges. In contrast, no control species showed similar declines. Our results demonstrate that birds locally associated with hemlock habitat, when evaluated at a broad spatiotemporal scale, also decline in abundance following infestation. At the same time, declines are not universal and are dampened as compared to local studies suggesting that spatial heterogeneity buffers demographic decline. We also found that cold winters are associated with smaller bird population declines, suggesting that rising winter temperatures due to climate change might remove this environmental barrier. Despite the difficulties of understanding and connecting landscape-scale processes with those at finer scales, it is critical to evaluate processes that govern biodiversity distribution from a regional perspective.
_______________________________________________________________________________________________________________________________________

## Code for formatting the raw data and run the analisys:
[1_createSpace.R](1_createSpace.R): R code to assign each BBS route to a hexagon from the mesh shape file according to its geographical location. 

[2_combineData.R](2_combineData.R): R code to combine the BBS database with the HWA database. BBS data from all 22 states with the Eastern hemlock trees were used (Alabama, Connecticut, Delaware, Georgia, Kentucky, Maine, Maryland, Massachusetts, Michigan, North Carolina, New Hampshire, New Jersey, New York, Ohio, Rhode Island, South Carolina, Tennessee, Vermont, Virginia, West Virginia, Washington, Wisconsin). Hemlock woolly adelgid infestation data were extracted from a shape file with county level and year information about adelgid status.

[3_filterData.R](3_filterData.R):  R code to filter the BBS bird data that will be used in the analysis. Only routes within the distribution of the Eastern hemlock trees will be used. Only one type of observation, and one observation per occasion, were kept.

[4_singleSps.R](4_singleSps.R): get data for only the species that I'm interested in analyzing, and add no detections (zeros) for years where the species was not found but was previously found, and create species specific tibbles.

[5_formulasModels.R](5_formulasModels.R): script with all 11 models created to look at relationship of birds and temperature and infestation, considering space, changes in intercept and slope, and delay after infestation.

[6_model.R](6_model.R): R code to run all 11 models (source 5_formulasModels.R) to all 7 hemlock associate and the 7 control species for each 15 infestation offsets and save all the outputs for each models.

[7_extract_fixed_pars.R](7_extract_fixed_pars.R): functions to extract parameter estimates for the summary objects of the model results.

[8_modelResults.R](8_modelResults.R): code for extracting model results from all model outputs that were saved in individual files and put them in the same tibble, to compare WAIC and select the best model and offset. 

[9_ExtractPars.R](9_ExtractPars.R): select the best model and offsets for each species according to WAIC and extract coefficient estimates.

[10_Predict_quantiles.R](10_Predict_quantiles.R):  R code to use the best model and offset parameters for each species to make predictions of population numbers for each temperature quantile (0.2, 0.5, 0.8).

[11_PlotPercent.R](11_PlotPercent.R): R code to make plot of percent of population change after 20 years of infestation (Figure 3). 

[12_PlotCoefs..R](12_PlotCoefs..R): R code to plot coefficient estimates, best model and offset (Figure 2 and 4).

[13_Simulation.R](13_Simulation.R): R code to simulate data for the hermit thrush according to the parameters for the best model for the species. We refit the model for every simulation generated (Figure S5).

[14_Sensitivity.R](14_Sensitivity.R): Sensitivity test of the best model and offset year. Sensitivity analysis removes each route at a time and re-fit the model.

[15_PlotSensi.R](15_PlotSensi.R): Create a plot with the predictions of the sensitivity analysis plus the results of the full dataset (Figure S3).
 
[16_Permutation.R](16_Permutation.R): Permutation analysis of the best model and offset year. The permutation analysis randomizes the infestation year x times and refit the model.

[17_PlotPerm.R](17_PlotPerm.R): R code to combine the BBS database with the HWA database (Figure S4).

[18_finalDataset.R](18_finalDataset.R): get final dataset numbers.

[19_Map_inf_county.R](19_Map_inf_county.R): get which counties were used in the analysis, and for how long each has been infested.

## Data for the change in trend after infestation model:
All the bird and hemlock woolly adelgid invasion data are publicly available. [BBS data](https://www.pwrc.usgs.gov/bbs/rawdata/) were downloaded as a .cvs file from https://www.pwrc.usgs.gov/bbs/index.cfm, adelgid infestation shape files were requested to the USDA Forest Service, and temperature data from WorldClim was downloaded using the R package ‘raster’ (version 3.4-13). The following files are the exact same files that are publicly available BBS (3, 4, 6, 8), have been intersected with adelgid data in ArcGis (2), or been created for this analysis (1, 5 and 7). They are all located in data/src folder:

**1. HexMap:** folder with shape file of hexagon mesh of the study area (hexagons.shp).

**2. infestations.rds:** rds file with information of when each BBS route was infested.

**3. route_coor.csv:** cvs table with BBS routes and their geographical coordinates.

**4. route_hex.rds:** rds file with the hexagon location of each route in the spatial mesh.

**5. sps_list:** list of hemlock associates and control species used in the analysis.

**6. StateData:** folder with BBS .csv files with BBS data for all states with hemlock trees.

**7. weather:** csv table indicating observer ID for each BBS route and year.

**8. RouteFips.csv:** cvs table indicating in which FIPS each BBS route is in.

 All the other files loaded in the analysis were created with the code (in data folder), i.e. script 4_singleSps.R creates the .rds files used in 6_model.R.
