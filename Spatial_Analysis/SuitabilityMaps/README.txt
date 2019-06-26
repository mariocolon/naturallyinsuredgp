In this Suitability_Maps folder, you will find all folders & files used to create coral habitat suitability maps:


America_Shapefile folder
- Contains the americas_cropped.shp used in ArcGIS files


Depth_Layer folder
- Contains the depth mask used to mask out depths >100m from the Maxent outputs for each time period and climate scenario.


Final_Map_Images folder
- Contains .png files for suitability maps for present and far-future (2070-2080) time periods for RCP 4.5 and 8.5.
- Files with boxes highlight regions where we recommend to look closer at for possible successful coral restoration projects.


Maxent_OutputASCII folder
- Contains the Maxent .asc outputs for present, near-future (2040-2050) and far-future (2070-2080) time periods for RCP 4.5 and 8.5.
- For example, "coral_far45.asc" is the output for the far-future under RCP 4.5.


Suitability_Maps.gdb folder
- Contains all masked files and difference maps.
- Can only be viewed in ArcGIS.


Suitability_Maps.mxd file 
- Used to mask out depths >100m from Maxent ASCII outputs for each time period in each climate scenario.
- Next, difference maps were created by subtracting the masked present suitability map from the masked future suitability maps.
- These steps are easily outlined within the ArcGIS model found in Suitability_Maps.gdb > Depth_Toolbox (must be viewed within ArcGIS).


SuitabilityMaps_Final.mxd file
- Used to create finalized images created for presentation, brief, and poster. The exported images of these maps are found in the Final_Map_Images folder.
- Present_Habitat suitability = Present_Masked found in Suitability_Maps.gdb
- Far45_Suitability Change = Far45_Masked found in Suitability_Maps.gdb
- Far85_Suitability Change = Far85_Masked found in Suitability_Maps.gdb
- Near45_Suitability Change = Near45_Masked found in Suitability_Maps.gdb
- Near85_Suitability Change = Near85_Masked found in Suitability_Maps.gdb
- Americas = americas_cropped.shp found in America_Shapefile folder

