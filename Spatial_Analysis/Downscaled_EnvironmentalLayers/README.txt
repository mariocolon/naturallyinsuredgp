This folder contains all files used to downscale climate model environmental variables from 1 degree resolution to 0.083 degree resolution.


Downscaling_Geodatabases folder
- Contains all .gdb's used in "Climate_Data.mxd", containing a folder for each variable for RCP 4.5 and RCP 8.5 and the model outlining each tool used.
- For example, "Downscaling_PAR_4_5.gdb" contains each step to downscale photosynthetically active radiation modelled under RCP 4.5.
- "4_5Crop_Export_Files.gdb" and "8_5Crop_Export_files.gdb" contain models to crop & export the downscaled files as .asc files


Inputs folder
- Contains climate model and observed present data.
- "Metadata.xlsx" contains detailed information on each file.


Outputs folder
- Contains final .asc downscaled files for 5 environmental variables in near-future (2040-2050) and far-future (2070-2080) time periods under RCP 4.5 and RCP 8.5 in 0.083 degree resolution.


"Climate_Data.mxd" file
- ArcGIS file used to downscale climate model data from 1 degree resolution to 0.083 degree resolution.
- Exported files are found in the outputs folder.


"Environmental Variable Downscaling Technique" document
- Detailed guide for downscaling in each ArcGIS model found in each .gdb for environmental variables.



