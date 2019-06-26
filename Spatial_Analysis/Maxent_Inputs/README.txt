In this folder you will find all files used to create inputs for use in Maxent.


ASCII_Downscaled_Files folder
- **Same file as found in Downscaling > Outputs**
- Contain downscaled ascii files to 0.083 degree resolution of environmental variables for climate scenarios RCP 4.5 and RCP 8.5 in near future (2040-2050) and far future (2070-2080).
- Environmental variables = pH, photosynthetically active radiation (PAR), salinity, current speeds, sea surface temperature (SST).


Inputs folder
- Contains final inputs for use in Maxent.
- Each folder contains the files for each environmental variable separated by time period and climate scenario.
- For example, folder "RCP4_5_Future" contains the .asc files for current speeds, PAR, pH, salinity, and SST.
- "Coral_Families_Presence.csv" contains the coral locality information for 4 coral families (Acroporidae, Montastraeidae, Merulinidae, and Poritidae) found throughout the Caribbean.


Present_ObservationData folder
- Contains the present observed data for each environmental variable taken from BioOracle or Global Marine Environmental Datasets.
- Detailed information on this can be found in Downscaling > Metadata.xlsx.


Scripts folder
- Contains various RStudio scripts used.
- "GBIF_Species_Locality_Pull.Rmd" contains code used to take coral presence locations for 4 coral families across the Caribbean.
- "Maxent_Input_Wrangling.Rmd" contains code used to crop and resample all .asc downscaled files of enviromental variables to the correct and same extents for use in Maxent.
	- These exported files are found in the "Inputs" folder.