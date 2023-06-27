# relatives_and_neighbours
scripts for reproducing the analysis presented in  Bromham and Yaxley, Relatives and neighbours: the importance of accounting for spatial distribution when testing hypotheses in cultural evolution


The repository contains a vignette allowing the reader to reproduce the analyses presented in the study, specifically the analysis of hand/finger lexical category and language endangerment (hand_finger) and the tonal language and humidity analysis.

The two files contain the data necessary for reproducing these analyses. 

Contents of tonal_languages: 
- Humidity.csv, monthly mean humidity estimates obtained from Kalnay E, Kanamitsu M, Kistler R, Collins W, Deaven D, Gandin L, Iredell M, Saha S, White G, Woollen J, Zhu Y, Leetmaa A, Reynolds B, Chelliah N, Ebisuzaki W, Higgins W, Janowiak J, Mo KC, Ropelewski C, Wang J, Jenne R, Joseph D (2022) Dataset: NOAA NCEP-NCAR CDAS-1 MONTHLY Diagnostic Above_ground.” http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/ In, 
Kummu M, Varis O (2011) The world by latitudes: A global analysis of human population, development level and environment across the north–south axis over the past half century. Applied geography 31(2):495-507

- WALS_data.csv WALS database for tonal languages from Maddieson I (2013) Tone. In: Dryer MS, Haspelmath M (eds) WALS Online (v2020.3)  https://doi.org/10.5281/zenodo.7385533 
(Available online at http://wals.info/chapter/13, Accessed on 2023-03-21.). Zenodo

- passiveWALS.csv WALS database for passive constructions from Siewierska A (2013) Passive Constructions (Available online at http://wals.info/chapter/107). In: Dryer MS, Haspelmath M (eds) WALS Online (v2020.3). https://doi.org/10.5281/zenodo.7385533 Zenodo

- amphibian_richness_raster.rds - the raster of amphibian specie richness produced from the IUCN rang map data. We include the rasterised version as creating the raster from scratch is computationally intensive. Range data was sourced from IUCN (2022) The IUCN Red List of Threatened Species. Version 2022-2.  https://www.iucnredlist.org. Accessed on [03 March 2023]. In: Nature IUftCo (ed)

- idw_interpolation.rds, our interpolated raster of global mean humidity used in the tonality analysis. We include this because production of the raster is computationally intensive and readers using our code as a tutorial may find it impractical to implement the interpolation on their own machines.

Contents of hand_finger: 

- Supplementary_data_S1.csv, language database including endangerment scores. Obtained from the supplementary materials of Bromham L, Dinnage R, Skirgård H, Ritchie A, Cardillo M, Meakins F, Greenhill S, Hua X (2022) Global predictors of language endangerment and the future of linguistic diversity. Nature ecology & evolution 6(2):163-173

- hand_finger.csv - WALS database for hand/finger lexical category, Brown CH (2013) Finger and Hand. In: Dryer MS, Haspelmath M (eds) WALS Online (v2020.3). https://doi.org/10.5281/zenodo.7385533 Zenodo




