# Landscape permeability in urban landscapes

This repository shares the code and data from the paper Bhakti et al. (*in review*), on integrating land cover, animal behavior, and master plan regulations to assess urban landscape permeability for birds. The paper is under review on *Landscape and Urban Planning*.

The repository presents four folders: `input`, `simulated_corridors`, `code`, and `analysis_sensibility`. It also has an `output` folder for saving the output from analyses.

## input

This folder presents all the raster and vector maps used for simulating mutliple least cost corridors: resistance maps for both the land cover (LandC) and Land cover + Urban Zoning (LandC+UrbZ) scenarios, maps or source-target (ST) patches and a list of STs linked by corridors, and land cover and urban zoning maps.

## simulated_corridors

This folder presents the resulting simulated corridors for all species modelled in both simulation scanarios, LandC and LAndC+UrbZ. The corridors were simulated with LSCorridors package ([Ribeiro et al. 2017](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12750)), which is freely available in this [Github repo](https://github.com/LEEClab/LS_CORRIDORS).

## code

All the analyses of simulated corridors was done within [R statistical environment](https://www.r-project.org/). This folder presents all the R code used for such analyses.

## analysis_sensitivity

This presents additional simulated corridors used to assess the effects of the model assumptions and parameters on the output simulated corridors.

## Citation

Bhakti, T.; Pena, J.C.; Niebuhr, B.B.; Sampaio, J.; Goulart, F.F.; Azevedo, C.S.; Ribeiro, M.C., and Antonini, Y. Combining land cover, animal behavior, and master plan regulations to assess landscape permeability for birds. *In review*.

You can also cite this repository, which contains both the code and data used in the publication:  
Bhakti, T.; Pena, J.C.; Niebuhr, B.B.; Sampaio, J.; Goulart, F.F.; Azevedo, C.S.; Ribeiro, M.C., and Antonini, Y. (2020). Code and Data from Bhakti et al. on landscape permeability of urban landscape (Version 1.0) [Code and data set]. Zenodo.   
[![DOI](https://zenodo.org/badge/225414362.svg)](https://zenodo.org/badge/latestdoi/225414362)

# On the Media

No info from this study on the news yet.

## Contact

If you have questions or suggestions, do not hesitate to contact us:
+ Tulaci Bhakti <<tulaci.faria.duarte@gmail.com>>  
+ João Carlos Pena <<joaocpena@gmail.com>>  
+ Bernardo Brandão Niebuhr <<bernardo_brandaum@yahoo.com.br>>  

