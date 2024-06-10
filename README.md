
R scripts developed in the frame of the [V2MOC projet](https://envt.fr/actualites/focus-sur-le-projet-v2moc/) (*Impact de la végétalisation des villes sur le risque vectoriel chez l’Homme et les animaux*), as part the PhD thesis of Colombine Bartholomée (IRD, UMR MIVEGEC). 

These script aim at modeling the spatio-temporal distribution and abundance of the mosquito *Aedes Albopictus* in the city of Montpellier (France) using multisource fine-scale data (entomological, meteorological, and landscape). 

- `1_rawdata_preparation.R` : script to pre-process the raw data (cropping, filtering, etc.).
- `2_variables_extraction.R` : script to generate the statistical variables for the models.
- `3_data_visualisation.R` : script to visualize the associations between the dependent and independent variables.
- `4_bivariate_model_building.R` : script for the generation of the bivariate models (cross correlation maps, etc).
- `5_bivariate_model_interpretation.R` : script to visualize graphically the outputs of the bivariate models. 
- `6_multivariate_model_building.R` : script for the generation of the multivariate models (random forests).
- `7_multivariate_model_interpretation.R` : script to visualize graphically the outputs of the multivariate models (variable importance plots, partial dependence plots). 
- `function.R` : set of functions used in the model generation and interpretation scripts.

The associated publication is in preparation.


![](picture_albo_montpellier.png)
![](funding.png)