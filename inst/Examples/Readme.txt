These Examples are part of the salbm package.  

1. 
The data is in Example?/mkData/Txt and are stored as csv files.
To convert them to .rds files run Example?/mkData/toRDS.R
This will produce Example?/RDS/Eg?_1.rds and Example?/RDS/Eg?_2.rds

2.
In Example?/Est/Bounds there is a program bounds.R.  This runs
salbm or salbmM on the data to determine the range of sensitivity
parameters, alpha.  After running bounds.R run BoundsPlotE.R 
and BoundsPlotS.R to produce two plots (in the Plots subdirectory).  
Choose a lower alpha where the curve begins to move upward and a
upper alpha where the curve levels off. (see the plots).

3.
In Example?/Est run the Main.R program with the range of alphas
found in 2.  This produces the results from the main datasets.
samp_part.R then produces bootstrap samples (these can take
some time to run and samp_part.R can be run on multiple processors)
The program get.R then assembles the parts and computes CI etc.

4.
Once the main results and bootstraps have been assembled, the
programs in Example?/Plots produce plots of different types.

5.
The Example?/InferenceAssumptions/InfAss.R produces a table 
comparing different missingness assumptions placed on the data.

6.
The Example?/Preliminaries directory contains programs to 
produce simple descriptive tables of the data.
