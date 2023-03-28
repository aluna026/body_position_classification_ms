# Body Position Classification

This repository contains the data and code in support of the manuscript "Long-form recording of infant body position in the home using wearable inertial sensors". There are two main pieces. 

First, the folder "group_split_comparison" contains the windowed motion feature data used to train and test models, and has a script that runs the classification process (exporting results to the "data" folder). This computation takes a long time, and so is separated from the RMarkdown manuscript. The results of this calculation already appear in "data", so do not need to be re-run by someone who would like to reproduce the manuscript.

Second, the file manuscript.Rmd contains all text and code that generate the submitted manuscript, manuscript.pdf. All intermediary data files are drawn from the "data" folder. With the exception of Figure 1 (which is in the figure folder), all figures are generated directly from within the .Rmd file.

Session_Info.txt contains the R version and package versions required to reproduce the calculations in an identical software environment.
