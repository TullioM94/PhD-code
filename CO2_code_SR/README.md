# Code for "Machine Learning the Carbon Footprint of Bitcoin Mining"

This is the README file for the code used in "Machine Learning the Carbon footprint of Bitcoin Mining"

There are two files: 

1) Sheet1.R
2) Sheet2.R 

At the begininng of both documents, the relevant packages needed for the execution of the code are loaded. Additionally, in order to use the Keras library for RStudio, it is necessary to create a virutal enviromnment called "r-tensorflow" which is subsequently activated with the command use_condaenv("r-tensorflow"). 

The first script (Sheet1.R) allows obtaining the realistic level of CO2 using a top down approach with either brown or green energy, and with the novel bottom up approach proposed in the paper. Additionally, it provides the base code for the implementation of the MC Dropout. In-line comments allow the reader navigating through the different operations. 

- From line 10 to line 47 the libraries and the datasets are loaded.  
- From line 59 to line 140 the relevant code for data analysis is reported.
- From line 190 to line 277 the code for the top down approach is reported. 
- From line 289 to line 498 the code for the botton up approach is reported. 
- From line 510 to line 571 sample code for the implementation of the MC dropout is provided. 
- From line 601 to line 632 the code for the fitting of the Random forest is reported. 

The second script (Sheet2.R) allows the computation of the upper and lower bounds obtained from Hayes (2017) in terms of both electricity consumption and CO2 emissions. When the bounds for the carbon footprint are computed, the two conversion factors adopted are reported. Also in this case the relevant libraries necessary for the implementation are loaded at the beginning of the script. In this case, it is not necessary to activate the "r-tensorflow" environment as there is no neural network fitting involved. Also in thi case, in line comments are reported. 

- From line 10 to line 18 the libraries and the datasets are loaded.
- From line 29 to line 96 the code for the computation of the average electricity price is reported. 
- From line 109 to line 123 the code for the upper bound (energy consumption) is reported. 
- From line 132 to line 193 the code for the lower bound (energy consumption) is reported.
- The remaining lines of code allows the computation of the upper and lower economic bounds for the CO2 emissions assuming either brown or green energy.
