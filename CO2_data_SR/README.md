# Dataset for "Machine Learning the Carbon Footprint of Bitcoin Mining"

This is the README file for the dataset used in "Machine Learning the Carbon footprint of Bitcoin mining". 

The files: 

1) "chinaprice.csv"
2) "usaprice.csv"
3) "russiaprice.csv" 

Contain the historical prices used for the computation of the electricity price in Russia, China, and U.S.A. The three files are then merged into "electricity.xlsx"

The files: 

1) "difficulty.csv"
2) "hashrate.csv"
3) "marketprice.csv"
4) "minersrevenue.scv"

Are the Bitcoin's network statistics. These files are then grouped in the file "networkstat.csv". Additionally, the merged file with no missing data after the MissForest algorithm implementation, and including the number of Bitcoins mined per day is included as "nonmissing.csv". 

The file: 

1) "efficiency.csv"

Contains the historical efficiencies for the different ASIC mining chips as reported in the ASIC miner index. 

The files: 

1) "hashrate_Asia.csv"
2) "hashrate_Europe.csv"
3) "hashrate_USA.csv"

Include the weights and the conversion factors for the considered regions in Asia, Europe, and U.S.A. for the implementation of the novel bottom-up approach. They are collected from CBECI and Shodan.io search engine. 