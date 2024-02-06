# nea_ehi_pupae_analyzer
The purpose of the web application is to accurately split a bimodal distribution to 2 normal distributions with a confidence interval of >90% and Type1 error of 0.1% through computational modelling. 

Problem: In Project Wolbachia, only male mosquitoes can be release into the environment. At the puppae stage, male puppae are much smaller thn female puppae. A sample of puppae is measured through a highthroughput comptuter which differentiate the puppaes and measure them. A bimodal distribution curve is plotted where the left peak is the male and the right peak is the female. Currently, splitting of the bimodal distribution is done manually by looking at the graph. Human error and biasness might cause the splitting to be imprecise. The aim of this anaylzer is to utilize computational modelling (mixmodel package) to split the 2 distribution mathematically. Next, with the model, create an easy to use web application for employees at NEA to use. 

The web application can be accessed via the website below:
https://rshinyapplications.shinyapps.io/EHI_Pupae_Data_Analyzer/ 

Dataset from the highthroughput computer can be found in the date folder. 
