# covid_endemic_vaccine

This is the code repository for running the simulations associated with the manuscript "Capturing SARS-CoV-2 immune landscapes to inform future strategies for COVID-19 vaccination in a high-income setting: a mathematical modelling study".  

The file main.R lists the code directories and the order in which to run the simulations. Briefly, simulations are run using an individual-based model of SARS-CoV-2 transmission and COVID-19 disease. The model is available at https://github.com/abhogan/safir_immunity, and is a modified version of the model at https://github.com/mrc-ide/safir. 

## Details about the scripts  

0_set_up_runs contains the scripts to set up the runs and parameters

The raw files are too large to upload here, but the processed runs are saved in "processed_outputs". You can recreate the plots in the manuscript using the scripts in "analysis". 

Plotting files are as follows  
1A_illustration_curves.R. Produces curves in Figure 1A  
2A_scenario_curves.R. Produces figures in Figure 2A  

Figure S3: plots_calibration_initial_FigS3.R (File: calibrationrtb1mu)  
Figure 2, Figure S2: plots_calibration_mud1_Fig2.R (File: calibrationmud1)  
Fig 3, Fig S5: plots_routine_Fig3.R (File: routinemud1)  
Fig 4, Fig S7, Fig S8: plots_routineescape_fig4.R (File: routineescapemud1)  

File to test out model calibration: calibrationagertb1  

# Tables
analysis/table_routine.R  
analysis/table_routineescape.R
