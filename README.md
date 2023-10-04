# Pfizer

To use, download this repository and apply the following steps:
It is advised to run the script on a sample of the data. In the folder \CDMInstances you can find the script Take_sample.R. On row 35 of that script the path to the CDM needs to be specified under Inputfolder. On row 36 the new path of the sample should be specified onder Outputfolder. If you run the script a sample of 10000 subjects will be writen to the specified Output folder. 


1   Go to \CDMInstances and create a new folder with a self choosen name and copy the CDM tables to the created folder OR specify the location of the CDM by setting StudyName <- NULL and specify the variable path. 

2   Open the to_run file in R studio. This is located under \Data characterisation\PfizerScript.

3   The variable StudyName (at the biginning) needs to be named with the folder name choosen at step 1. (StudyName <- "FolderName") OR if path is filled set StudyName to NULL.

4   The variable DAP (at the biginning) needs to be named correctly. The correct name can be choosen from 1 row further. 

6  Fill the start_study_date and end_study_date correctly. Date of first vaccination in your country for start_study_date and the day of running the script for end_study_date (today).

7 If you ran the pregnancy algorithm you need to fill the path where the output (D3_pregnancy_final.RData) is located using the varaible preg_dir at the beginning of the to_run file.

8   Run the to_run file. This can be done step wise or in 1 step by selecting all and run. If you want to prematch first run to_run_prematch and fill your CDM EVENT tables using 
    Data characterisation\PfizerScript\g_output\SUBJECT_TO_EXTRACT.csv. After extraction add the EVENT files to the other CDM files and run to_run_after_event_extraction.

9 Save the "Data characterisation\PfizerScript\g_intermediate" folder on a different location of your own choice. (When rerunning all the produced table are deleted). 

10 Run to_run_t4_new and upload the results in \Data characterisation\PfizerScript\g_output\OutputDRE_DATE to DRE



