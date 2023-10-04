
#If you want to bootstrap you may need to do the following things
#1  Spilt up the matching procedure. Now only 1 match is picked but now all possible matches need to be stored. 
#2  Reorder a bit and create the database, step 11 directly after making the table with all possible matches
#3  Get the second part of the matching procedure which is the picking of 1 subject and add the sampling before that selecting of 1 match
#4  Extract covaraites, aesi's, do IPW and run needed T4 analyses

#You can choose to extract covariates 500 times or once for all

#The to run file may need to look like the following then

system.time(source(paste0(pre_dir,"Step_09_MatchingProcedure1.R")))

#Reorder, and maybe change the model in way thet the concepts and cohorts are in long format and not separate tables
###
system.time(source(paste0(pre_dir,"Step_11_PutConceptsInDatabase.R")))
system.time(source(paste0(pre_dir,"Step_11b_CleanContiousConcepts.R")))
system.time(source(paste0(pre_dir,"Step_11b_CreateAdditionalConcepts.R")))

#Create covid outcomes
###
system.time(source(paste0(pre_dir,"Step_11d_create_covid_episodes.R")))
system.time(source(paste0(pre_dir,"Step_11e_create_covid_outcomes.R")))

###



system.time(source(paste0(pre_dir,"Step_11c_PrepareDatabase.R")))


############################## Start bootsrtap loop 500 times (or do not run use Step_09_MatchingProcedure2 and add covaraites for all possible matches)

###
system.time(source(paste0(pre_dir,"Step_09_MatchingProcedure2.R"))) #Add sampling in sqlite and from that sampling you extract only 1
system.time(source(paste0(pre_dir,"Step_10_CombineExposedControl.R")))

#Optimize
###
system.time(source(paste0(pre_dir,"Step_13_AddAESI.R")))
peakRAM(source(paste0(pre_dir,"Step_12_AddCoVariates.R")))
# ->peakRAM(source(paste0(pre_dir,"Step_12_AddCoVariates_new.R")) using current model of concepts database
# ->peakRAM(source(paste0(pre_dir,"Step_12_AddCoVariates_new2.R")) using a new model of concetps.db
###

#

############################## End bootsrtap loop