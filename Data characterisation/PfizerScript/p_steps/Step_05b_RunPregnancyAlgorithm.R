#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 6/04/20223

##Aim
#DAP's do run a ARS script locally. This outputs a file containing pregnancy's.The aim of this script is to retrieve the data and store in a way it can be used 
#for the matching (.rds) and co variates sqlite database concepts.db

##in/output
#Input 1: D3_pregnancy_final.RData
#Output 1 2: L_PREGNSTATUS_COV.rds
#Output 2: L_PREGNSTATUS_COV in concepts.db

#Note that this procedure is not well tested because the lack of smample data that is provided.

if(!is.null(preg_dir)){

         #ARS is providing a quality status per pregnancy. Choose here with what to continue
         qualityPregnancy <- c("2_yellow", "1_green", "3_blue")
         
         if(file.exists(paste0(preg_dir,'/D3_pregnancy_final.RData')) ){#& DAP != 'CPRD'){
            load(paste0(preg_dir,'/D3_pregnancy_final.RData'))
            cols <- c('person_id','pregnancy_start_date','pregnancy_end_date')
            D3_pregnancy_final <- as.data.table(D3_pregnancy_final)[highest_quality %in% qualityPregnancy,..cols]
            setnames(D3_pregnancy_final, c("pregnancy_start_date","pregnancy_end_date"), c("ST","EN"))
            saveRDS(D3_pregnancy_final,paste0(concepts_dir,"L_PREGNSTATUS_COV.rds"))
            
            #Open the connection
            ###
            mydb <- dbConnect(RSQLite::SQLite(), dbconcepts)
            dbWriteTable(mydb, "L_PREGNSTATUS_COV" , D3_pregnancy_final, overwrite = T, append = F)
            dbDisconnect(mydb)
            ###
            
            rm(D3_pregnancy_final)
          }


}



