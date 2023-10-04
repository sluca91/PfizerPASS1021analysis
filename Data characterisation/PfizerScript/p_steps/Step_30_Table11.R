#Author: Albert Cid Royo MSc.
#email: a.cidroyo@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/01/2022

##Aim
#Generate Table 11

##in/output
#Input 1: 

#Output 1:

library(data.table)



SCRIPT <- RUN_SCRIPT(name = "Step_13_AddAESI.R")
M_Studycohort3 <- readRDS(SCRIPT[["OUTPUT1"]][["path"]])

Vaccinated <- M_Studycohort3[S_Group == 'EXPOSED'][,month_t0 := paste0(sprintf("%02d",month(FIRST_PFIZER)),'-',sprintf("%04d",year(FIRST_PFIZER)))]
UnVaccinated <- M_Studycohort3[S_Group == 'CONTROL',][,month_t0 := paste0(sprintf("%02d",month(FIRST_PFIZER)),'-',sprintf("%04d",year(FIRST_PFIZER)))]

Distribution1_age <- summary(Vaccinated$AGE_T0)
Distribution3_age <- summary(UnVaccinated$AGE_T0) 

###FIRST DOSE POPULATION

#Count Per Age band
bands1 <- as.data.table(CreateBands(c(0,6,12,18,30,40,50,60,70,80), NEWBORNS = F))
bands2 <- as.data.table(CreateBands(c(80,120), NEWBORNS = F))[band0 == "080-119", band0 := "80+"]
bands <- rbind(bands1, bands2)[,.(INT, band0)]
setnames(bands, "band0", "band")

#In case we don't have all the population we still need to generate the same amount of bands
#THis is a automatic way to do it.
# Create the fixed bands (code above) and then merge with the ones we calculate from the data

uniqueBands <- data.frame('band' = unique(bands$band), 'N' = rep(0,length(bands)))

countBand <- Vaccinated[, .N, by = band][order(band)]

results<-merge(x=uniqueBands,y=countBand,by='band',all.x=TRUE)


listCountBand <- results$N.y
names(listCountBand) <- results$band


# #Count Per Region band
countRegion <- Vaccinated[, .N, by = REGION][order(REGION)]
listCountRegion <- countRegion$N
names(listCountRegion) <- countRegion$REGION

# Count Per Month band
countMonth <- Vaccinated[, .N, by = month_t0][order(month_t0)]
listCountMonth <- countMonth$N
names(listCountMonth) <- countMonth$month_t0


col1 = as.list(c('N' = Vaccinated[,.N],
                 '',
                 '',
                 'age_mean' = Distribution1_age[[4]],
                 'age_median' = Distribution1_age[[3]],
                 'N',
                 listCountBand,
                 'female' = Vaccinated[sex_at_instance_creation == 'F',.N],
                 '',
                 listCountRegion,
                 '',
                 listCountMonth,
                 'PriorInfection' = Vaccinated[FIRST_COV_INF == TRUE,.N],
                 'PriorInfluenzaVac' = Vaccinated[INFP5 == TRUE,.N]
)) 
col2 = as.list(c('N' = Vaccinated[,.N],
                 '',
                 '',
                 'std' = sd(Vaccinated$AGE_T0),
                 'q1_q3' = paste0('(',Distribution1_age[[2]],',',Distribution1_age[[5]],')'),
                 '%',
                 listCountBand/Vaccinated[,.N]*100,
                 'female' = Vaccinated[sex_at_instance_creation == 'F',.N]/Vaccinated[,.N]*100,
                 '',
                 listCountRegion/Vaccinated[,.N]*100,
                 '',
                 listCountMonth/Vaccinated[,.N]*100,
                 'PriorInfection_per' = Vaccinated[FIRST_COV_INF == TRUE,.N]/Vaccinated[,.N]*100,
                 'PriorInfluenzaVac_per' = Vaccinated[INFP5 == TRUE,.N]/Vaccinated[,.N]*100
))



N <- length(col2)+1  # total number of rows to preallocate--possibly an overestimate
template_table11 <- data.frame(HEADER=rep('', N), col1=rep('', N),col2=rep('', N),  # as many cols as you need
                              stringsAsFactors=FALSE)
template_table11$HEADER =  c( '',
                             'Baseline Characteristics',
                             'Demographics',
                             'Age (years)',
                             'Mean (SD)',
                             'Median (Q1, Q3)',
                             'Age groups (years), n (%)',
                             '0-5 ',
                             '6-11',
                             '12-17 ',
                             '18-29 ',
                             '30-39',
                             '40-49',
                             '50-59',
                             '60-69',
                             '70-79',
                             '80+',
                             'Female, n (%)',
                             'Geographic region, n (%)',
                             names(listCountRegion),
                             'Date of vaccination (year or month), n (%)' ,
                             names(listCountMonth),
                             'Prior COVID‑19 infection, n (%)',
                             'Prior Influenza vaccination, n (%)'
)

template_table11[1,c(2:3)] <- c('Vaccinated')


### Put template and results together

table11_1 <- data.table(cbind(col1,col2))
filledTemplate11_1 <- Add_Results_To_Template(template_table11,table11_1)


fwrite(filledTemplate11_1, file = paste0(output_dir,DAP,'_Table11_Vaccinated.csv'))



###################################################
#Unvaccinated TABLE
###################################################

countBand <- UnVaccinated[, .N, by = band][order(band)]

results<-merge(x=uniqueBands,y=countBand,by='band',all.x=TRUE)


listCountBand <- results$N.y
names(listCountBand) <- results$band


# #Count Per Region band
countRegion <- UnVaccinated[, .N, by = REGION][order(REGION)]
listCountRegion <- countRegion$N
names(listCountRegion) <- countRegion$REGION

# Count Per Month band
countMonth <- UnVaccinated[, .N, by = month_t0][order(month_t0)]
listCountMonth <- countMonth$N
names(listCountMonth) <- countMonth$month_t0


col1_2 = as.list(c('N' = UnVaccinated[,.N],
                 '',
                 '',
                 'age_mean' = Distribution3_age[[4]],
                 'age_median' = Distribution3_age[[3]],
                 'N',
                 listCountBand,
                 'female' = UnVaccinated[sex_at_instance_creation == 'F',.N],
                 '',
                 listCountRegion,
                 '',
                 listCountMonth,
                 'PriorInfection' = UnVaccinated[FIRST_COV_INF == TRUE,.N],
                 'PriorInfluenzaVac' = UnVaccinated[INFP5 == TRUE,.N]
)) 
col2_2 = as.list(c('N' = UnVaccinated[,.N],
                 '','',
                 'std' = sd(UnVaccinated$AGE_T0),
                 'q1_q3' = paste0('(',Distribution3_age[[2]],',',Distribution1_age[[5]],')'),
                 '%',
                 listCountBand/UnVaccinated[,.N]*100,
                 'female' = UnVaccinated[sex_at_instance_creation == 'F',.N]/UnVaccinated[,.N]*100,
                 '',
                 listCountRegion/UnVaccinated[,.N]*100,
                 '',
                 listCountMonth/UnVaccinated[,.N]*100,
                 'PriorInfection_per' = UnVaccinated[FIRST_COV_INF == TRUE,.N]/UnVaccinated[,.N]*100,
                 'PriorInfluenzaVac_per' = UnVaccinated[INFP5 == TRUE,.N]/UnVaccinated[,.N]*100
))





N <- length(col2_2)+1  # total number of rows to preallocate--possibly an overestimate
template_table11_2 <- data.frame(HEADER=rep('', N),col1=rep('', N),col2=rep('', N),  # as many cols as you need
                              stringsAsFactors=FALSE)
template_table11_2[1,c(2:3)] <- c('Unvaccinated')
template_table11_2$HEADER =  c( '',
                              'Baseline Characteristics',
                              'Demographics',
                              'Age (years)',
                              'Mean (SD)',
                              'Median (Q1, Q3)',
                              'Age groups (years), n (%)',
                              '0-5 ',
                              '6-11',
                              '12-17 ',
                              '18-29 ',
                              '30-39',
                              '40-49',
                              '50-59',
                              '60-69',
                              '70-79',
                              '80+',
                              'Female, n (%)',
                              'Geographic region, n (%)',
                              names(listCountRegion),
                              'Date of vaccination (year or month), n (%)' ,
                              names(listCountMonth),
                              'Prior COVID‑19 infection, n (%)',
                              'Prior Influenza vaccination, n (%)'
)


table11_2 <- data.table(cbind(col1_2,col2_2))
filledTemplate11_2 <- Add_Results_To_Template(template_table11_2,table11_2)

fwrite(filledTemplate11_2, file = paste0(output_dir,DAP,'_Table11_UnVaccinated.csv'))



#################


# flexTable_Table2 <- Create_FlexTable(filledTemplate2,1,"This a footer")
# flexTable_Table2






