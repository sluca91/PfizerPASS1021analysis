

StudyVarsToTest <- c("L_GEOREGION_COV", 
                     "I_COVID_COV", 
                     "L_SOCIOECO_COV", 
                     "V_CDC_COV" , 
                     "IM_IMC_COV", 
                     "INF",
                     "L_SMOKESTATUS_COV", 
                     "L_BMICALC_COV",
                     "I_SEPSIS_COV", 
                     "L_CHARLSON_COV," ,
                     "Im_ALLERGIES_COV",
                     "C_HF_AESI",
                     "O_DEATHANY_AESI",
                     "L_RACE_COV",
                     "L_SEX_COV",
                     "O_YEARBIRTH_COV",
                     "COV",
                     "L_CHARLSON_COV",
                     "B_TTS_AESI",
                     "H_HOSPNUM_COV",
                     "DP_COVCANCER",
                     "L_PREGNSTATUS_COV",
                     "H_HOSPNUM_COV",
                     "H_EMERG_COV",
                     "H_NURSING_COV",
                     "H_PRIMARYHCUSE_COV",
                     "TP_CANCERSCREEN_COV",
                     "H_PREVENTIVE_COV",
                     "TP_COVID19TEST_COV",
                     "H_HOSPPOP_POP",
                     "B_ITP_AESI",
                     "I_SEPSIS_COV",
                     "Im_ANAPHYLAXIS_AESI"
                     
                     
                     
                     
                     
)

StudyVarsToTest <- toupper(unique(c(TEMP2[NEW_CONCEPT %in% StudyVarsToTest,]$CONCEPT, StudyVarsToTest)))
TEMP2 <- TEMP2[toupper(NEW_CONCEPT) %in% StudyVarsToTest | toupper(CONCEPT) %in% StudyVarsToTest,]
TEMP1 <- TEMP1[toupper(VarName) %in% StudyVarsToTest,]
