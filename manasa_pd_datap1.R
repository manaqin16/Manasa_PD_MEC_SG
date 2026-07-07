getwd()
#set wd
setwd("C:/Users/MANASA Y/OneDrive/Desktop/YingYuan")

#load packages
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)

#load dataset
df <- read.csv('yy20230818.csv')

#rename columns
df <- df %>%
  rename("regularmed"="B11","pmh_hd"="B21","pmh_stroke"="B31",
         "pmh_tia"="B32","pmh_hbp"="B41","pmh_diabetes"="B51","pmh_hc"="B61","pmh_asthma"="B711",
         "pmh_rheumatism"="B712","pmh_thyroid"="B713","pmh_arth"="B714","pmh_gas"="B715",
         "pmh_bron"="B716","pmh_emphy"="B717","pmh_cancer"="B718","maritalstatus"="H4",
         "workstatus"="H5","income"="H6","housing"="H7","education"="H8")

#drop columns
df <- df %>%
  select(-"hyperchol_measured2",-"diabetes_measured",-"htn_measured")

#change continuous variables to numeric
df <- df %>%
  mutate(across(c("height","weight","waist","hip","systolic1","systolic2","systolic3","diastolic1",
                  "diastolic2","diastolic3","CHO","LDLC","LDLM","HDL","TG","GLUR","GLUF","total_mvpa",
                  "Q31","Q32","Q33","Q34","Q35","Q36","Q37","Q38","Q39","Q310",
                  "orienttime","orientplace","regscore","nooftrails","attcalc","recall","naming",
                  "repet","compre","reading","writing","drawing"), as.numeric))

#histograms
#hist(df$height)
#hist(df$weight)
#hist(df$waist)
#hist(df$hip)
# hist(df$systolic1)
# hist(df$systolic2)
# hist(df$systolic3)
# hist(df$diastolic1)
# hist(df$diastolic2)
# hist(df$diastolic3)
# hist(df$CHO)
# hist(df$LDLC)
# hist(df$LDLM)
# hist(df$HDL)
#hist(df$TG)
# hist(df$GLUR)
# hist(df$GLUF)
#hist(df$total_mvpa)

#create new column interview year
df<- separate(df,intdate, into=c("intday","intmonth","intyear", sep="/"))

#create new column age
df <- df %>%
  mutate(across(c("intyear","dobyear"), as.numeric))
df$age <- df$intyear - df$dobyear

#create new column for alcohol consumption. 1=Yes, 2=No 
df$heavyalcohol <- ifelse(df$A22a == "NULL", df$A22b, df$A22a)

#create new column mmsesum
df$mmsesum <- df$orienttime + df$orientplace + df$regscore +
  df$attcalc + df$recall + df$naming + df$repet +
  df$compre + df$reading + df$writing + df$drawing

df$mmsesum <- ifelse(is.na(df$mmsesum) | df$mmsesum > 30, "NA",df$mmsesum)

#create new column k10sum
df$k10sum <- df$Q31 + df$Q32 + df$Q33 + df$Q34 + df$Q35 + df$Q36 +
  df$Q37 + df$Q38 + df$Q39 + df$Q310

df$k10sum <- ifelse(is.na(df$k10sum) | df$k10sum > 50, "NA",df$k10sum)

#create new column BMI
df$height <- ifelse(df$height == 8.88, NA, df$height)
df$bmi <- df$weight/(df$height * df$height)
df$bmi <- round(df$bmi, digits = 1)

#create new column WHR
df$whr <- df$waist/df$hip
df$whr <- round(df$whr, digits=2)

#create new sbp_calculated column 
df <- df %>%
  mutate(
    s1s2avg = (systolic1 + systolic2) / 2,
    s2s3avg = (systolic2 + systolic3) / 2,
    s1s3avg = (systolic1 + systolic3) / 2,
    diffs1s2 = abs(systolic1 - systolic2),
    diffs2s3 = abs(systolic2 - systolic3),
    diffs1s3 = abs(systolic1 - systolic3),
    code_sbp = case_when(
      diffs1s2 == diffs2s3 ~ 2,
      diffs2s3 == diffs1s3 ~ 2,
      diffs1s2 == diffs1s3 ~ 2,
      diffs1s2 < diffs2s3 & diffs1s2 < diffs1s3 ~ 1,
      diffs2s3 < diffs1s2 & diffs2s3 < diffs1s3 ~ 2,
      diffs1s3 < diffs1s2 & diffs1s3 < diffs2s3 ~ 3
    ),
    sbp_calculated = case_when(
      is.na(systolic2) & is.na(systolic3) ~ systolic1,
      is.na(systolic3) ~ s1s2avg,
      !is.na(systolic1) & !is.na(systolic2) & !is.na(systolic3) & code_sbp == 1 ~ s1s2avg,
      !is.na(systolic1) & !is.na(systolic2) & !is.na(systolic3) & code_sbp == 2 ~ s2s3avg,
      !is.na(systolic1) & !is.na(systolic2) & !is.na(systolic3) & code_sbp == 3 ~ s1s3avg,
      TRUE ~ NA_real_
    )
  )

df$sbp_calculated <-ifelse(is.na(df$sbp_calculated),df$s2s3avg,df$sbp_calculated)
df$sbp_calculated <- ceiling(df$sbp_calculated)

#create new dbp_calculated column
df <- df %>%
  mutate(
    d1d2avg = (diastolic1 + diastolic2) / 2,
    d2d3avg = (diastolic2 + diastolic3) / 2,
    d1d3avg = (diastolic1 + diastolic3) / 2,
    diffd1d2 = abs(diastolic1 - diastolic2),
    diffd2d3 = abs(diastolic2 - diastolic3),
    diffd1d3 = abs(diastolic1 - diastolic3),
    code_dbp = case_when(
      diffd1d2 == diffd2d3 ~ 2,
      diffd2d3 == diffd1d3 ~ 2,
      diffd1d2 == diffd1d3 ~ 2,
      diffd1d2 < diffd2d3 & diffd1d2 < diffd1d3 ~ 1,
      diffd2d3 < diffd1d2 & diffd2d3 < diffd1d3 ~ 2,
      diffd1d3 < diffd1d2 & diffd1d3 < diffd2d3 ~ 3
    ),
    dbp_calculated = case_when(
      is.na(diastolic2) & is.na(diastolic3) ~ diastolic1,
      is.na(diastolic3) ~ d1d2avg,
      !is.na(diastolic1) & !is.na(diastolic2) & !is.na(diastolic3) & code_dbp == 1 ~ d1d2avg,
      !is.na(diastolic1) & !is.na(diastolic2) & !is.na(diastolic3) & code_dbp == 2 ~ d2d3avg,
      !is.na(diastolic1) & !is.na(diastolic2) & !is.na(diastolic3) & code_dbp == 3 ~ d1d3avg,
      TRUE ~ NA_real_
    )
  )

df$dbp_calculated <-ifelse(is.na(df$dbp_calculated),df$d2d3avg,df$dbp_calculated)
df$dbp_calculated <- ceiling(df$dbp_calculated)

#create new column education_combined where NULL values in education is replaced with baseline education values
df$education_combined <- ifelse(
  df$education == "NULL"|df$education == "666"|df$education == "888",
  df$edu_baseline, df$education)

#create new column housing_combined where NULL values in housing is replaced with baseline housing values
df$housing_combined <- ifelse(
  df$housing == "NULL"|df$housing == "666"|df$housing == "999",
  df$h12housety, df$housing)

#c2missing
df$c2missing <- ifelse(
  apply(df[, c("C2father", "C2mother", "C2brother", "C2sister", 
               "C2son", "C2daughter", "C2PUncle", "C2PAunty", "C2MUncle", "C2MAunty")], 1, function(row) any(row %in% c("1", "2"))),
  "no",
  "NULL"
)

#c4missing
df$c4missing <- ifelse(
  apply(df[, c("C4father","C4mother", "C4brother","C4sister","C4son","C4daughter","C4Puncle","C4PAunty","C4MUncle","C4MAunty"	
  )], 1, function(row) any(row %in% c("1", "2"))),
  "no",
  "NULL"
)

#C6missing
df$c6missing <- ifelse(
  apply(df[, c("C6father","C6mother","C6brother","C6sister",
               "C6son","C6daughter","C6PUncle","C6PAunty","C6MUncle","C6MAunty")], 1, function(row) any(row %in% c("1", "2"))),
  "no",
  "NULL"
)

#C8missing
df$c8missing <- ifelse(
  apply(df[, c("C8father","C8mother","C8brother","C8sister","C8son","C8daughter",
               "C8PUncle","C8PAunty","C8MUncle","C8MAunty")], 1, function(row) any(row %in% c("1", "2"))),
  "no",
  "NULL"
)

#create new column family history of heart disease. 1= Yes, 2 = No
familyhist_hd<- c("C2father","C2mother", "C2brother",
                  "C2sister","C2son","C2daughter","C2PUncle","C2PAunty","C2MUncle","C2MAunty")
df <- df%>%
  mutate(familyhist_hd = ifelse(rowSums(df[,familyhist_hd] == 1) > 0, 1, 2))  
df$familyhist_hd <- ifelse(df$c2missing == "NULL", "NULL", df$familyhist_hd)

#create new column family history of high blood pressure 1= Yes, 2 = No
familyhist_hbp<- c("C4father","C4mother","C4brother","C4sister","C4son","C4daughter","C4Puncle",
                   "C4PAunty","C4MUncle","C4MAunty")
df<- df %>%
  mutate(familyhist_hbp = ifelse(rowSums(df[,familyhist_hbp] == 1) > 0, 1, 2))  
df$familyhist_hbp <- ifelse(df$c4missing == "NULL", "NULL", df$familyhist_hbp)

#create new column family history of diabetes 1= Yes, 2 = No
familyhist_diabetes<- c("C6father","C6mother","C6brother","C6sister","C6son","C6daughter",
                        "C6PUncle","C6PAunty","C6MUncle","C6MAunty")
df <- df %>%
  mutate(familyhist_diabetes = ifelse(rowSums(df[,familyhist_diabetes] == 1) > 0, 1, 2))  
df$familyhist_diabetes <- ifelse(df$c6missing == "NULL", "NULL", df$familyhist_diabetes)

#create new column family history of cancer 1= Yes, 2 = No
familyhist_cancer<- c("C8father","C8mother","C8brother",
                      "C8sister","C8son","C8daughter","C8PUncle","C8PAunty","C8MUncle","C8MAunty")
df <- df %>%
  mutate(familyhist_cancer = ifelse(rowSums(df[,familyhist_cancer] == 1) > 0, 1, 2))
df$familyhist_cancer <- ifelse(df$c8missing == "NULL", "NULL", df$familyhist_cancer)

#create new column for any family history
df <- df %>%
  mutate(
    familyhist_any = case_when(
      rowSums(select(., starts_with("familyhist_")) == "NULL") == 4 ~ "NULL",
      rowSums(select(., starts_with("familyhist_")) == "2") == 4 ~ "2",
      any(rowSums(select(., starts_with("familyhist_")) == "1") > 0) ~ "1",
      TRUE ~ NA_character_
    )
  )

#create new column for hypertension_measured
df$hypertension_measured <- ifelse(df$sbp_calculated >139 | df$dbp_calculated >89,1,2)

#create new column hypertension_combined for self-reported hypertension and hypertension_measured
df$hypertension_measured <- as.character(df$hypertension_measured)
df <- df %>%
  mutate(hypertension_combined = case_when(
    pmh_hbp == "1" & hypertension_measured == "1" ~ "1",
    pmh_hbp == "2" & hypertension_measured == "1" ~ "1",
    pmh_hbp == "2" & hypertension_measured == "2" ~ "2",
    pmh_hbp == "1" & hypertension_measured == "2" ~ "1",
    is.na(hypertension_measured) ~ pmh_hbp,
    pmh_hbp %in% c("NULL", "999") ~ hypertension_measured,
    TRUE ~ NA_character_
  ))

df$hypertension_combined <- ifelse(df$hypertension_combined == "999", "NULL",df$hypertension_combined)


#create new column for diabetes_measured
df$diabetes_measured_gluf <- ifelse(df$GLUF == "NA", "NA",
                                    ifelse(df$GLUF >=7,1,2))

df$diabetes_measured_glur <- ifelse(df$GLUR == "NA", "NA",
                                    ifelse(df$GLUR >=11.1,1,2))

df$diabetes_measured <- ifelse(is.na(df$diabetes_measured_gluf) & is.na(df$diabetes_measured_glur), NA,
                               ifelse(is.na(df$diabetes_measured_gluf), df$diabetes_measured_glur, df$diabetes_measured_gluf))

#create new column diabetes_combined for self-reported diabetes and diabetes_measured
df$diabetes_measured <- as.character(df$diabetes_measured)
df <- df %>%
  mutate(diabetes_combined = case_when(
    pmh_diabetes == "1" & diabetes_measured == "1" ~ "1",
    pmh_diabetes == "2" & diabetes_measured == "1" ~ "1",
    pmh_diabetes == "2" & diabetes_measured == "2" ~ "2",
    pmh_diabetes == "1" & diabetes_measured == "2" ~ "1",
    is.na(diabetes_measured) ~ pmh_diabetes,
    pmh_diabetes %in% c("NULL", "999") ~ diabetes_measured,
    TRUE ~ NA_character_
  ))

df$diabetes_combined <- ifelse(df$diabetes_combined == "999", "NULL",df$diabetes_combined)

#create new column for dyslipidaemia_measured
df$dyslipidaemia_measured_CHO <- ifelse(df$CHO == "NA", "NA",
                                        ifelse(df$CHO >=6.2,1,2))

df$dyslipidaemia_measured_LDLM <- ifelse(df$LDLM == "NA", "NA",
                                         ifelse(df$LDLM >=4.1,1,2))

df$dyslipidaemia_measured_TG <- ifelse(df$TG == "NA", "NA",
                                       ifelse(df$TG >=2.3,1,2))

df$dyslipidaemia_measured <- ifelse(is.na(df$dyslipidaemia_measured_CHO) & is.na(df$dyslipidaemia_measured_LDLM) & is.na(df$dyslipidaemia_measured_TG), NA,
                                    ifelse(df$dyslipidaemia_measured_CHO == 1 | df$dyslipidaemia_measured_LDLM == 1 | df$dyslipidaemia_measured_TG == 1, 1, 2))

#create new column hypertension_combined for self-reported dyslipidaemia and dyslipidaemia_measured
df$dyslipidaemia_measured <- as.character(df$dyslipidaemia_measured)
df <- df %>%
  mutate(dyslipidaemia_combined = case_when(
    pmh_hc == "1" & dyslipidaemia_measured == "1" ~ "1",
    pmh_hc == "2" & dyslipidaemia_measured == "1" ~ "1",
    pmh_hc == "2" & dyslipidaemia_measured == "2" ~ "2",
    pmh_hc == "1" & dyslipidaemia_measured == "2" ~ "1",
    is.na(dyslipidaemia_measured) ~ pmh_hc,
    pmh_hc %in% c("666","NULL", "999") ~ dyslipidaemia_measured,
    TRUE ~ NA_character_
  ))

df$dyslipidaemia_combined <- ifelse(df$dyslipidaemia_combined == "666"|df$dyslipidaemia_combined == "999", "NULL",df$dyslipidaemia_combined)

#change 666,888,999 values in pmh columns  to NULL
columns_to_replace <- c("regularmed","pmh_hd","pmh_stroke", "pmh_tia","pmh_hbp",
                        "pmh_diabetes",	"pmh_hc", "pmh_asthma",	"pmh_rheumatism",	
                        "pmh_thyroid", "pmh_arth", "pmh_gas", "pmh_bron", "pmh_emphy", "pmh_cancer")

for (col in columns_to_replace) {
  df[[col]] <- ifelse(df[[col]] %in% c(666,888, 999), "NULL", df[[col]])
}


#create new column pmh_any for any personal medical history. 1 = Yes, 2 = No
df$pmh_any <- 2

row_all_null <- rowSums(df[, c("hypertension_combined", "diabetes_combined","dyslipidaemia_combined",
                               "pmh_hd","pmh_stroke", "pmh_tia","pmh_asthma",	"pmh_rheumatism",	
                               "pmh_thyroid", "pmh_arth", "pmh_gas", "pmh_bron", "pmh_emphy", "pmh_cancer")] == "NULL") == length(c("hypertension_combined", "diabetes_combined","dyslipidaemia_combined",
                                                                                                                                    "pmh_hd","pmh_stroke", "pmh_tia","pmh_asthma",	"pmh_rheumatism",	
                                                                                                                                    "pmh_thyroid", "pmh_arth", "pmh_gas", "pmh_bron", "pmh_emphy", "pmh_cancer"))
row_has_1 <- rowSums(df[, c("hypertension_combined", "diabetes_combined","dyslipidaemia_combined",
                            "pmh_hd","pmh_stroke", "pmh_tia","pmh_asthma",	"pmh_rheumatism",	
                            "pmh_thyroid", "pmh_arth", "pmh_gas", "pmh_bron", "pmh_emphy", "pmh_cancer")] == "1") > 0

df$pmh_any[row_all_null] <- "NULL"
df$pmh_any[row_has_1] <- "1"

#create new column for smoking_status_cat. 1 =  non-smoker. 2 = past smoker. 3 = current smoker.

df$smoking_status_cat <- ifelse(df$smoking_status == "4" , "3", df$smoking_status)

#create new column for education_cat. 1=no education, 2=primary education, 3=Secondary education and above
df <- df %>%
  mutate(
    education_cat = case_when(
      education_combined == "1" ~ "1",
      education_combined == "2" ~ "2",
      education_combined %in% c("3", "4", "5", "6") ~ "3",
      education_combined %in% c("888", "NULL") ~ "NULL",
      TRUE ~ NA_character_
    )
  )

#create new column cognition_cat for cognition group based on education level. 1= YES cognitively impaired 2=NO not cognitively impaired.
df <- df %>%
  mutate(across(c("mmsesum"), as.numeric))

df <- df %>%
  mutate(
    cognition_cat = case_when(
      education_cat == "1" & mmsesum <25 ~ "1",
      education_cat == "2" & mmsesum <27 ~ "1",
      education_cat == "3" & mmsesum <29 ~ "1",
      education_cat == "NULL" | is.na(mmsesum) ~ "NULL",
      TRUE ~ "2"
    )
  )

#categorise MET-min/week.1=  Low, 2 = Moderate, 3 = High
df <- df %>%
  mutate(across(c("total_mvpa"), as.numeric))
df$physical_activity_cat <- ifelse(df$total_mvpa >= 3000, "3",
                                   ifelse(df$total_mvpa >=600,"2","1"))

#drop unnecessary columns
df <- df %>%
  select(-"ethoth",-"intdate_m",-"intlang_m",-"intlang_mo",-"formlang_m",
         -"nooftrails",-"SF36Q1",-"SF36Q2",-"SF36Q3a",-"SF36Q3b",-"SF36Q3c",
         -"SF36Q3d",-"SF36Q3e",-"SF36Q3f",-"SF36Q3g",-"SF36Q3h",
         -"SF36Q3i",-"SF36Q3j",-"SF36Q4a",-"SF36Q4b",-"SF36Q4c",-"SF36Q4d",
         -"SF36Q5a",-"SF36Q5b",-"SF36Q5c",-"SF36Q6",-"SF36Q7",-"SF36Q8",
         -"SF36Q9a",-"SF36Q9b",-"SF36Q9c",-"SF36Q9d",-"SF36Q9d",
         -"SF36Q9e",-"SF36Q9f",-"SF36Q9g",-"SF36Q9h",
         -"SF36Q9i",-"SF36Q10",-"SF36Q11a",-"SF36Q11b",-"SF36Q11c",-"SF36Q11d",-"H5_otherWorkStatus",
         -"H7_otherTypeHouse",-"C2father",-"C2mother",-"C2brother",
         -"C2sister",-"C2son",-"C2daughter",-"C2PUncle",-"C2PAunty",-"C2MUncle",
         -"C2MAunty",-"C4father",-"C4mother",-"C4brother",-"C4sister",-"C4son",-"C4daughter",-"C4Puncle",
         -"C4PAunty",-"C4MUncle",-"C4MAunty",-"C6father",-"C6mother",-"C6brother",-"C6sister",-"C6son",-"C6daughter",
         -"C6PUncle",-"C6PAunty",-"C6MUncle",-"C6MAunty",-"C8father",-"C8mother",-"C8brother",
         -"C8sister",-"C8son",-"C8daughter",-"C8PUncle",-"C8PAunty",-"C8MUncle",-"C8MAunty",-"orienttime",
         -"orientplace",-"regscore",-"attcalc",-"recall",
         -"naming",-"repet",-"compre",-"reading",-"writing",-"drawing",-"Q31",-"Q32",
         -"Q33",-"Q34",-"Q35",-"Q36",-"Q37",-"Q38",-"Q39",-"Q310",-"A22a",-"A22b",-"intday",-"intmonth",-"/",
         -"c2missing",-"c4missing",-"c6missing",-"c8missing",-"edu_baseline",
         -"h12housety",-"LDLC",-"intyear",-"systolic1",
         -"systolic2",-"systolic3",-"diastolic1",-"diastolic2",-"diastolic3",-"dobyear",
         -"s1s2avg",-"s2s3avg",-"s1s3avg",-"diffs1s2",-"diffs2s3",-"diffs1s3",-"code_sbp",-"code_dbp",
         -"d1d2avg",-"d2d3avg",-"d1d3avg",-"diffd1d2",-"diffd2d3",-"diffd1d3",-"smoking_status",-"housing",-"education_combined")

#changing maritalstatus missing values to NA
df <- df %>%
  mutate(maritalstatus = ifelse(maritalstatus %in% c("666", "888", "NULL"), NA, maritalstatus))

#changing workstatus missing values to NA
df <- df %>%
  mutate(workstatus = ifelse(workstatus %in% c("666", "888", "NULL"), NA, workstatus))

#changing income missing values to NA
df <- df %>%
  mutate(income = ifelse(income %in% c("666", "888","999","NULL"), NA, income))

#changing income missing values to NA
df <- df %>%
  mutate(income = ifelse(income %in% c("666", "888","999","NULL"), NA, income))

#changing other column missing values to NA
columns_to_replace_2 <- c("gender","heavyalcohol","familyhist_hd","familyhist_hbp","familyhist_diabetes",
                          "familyhist_cancer","familyhist_any","hypertension_combined","diabetes_combined",
                          "dyslipidaemia_combined","pmh_any","smoking_status_cat","education_cat","cognition_cat")

for (col in columns_to_replace_2) {
  df[[col]] <- ifelse(df[[col]] %in% c("NULL"), "NA", df[[col]])
}

columns_to_replace_3 <- c("regularmed","pmh_hd","pmh_stroke", "pmh_tia","pmh_hbp",
                          "pmh_diabetes",	"pmh_hc", "pmh_asthma",	"pmh_rheumatism",	
                          "pmh_thyroid", "pmh_arth", "pmh_gas", "pmh_bron", "pmh_emphy", "pmh_cancer")

for (col in columns_to_replace_3) {
  df[[col]] <- ifelse(df[[col]] %in% c("NULL"), "NA", df[[col]])
}

#getting sample population

#remove NULL interview dates and NULL year of birth
df_filtered <- df[!is.na(df$age),]
nrow(df_filtered)

#remove age <45
df_filtered <- df_filtered[df_filtered$age >= 45,]
nrow(df_filtered)

#remove mmsesum NA 
df_filtered <- df_filtered[!is.na(df_filtered$mmsesum),]
nrow(df_filtered)

#remove k10sum NA
df_filtered$k10sum <- as.numeric(df_filtered$k10sum)
df_filtered <- df_filtered[!is.na(df_filtered$k10sum),]
nrow(df_filtered)

#remove ethderived=4
df_filtered <- df_filtered[df_filtered$ethderived != 4,]
nrow(df_filtered)

# remove missing education=NA
df_filtered <- df_filtered %>%
  filter(df_filtered$education_cat != "NA")
nrow(df_filtered)

# remove missing regularmed=NA
df_filtered <- df_filtered %>%
  filter(df_filtered$regularmed != "NA")
nrow(df_filtered)

# remove missing maritalstatus=NA
df_filtered <- df_filtered %>%
  filter(df_filtered$maritalstatus != "NA")
nrow(df_filtered)

# remove missing workstatus=NA
df_filtered <- df_filtered %>%
  filter(df_filtered$workstatus != "NA")
nrow(df_filtered)

# remove missing bmi=NA
df_filtered <- df_filtered[!is.na(df_filtered$bmi),]
nrow(df_filtered)


# remove missing whr=NA
df_filtered <- df_filtered[!is.na(df_filtered$whr),]
nrow(df_filtered)


# remove missing familyhist_any=NA
df_filtered <- df_filtered %>%
  filter(df_filtered$familyhist_any != "NA")
nrow(df_filtered)


# remove missing smoking_status_cat=NA
df_filtered <- df_filtered %>%
 filter(df_filtered$smoking_status_cat != "NA")
nrow(df_filtered)

#drop columns
df_filtered <- df_filtered %>%
  select(-"pmh_hd",-"pmh_stroke",-"pmh_tia",-"pmh_hbp",-"pmh_diabetes",-"pmh_hc",
         -"pmh_asthma",-"pmh_rheumatism",-"pmh_thyroid",-"pmh_arth",-"pmh_gas",
         -"pmh_bron",-"pmh_emphy",-"pmh_cancer",-"familyhist_hd",-"familyhist_hbp",
         -"familyhist_diabetes",-"familyhist_cancer",-"hypertension_measured",
         -"diabetes_measured_gluf",-"diabetes_measured_glur",-"diabetes_measured",
         -"dyslipidaemia_measured_CHO",-"dyslipidaemia_measured_LDLM",
         -"dyslipidaemia_measured_TG",-"dyslipidaemia_measured",-"education",
         -"height",-"weight",-"waist",-"hip",-"CHO",-"LDLM",-"HDL",-"TG",-"GLUR",-"GLUF",
         -"sbp_calculated",-"dbp_calculated",-"income",-"hypertension_combined",-"diabetes_combined",-"dyslipidaemia_combined")

#combine marital status separated 3 and divorced 4 into single category "6"
df_filtered$maritalstatus <- ifelse(df_filtered$maritalstatus %in% c(3, 4), 6, df_filtered$maritalstatus)
df_filtered$maritalstatus <- ifelse(df_filtered$maritalstatus %in% c(5, 6), 7, df_filtered$maritalstatus)


#Combine “Student (full-time)”,(2)  “Unemployed (able to work)” (5), “Unemployed (unable to work)” (6) and “Others” (7) into single category: “Not Working” (8)
df_filtered$workstatus <- ifelse(df_filtered$workstatus %in% c(2,5,6,7), 8, df_filtered$workstatus)
df_filtered$workstatus <- ifelse(df_filtered$workstatus %in% c(3,4), 8, df_filtered$workstatus)

#add new BMI category 
df_filtered <- df_filtered %>%
  mutate(
    bmicat = case_when(
      bmi<18.5 ~ "1",
      bmi>=18.5 & bmi<23.0 ~ "2",
      bmi>=23.0 & bmi<27.5 ~ "3",
      bmi >=27.5 ~"4"
    )
  )

#change others in housing
df_filtered <- df_filtered %>%
  mutate(
    housing_combined = ifelse(Subject_ID %in% c("028-19-05753", "028-19-03446", "028-19-03727","028-19-02480",
                                                "028-19-04766","028-19-08772","028-19-02790","028-19-00529",
                                                "028-19-13601","028-19-14131","028-19-08143"
    ), 4, housing_combined)
  )

df_filtered <- df_filtered %>%
  mutate(
    housing_combined = ifelse(Subject_ID %in% c("028-19-04323"), 6, housing_combined)
  )

df_filtered <- df_filtered %>%
  mutate(
    housing_combined = ifelse(Subject_ID %in% c("028-19-07869"), 5, housing_combined)
  )

df_filtered <- df_filtered %>%
  mutate(
    housing_combined = ifelse(Subject_ID %in% c("028-19-07869"), 5, housing_combined)
  )

df_filtered <- df_filtered %>%
  mutate(
    housing_combined = ifelse(Subject_ID %in% c("028-19-10570","028-19-03236"), 2, housing_combined)
  )

df_filtered <- df_filtered %>%
  mutate(
    housing_combined = ifelse(Subject_ID %in% c("028-19-00682","028-19-13932","028-19-10325","028-19-07384"), 3, housing_combined)
  )

#combine housing type- Combine “Private Condominium” (5)  and “Private House (landed property)” (6) to “Private Housing” (8)
df_filtered$housing_combined <- ifelse(df_filtered$housing_combined %in% c(5,6), 8, df_filtered$housing_combined)
df_filtered$housing_combined <- ifelse(df_filtered$housing_combined %in% c(1,2), 9, df_filtered$housing_combined)
df_filtered$housing_combined <- ifelse(df_filtered$housing_combined %in% c(3,4), 10, df_filtered$housing_combined)


#update numberings
df_filtered$maritalstatus <- ifelse(df_filtered$maritalstatus ==7 ,3, df_filtered$maritalstatus)
df_filtered$workstatus <- ifelse(df_filtered$workstatus ==8 ,2, df_filtered$workstatus)
df_filtered$housing_combined <- ifelse(df_filtered$housing_combined == 9, 1, df_filtered$housing_combined)
df_filtered$housing_combined <- ifelse(df_filtered$housing_combined == 10, 2, df_filtered$housing_combined)
df_filtered$housing_combined <- ifelse(df_filtered$housing_combined == 8, 3, df_filtered$housing_combined)

#recode no to 0
columns_to_change <- c("heavyalcohol", "pmh_any", "familyhist_any", "regularmed","cognition_cat")
df_filtered[columns_to_change][df_filtered[columns_to_change] == 2] <- 0

#add new variable agegroup
df_filtered$agegroup <- ifelse(df_filtered$age < 60, 1, 2)

#add new variable whr group
df_filtered$whrgroup <- ifelse((df_filtered$gender == 1 & df_filtered$whr > 0.9) | (df_filtered$gender == 2 & df_filtered$whr > 0.85), 1, 0)

write.csv(df_filtered,"yy20230901_cleaned.csv",row.names=FALSE)

