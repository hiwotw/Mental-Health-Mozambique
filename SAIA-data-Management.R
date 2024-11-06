##############################################
# AUTHOR: Hiwot Weldemariam 
# Project SAIA: Baseline data MH prescription 
# CREATED on: August 2023
##############################################

# Set working directory
rm(list=ls())
setwd("~/Desktop/Mozambique_SAIA/")

# Load libraries 
library("tidyverse")
library("haven")
library("rigr")
library(dplyr)

library(epiR)
library(dplyr)

library(haven)
library(table1)

# Read data in CSV data01 is first visit and data02 is subsequent and prescription data
patient_data<- read.csv("Data01.csv")
Visit_data<- read.csv("Data02.csv")

#inspect data 

head(patient_data)
head(Visit_data)
# Merge the two data set
merged_data <- merge( patient_data, Visit_data, by = "PatientID", all.x = TRUE)


# Quickly inspect dataset
glimpse(merged_data)
head(merged_data)

# Keep selected variables( remove variable we don't need)

# Look at dataset more carefully

summary(merged_data)

#Save the merged data to a new CSV file
write.csv(merged_data, "SaiaMerged.csv", row.names = FALSE)


#cleaning data 

#Maritial status
merged_data$Relationship.Status<- factor(merged_data$Relationship.Status, levels = c("casado(a)", "divorciado(a)", "separado(a)", "solteiro(a)", "união_de_facto", "viúvo(a)"),
                                   labels = c("Married", "Divorced", "Separated", "Single", "Common-law", "Widowed")
)

#about the form 
merged_data$patient.before.the.new.forms.were.implemented.<- factor(merged_data$patient.before.the.new.forms.were.implemented.,
                                         levels = c("1", "2", "3", "---"),
                                         labels = c("Yes", "No", "Unknown", NA )
)


#HIV Status 

merged_data$HIVStatus<- factor(merged_data$HIVStatus,
                               levels = c("1","0", "no1","yes1", "VIH_status_desconhecido"),
                               labels= c("Yes", "No", "No","Yes","Unknown"))


#TB status 
merged_data$TBStatus<- factor(merged_data$TBStatus,
                               levels = c("1","0", "no2","yes2", "TB_status_desconhecido"),
                               labels= c("Yes", "No", "No","Yes","Unknown"))

#Alcohol use
merged_data$AlcoholUse<- factor(merged_data$AlcoholUse,
                              levels = c("current_1","none1", "past_1"),
                              labels= c("Current Alcohol user ", "Never Alcohol user","Past Alcohol user"))


#Drug use 

merged_data$DrugUse<- factor(merged_data$DrugUse,
                                levels = c("current2","none2", "past2"),
                                labels= c("Current drug user ", "Never drug user","Past drug user"))


#First vs second form 

merged_data$firstvssecondform<- factor(merged_data$firstvssecondform,
                             levels = c("1","2"),
                             labels= c("Yes", "No"))

#lebeling primary Diagnosis 

icd_mapping<- c(
  "---" = "NA",
  "F00" = "Dementia in Alzheimer's disease",
  "F001" = "Vascular dementia",
  "F01" = "Vascular dementia",
  "F01.1" = "Dementia due to multiple infarcts",
  "F02.4" = "Dementia in HIV disease",
  "F03" = "Unspecified dementia",
  "F04" = "Organic amnestic syndrome not induced by alcohol or other psychoactive substances",
  "F05" = "Delirium not induced by alcohol or other psychoactive substances",
  "F06" = "Other mental disorders due to brain damage and dysfunction and to physical disease",
  "F07" = "Personality and behavioral disorders due to brain disease, damage, and dysfunction",
  "F08" = "Not Applicable",
  "F09" = "Unspecified organic or symptomatic mental disorder",
  "F09.0" = "Unspecified organic or symptomatic mental disorder",
  "F10" = "Mental and behavioral disorders due to alcohol use",
  "F10.1" = "Mental and behavioral disorders due to alcohol use",
  "F10.2" = "Mental and behavioral disorders due to alcohol use",
  "F10.5" = "Mental and behavioral disorders due to alcohol use",
  "F11" = "Mental and behavioral disorders due to opioid use",
  "F12" = "Mental and behavioral disorders due to cannabinoid use",
  "F12.5" = "Mental and behavioral disorders due to cannabinoid use",
  "F13" = "Mental and behavioral disorders due to sedative and hypnotic use",
  "F14" = "Mental and behavioral disorders due to cocaine use",
  "F17" = "Mental and behavioral disorders due to tobacco use",
  "F19" = "Mental and behavioral disorders due to multiple drug use and other psychoactive substances",
  "F19.0" = "Mental and behavioral disorders due to multiple drug use and other psychoactive substances",
  "F19.5" = "Mental and behavioral disorders due to multiple drug use and other psychoactive substances",
  "F2.4" = "Mental and behavioral disorders due to multiple drug use and other psychoactive substances",
  "F20" = "Schizophrenia",
  "F20.0" = "Paranoid schizophrenia",
  "F20.1" = "Hebephrenic schizophrenia",
  "F20.2" = "Catatonic schizophrenia",
  "F20.3" = "Undifferentiated schizophrenia",
  "F20.4" = "Post-schizophrenic depression",
  "F20.5" = "Residual schizophrenia",
  "F21" = "Schizotypal disorder",
  "F21.0" = "Schizotypal disorder",
  "F22" = "Persistent delusional disorders",
  "F22.0" = "Delusional disorder",
  "F22.1" = "Delusional disorder",
  "F23" = "Acute and transient psychotic disorders",
  "F23.0" = "Acute polymorphic psychotic disorder without symptoms of schizophrenia",
  "F23.1" = "Acute polymorphic psychotic disorder with symptoms of schizophrenia",
  "F23.2" = "Acute schizophrenia-like psychotic disorder",
  "F24" = "Induced delusional disorder (Folie à deux)",
  "F25.1" = "Schizoaffective disorder, depressive type",
  "F25.2" = "Schizoaffective disorder, mixed type",
  "F25.8" = "Other schizoaffective disorders",
  "F29" = "Unspecified nonorganic psychosis",
  "F29.2" = "Unspecified nonorganic psychosis",
  "F30" = "Manic episode",
  "F30.3" = "Hypomania",
  "F31" = "Bipolar affective disorder",
  "F32" = "Depressive episodes",
  "F32.0" = "Depressive episodes",
  "F32.3" = "Depressive episodes",
  "F33" = "Recurrent depressive disorder",
  "F36" = "Not Applicable",
  "F38" = "Other mood [affective] disorders",
  "F39" = "Unspecified mood [affective] disorder",
  "F40" = "Phobic anxiety disorders",
  "F40.0" = "Agoraphobia",
  "F40.2" = "Specific phobia",
  "F41" = "Other anxiety disorders",
  "F41.0" = "Panic disorder [episodic paroxysmal anxiety]",
  "F41.1" = "Panic disorder [episodic paroxysmal anxiety]",
  "F41.2" = "Panic disorder [episodic paroxysmal anxiety]",
  "F42" = "Obsessive-compulsive disorder",
  "F42.0" = "Obsessive-compulsive disorder",
  "F43" = "Reactions to severe stress and adjustment disorders",
  "F43.0" = "Acute stress reaction",
  "F43.1" = "Post-traumatic stress disorder",
  "F43.2" = "Adjustment disorders",
  "F44" = "Dissociative disorders (conversion disorders)",
  "F44.0" = "Dissociative amnesia",
  "F44.1" = "Dissociative fugue",
  "F45" = "Somatoform disorders",
  "F45.0" = "Somatization disorder",
  "F45.2" = "Not Applicable",
  "F48.0" = "Neurasthenia",
  "F50" = "Eating disorders",
  "F50.0" = "Anorexia nervosa",
  "F50.1" = "Anorexia nervosa",
  "F51" = "Nonorganic sleep disorders associated with emotional factors",
  "F51.0" = "Nonorganic insomnia",
  "F51.1" = "Nonorganic hypersomnia",
  "F52" = "Sexual dysfunction not caused by organic disorder",
  "F52.0" = "Sexual desire disorders",
  "F53" = "Mental and behavioral disorders associated with the puerperium, not elsewhere classified",
  "F53.0" = "Mild mental and behavioral disorders associated with the puerperium, not elsewhere classified",
  "F60" = "Specific personality disorder",
  "F60.0" = "Paranoid personality disorder",
  "F60.1" = "Schizoid personality disorder",
  "F69" = "Unspecified personality and behavioral disorder in adults",
  "F70" = "Mild intellectual disability",
  "F70.0" = "Mild intellectual disability",
  "F70.1" = "Mild intellectual disability",
  "F71" = "Moderate intellectual disability",
  "F80" = "Specific developmental disorders of speech and language",
  "F80.1" = "Expressive language disorder",
  "F81" = "Specific developmental disorders of scholastic skills",
  "F81.3" = "Mixed disorder of scholastic skills",
  "F90" = "Hyperkinetic disorders",
  "F90.0" = "Hyperkinetic disorders",
  "F91" = "Conduct disorders",
  "F95" = "Tic disorders",
  "F98" = "Other behavioral and emotional disorders with onset usually occurring in childhood or adolescence",
  "F98.0" = "Nonorganic enuresis",
  "G40" = "Epilepsy",
  "G41" = "Status epilepticus",
  "G43" = "Migraine",
  "G43.8" = "Other migraine syndromes",
  "G47.0" = "Insomnia",
  "G50.0" = "Trigeminal neuralgia",
  "NA" = "Not Applicable",
  "R41.3" = "Other and unspecified amnesia",
  "TPA" = "Tissue Plasminogen Activator"
)



# Rename the ICD codes in the data set using icd_mapping

merged_data$PrimaryDiagnosis <- factor(merged_data$PrimaryDiagnosis,
  levels= names(icd_mapping),
  labels = icd_mapping
)



# labeling secondary diagnosis 

s_icd_mapping <- c(
  "B23.0" = "Human immunodeficiency virus [HIV] disease resulting in encephalopathy",
  "B50" = "Plasmodium falciparum malaria",
  "F01" = "Vascular dementia",
  "F09" = "Unspecified organic or symptomatic mental disorder",
  "F10" = "Mental and behavioral disorders due to alcohol use (Alcohol use disorder)",
  "F19" = "Mental and behavioral disorders due to multiple drug use and use of other psychoactive substances",
  "F20" = "Schizophrenia",
  "F20.0" = "Paranoid schizophrenia",
  "F20.3" = "Undifferentiated schizophrenia",
  "F22" = "Delusional disorders",
  "F23" = "Brief psychotic disorder",
  "F23.0" = "Acute polymorphic psychotic disorder without symptoms of schizophrenia",
  "F24.0" = "Shared psychotic disorder",
  "F30" = "Manic episode (Bipolar disorder, manic episode)",
  "F30.1" = "Hypomanic episode (Bipolar disorder, hypomanic episode)",
  "F32" = "Depressive episode (Major depressive disorder)",
  "F32.0" = "Mild depressive episode",
  "F40" = "Phobic anxiety disorders (Phobias)",
  "F41" = "Other anxiety disorders (Anxiety disorders)",
  "F41.1" = "Generalized anxiety disorder",
  "F51" = "Nonorganic sleep disorders (Insomnia)",
  "F51.0" = "Insomnia due to medical condition",
  "F52" = "Sexual dysfunction not due to a substance or known physiological condition (Sexual disorders)",
  "F70" = "Mild intellectual disabilities (Intellectual disability)",
  "F70.0" = "Mild intellectual disabilities, profound mental retardation",
  "F70.1" = "Mild intellectual disabilities, severe mental retardation",
  "F71" = "Moderate intellectual disabilities (Moderate intellectual disability)",
  "F72" = "Severe intellectual disabilities (Severe intellectual disability)",
  "F80" = "Specific developmental disorders of speech and language (Speech and language disorders)",
  "F90" = "Hyperkinetic disorders (Attention-deficit/hyperactivity disorder, ADHD)",
  "G40" = "Epilepsy",
  "G40.1" = "Benign childhood epilepsy with centrotemporal spikes (Rolandic epilepsy)",
  "HTA" = "Hypertension",
  "NA" = "Not Applicable",
  "R41.3" = "Other amnestic syndromes (Amnestic disorders)"
)

# Rename the ICD codes in the  data set using icd_mapping for secondary diagnosis 

merged_data$SecondaryDiagnosis <- factor(merged_data$SecondaryDiagnosis,
                                       levels= names(s_icd_mapping),
                                       labels = s_icd_mapping
)

#Labeling thought of suicide 

merged_data$ThoughtsofSuicide<- factor(merged_data$ThoughtsofSuicide,
                                       levels = c("0","1", "3","---", "NA"),
                                       labels= c("No", "Yes", "NA", "NA", "NA"))


#Alcoholand Drug use
merged_data$Alcohol..DrugsUse <- factor(merged_data$Alcohol..DrugsUse,
                                        levels = c(1, 2, 3, 4, 5, "---"),
                                        labels = c("Alcohol", "Drug", "Both drug and alcohol", "No", "Response not recorded", NA)
)

#Labeling if patient is pregnant

merged_data$PatientPregnant<- factor(merged_data$PatientPregnant,
                                       levels = c("0","1", "3","---", "NA"),
                                       labels= c("No", "Yes", "NA", "NA", "NA"))

#Labeling if patient is engaged in family planning 

merged_data$EngagedinFamilyPlanning<- factor(merged_data$EngagedinFamilyPlanning,
                                     levels = c("0","1", "3","---", "NA"),
                                     labels= c("No", "Yes", "NA", "NA", "NA"))


#labeling Initial prescription

merged_data$Initialprescription<- factor(merged_data$Initialprescription,
                                             levels = c("0","1", "3","---", "NA"),
                                             labels= c("No", "Yes", "NA", "NA", "NA"))
#labeling First Medication 

# Create a named vector mapping medication names
medication_mapping <- c(
  "Tramadol em cápsulas" = "Tramadol capsules",
  "Tramadol" = "Tramadol capsules",
  "Tramado" = "Tramadol capsules",
  "NA" = "NA",
  "FJ4 (Flufenazina)" = "Fluphenazine",
  "FJ10/(60)" = "Fluphenazine",
  "FJ10" = "Fluphenazine",
  "F-J-10" = "Fluphenazine",
  "F-D1" = "Fluphenazine",
  "complexo  B" = "Vitamin B complex",
  "8A5" = "Fluphenazine",
  "7P4/ (60)" = "Fluphenazine",
  "7J9/ 30cmp" = "Thioridazine Comp. 30 capsules",
  "7J9" = "Thioridazine",
  "7J7" = "Haloperidol Inj. 5mg/1ml",
  "7J6" = "Haloperidol Comp. 5mg",
  "7J5" = "Fluphenazine Decanoate (Modecate) Inj. 25mg/2ml",
  "7J4/ 300cpm" = "Fluphenazine Comp. 300 capsules",
  "7J4" = "Fluphenazine",
  "7J3" = "Chlorpromazine Comp. 25mg",
  "7J2 (Clorpromazina)" = "Chlorpromazine Comp. 25mg",
  "7J2" = "Chlorpromazine Comp. 25mg",
  "7J14" = "NA",
  "7J11" = "Trifluoperazine Comp. 5mg",
  "7J10 (60)" = "Thioridazine 60 capsules",
  "7J10" = "Thioridazine",
  "7J1" = "Chlorpromazine Comp. 25mg",
  "7I8" = "Flunitrazepam Comp. 2mg",
  "7I4" = "Chlordiazepoxide Comp. 10mg",
  "7G7" = "Paroxetine Caps. 20mg",
  "7G6 /(30)" = "Maprotiline 30 capsules",
  "7G6" = "Maprotiline",
  "7G4" = "Imipramine Comp. 25mg",
  "7G2 /(15)" = "Amitriptyline 15 capsules",
  "7G2 (Amitriplina)" = "Amitriptyline Comp. 25mg",
  "7G2" = "Amitriptyline Comp. 25mg",
  "7G1 (Amitriplina)" = "Amitriptyline Comp. 10mg",
  "7G1" = "Amitriptyline Comp. 10mg",
  "7F10" = "Biperiden Comp. 2mg",
  "7F1" = "Biperiden Comp. 2mg",
  "7D7" = "Phenobarbital Comp. 15mg",
  "7D6" = "Phenobarbital Comp. 100mg",
  "7D4 (Fenitoina)" = "Phenytoin Comp. 100mg",
  "7D4 (Fenetoina)" = "Phenytoin Comp. 100mg",
  "7D4" = "Phenytoin Comp. 100mg",
  "7D1A" = "Carbamazepine Comp. 400mg",
  "7D14 (4 frascos)" = "Sodium Valproate Xpe 200mg/5 ml",
  "7D14 (3frascos)" = "Sodium Valproate Xpe 200mg/5 ml",
  "7D14" = "Sodium Valproate Xpe 200mg/5 ml",
  "7D13(60)" = "Sodium Valproate 60 capsules",
  "7D13" = "Sodium Valproate Comp. 200mg",
  "7D11" = "Lamotrigine Comp. 50mg",
  "7D10 - 60 cpm" = "Lamotrigine Comp. 50mg",
  "7D1/ 60cpm" = "Carbamazepine 60 capsules",
  "7D1/ 60cp" = "Carbamazepine 60 capsules",
  "7D1/ 600cpm" = "Carbamazepine 60 capsules",
  "7D1/ 60 cpm" = "Carbamazepine 60 capsules",
  "7D1/ (60)" = "Carbamazepine 60 capsules",
  "7D1/ (30)" = "Carbamazepine 30 capsules",
  "7D1(Carbamazepina)" = "Carbamazepine Comp. 200mg",
  "7D1 /(7)" = "Carbamazepine Comp. 200mg",
  "7D1 / (30)" = "Carbamazepine 30 capsules",
  "7D1 (Carbamazepina)" = "Carbamazepine Comp. 200mg",
  "7D1 (60)" = "Carbamazepine 60 capsules",
  "7D1 (60 cp)" = "Carbamazepine 60 capsules",
  "7D1 (30)" = "Carbamazepine 30 capsules",
  "7D1 (3 FRASCOS)" = "Carbamazepine Comp. 200mg",
  "7D1 - 30cpm" = "Carbamazepine 30 capsules",
  "7D1 - 30 cpm" = "Carbamazepine 30 capsules",
  "7D1 -  30cpm" = "Carbamazepine 30 capsules",
  "7D1 -  30cp" = "Carbamazepine 30 capsules",
  "7D1  (60 cp)" = "Carbamazepine 60 capsules",
  "7D1" = "Carbamazepine Comp. 200mg",
  "7B1" = "NA",
  "7-J-D" = "Chlorpromazine Comp. 25mg",
  "7-J-9" = "Thioridazine Comp. 10mg",
  "7-J-7" = "Haloperidol Inj. 5mg/1ml",
  "7-J-6" = "Haloperidol Comp. 5mg",
  "7-J-5" = "Fluphenazine Decanoate (Modecate) Inj. 25mg/2ml",
  "7-J-4" = "Fluphenazine Comp. 2.5mg",
  "7-J-3" = "Chlorpromazine Comp. 25mg",
  "7-J-2" = "Chlorpromazine Comp. 25mg",
  "7-J-15" = "Risperidone Comp. 2mg",
  "7-J-14" = "Risperidone Comp. 2mg",
  "7-J-13" = "Risperidone Comp. 2mg",
  "7-J-10" = "Thioridazine Comp. 100mg",
  "7-J-1/60" = "Chlorpromazine Comp. 25mg",
  "7-J-1" = "Chlorpromazine Comp. 25mg",
  "7-J_6" = "Haloperidol Comp. 5mg",
  "7-I-7" = "Diazepam Inj. 10mg/2ml",
  "7-I-4" = "Chlordiazepoxide Comp. 10mg",
  "7-G-6" = "Maprotiline Comp. 25mg",
  "7-G-4" = "Imipramine Comp. 25mg",
  "7-G-3" = "Fluoxetine Caps. 20mg",
  "7-G-2" = "Amitriptyline Comp. 25mg",
  "7-G-14" = "Amitriptyline Comp. 25mg",
  "7-G-10" = "Amitriptyline Comp. 25mg",
  "7-G-1" = "Amitriptyline Comp. 10mg",
  "7-F-9" = "Biperiden (Akineton) Inj. 5mg/ml",
  "7-F-2" = "Biperiden (Akineton) Inj. 5mg/ml",
  "7-F-1" = "Biperiden Comp. 2mg",
  "7-D14" = "Sodium Valproate Xpe 200mg/5 ml",
  "7-D1" = "Carbamazepine Comp. 200mg",
  "7-D-7" = "Phenobarbital Comp. 15mg",
  "7-D-6" = "Phenobarbital Comp. 100mg",
  "7-D-4" = "Phenytoin Comp. 100mg",
  "7-D-1A" = "Carbamazepine Comp. 400mg",
  "7-D-14" = "Sodium Valproate Xpe 200mg/5 ml",
  "7-D-13" = "Sodium Valproate Comp. 200mg",
  "7-D-11" = "Lamotrigine Comp. 50mg",
  "7-D-1/60" = "Carbamazepine 60 capsules",
  "7-D-1/30" = "Carbamazepine 30 capsules",
  "7-D-1" = "Carbamazepine Comp. 200mg",
  "7-B-9" = "NA",
  "3J10" = "NA",
  "12D8" = "NA",
  "12D1" = "NA",
  "---" = "NA"
)

# Rename the ICD codes in the  data set using medication_mapping 

merged_data$FirstMedication <- factor(merged_data$FirstMedication,
                                         levels= names(medication_mapping ),
                                         labels = medication_mapping 
)

#Labeling Dosage of first medication: the dose is numeric in mg/ etc labeling won't be necessary 

#Labeling frequency of first medication

merged_data$FrequencyFirstMedication <- factor(merged_data$FrequencyFirstMedication,
                                               levels = c("1vez_por_dia", "de_12_em_12_horas", "de_4_em_4_horas", "de_8_em_8_horas", "---", "uma_vez"),
                                               labels = c("Once a day", "Every 12 hours", "Every 4 hours", "Every 8 hours", "NA", "Only one time")
)

#Is the prescribed medicaiton recived 

merged_data$PrescribedMedicationRecieved.<- factor(merged_data$PrescribedMedicationRecieved.,
                                     levels = c("0","1","---"),
                                     labels= c("No", "Yes", "NA"))


#Reason for prescribed medication not received 

merged_data$ReasonPrescribedMedicationWasNotRecieved<- factor(merged_data$ReasonPrescribedMedicationWasNotRecieved,
                                                   levels = c("1","---", "NA"),
                                                   labels= c("Not available in the pharmacy", "NA", "NA"))


#additional/Second medication 

merged_data$AdditionofSecond.Medication.<- factor(merged_data$AdditionofSecond.Medication.,
                                                              levels = c("0","1","---"),
                                                              labels= c("NO", "Yes", "NA"))

#Second medication 

# Create a mapping for medication names

Secondmedication_mapping <- c(
  "Ácido fólico" = "Folic acid",
  "acido fólico" = "Folic acid",
  "12-D-8" = "NA",
  "12A10" = "NA",
  "12A14" = "NA",
  "12A18" = "NA",
  "12A8" = "NA",
  "12A8/ (60)" = "NA",
  "12D1" = "NA",
  "12D10" = "NA",
  "12D11" = "NA",
  "12D14" = "NA",
  "12D2" = "NA",
  "12D4" = "NA",
  "12D5" = "NA",
  "12D8" = "NA",
  "13-A-4" = "Diphenhydramine Inj. 50mg/ml",
  "13-A-5" = "Promethazine Comp. 10mg",
  "13-A-7" = "Promethazine Inj. 50mg/2ml",
  "13-A-8" = "Promethazine Inj. 50mg/2ml",
  "13A2" = "Chlorpheniramine Comp. 4mg",
  "13A3" = "Chlorpheniramine Xpe 2mg/5ml",
  "13A4" = "Diphenhydramine Inj. 50mg/ml",
  "13A5" = "Promethazine Comp. 10mg",
  "13A8" = "Promethazine Inj. 50mg/2ml",
  "13A8 - 60cpm" = "Promethazine Inj. 50mg/2ml",
  "13A9" = "Promethazine Inj. 50mg/2ml",
  "14D12" = "NA",
  "1F1" = "NA",
  "6A1" = "NA",
  "6A4" = "NA",
  "6A7" = "NA",
  "6A7/ 60cpm" = "NA",
  "7-A-3" = "NA",
  "7-A-5" = "NA",
  "7-D-1" = "Carbamazepine Comp. 200mg",
  "7-D-6" = "Phenobarbital Comp. 100mg",
  "7-F-1" = "Biperiden Comp. 2mg",
  "7-F-1/60" = "Biperiden 60 capsules",
  "7-F-11" = "NA",
  "7-F-2" = "Biperiden (Akineton) Inj. 5mg/ml",
  "7-F-F" = "Biperiden (Akineton) Inj. 5mg/ml",
  "7-F1" = "Biperiden Comp. 2mg",
  "7-G-1" = "Amitriptyline Comp. 10mg",
  "7-G-10" = "NA",
  "7-G-2" = "Amitriptyline Comp. 25mg",
  "7-G-6" = "Maprotiline Comp. 25mg",
  "7-I-4" = "Chlordiazepoxide Comp. 10mg",
  "7-I-41" = "Chlordiazepoxide Comp. 10mg",
  "7-I-6" = "Diazepam Comp. 10mg",
  "7-J-1" = "Chlorpromazine Comp. 25mg",
  "7-J-10" = "Thioridazine Comp. 100mg",
  "7-J-2" = "Chlorpromazine Comp. 25mg",
  "7-J-4" = "Fluphenazine Comp. 2.5mg",
  "7-J-7" = "Haloperidol Inj. 5mg/1ml",
  "7A3" = "NA",
  "7D1" = "Carbamazepine Comp. 200mg",
  "7D1 /(15)" = "Carbamazepine  15 capsules",
  "7D10" = "NA",
  "7D3" = "NA",
  "7D5" = "NA",
  "7F1" = "Biperiden Comp. 2mg",
  "7F1 (60)" = "Biperiden 60 capsules",
  "7F1/(60)" = "Biperiden 60 capsules",
  "7F2" = "Biperiden (Akineton) Inj. 5mg/ml",
  "7F9" = "NA",
  "7G1" = "Amitriptyline Comp. 10mg",
  "7G2" = "Amitriptyline Comp. 25mg",
  "7G4" = "Imipramine Comp. 25mg",
  "7G6" = "Maprotiline Comp. 25mg",
  "7G6/(15)" = "Maprotiline 15 capsules",
  "7G9" = "NA",
  "7J1" = "Chlorpromazine Comp. 25mg",
  "7J10" = "Thioridazine Comp. 100mg",
  "7J2" = "Chlorpromazine Comp. 25mg",
  "7J4" = "Fluphenazine Comp. 2.5mg",
  "7J6" = "Haloperidol Comp. 5mg",
  "8-I-1" = "NA",
  "8A1" = "NA",
  "8A8" = "NA",
  "8L1" = "NA",
  "8N4" = "NA",
  "A16" = "NA",
  "clorfeni" = "Chlorpheniramine",
  "Complexo B" = "Vitamin B complex",
  "Imiprqmina" = "Imipramine",
  "NA" = "NA",
  "PROMETAZINA/ (60)" = "Promethazine (60 capsules)",
  "Vit E" = "Vitamin E"
)

# Apply the mapping to the dataset

merged_data$NameofSecondMedication <- factor(merged_data$NameofSecondMedication,

levels= names(Secondmedication_mapping ),
labels = Secondmedication_mapping 
)

#dose of second medication is numeric 

#Frequency of second medication 

merged_data$FrequencySecondMedication <- factor(merged_data$FrequencySecondMedication,
                                               levels = c("1vez_por_dia", "de_12_em_12_horas", "de_6_em_6_horas", "de_8_em_8_horas", "---"),
                                               labels = c("Once a day", "Every 12 hours", "Every 6 hours", "Every 8 hours", "NA")
)


#Is the second prescribed medication received 

merged_data$Prescribed2MedicationRecieved<- factor(merged_data$Prescribed2MedicationRecieved,
                                                   levels = c("1","---"),
                                                   labels= c("Yes", "NA"))


#Reason for second prescribed medication not received 

merged_data$Reason2PrescribedMedicationNotRecieved<- factor(merged_data$Reason2PrescribedMedicationNotRecieved,
                                                              levels = c("---", "NA"),
                                                              labels= c( "NA", "NA"))

#Third medication

#additional/Second medication 

merged_data$ThirdMedication<- factor(merged_data$ThirdMedication,
                                                  levels = c("0","1","---"),
                                                  labels= c("NO", "Yes", "NA"))

#Name of third medication 

# Define the mapping
Thirdmedication_mapping <- c(
  "---" = "NA",
  "10A3" = "NA",
  "10A6" = "NA",
  "12A5" = "NA",
  "12D1" = "NA",
  "12D4" = "NA",
  "13-A-5" = "Promethazine Comp. 10mg",
  "13-A-8" = "Promethazine Inj. 50mg/2ml",
  "13A2" = "Chlorpheniramine Comp. 4mg",
  "13A5" = "Promethazine Comp. 10mg",
  "13A8" = "Promethazine Inj. 50mg/2ml",
  "14A3" = "NA",
  "14A5" = "NA",
  "3A4" = "NA",
  "4-D-1" = "NA",
  "7-B-9" = "NA",
  "7-D-1" = "Carbamazepine Comp. 200mg",
  "7-F-1" = "Biperiden Comp. 2mg",
  "7-G-1" = "NA",
  "7-G-2" = "NA",
  "7-G-6" = "NA",
  "7-I-6" = "Diazepam Comp. 10mg",
  "7-I-7" = "Diazepam Inj. 10mg/2ml",
  "7-J-1" = "Chlorpromazine Comp. 25mg",
  "7-J-5" = "Fluphenazine Decanoate (Modecate) Inj. 25mg/2ml",
  "7-J-6" = "Haloperidol Comp. 5mg",
  "7-J-9" = "Thioridazine Comp. 10mg",
  "7A2" = "NA",
  "7A3" = "NA",
  "7A8" = "NA",
  "7D1" = "Carbamazepine Comp. 200mg",
  "7D1/ (30)" = "Carbamazepine 30 capsule",
  "7F1" = "Biperiden Comp. 2mg",
  "7G2" = "Amitriptyline Comp. 25mg",
  "7G4" = "Imipramine Comp. 25mg",
  "7G6" = "Maprotiline Comp. 25mg",
  "7J1" = "Chlorpromazine Comp. 25mg",
  "7J4" = "Fluphenazine Comp. 2.5mg",
  "8-L-1" = "NA",
  "NA" = "NA"
)

# Apply the mapping to your data
merged_data$NameofThirdMedication <- factor(merged_data$NameofSecondMedication,
                                            
                                            levels= names(Thirdmedication_mapping ),
                                            labels = Thirdmedication_mapping 
)


#dose of third medication is numeric 

#Frequency of third medication 

merged_data$FrequencyThirdMedication <- factor(merged_data$FrequencyThirdMedication,
                                                levels = c("1vez_por_dia", "de_12_em_12_horas", "de_8_em_8_horas", "---"),
                                                labels = c("Once a day", "Every 12 hours",  "Every 8 hours", "NA")
)


#Is the third prescribed medication received 

merged_data$Prescribed3MedicationRecieved<- factor(merged_data$Prescribed3MedicationRecieved,
                                                   levels = c("1","---"),
                                                   labels= c("Yes", "NA"))


#Reason for third prescribed medication not received 

merged_data$Reason3PrescribedMedicationNotRecieved<- factor(merged_data$Reason3PrescribedMedicationNotRecieved,
                                                            levels = c("---", "NA"),
                             
                                                         labels= c( "NA", "NA"))

# we didn't label forth, fifth and related variables since 99% null value( we can drop this variables as well)

#temperature, SBY, DBY, weight and height are numeric 

#follow up appointment scheduled? 

merged_data$Follow.upApptScheduled.<- factor(merged_data$Follow.upApptScheduled.,
                                                   levels = c("1","0","---"),
                                                   labels= c("Yes","No", "NA"))


# Let us remove variables that are duplicate and we won't be using and keep those relevant to our analysis 


# Create a vector of column names you want to keep
vars_to_keep <- c(
  "PatientID",
  "VisitID.x",
  "Region",
  "ClinicSofala",
  "ClinicManica",
  "Sex",
  "AgeatIntake",
  "DateEnrolled",
  "patient.before.the.new.forms.were.implemented.",
  "Relationship.Status",
  "HIVStatus",
  "TBStatus",
  "AlcoholUse",
  "DrugUse",
  "InitialWHODASScore",
  "PatientAge.cal.",
  #"VisitID.y",
  #"firstvssecondform",
  "Datepage1completed",
  "PrimaryDiagnosis",
  "SecondaryDiagnosis",
  "ThoughtsofSuicide",
  "Alcohol..DrugsUse",
  "PatientPregnant",
  "EngagedinFamilyPlanning",
  "Initialprescription",
  "FirstMedication",
  "DosageFirstMedication",
  "FrequencyFirstMedication",
  "PrescribedMedicationRecieved.",
  "ReasonPrescribedMedicationWasNotRecieved",
  "DurationofFirstMedication.in.days.",
  "AdditionofSecond.Medication.",
  "NameofSecondMedication",
  "DosageSecondMedication",
  "FrequencySecondMedication",
  "Prescribed2MedicationRecieved",
  "Reason2PrescribedMedicationNotRecieved",
  "ThirdMedication",
  "NameofThirdMedication",
  "DosageThirdMedication",
  "FrequencyThirdMedication",
  "Prescribed3MedicationRecieved",
  "Reason3PrescribedMedicationNotRecieved",
  #"FourthMedication",
  #"NameFourthMedication",
  #"DosageofFourthMedication",
 # "FrequencyFourthMedication",
  #"Prescribed4MedicationRecieved.",
  #"Reason4PrescribedMedicationNotRecieved",
  #"FifthMedication",
 # "NameofFifthMedication",
  #"DosageofFifthMedication",
  #"FrequencyofFifthMedication",
 # "Prescribed5MedicationRecieved.",
  #"Reason5MedicationWasNotRecieved",
  "TemperatureCentigrade",
  "SystolicBloodPressure",
  "DiastolicBloodPressure",
  "WeightKG",
  "HeightCM",
  "Follow.upApptScheduled.",
  #"PrimaryDiagnosis.2",
  #"SecondaryDiagnosis.2",
  "DateofFollowupAppt",
  "WHODASScoreatFollowupAppt",
 # "ThoughtsofSuicideReported.",
 # "Patient.UsingAlcoholorDrugs.FA1.",
  #"PatientPregnant.FA.",
  #"EngagedinFamilyPlanning.FA.",
 # "PatientReportsTakingMedicationAsPrescribed",
 # "PatientPrescribedMedicationDuringFollowup",
  "FirstFAMedication",
  "DosageFirstFAMedication",
  "FrequencyofFirstFAMedication",
  "WasPrescribedFAMedicationRecieved.",
  "ReasonPrescribedFAMedicationNotRecieved",
  "DurationofFirstF.Medication.in.days.",
  #"SecondFAMedication",
 # "NameofSecondFAMedication",
 # "DosageofSecondFAMedication",
 # "FrequencyofSecondFAMedication",
  #"WasPrescribed2FAMedicationRecieved.",
  #"Reason2FAPrescribedMedicationNotRecieved",
  #"ThirdFAMedication",
  #"NameofThirdFAMedication",
  #"DosageofThirdFAMedication",
 # "FrequencyofThird.FAMedication",
  #"Was3FAMedicationRecieved.",
  #"Reason3FAMedicationNotRecieved.",
 # "FourthFAMedication",
 # "NameofFourthFAMedication",
 # "DosageofFourthFAMedication",
 # "FrequencyofFourthFA.Medication",
 # "Was4FAMedicationRecieved..",
 # "Reason4FAMedicationNotRecieved",
  #"FifthFAMedication",
 # "NameofFifthFAMedication",
  #"DosageofFifthFAMedication",
  #"FrequencyofFifthFAMedication",
  #"Was5FAMedicationRecieved.",
  #"Reason5FAMedicationNotRecieved.",
  "TemperatureCentigrade.FA.",
  "SystolicBloodPressure.FA.",
  "DiastolicBloodPressure.FA.",
  "Weightkg.FA.",
  "Height.cm.FA.",
  "Follow.upApptScheduled..2nd.",
  "NextApptDate.FA2.",
  "BMIAutomaticCalculation",
  "ConversionCentimeterstoMeters",
  "DisplayBMI",
  "BMIAutomaticCalculation.1",
  "ConversionCentimeterstoMeters.2",
  "DisplayBMI2",
  "Dateofcurrentconsult",
  "Dateoflastconsult",
  "Dateofnextappointment.scheduled.",
  "Numberofdaysbetweencurrentapptanddatescheduled",
  "Did.the.patient.come.within.5.days..before.or.after..they.were.scheduled",
  "Duration.of.the.patient.s.last.prescription",
  "Number.of.days.between.current.appt.and.previous.appt",
  "Did.the.patient.come.to.appt.before.running.out.of.medication.AND.report.taking.medication.as.prescribed",
  "WHODAS.Score.at.current.appt",
  "Percent.of.first.WHODAS",
  "Percent.change.in.WHODAS.from.first.WHODAS.entered",
  "Patient.WHODAS.under.10",
  "Percent.change.in.WHODAS.is...or...50..or.WHODAS.less.than.10",
  "Patient.eligible.for.follow.up.cascade..prescribed.medication.",
  "Step.1.Achieved",
  "Step.2.Achieved",
  "Step.3.Achieved",
  "Patient.eligible.for.follow.up.cascade",
  "Step.4.Achieved",
  "Step.6.Achieved",
  "Step.7.Achieved",
  "Step.8.Achieved",
  "Patient.Sex",
  "Date.of.Patient.s.last.appt",
  "Date.Patient.s.first.appt.entry",
  "Baseline.WHODAS.Score",
  "Appt.number.Key."
  #"Name.of.Clinic.in.Sofala.Province",
 # "Name.of.Clinic.in.Manica.Province",
  #"Name.of.Clinic",
  #"Medication.Duration",
  #"Primary.Diagnosis",
 # "Thoughts.of.Suicide.Reported",
  #"Patient.eligible.for.follow.up.cascade..prescribed.medication..2",
 # "Patient.eligible.for.follow.up.cascade..prescribed.medication..3",
  #"Patient.Age"
)


# Subset the dataframe to keep only the selected columns
merged_data <- merged_data[, vars_to_keep]



# Define the mappingfor follow up medication 
FAmedication_mapping <- c(
  "7D1" = "Carbamazepine Comp. 200mg",
  "---" = "NA",
  "7-D-1" = "Carbamazepine Comp. 200mg",
  "7-D-1A" = "Carbamazepine Comp. 400mg",
  "7-D-14" = "Sodium Valproate Xpe 200mg/5 ml",
  "7-D-13" = "Sodium Valproate Comp. 200mg",
  "7J10" = "Thioridazine Comp. 100mg",
  "7-J-10" = "Thioridazine Comp. 100mg",
  "7-J-4" = "Fluphenazine Comp. 2.5mg",
  "7-J-1" = "Chlorpromazine Comp. 25mg",
  "7-G-1" = "Amitriptyline Comp. 10mg",
  "7J2" = "Chlorpromazine Comp. 25mg",
  "7-J-2" = "Chlorpromazine Comp. 25mg",
  "7J4" = "Fluphenazine Comp. 2.5mg",
  "7-G-2" = "Amitriptyline Comp. 25mg",
  "7-J-6" = "Haloperidol Comp. 5mg",
  "7-D-4" = "Phenytoin Comp. 100mg",
  "7D14" = "Sodium Valproate Xpe 200mg/5 ml",
  "7G1" = "Amitriptyline Comp. 10mg",
  "7-D-7" = "Phenobarbital Comp. 15mg",
  "7D1A" = "Carbamazepine Comp. 400mg",
  "7G2" = "Amitriptyline Comp. 25mg",
  "7J6" = "Haloperidol Comp. 5mg",
  "7-J-9" = "Thioridazine Comp. 10mg",
  "7-F-1" = "Biperiden Comp. 2mg",
  "7J9" = "Thioridazine Comp. 10mg",
  "7D1 (Carbamazepina)" = "Carbamazepine Comp. 200mg",
  "7-G-7" = "Paroxetine Caps. 20mg",
  "7D13" = "Sodium Valproate Comp. 200mg",
  "7J1" = "Chlorpromazine Comp. 25mg",
  "7D4" = "Phenytoin Comp. 100mg",
  "7J5" = "Fluphenazine Decanoate (Modecate) Inj. 25mg/2ml",
  "7-G-4" = "Imipramine Comp. 25mg",
  "7G6" = "Maprotiline Comp. 25mg",
  "7-D-6" = "Phenobarbital Comp. 100mg",
  "7F1" = "Biperiden Comp. 2mg",
  "7D6" = "Phenobarbital Comp. 100mg",
  "7-J-14" = "NA",
  "7-G-10" = "NA",
  "7-I-4" = "Chlordiazepoxide Comp. 10mg",
  "F-J-10" = "Thioridazine Comp. 100mg",
  "7D1 (60)" = "Carbamazepine 60 capsule",
  "7J10 (Tioridazina)" = "Thioridazine Comp. 100mg",
  "7-G-3" = "Fluoxetine Cáps. 20mg",
  "7-D1A" = "Carbamazepine Comp. 400mg",
  "7D14 (4frascos)" = "Sodium Valproate Xpe 200mg/5 ml",
  "7G4" = "Imipramine Comp. 25mg",
  "7d1 - 60 cpm" = "Carbamazepine Comp. 200mg",
  "7-J-7" = "Haloperidol Inj. 5mg/1ml",
  "6D1" = "NA",
  "Tramadol" = "Tramadol",
  "7Dq4" = "Phenytoin Comp. 100mg",
  "F-J-1" = "Chlorpromazine Comp. 25mg",
  "7-J-5" = "Fluphenazine Decanoate (Modecate) Inj. 25mg/2ml",
  "7-J-15" = "Risperidone Comp. 2mg",
  "7-D-2" = "Clonazepam Comp. 2mg",
  "7D1 (120)" = "Carbamazepine 120 capsule",
  "7I10" = "NA",
  "7D" = "Carbamazepine Comp. 200mg",
  "7J10/ (60)" = "Thioridazine Comp. 100mg 60 capsule",
  "7-G-1/60" = "Amitriptyline Comp. 10mg 60 capsule",
  "FJ2" = "NA",
  "999" = "NA",
  "7J1 (Clorpromazina)" = "Chlorpromazine Comp. 25mg",
  "7D1 (15)" = "Carbamazepine Comp. 200mg 15 capsule",
  "7J2 (Clorpromazina)" = "Chlorpromazine Comp. 25mg",
  "7--D-14" = "Sodium Valproate Xpe 200mg/5 ml",
  "F-J-15" = "Risperidone Comp. 2mg",
  "7-G-13" = "NA",
  "NA" = "NA",
  "7D1-60cpm" = "Carbamazepine 60 capsule",
  "7I8" = "Flunitrazepam Comp. 2mg",
  "7D1(Carbamazepina)" = "Carbamazepine Comp. 200mg",
  "7J10 (120)" = "Thioridazine Comp. 100mg 120 capsule",
  "7D1/ (120)" = "Carbamazepine 120 capsule",
  "7J15 (Risperidona)" = "Risperidone Comp. 2mg",
  "7D1/ (60)" = "Carbamazepine 60 capsule",
  "7-D4 60cp" = "Phenytoin Comp. 100mg 60 cp",
  "7D1/ 60 cpm" = "Carbamazepine 60 capsule",
  "7D1 - 30 cpm" = "Carbamazepine 30 capsule",
  "7D1/60cpm" = "Carbamazepine 60 capsule",
  "FD1 (Carbamazepina)" = "Carbamazepine Comp. 200mg",
  "7D10" = "NA",
  "FJ5" = "Fluphenazine Decanoate (Modecate) Inj. 25mg/2ml",
  "7D10 - 60cpm" = "NA",
  "01/04/2023" = "NA",
  "7-G-14" = "NA",
  "7I4" = "Chlordiazepoxide Comp. 10mg",
  "7D13 (2 frascos)" = "Sodium Valproate Comp. 200mg",
  "7-D-1" = "Carbamazepine Comp. 200mg",
  "6A7" = "NA",
  "7-J-3" = "Chlorpromazine Comp. 25mg",
  "7-D-1A 30cp" = "Carbamazepine Comp. 400mg 30 capsule",
  "7-D14" = "Sodium Valproate Xpe 200mg/5 ml",
  "7-D-1 15cp" = "Carbamazepine Comp. 200mg 15 capsule",
  "7J12 (30)" = "NA",
  "7-D-3" = "NA",
  "7-D14A" = "Sodium Valproate Xpe 200mg/5 ml",
  "7-D-1 30cp" = "Carbamazepine Comp. 200mg 30 capsuel",
  "7-D-10" = "NA",
  "7De1" = "Carbamazepine Comp. 200mg",
  "7-D-12" = "Topiramate Comp.100mg",
  "7-I-6" = "Diazepam Comp. 10mg",
  "12D1" = "NA",
  "7-I,-4" = "Chlordiazepoxide Comp. 10mg",
  "F-J-14" = "NA",
  "7F1 (Biperideno)" = "Biperiden Comp. 2mg",
  "F-J-2" = "Chlorpromazine Comp. 25mg",
  "7F10" = "NA",
  "FJ10" = "Thioridazine Comp. 100mg",
  "7-D-4 60cp" = "Phenytoin Comp. 100mg",
  "FJ4 (Flufenazina)" = "Fluphenazine Comp. 2.5mg",
  "7-D-1 60cp" = "Carbamazepine Comp. 200mg 60 cp",
  "G40" = "NA",
  "7-D-1/30" = "Carbamazepine Comp. 200mg 30 cp",
  "7D1 - (60)" = "Carbamazepine Comp. 200mg 60 cp",
  "7-D-D" = "Carbamazepine Comp. 200mg",
  "7D1 - 30cpm" = "Carbamazepine Comp. 200mg 30 cp",
  "7G7" = "Paroxetine Caps. 20mg"
)





# Apply the mapping to your data
merged_data$FirstFAMedication <- factor(merged_data$FirstFAMedication,
                                            
                                            levels= names(FAmedication_mapping ),
                                            labels = FAmedication_mapping 
)

#Frequency of first follow up medication 


merged_data$FrequencyofFirstFAMedication <- factor(merged_data$FrequencyofFirstFAMedication,
                                               levels = c("1vez_por_dia", "de_12_em_12_horas", "de_8_em_8_horas", "de_6_em_6_horas","uma_vez","---"),
                                               labels = c("Once a day", "Every 12 hours",  "Every 8 hours","Every 6 hours", "One time only", "NA")
)


#Is the First follow up(FA) medication prescribed  received 

merged_data$WasPrescribedFAMedicationRecieved.<- factor(merged_data$WasPrescribedFAMedicationRecieved.,
                                                   levels = c("1","0", "---"),
                                                   labels= c("Yes","No", "NA"))


#Reason for the First follow up(FA)  medication not received 

merged_data$ReasonPrescribedFAMedicationNotRecieved<- factor(merged_data$ReasonPrescribedFAMedicationNotRecieved,
                                                            levels = c("1", "---", "NA"),
                                                            
                                                            labels= c( "Not available in the pharmacy", "NA", "NA"))

#Follow up appointment scheduled 

merged_data$Follow.upApptScheduled..2nd.<- factor(merged_data$Follow.upApptScheduled..2nd.,
                                                   levels = c("0","1","---"),
                                                   labels= c("No", "Yes", "NA"))


# Did patient came 5 days before medication/after they were scheduled 

merged_data$Did.the.patient.come.within.5.days..before.or.after..they.were.scheduled<- 
  factor(merged_data$Did.the.patient.come.within.5.days..before.or.after..they.were.scheduled,
                                                                                              
                                                   levels = c("0","1","---"),
                                                   labels= c("No", "Yes", "NA"))


#Did the patient came to the appointment before running out of their medication 

merged_data$Did.the.patient.come.to.appt.before.running.out.of.medication.AND.report.taking.medication.as.prescribed<- 
  factor(merged_data$Did.the.patient.come.to.appt.before.running.out.of.medication.AND.report.taking.medication.as.prescribed,
                                                   levels = c("0","1","NA"),
                                                   labels= c("No", "Yes", "NA"))

#patient illegible for follow up 

merged_data$Patient.eligible.for.follow.up.cascade..prescribed.medication.<- 
  factor(merged_data$Patient.eligible.for.follow.up.cascade..prescribed.medication.,
         levels = c("0","1","NA"),
         labels= c("No", "Yes", "NA"))

#Step 1 achieved ? 

merged_data$Step.1.Achieved<- 
  factor(merged_data$Step.1.Achieved,
         levels = c("0","1"),
         labels= c("No", "Yes"))

#Step 2 achieved ? 

merged_data$Step.2.Achieved<- 
  factor(merged_data$Step.2.Achieved,
         levels = c("0","1"),
         labels= c("No", "Yes"))
#Step 3 achieved ? 

merged_data$Step.3.Achieved<- 
  factor(merged_data$Step.3.Achieved,
         levels = c("0","1"),
         labels= c("No", "Yes"))
#Patient illegible for follow up cascade 

merged_data$Patient.eligible.for.follow.up.cascade<- 
  factor(merged_data$Patient.eligible.for.follow.up.cascade,
         levels = c("0","1", "---"),
         labels= c("No", "Yes", "NA"))


#Step 4 achieved ? 

merged_data$Step.4.Achieved<- 
  factor(merged_data$Step.4.Achieved,
         levels = c("0","1"),
         labels= c("No", "Yes"))

#Step 6 achieved ? 

merged_data$Step.6.Achieved<- 
  factor(merged_data$Step.6.Achieved,
         levels = c("0","1"),
         labels= c("No", "Yes"))

#Step 7 achieved ? 

merged_data$Step.7.Achieved<- 
  factor(merged_data$Step.7.Achieved,
         levels = c("0","1"),
         labels= c("No", "Yes"))

#Step 8 achieved ? 

merged_data$Step.8.Achieved<- 
  factor(merged_data$Step.8.Achieved,
         levels = c("0","1"),
         labels= c("No", "Yes"))
# Save the clean data



#Table 1 descriptive statistics 

# Define the age categories
merged_data$AgeCat <- cut(merged_data$AgeatIntake,
                       breaks = c(0, 18, 25, 35, 45, 55, Inf), # Define the age intervals
                       labels = c("<18", "18-25", "26-35", "36-45", "46-55", "56+"), # Labels for categories
                       right = FALSE) # Include the left endpoint in the interval

#Define BMI category
class(merged_data$DisplayBMI)
merged_data$DisplayBMI <- as.numeric(merged_data$DisplayBMI)
any(is.na(merged_data$DisplayBMI) | !is.numeric(merged_data$DisplayBMI))


merged_data$BMICat <- cut(merged_data$DisplayBMI,
                          breaks = c(0, 18.5, 24.9, 29.9, Inf), # Define the age intervals
                          labels = c("<18.5", "18.5-24.9", "25-29.9", "30+"), # Labels for categories
                          right = FALSE) # Include the left endpoint in the interval

#Define SYB category

class(merged_data$SystolicBloodPressure)
merged_data$SystolicBloodPressure <- as.numeric(merged_data$SystolicBloodPressure)
any(is.na(merged_data$SystolicBloodPressure) | !is.numeric(merged_data$SystolicBloodPressure))


merged_data$SYBCat <- cut(merged_data$SystolicBloodPressure,
                          breaks = c(0, 120, 129, 139, Inf), # Define the age intervals
                          labels = c("<120", "120-129", "130-139", "140+"), # Labels for categories
                          right = FALSE) # Include the left endpoint in the interval

#Define DYB category

class(merged_data$DiastolicBloodPressure)
merged_data$DiastolicBloodPressure <- as.numeric(merged_data$DiastolicBloodPressure)
any(is.na(merged_data$DiastolicBloodPressure) | !is.numeric(merged_data$DiastolicBloodPressure))


merged_data$DYBCat <- cut(merged_data$DiastolicBloodPressure,
                          breaks = c(0, 80, 89, Inf), # Adjust the breaks
                          labels = c("<80", "80-89", "90+"), # Three labels
                          right = FALSE) # Include the left endpoint in the interval





#Save the merged data to a new CSV file
#write.csv(merged_data, "SAIAFinalV1.csv", row.names = FALSE)


# Create the table
summary_table <- table1(
  ~ AgeCat + Sex + Relationship.Status + HIVStatus + TBStatus + SYBCat + DYBCat +
     BMICat + AlcoholUse + DrugUse + Alcohol..DrugsUse + 
    ThoughtsofSuicide + PatientPregnant + EngagedinFamilyPlanning + 
    InitialWHODASScore + PrimaryDiagnosis + SecondaryDiagnosis + FirstMedication + 
    NameofSecondMedication | Region,
  data = merged_data,
  summarize = list(
    Count = n_distinct(merged_data$PatientID),  # Corrected column reference here
    render.continuous = c(. = "Mean (SD)", "Median [IQR]" = "Median [Q1, Q3]", "Range" = "[Min, Max]"))
)


# Print the summary table
summary_table


# Create the table
unique_df <- merged_data %>%
  distinct(PatientID, AgeCat , Sex, Relationship.Status , HIVStatus , TBStatus , SYBCat , DYBCat,
             BMICat , AlcoholUse , DrugUse , Alcohol..DrugsUse , 
             ThoughtsofSuicide , PatientPregnant , EngagedinFamilyPlanning ,
             InitialWHODASScore , PrimaryDiagnosis , SecondaryDiagnosis , FirstMedication, 
             NameofSecondMedication, Region)

summary_table <- table1(
  ~ AgeCat + Sex + Relationship.Status + HIVStatus + TBStatus + SYBCat + DYBCat +
     BMICat + AlcoholUse + DrugUse + Alcohol..DrugsUse + 
    ThoughtsofSuicide + PatientPregnant + EngagedinFamilyPlanning + 
    InitialWHODASScore + PrimaryDiagnosis + SecondaryDiagnosis + FirstMedication + 
    NameofSecondMedication | Region,
   data = unique_df
)

# Print the summary table
summary_table


summary_table <- table1(
  ~ Sex,
  data = merged_data,
  summarize=list(Count = n_distinct(merged_data$PatientID ))
)
summary_table

#library(table1)

# Create a data frame with the counts
count_data <- merged_data %>%
  group_by(PatientID, Sex, Region)
  #summarize(Count = n_distinct(PatientID))

unique_df <- merged_data %>%
  distinct(PatientID, Sex, Region)

unique_df <- merged_data %>%
  distinct(PatientID, AgeCat , Region, Sex, Relationship.Status,
           HIVStatus , TBStatus , #SYBCat , DYBCat,# BMICat, PatientPregnant ,EngagedinFamilyPlanning ,
            #AlcoholUse , #DrugUse , #Alcohol..DrugsUse , 
          # ThoughtsofSuicide ,  
            InitialWHODASScore ,  SecondaryDiagnosis , FirstMedication, 
           NameofSecondMedication #PrimaryDiagnosis 
)


unique_df
# Use table1 to create the summary table
summary_table <- table1(
  ~ Sex | Region,
  data = unique_df,
  total = list(format = FALSE),
  includeNA = TRUE
)

summary_table





# descriptive stat by region and sex
merged_data %>%
  group_by(Sex) %>%
  summarize(Count = n_distinct(PatientID))

#by region and age group

merged_data %>%
  group_by(AgeCat, Region) %>%
  summarize(Count = n_distinct(PatientID))

##############################################
# AUTHOR: Hiwot Weldemariam 
# Project SAIA: Baseline data MH prescription 
# CREATED on: August 2023
# updated on: April 2024
##############################################

# Set working directory
rm(list=ls())
setwd("~/Desktop/ThesisData/")

# Load libraries 
library("tidyverse")
library("haven")
library("rigr")
library(dplyr)

library(epiR)
library(dplyr)

library(haven)
library(table1)
library(survival)
library(nlme)
library(lme4)

library(survminer)
library(ggplot2)


# survival object
saiasurv<- read.csv("~/Desktop/ThesisData/New/Saia_surv_march21_final.csv")
head(saiasurv)

view(saiasurv)



# Create the Surv object with the filtered data

s.saia <- Surv(time = saiasurv$Time, event = saiasurv$Event)
summary(s.saia)

saia.km = survfit(s.saia ~ 1,cluster = saiasurv$facility, 
                  conf.type = "log-log")
summary(saia.km)

# km curve 

plot(saia.km, main = "Kaplan-Meier survival estimate", 
     xlab = "Survival time (Days)", 
     ylab = "Survival Probability",cex=1.5)
       col= "red"
abline(h=0.5, col="blue", lty=3, lwd=1.5)
segments(60, -0.5, 60, 0.5, col="red", lty=3, lwd=1.5)
segments(60, -0.5, 60, 0.5, col="orange", lty=3, lwd=1.5)
segments(60, -0.5, 60, 0.5, col="orange", lty=3, lwd=1.5)
# the median survival time for all
surv_median(saia.km)

saiasurv$primaryMed<- ifelse(saiasurv$Firstmeds =="Carbamazepine",0,
                             ifelse(saiasurv$Firstmeds=="Haloperidol",1, 
                                    ifelse(saiasurv$Firstmeds=="Amitriptyline",2,
                                           ifelse(saiasurv$Firstmeds=="Thioridazine",3,4))))

table(saiasurv$primary_diagnosis)
#primary diagnosis 
saiasurv$primaryDiag<- ifelse(saiasurv$PrimaryD=="Epilepsy",0,
                              ifelse(saiasurv$PrimaryD=="Schizophrenia",1,
                                     ifelse(saiasurv$PrimaryD=="Delusional disorder",2,
                                            ifelse(saiasurv$PrimaryD=="Depressive episodes",3,
                                                   ifelse(saiasurv$PrimaryD %in% c("Mental and behavioral disorders due to alcohol use",
                                                                                "Mental and behavioral disorders due to multiple drug use",
                                                                                "Mental and behavioral disorders due to opioid use"), 4, 5)))))

table(saiasurv$AgeCat)

saiasurv$AgeCat <- factor(saiasurv$AgeCat, levels = c("18-35", "<18", "36-55", "56+"))

table(saiasurv$AgeCat)

table(saiasurv$primaryDiag)
table(saiasurv$primaryMed)
library(gtsummary)

# Your existing code with modification for statistic argument
tab1_adh <- saiasurv %>%
    select(Event, AgeCat, sex2, Martial, primaryDiag, primaryMed, SBP, DBP, Weight, BMI, WD_trueBL_all, Region) %>%
    tbl_summary(by = Event, 
                label = list(AgeCat ~ "Patient age group (years)",
                             sex2 ~ "Patient being male",
                             Martial ~ "Martial status",
                             primaryDiag ~ "primary Diagnosies",
                             primaryMed ~ "Primary Medication",
                             SBP ~ "systolic blood pressure",
                             DBP ~ "Diastolic blood pressure",
                             Weight ~ "weight",
                             BMI ~ "Body mass index",
                             WD_trueBL_all ~ "WHODAS score"),
                type = list(all_continuous() ~ 'continuous2'),
                statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"), # Add mean and SD, and median with IQR
                                 all_categorical() ~ "{n} ({p}%)"),
                digits = list(all_continuous() ~ c(1, 1, 1, 1), # Adjust digits to display for each statistic
                              all_categorical() ~ c(0, 1)),
                missing = "ifany") %>%
    add_p()

tab1_adh


# Fit the Cox model

#crude gender

saiasurv$s.saia <- Surv(time = saiasurv$Time, event = saiasurv$Event)
summary(s.saia)
saia.cox_sex_c = coxph(s.saia ~ sex2  ,cluster = saiasurv$facility, data = saiasurv)
# Display the summary of the model
summary(saia.cox_sex_c)

saiasurv$sex.group<- ifelse(saiasurv$sex2=="female",1,2)

saiasurv$mar.group <- ifelse(saiasurv$Martial == "Common-law", 1,
                             ifelse(saiasurv$Martial == "Divorced", 2,
                                    ifelse(saiasurv$Martial == "Married", 3,
                                           ifelse(saiasurv$Martial == "Separated", 4,
                                                  ifelse(saiasurv$Martial == "Single", 5, 
                                                         6)))))  # widowed

saia.cox.globaltest = coxph(s.saia ~ primaryMed + primaryDiag + Age + mar.group +sex.group +Weight+SBP+DBP+WD_trueBL_all, 
                     cluster = saiasurv$facility,
                     data = saiasurv
)
summary(saia.cox.globaltest)
#### adding weght, BP and WHODAS 

saia.cox_weight = coxph(s.saia ~ saiasurv$Weight,cluster = saiasurv$facility, data = saiasurv)
# Display the summary of the model
summary(saia.cox_weight)


saia.cox_whodas = coxph(s.saia ~ saiasurv$WD_trueBL_all,cluster = saiasurv$facility, data = saiasurv)
# Display the summary of the model
summary(saia.cox_whodas)

saia.cox_spb = coxph(s.saia ~ saiasurv$SBP,cluster = saiasurv$facility, data = saiasurv)
# Display the summary of the model
summary(saia.cox_spb)

saia.cox_dbp = coxph(s.saia ~ saiasurv$DBP,cluster = saiasurv$facility, data = saiasurv)
# Display the summary of the model
summary(saia.cox_dbp)







#Km
Surv_sex.km <- survfit(s.saia~ saiasurv$sex2, cluster = saiasurv$facility, data = saiasurv)

surv_median(Surv_sex.km)# median survival time by sex 

#km plot

plot(Surv_sex.km, main = "KM survival estimate by sex",
     xlab = "Survival time(Days)",
     ylab = "Survival Probability",
     col = c("red", "blue"))
legend("topright", 
       legend = c("Male", "Female"),
       col = c("red", "blue"), 
       lty = 1,
bty = "n")
abline(h=0.5, col="blue", lty=3, lwd=1.5)


# log rank test

survdiff(s.saia ~ sex2, data=saiasurv) # high p-value no statistical difference between male and female 

### age group
#crude

#saiasurv$AgeCat <- factor(saiasurv$AgeCat, levels = c("18-35", "<18", "36-55", "55+"))

# Run the Cox model
cox.age <- coxph(s.saia ~ as.factor(AgeCat), cluster = saiasurv$facility, data = saiasurv)

# View the summary
summary(cox.age)





Surv_age.km <- survfit(s.saia~ saiasurv$AgeCat, cluster = saiasurv$facility, data = saiasurv)

surv_median(Surv_age.km)


# Plot the km curve 
plot(Surv_age.km, 
     main = "KM Survival estimate by Age Group", 
     xlab = "Survival time in (Days)", 
     ylab = "Survival Probability", 
     col = c("red", "blue", "yellow", "purple"))
legend("topright", 
       legend = c("<18", "18-35", "36-55",  "56+"),
       lty = 1, 
       col = c("red", "blue", "yellow", "purple"),
       bty = "n")
abline(h=0.5, col="blue", lty=3, lwd=1.5)
    
#log rank age
table(saiasurv$AgeCat)
survdiff(s.saia ~ AgeCat, data=saiasurv) # p=0.1 no difference b.n age group 
### Martial status 

# Converting 'Martial' to a factor
saiasurv$Martial <- as.factor(saiasurv$Martial)

# Setting 'Single' as the reference level
saiasurv$Martial <- relevel(saiasurv$Martial, ref = "Single")

# Running the Cox regression with 'Single' as the reference
cox.mar <- coxph(s.saia~ as.factor(Martial), cluster = saiasurv$facility, data = saiasurv)

# Summary of the model
summary(cox.mar)


surv_mar.km <- survfit(s.saia ~  as.factor(Martial) , cluster = saiasurv$facility, data = saiasurv)
surv_median(surv_mar.km)


# Plot the survival curves
plot(surv_mar.km, 
     main = "KM survival estimate by Marital Status", 
     xlab = "Survival time(Days)", 
     ylab = "Survival Probability", 
     col = c("red", "blue", "green", "purple", "orange", "brown"))

# Add a legend
legend("topright", 
       legend = c("Divorced", "Separated", "Widowed",
                  "Married", "Common-law", "Single"), 
       lty = 1, 
       col = c("red", "blue", "green", "purple", "orange", "brown"),
       bty = "n")
abline(h=0.5, col="blue", lty=3, lwd=1.5)
#log rank in marital status 
survdiff(s.saia ~ Martial, data=saiasurv)
### primary diag. 
 
saia.cox.diag= coxph(s.saia ~ as.factor(primaryDiag), cluster = saiasurv$facility, data = saiasurv)
summary(saia.cox.diag)

surv_diag.km <- survfit(s.saia ~ as.factor(primaryDiag), cluster = saiasurv$facility, data = saiasurv)
surv_median(surv_diag.km)

###plot 

# Adjust the margins of the plot (increase the bottom margin)
par(mar = c(8, 4, 4, 2))

# Plot the survival 
plot(surv_diag.km, 
     main = "KM survival estimate by Primary Diagnosies", 
     xlab = "Survival time(Days)", 
     ylab = "Estimated survival Probability", 
     col = c("red", "blue", "yellow", "purple", "orange", "black"))

# Add a legend below the plot
# Using normalized parent coordinates for positioning
library(grid)
legend("bottom", 
       legend = c("Epilepsy", "Schizophrenia related disorders",
                  "Other psychotic and delusional disorders",
                  "Depression related disorders", 
                  "Mental and Behavioural disorders due to substance use",
                  "All other diagnoses"), 
       lty = 1, 
       col = c("red", "blue", "yellow", "purple", "orange", "black"), 
       bty = "n", 
       horiz = TRUE,
       xpd = TRUE, 
       inset = c(0, -1))

abline(h = 0.5, col = "blue", lty = 3, lwd = 1.5)


# Create a new blank plotting window
plot.new()

# Add a legend to the blank plotting window
legend(x = "center", y = "center",
       legend = c("Epilepsy", "Schizophrenia related disorders",
                  "Other psychotic and delusional disorders",
                  "Depression related disorders", 
                  "Mental and Behavioural disorders due to substance use",
                  "All other diagnoses"), 
       lty = 1, 
       col = c("red", "blue", "yellow", "purple", "orange", "black"), 
       bty = "n", 
       horiz = FALSE, 
       cex = 1)  # Adjust the cex value for text size if needed




#plot PM 


plot(surv_diag.km, 
     main = "KM survival estimate by Primary Diagnosies", 
     xlab = "Survival time(Days)", 
     ylab = "Survival Probability", 
     col = c("red", "blue", "yellow", "purple", "orange", "black"))

legend("topright", #inset=c(-0.2, -0.2), 
       legend=c("Epilepsy", "Schizophrenia related disorders",
                "Other psychotic and delusional disorders",
                "Depression related disorders", 
                "Mental and Behavioural disorders due to substance use",
                "All other diagnoses"), 
       pch=c(1, 1, 1, 1, 1, 1), # Adjust this as needed
       col = c("red", "blue", "yellow", "purple", "orange", "black"), 
       title=" ", bty="n", cex = 0.5)

abline(h=0.5, col="blue", lty=3, lwd=1.5)


survdiff(s.saia ~ primaryDiag, data=saiasurv) #p= 0.004 there is differnece 

## Primary medication

saia.cox.med = coxph(s.saia ~ as.factor(primaryMed), cluster = saiasurv$facility, data = saiasurv)
summary(saia.cox.med)

surv_med.km <- survfit(s.saia ~ as.factor(primaryMed), cluster = saiasurv$facility, data = saiasurv)
surv_median(surv_med.km)

## plot 
plot(surv_med.km, main = "KM survival estimate by primary medication", 
     xlab="Survival time(Days)", ylab="Estimated survival Probability",
     col = c("red", "blue","yellow","purple", "orange"))
legend("topright",c("Carbamazepine","Haloperidol", "Amitriptyline", "Thioridazine", "Other"), lty = c(1,1,1,1,1), 
       col = c("red", "blue", "yellow", "purple", "orange") ,bty="n")

abline(h=0.5, col="blue", lty=3, lwd=1.5)
# log rank test
survdiff(s.saia ~ primaryMed, data=saiasurv) # p=0.006 there is difference 








#####adjusted 

saia.cox.adj = coxph(s.saia ~ as.factor(primaryMed) + 
        as.factor(primaryDiag) + as.factor(saiasurv$Martial) +saiasurv$AgeCat +saiasurv$sex2 +saiasurv$Weight+saiasurv$DBP+
          saiasurv$SBP+ saiasurv$WD_trueBL_all, 
    cluster = saiasurv$facility,
    data = saiasurv
)
summary(saia.cox.adj)

#####testing if prop hazard assumption holds using schoenfeld residual test 

saia.cox.adj.schoenfeld = coxph(s.saia ~ as.factor(primaryMed) + 
                       as.factor(primaryDiag) + as.factor(saiasurv$Martial) +saiasurv$AgeCat 
                       +saiasurv$sex2, 
                     cluster = saiasurv$facility,
                     data = saiasurv
)
summary(saia.cox.adj.schoenfeld)

#test
# Test the proportional hazards assumption
ph_test_adj <- cox.zph(saia.cox.adj.schoenfeld)

# View the test results
print(ph_test_adj)

### test plot 

# Plot Schoenfeld residuals to visually assess proportional hazards
plot(ph_test_adj)

### primary diag violates so we did stratify 

cox_stratified <- coxph(s.saia ~ as.factor(primaryMed) + strata(as.factor(primaryDiag)) + 
                          as.factor(saiasurv$Martial) + saiasurv$AgeCat + saiasurv$sex2, 
                        cluster = saiasurv$facility, data = saiasurv)

summary(cox_)


#first med 
table(saiasurv$PrimaryD)
table(saiasurv$Firstmeds)

library(dplyr)

# Filter and then count the occurrences of each PrimaryD
result <- saiasurv %>%
    filter(Firstmeds == "Amitriptyline") %>%
    count(PrimaryD)

# View the result
print(result)
### model selection 

modelAll.coxph <- coxph(s.saia ~ as.factor( saiasurv$primaryMed) + 
                            as.factor( saiasurv$primaryDiag) + as.factor( saiasurv$Martial) +
                            saiasurv$Age + saiasurv$sex2, 
                        data = saiasurv)

result.step <- step(modelAll.coxph, scope=list(upper=~as.factor( saiasurv$primaryMed) +
                                                   
                                                   as.factor( saiasurv$primaryDiag) + 
                                                   as.factor( saiasurv$Martial) +
                                                   saiasurv$Age + saiasurv$sex2, 
                                               lower=~as.factor( saiasurv$primaryMed)))




### ploting all the KM together 
graphics.off()  # This will clear all graphics devices

layout(matrix(1:6, nrow=3, ncol=2, byrow=TRUE))

# Adjust label font size (example: 0.8 for smaller labels)
label_font_size = 0.4
# First plot - spans the entire first row
plot(saia.km, main = "Kaplan-Meier survival estimate", 
     xlab = "Survival time (Days)", 
     ylab = "Survival Probability", cex=1.5)
abline(h=0.5, col="blue", lty=3, lwd=1.5)

# Fifth plot - second in the third row
plot(surv_med.km, main = "Primary medication", 
     xlab = "Survival time (Days)", ylab = "survival Probability",
     col = c("red", "blue", "yellow", "purple", "orange"))
legend("topright", c("Carbamazepine", "Haloperidol", "Amitriptyline", "Thioridazine", "Other"), 
       lty = c(1, 1, 1, 1, 1), col = c("red", "blue", "yellow", "purple", "orange"), bty = "n")
abline(h=0.5, col="blue", lty=3, lwd=1.5)

# Second plot - first in the second row
plot(Surv_sex.km, main = "Gender",
     xlab = "Survival time (Days)",
     ylab = "Survival Probability",
     col = c("red", "blue"))
legend("topright", legend = c("Male", "Female"),
       col = c("red", "blue"), lty = 1, bty = "n")
abline(h=0.5, col="blue", lty=3, lwd=1.5)


# Fourth plot - first in the third row
plot(surv_diag.km, 
     main = "Primary diagnoses", 
     xlab = "Survival time (Days)", 
     ylab = "Survival Probability", 
     col = c("red", "blue", "yellow", "purple", "orange", "black"))
legend("topright", legend = c("Epilepsy", 
                              "Schizophrenia related disorders",
                              "Other psychotic and delusional disorders",
                              "Depression related disorders", 
                              "Mental and Behaviouraldisorders due to substance use",
                              "All other diagnoses"), 
       lty = c(1, 1, 1, 1, 1, 1), col = c("red", "blue", "yellow", "purple", "orange", "black"), 
       bty="n",   
       cex = 0.6)

abline(h=0.5, col="blue", lty=3, lwd=1.5)

# Third plot - second in the second row
plot(Surv_age.km, 
     main = "Age group", 
     xlab = "Survival time (Days)", 
     ylab = "survival Probability", 
     col = c("red", "blue", "yellow", "purple"))
legend("topright", legend = c("<18", "18-35", "36-55", "56+"),
       lty = 1, col = c("red", "blue", "yellow", "purple"), bty = "n")
abline(h=0.5, col="blue", lty=3, lwd=1.5)



# Sixth plot - third in the third row
plot(surv_mar.km, 
     main = "Marital status", 
     xlab = "Survival time (Days)", ylab = "Survival Probability", 
     col = c("red", "blue", "green", "purple", "orange", "brown"))
legend("topright", c("Divorced", "Separated", "Widowed",
                     "Married", "Common-law", "Single"), 
       lty = c(1, 1, 1, 1, 1, 1), col = c("red", "blue", "green", "purple", "orange", "brown"), bty = "n")
abline(h=0.5, col="blue", lty=3, lwd=1.5)
       
#risk difference or attributable risk 

install.packages("rms")
library("rms")


# Fit the model absolute hazard risk differnce 
#adjusted 
saia.cox_RD_adj <- ah(coxph(Surv(time, status)  ~ as.factor(primaryMed) + 
                         as.factor(primaryDiag) + as.factor(saiasurv$Martial) +saiasurv$Age +saiasurv$sex2, 
                     cluster = saiasurv$facility,
                     data = saiasurv ))

summary(saia.cox_RD_adj)




######### repeated measure - mixed model analysis ####

saiaMixed<- read.csv("~/Desktop/ThesisData/New/Saia_LMM_V4_final.csv")
head(saiaMixed)

saiaMixed$PatientID <- as.factor(saiaMixed$formcasecase_id)

#  basic model
model <- lme(WD_trueBL_all ~ 1, random = ~1 | PatientID, data = saiaMixed)
model



#  predictors

library(nlme)

table(saiaMixed$Firstmeds)
table(saiaMixed$PrimaryD)

saiaMixed$PriMed<- ifelse(saiaMixed$Firstmeds =="Carbamazepine",0,
                             ifelse(saiaMixed$Firstmeds=="Haloperidol",1, 
                                    ifelse(saiaMixed$Firstmeds=="Amitriptyline",2,
                                           ifelse(saiaMixed$Firstmeds=="Thioridazine",3,4))))

table(saiaMixed$PriMed)

table(saiaMixed$PrimaryDiag)
#primary diagnosis 
saiaMixed$priDig<- ifelse(saiaMixed$PrimaryD=="Epilepsy",0,
                              ifelse(saiaMixed$PrimaryD=="Schizophrenia",1,
                                     ifelse(saiaMixed$PrimaryD=="Delusional disorder",2,
                                            ifelse(saiaMixed$PrimaryD=="Depressive episodes",3,
                                                   ifelse(saiaMixed$PrimaryD %in% c("Mental and behavioral disorders due to alcohol use",
                                                                                            "Mental and behavioral disorders due to multiple drug use",
                                                                                            "Mental and behavioral disorders due to opioid use"), 4, 5)))))

                                                   
                                                   





# crude primary med


modelWhoda_c <- lme(WD_trueBL_all ~ as.factor(PriMed), 
                    random = ~ 1 | facility/PatientID,
                    data = saiaMixed[saiaMixed$Age > 18, ],
                    na.action = na.exclude)

confint_modelWhoda_c <- intervals(modelWhoda_c, which = "fixed")
print(confint_modelWhoda_c$fixed)
# Summary of the model
summary(modelWhoda_c)

#global F for primary med

modelWhoda_c.pm <- lme(WD_trueBL_all ~ PriMed, 
                    random = ~ 1 | facility/PatientID,
                    data = saiaMixed[saiaMixed$Age > 18, ],
                    na.action = na.exclude)
summary(modelWhoda_c.pm)

#crude primary diag


modelWhoda_cp <- lme(WD_trueBL_all ~ as.factor(priDig), 
                    random = ~ 1 | facility/PatientID,
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model
summary(modelWhoda_cp)

confint_modelWhoda_cp <- intervals(modelWhoda_cp, which = "fixed")
print(confint_modelWhoda_cp$fixed)


## global f test for primary diag 

modelWhoda_cp.gf <- lme(WD_trueBL_all ~ priDig, 
                     random = ~ 1 | facility/PatientID,
                     data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model
summary(modelWhoda_cp.gf)
## Sex crude 

modelWhoda_s <- lme(WD_trueBL_all ~ sex2, 
                     random = ~ 1 | facility/PatientID,
                     data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model
summary(modelWhoda_s)

confint_modelWhoda_s <- intervals(modelWhoda_s, which = "fixed")
print(confint_modelWhoda_s$fixed)

#global f test sex 
saiaMixed$sexgf<-ifelse(saiaMixed$sex2=="female",1,2)
modelWhoda_s.gf <- lme(WD_trueBL_all ~ sexgf, 
                    random = ~ 1 | facility/PatientID,
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model
summary(modelWhoda_s.gf)
## age
table(saiaMixed$AgeCat)
saiaMixed$Agegroup<- factor(saiaMixed$AgeCat, levels = c("18-35", "<18", "36-55", "56+"))

modelWhoda_age <- lme(WD_trueBL_all ~ as.factor(Agegroup), 
                    random = ~ 1 | facility/PatientID,
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model

summary(modelWhoda_age)
confint_modelWhoda_age <- intervals(modelWhoda_age, which = "fixed")
print(confint_modelWhoda_age$fixed)

#age for global test
modelWhoda_age.gf <- lme(WD_trueBL_all ~Age, 
                      random = ~ 1 | facility/PatientID,
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model

summary(modelWhoda_age.gf)


saiaMixed$Mar<- ifelse(saiaMixed$Martial=="Single",0,
                      ifelse(saiaMixed$Martial=="Married", 1, 
                             ifelse(saiaMixed$Martial=="Separated", 2, 
                                    ifelse(saiaMixed$Martial=="Divorced",3,4))))
## Martial
modelWhoda_mar <- lme(WD_trueBL_all ~ as.factor(Mar), 
                      random = ~ 1 | facility/PatientID,
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model

summary(modelWhoda_mar)
confint_modelWhoda_mar <- intervals(modelWhoda_mar, which = "fixed")
print(confint_modelWhoda_mar$fixed)

# martial global test

modelWhoda_mar.gf <- lme(WD_trueBL_all ~ Mar, 
                      random = ~ 1 | facility/PatientID,
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelWhoda_mar.gf)


## Days
saiaMixed$DAYS_30 = saiaMixed$DAYS / 30

modelWhoda_days <- lme(WD_trueBL_all ~ DAYS_30, 
                      random = ~ 1 | facility/PatientID,
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model

summary(modelWhoda_days)

confint_modelWhoda_days <- intervals(modelWhoda_days, which = "fixed")
print(confint_modelWhoda_days$fixed)

#model_coef <- summary(modelWhoda_days)$tTable

#model_coef["DAYS", "Value"] <- model_coef["DAYS", "Value"] * 30

#confint_modelWhoda_days <- intervals(modelWhoda_days, which = "fixed")
#confint_modelWhoda_days$fixed["DAYS",] <- confint_modelWhoda_days$fixed["DAYS",] * 30
#print(confint_modelWhoda_days$fixed)



#adjusted 


modelWhoda_adj <- lme(WD_trueBL_all ~ as.factor(PriMed)  + as.factor(priDig) + 
                          DAYS_30 +as.factor(Agegroup) +as.factor(Mar) +sex2, 
                  random = ~ 1 | facility/PatientID,
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model
summary(modelWhoda_adj)


confint_modelWhoda_adj <- intervals(modelWhoda_adj, which = "fixed")
print(confint_modelWhoda_adj$fixed)


#adjsuted for globla test 

modelWhoda_adj.gf <- lme(WD_trueBL_all ~ PriMed  + priDig + 
                          DAYS_30 +Age +Mar +sexgf, 
                      random = ~ 1 | facility/PatientID,
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model
summary(modelWhoda_adj.gf)


confint_modelWhoda_adj <- intervals(modelWhoda_adj, which = "fixed")
print(confint_modelWhoda_adj$fixed)

#int 
modelWhoda_int <- lme(WD_trueBL_all ~ as.factor(PriMed)  + as.factor(priDig) + 
                         DAYS +as.factor(Agegroup) +as.factor(Mar) +sex2 + as.factor(PriMed)*DAYS, 
                     random = ~ 1 | facility/PatientID,
                     data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

# Summary of the model
summary(modelWhoda_int)


confint_modelWhoda_int <- intervals(modelWhoda_int, which = "fixed")
print(confint_modelWhoda_int$fixed)

# Plot a histogram of residuals
hist(resid(modelWhoda), main = "Residuals Histogram", xlab = "Residuals")

# QQ-plot of residuals
qqnorm(resid(modelWhoda))
qqline(resid(modelWhoda), col = "red")


#systolic BP

#SBP Curde 

saiaMixed$SBP<-as.numeric( saiaMixed$SBP)
#Meds
modelsbp_c <- lme(SBP ~ as.factor(PriMed) , 
                random = ~1 | facility/PatientID,  # Nested random effects
                data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modelsbp_c)
confint_modelsbp_c  <- intervals(modelsbp_c, which = "fixed")
print(confint_modelsbp_c$fixed)

#med for global test

modelsbp_c.gf <- lme(SBP ~ PriMed , 
                  random = ~1 | facility/PatientID,  # Nested random effects
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modelsbp_c.gf)

# Diag
modelsbp_cp <- lme(SBP ~ as.factor(priDig) , 
                  random = ~1 | facility/PatientID,  # Nested random effects
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modelsbp_cp)
confint_modelsbp_cp  <- intervals(modelsbp_cp, which = "fixed")
print(confint_modelsbp_cp$fixed)


#diag for global test 

modelsbp_cp.gf <- lme(SBP ~ priDig , 
                   random = ~1 | facility/PatientID,  # Nested random effects
                   data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modelsbp_cp.gf)

#martial 

modelsbp_mar <- lme(SBP ~ as.factor(Mar) , 
                   random = ~1 | facility/PatientID,  # Nested random effects
                   data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modelsbp_mar)

confint_modelsbp_mar <- intervals(modelsbp_mar, which = "fixed")
print(confint_modelsbp_mar $fixed)

# mar global f test 

modelsbp_mar.gf <- lme(SBP ~ Mar, 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modelsbp_mar.gf)
#gender

modelsbp_s <- lme(SBP ~ sex2 , 
                   random = ~1 | facility/PatientID,  # Nested random effects
                   data = saiaMixed[saiaMixed$Age > 18, ])

summary(modelsbp_s)

confint_modelsbp_s  <- intervals(modelsbp_s, which = "fixed")
print(confint_modelsbp_s$fixed)
#days 
modelsbp_day <- lme(SBP ~ DAYS_30 , 
                   random = ~1 | facility/PatientID,  # Nested random effects
                   data = saiaMixed[saiaMixed$Age > 18, ])

summary(modelsbp_day)

confint_modelsbp_day<- intervals(modelsbp_day, which = "fixed")
print(confint_modelsbp_day$fixed)
#age 
modelsbp_age <- lme(SBP ~ as.factor(Agegroup) , 
                   random = ~1 | facility/PatientID,  # Nested random effects
                   data = saiaMixed[saiaMixed$Age > 18, ])

summary(modelsbp_age)


confint_modelsbp_age <- intervals(modelsbp_age, which = "fixed")
print(confint_modelsbp_age$fixed)


#age global 

modelsbp_age.gf <- lme(SBP ~ Age , 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ])

summary(modelsbp_age.gf)

##SBP Adjsuted 

modelsbp_adj <- lme(SBP ~ as.factor(PriMed) +as.factor(priDig) + DAYS_30 + as.factor(Agegroup) +as.factor(Mar) +sex2, 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ],na.action = na.exclude )

summary(modelsbp_adj)
confint_modelsbp_adj <- intervals(modelsbp_adj, which = "fixed")
print(confint_modelsbp_adj$fixed)

#adj global test

modelsbp_adj.gf <- lme(SBP ~ PriMed +priDig + DAYS_30 + Age +Mar +sex2, 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ],na.action = na.exclude )

summary(modelsbp_adj.gf)




#int

modelsbp_int <- lme(SBP ~ as.factor(PriMed) +as.factor(priDig) + DAYS+ as.factor(Agegroup)
                    +as.factor(Mar) +sex2+ as.factor(PriMed)*DAYS, 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ],na.action = na.exclude )

summary(modelsbp_int)
confint_modelsbp_int <- intervals(modelsbp_int, which = "fixed")
print(confint_modelsbp_int$fixed)


# Plot a histogram of residuals
hist(resid(modelsbp), main = "Residuals Histogram", xlab = "Residuals")

# QQ-plot of residuals
qqnorm(resid(modelsbp))
qqline(resid(modelsbp), col = "red")


#diastolic crude 

#days

modeldbp_day <- lme(DBP ~ DAYS_30 , 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_day)

confint_modeldbp_day<- intervals(modeldbp_day, which = "fixed")
print(confint_modeldbp_day$fixed)

#gender 


modeldbp_s <- lme(DBP ~ sex2 , 
                  random = ~1 | facility/PatientID,  # Nested random effects
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_s)

confint_modeldbp_s  <- intervals(modeldbp_s, which = "fixed")
print(confint_modeldbp_s$fixed)


#gender global test

modeldbp_s.gf <- lme(DBP ~ sexgf , 
                  random = ~1 | facility/PatientID,  # Nested random effects
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_s.gf)

#age

modeldbp_age <- lme(DBP ~ as.factor(Agegroup) , 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_age)


confint_modeldbp_age <- intervals(modeldbp_age, which = "fixed")
print(confint_modeldbp_age$fixed)

# age global test

modeldbp_age.gf <- lme(DBP ~ Age , 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_age.gf)


#Martial 

modeldbp_mar <- lme(DBP ~ as.factor(Mar) , 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_mar)

confint_modeldbp_mar <- intervals(modeldbp_mar, which = "fixed")
print(confint_modeldbp_mar $fixed)


#mar global test 

modeldbp_mar.gf <- lme(DBP ~ Mar , 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_mar.gf)


#med

modeldbp_c <- lme(DBP ~ as.factor(PriMed), 
                random = ~1 | facility/PatientID,  # Nested random effects
                data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modeldbp_c)

confint_modeldbp_c <- intervals(modeldbp_c, which = "fixed")
print(confint_modeldbp_c $fixed)


#med global test
modeldbp_c.gf <- lme(DBP ~ PriMed, 
                  random = ~1 | facility/PatientID,  # Nested random effects
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modeldbp_c.gf)

# diag 
modeldbp_cp <- lme(DBP ~ as.factor(priDig) , 
                   random = ~1 | facility/PatientID,  # Nested random effects
                   data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_cp)
confint_modeldbp_cp  <- intervals(modeldbp_cp, which = "fixed")
print(confint_modeldbp_cp$fixed)


#Diag global test
modeldbp_cp.gf <- lme(DBP ~ priDig, 
                   random = ~1 | facility/PatientID,  # Nested random effects
                   data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)

summary(modeldbp_cp.gf)

## DBP adj 

modeldbp_adj <- lme(DBP ~ as.factor(PriMed) +as.factor(priDig)+ DAYS_30
                    + as.factor(Mar)+ sex2 + as.factor(Agegroup), 
                  random = ~1 | facility/PatientID,  # Nested random effects
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modeldbp_adj)

confint_modeldbp_adj  <- intervals(modeldbp_adj, which = "fixed")
print(confint_modeldbp_adj$fixed)



#adjusted global 
modeldbp_adj.gf<- lme(DBP ~ PriMed + priDig+ DAYS_30
                    + Mar + sex2 + Age, 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modeldbp_adj.gf)

#inter

modeldbp_int <- lme(DBP ~ as.factor(PriMed) +as.factor(priDig)+ DAYS
                    + as.factor(Mar)+ sex2 + as.factor(Agegroup)+ as.factor(PriMed)*DAYS, 
                    random = ~1 | facility/PatientID,  # Nested random effects
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modeldbp_int)

confint_modeldbp_int<- intervals(modeldbp_int, which = "fixed")
print(confint_modeldbp_int$fixed)

# Plot a histogram of residuals
hist(resid(modeldbp), main = "Residuals Histogram", xlab = "Residuals")

# QQ-plot of residuals
qqnorm(resid(modeldbp))
qqline(resid(modeldbp), col = "red")

#weight crude
saiaMixed$Weight

# days 

modelweight_day<- lme(Weight ~ DAYS_30 , 
                    random = ~1 | facility/PatientID, 
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_day)

confint_modelweight_day  <- intervals(modelweight_day, which = "fixed")
print(confint_modelweight_day$fixed)

#gender
modelweight_s<- lme(Weight ~ sex2 , 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_s)

confint_modelweight_s  <- intervals(modelweight_s, which = "fixed")
print(confint_modelweight_s$fixed)

#AGE

modelweight_age<- lme(Weight ~ as.factor(Agegroup) , 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_age)

confint_modelweight_age  <- intervals(modelweight_age, which = "fixed")
print(confint_modelweight_age$fixed)


#age global test
modelweight_age.gf<- lme(Weight ~ Age , 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_age.gf)
#mar

modelweight_mar<- lme(Weight ~ as.factor(Mar) , 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_mar)

confint_modelweight_mar  <- intervals(modelweight_mar, which = "fixed")
print(confint_modelweight_mar$fixed)

#mar global test 
modelweight_mar.gf<- lme(Weight ~ Mar, 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_mar.gf)


#meds 

modelweight_c<- lme(Weight ~ as.factor(PriMed), 
                  random = ~1 | facility/PatientID, 
                  data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_c)

confint_modelweight_c  <- intervals(modelweight_c, which = "fixed")
print(confint_modelweight_c$fixed)


#med global test 
modelweight_c.gf<- lme(Weight ~ PriMed, 
                    random = ~1 | facility/PatientID, 
                    data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_c.gf)

#diag

modelweight_cp<- lme(Weight ~ as.factor(priDig) , 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_cp)

confint_modelweight_cp  <- intervals(modelweight_cp, which = "fixed")
print(confint_modelweight_cp$fixed)


# global diag
modelweight_cp.gf<- lme(Weight ~ priDig , 
                     random = ~1 | facility/PatientID, 
                     data = saiaMixed[saiaMixed$Age > 18, ], na.action = na.exclude)
summary(modelweight_cp.gf)


#weight adjusted 

modelweight_adj<- lme(Weight ~ as.factor(PriMed) + as.factor(priDig)+DAYS_30+ as.factor(Agegroup)
                      + as.factor(Mar)+ sex2, 
                  random = ~1 | facility/PatientID, 
                  data = saiaMixed[saiaMixed$Age > 18, ],na.action = na.exclude )
summary(modelweight_adj)

confint_modelweight_adj  <- intervals(modelweight_adj, which = "fixed")
print(confint_modelweight_adj$fixed)


# adjusted globla test 

modelweight_adj.df<- lme(Weight ~ PriMed + priDig+DAYS_30+ Age
                      + Mar+ sex2, 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ],na.action = na.exclude )
summary(modelweight_adj.df)


#interaction

modelweight_int<- lme(Weight ~ as.factor(PriMed) + as.factor(priDig)+DAYS+ as.factor(Agegroup)
                      + as.factor(Mar)+ sex2 + as.factor(PriMed)*DAYS, 
                      random = ~1 | facility/PatientID, 
                      data = saiaMixed[saiaMixed$Age > 18, ],na.action = na.exclude )
summary(modelweight_int)

confint_modelweight_int<- intervals(modelweight_int, which = "fixed")
print(confint_modelweight_int$fixed)



# Plot a histogram of residuals
hist(resid(modelweight), main = "Residuals Histogram", xlab = "Residuals")

# QQ-plot of residuals
qqnorm(resid(modelweight))
qqline(resid(modelweight), col = "red")

###Adherance matter??

# Model for adherent patients
modelWhoda_adherent <- lme(WD_trueBL_all ~ as.factor(PriMed) + Time + priDig,
                           random = ~ 1 | facility/PatientID,
                           data = subset(saiaMixed_clean, Age > 18 & Event == "NO"))

summary(modelWhoda_adherent)
# Model for non-adherent patients
modelWhoda_nonadherent <- lme(WD_trueBL_all ~ as.factor(PriMed) + Time + priDig, 
                              random = ~ 1 | facility/PatientID,
                              data = subset(saiaMixed_clean, Age > 18 & Event == "Yes"))

# Summaries of the models

summary(modelWhoda_nonadherent)




# Fit the linear mixed-effects model including interaction with outcome
modelWhoda_event <- lme(WD_trueBL_all ~ as.factor(PriMed) * Time * Event + priDig, 
                        random = ~ 1 | facility/PatientID,
                        data = saiaMixed_clean[saiaMixed_clean$Age > 18, ])

# Summary of the model
summary(modelWhoda_event)
####Mixed model plots 

mwhoda <- lm(modelWhoda, data=saiaMixed_clean)
summary(mwhoda)

#  plotting the whoda overtime by each patient to see the association 

library(tidyverse)
average_weight <- mean(saiaMixed$Weight.in.Kilos)

ggplot(data = saiaMixed_clean[saiaMixed_clean$Age > 18, ], aes(x = Time, y = Weight.in.Kilos, color = PriMed)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        labs(title = "Patient Weight Over Time by Medication Group",
             x = "Date",
             y = "Weight in Kg") +
        theme_minimal()

#average weight for each patient 

library(dplyr)
view(saiaMixed)

average_weights_per_patient <- saiaMixed %>%
        group_by(PatientID) %>%
        summarize(average_weight = mean(Weight.in.Kilos))
saiaMixed_with_avg <- saiaMixed %>%
        left_join(average_weights_per_patient, by = "PatientID")
ggplot(data = saiaMixed_with_avg, aes(x =Time, y = average_weight, color = PriMed)) +
        geom_point() +
        geom_line(aes(group = PatientID)) +
        labs(title = "Average Patient Weight Over Time by Medication Group",
             x = "Days Since Enrollment",
             y = "Average Weight (kg)") +
        theme_minimal()




saiaMixed_filtered <- saiaMixed %>%
        filter(Age > 18, Weight.in.Kilos<140)

average_weights_over_time <- saiaMixed_filtered %>%
        group_by(Time, PriMed) %>%
        summarize(average_weight = mean(Weight.in.Kilos, na.rm = TRUE))

ggplot(data = average_weights_over_time, aes(x =Time, y = average_weight, color = PriMed)) +
        geom_line() +
        labs(title = "Average Weight of All Patients Over Time by Primary Medication",
             x = "Days Since Enrollment",
             y = "Average Weight (kg)") +
        theme_minimal()



# Calculate the average weight per medication group for each time point
average_weights_over_time <- saiaMixed_filtered %>%
        group_by(Time, PriMed) %>%
        summarize(average_weight = mean(Weight.in.Kilos, na.rm = TRUE)) %>%
        ungroup()  # Ungroup for plotting

# Plot the data with an average line for each medication group
ggplot(data = saiaMixed_filtered, aes(x = Time, y = Weight.in.Kilos, group = PriMed, color = as.factor(PriMed))) +
        geom_point(alpha = 0.5) +  # Use alpha to make points semi-transparent if there's overplotting
        geom_line(data = average_weights_over_time, aes(y = average_weight)) +
        labs(title = "Average Weight of All Patients Over Time by Primary Medication",
             x = "Days Since Enrollment",
             y = "Average Weight (kg)",
             color = "Primary Medication") +
        theme_minimal() +
        scale_color_brewer(palette = "Set1")  # Optional: for a specific color palette




















