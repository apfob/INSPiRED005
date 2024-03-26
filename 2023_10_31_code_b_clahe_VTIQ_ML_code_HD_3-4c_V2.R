#### (1) Install and load relevant packages ####
#setwd("/Users/andre/Documents/one drive/OneDrive/Forschung/2_ML/2020_VTIQ multi/ML Elastography/r")
#setwd ("D://OneDrive/Forschung/2_ML/2021_ML practical guidelines/r")


require(RTextTools)
require(tm)
require(SnowballC)
require(glmnet)
require(NLP)
require(SparseM)
require(rpart)
require(randomForest)
require(nnet)
require(e1071)
require(ROCR)
require(gmodels)
require(wordcloud)
require(dplyr)
require(methods)
require(pROC)
require(e1071)
require(rpart)
require(readxl)
require(foreach)
require(caret)
require(tensorflow)
require(Rfast) ###issue with ROC?
require(keras)
require(reticulate)
require(shapper)
require(DALEX)
require(xgboost)
require(tidyverse)
require(SHAPforxgboost)
require(lime)
require(rsample)
require(kernlab)
require(gmodels)
require(Amelia)
require(mlbench)
require(corrplot)
require(earth)
require(rpact)
require(gmodels)
require(varhandle)
require(plyr)
require(recipes)
require(doParallel)
require(MLmetrics )
require(Rmisc)
require(doParallel)
require(DALEX)
require(pdp)
require(vctrs)#install.packages("readxl")  # Install the readxl package
library(readxl)  # Load the readxl package
library(magrittr)
library(caret)
library(gmodels)

#setwd("C:/Users/andre/Documents/VTIQ ML tanja/")


#### (2) Load data  ####

db <- read.csv("split_b_clahe_main_merged.csv", stringsAsFactors = F)
db_original <- read.csv("split_b_clahe_main_merged.csv", stringsAsFactors = F)

colnames(db)
#replace >10m/s lesion meassure with 10 for lesion meassure
db$lesion [db$lesionresp==1] <- 10
#replace >10m/s lesion meassure with 10 for lesion meassure
db$lesion2 [db$lesionresp2==1] <- 10
#replace >10m/s lesion meassure with 10 for lesion meassure
db$lesion3 [db$lesionresp3==1] <- 10

#replace schallkopfüberschreitend in strain_elasto with 50
db$strainelasto [db$elastresp==1] <- 50

#replace " " in with NA
db$strainbmode [(db$strainbmode== " ")] <- NA
db$strainelasto [(db$strainelasto== " ")] <- NA
db$lesion [(db$lesion== " ")] <- NA
db$lesion2 [(db$lesion2== " ")] <- NA
db$lesion3 [(db$lesion3== " ")] <- NA


#change character to numeric / replace , by .
db$lesion <- gsub(",", '.', db$lesion)
db$lesion <- as.numeric(db$lesion)

db$lesion2 <- gsub(",", '.', db$lesion2)
db$lesion2 <- as.numeric(db$lesion2)

db$lesion3 <- gsub(",", '.', db$lesion3)
db$lesion3 <- as.numeric(db$lesion3)

db$lesiondist <- gsub(",", '.', db$lesiondist)
db$lesiondist <- as.numeric(db$lesiondist)

db$lesiondist2 <- gsub(",", '.', db$lesiondist2)
db$lesiondist2 <- as.numeric(db$lesiondist2)

db$lesiondist3 <- gsub(",", '.', db$lesiondist3)
db$lesiondist3 <- as.numeric(db$lesiondist3)

db$fat <- gsub(",", '.', db$fat)
db$fat <- as.numeric(db$fat)

db$fat2 <- gsub(",", '.', db$fat2)
db$fat2 <- as.numeric(db$fat2)

db$fat3 <- gsub(",", '.', db$fat3)
db$fat3 <- as.numeric(db$fat3)

db$fatdist <- gsub(",", '.', db$fatdist)
db$fatdist <- as.numeric(db$fatdist)

db$fatdist2 <- gsub(",", '.', db$fatdist2)
db$fatdist2 <- as.numeric(db$fatdist2)

db$fatdist3 <- gsub(",", '.', db$fatdist3)
db$fatdist3 <- as.numeric(db$fatdist3)

db$strainbmode <- gsub(",", '.', db$strainbmode)
db$strainbmode <- as.numeric(db$strainbmode)

db$strainelasto <- gsub(",", '.', db$strainelasto)
db$strainelasto <- as.numeric(db$strainelasto)


typeof(db$lesion) 
db$lesion <- as.numeric(db$lesion)




#names(db)
#db_column_names <- as.list(names(db))
#print(db_column_names)
#replacement_list <-
  
colname <-  c(  "Unnamed..0" , "VTIQ.Number", "siteno",                      "patno",                       
                 "patnumber",                    "yob",                         
                 "enrol",                        "age",                         
                 "patno_der",                    "bookvorh",                    
                 "eligvorh",                     "recstat_lesion",              
                 "palpability",                         "ultrasound_axis",                        
                 "ultrasound_perpendicular",                      "ultrasound_orthogonal",                       
                 "ultrasound_tissue",                       "ultrasound_shape",                       
                 "ultrasound_orientation",                       "ultrasound_margin",                      
                 "ultrasound_margin_indistinct",                       "ultrasound_margin_angular",                     
                 "ultrasound_margin_microlobulated",                     "ultrasound_margin_spiculated",                       
                 "echo",                         "post",                        
                 "ultrasound_calcification",                         "calcinmass",                  
                 "calcoutside",                  "intraduct",                   
                 "birads",                       "likelihood",                  
                 "lesionvorh",                   "recstat_patho",               
                 "type",                         "surgreason",                  
                 "surgreason_sp",                "outcome",                        
                 "patho1",                       "patho1_sp",                   
                 "patho2",                       "patho2_sp",                   
                 "grade",                        "er",                          
                 "pgr",                          "her2",                        
                 "ki67",                         "pathovorh",                   
                 "recstat_viol",                 "withdr",                      
                 "withdrreas",                   "withdrreas_sp",               
                 "viol",                         "viol_sp",                     
                 "violvorh",                     "viol_major",                  
                 "examinerID",                   "measureID",                   
                 "swe1_quality",                         "low",                         
                 "swe1_position",                          "swe1_lesion",                      
                 "lesionresp",                   "lesiondist",                  
                 "measurefat",                   "fat",                         
                 "fatresp",                      "fatdist",                     
                 "measureID2",                   "swe2_quality",                       
                 "low2",                         "swe2_position",                        
                 "swe2_lesion",                      "lesionresp2",                 
                 "lesiondist2",                  "measurefat2",                 
                 "fat2",                         "fatresp2",                    
                 "fatdist2",                     "measureID3",                  
                 "swe3_quality",                        "low3",                        
                 "swe3_position",                         "swe3_lesion",                     
                 "lesionresp3",                  "lesiondist3",                 
                 "measurefat3",                  "fat3",                        
                 "fatresp3",                     "fatdist3",                    
                 "examinerID2",                  "measureID4",                  
                 "qual4",                        "low4",                        
                 "pos4",                         "lesion4",                     
                 "lesionresp4",                  "lesiondist4",                 
                 "measurefat4",                  "fat4",                        
                 "fatresp4",                     "fatdist4",                    
                 "measureID5",                   "qual5",                       
                 "low5",                         "pos5",                        
                 "lesion5",                      "lesionresp5",                 
                 "lesiondist5",                  "measurefat5",                 
                 "fat5",                         "fatresp5",                    
                 "fatdist5",                     "measureID6",                  
                 "qual6",                        "low6",                        
                 "pos6",                         "lesion6",                     
                 "lesionresp6",                  "lesiondist6",                 
                 "measurefat6",                  "fat6",                        
                 "fatresp6",                     "fatdist6",                    
                 "examinerID3",                  "strainbmode",                 
                 "strainelasto",                 "elastresp",                   
                 "ratio",                        "examinerID4",                 
                 "strainmeasure2",               "strainbmode2",                
                 "strainelasto2",                "elastresp2",                  
                 "ratio2",                       "expert_name_b",               
                 "tissue_b",                     "shape_b",                     
                 "orientation_b",                "margin_b",                    
                 "margin_indistinct_b",          "margin_angular_b",            
                 "margin_microlobulated_b",      "margin_speculated_b",         
                 "echo_b",                       "posterior_b",                 
                 "calcifications_b",             "in_mass_b",                   
                 "outside_mass_b",               "intraductal_b",               
                 "birads_b",                     "likelihood_b",                
                 "expert_name_g",                "tissue_g",                    
                 "shape_g",                      "orientation_g",               
                 "margin_g",                     "margin_indistinct_g",         
                 "margin_angular_g",             "margin_microlobulated_g",     
                 "margin_speculated_g",          "echo_g",                      
                 "posterior_g",                  "calcifications_g",            
                 "in_mass_g",                    "outside_mass_g",              
                 "intraductal_g",                "birads_g",                    
                 "likelihood_g",                 "expert_name_d",               
                 "tissue_d",                     "shape_d",                     
                 "orientation_d",                "margin_d",                    
                 "margin_indistinct_d",          "margin_angular_d",            
                 "margin_microlobulated_d",      "margin_speculated_d",         
                 "echo_d",                       "posterior_d",                 
                 "calcifications_d",             "in_mass_d",                   
                 "outside_mass_d",               "intraductal_d",               
                 "birads_d",                     "likelihood_d",                
                 "final_birads",                 "final_likelihood",            
                 "FAS",                          "final_likelihood_mean",       
                 "final_likelihood_median",      "vtiq_meas_1",                 
                 "vtiq_meas_2",                  "vtiq_meas_3",                 
                 "vtiq_meas_4",                  "vtiq_meas_5",                 
                 "vtiq_meas_6",                  "QM1",                         
                 "QM2",                          "QM3",                         
                 "QM4",                          "QM5",                         
                 "QM6",                          "vtiq_QM_1",                   
                 "vtiq_QM_2",                    "vtiq_QM_3",                   
                 "vtiq_QM_4",                    "vtiq_QM_5",                   
                 "vtiq_QM_6",                    "fatdist_QM_1",                
                 "fatdist_QM_2",                 "fatdist_QM_3",                
                 "vtiq_fat_ratio_QM_1",          "vtiq_fat_ratio_QM_2",         
                 "vtiq_fat_ratio_QM_3",          "qual_QM_1",                   
                 "qual_QM_2",                    "qual_QM_3",                   
                 "vtiq_ex1_mean",                "vtiq_ex1_mean_1_2",           
                 "vtiq_fat_ratio_ex1_mean",      "vtiq_ex1_QM_first",           
                 "vtiq_ex1_QM_1_2",              "vtiq_ex1_QM_mean",            
                 "vtiq_ex1_QM_median",           "vtiq_ex1_QM_max",             
                 "vtiq_ex2_QM_first",            "vtiq_ex2_QM_mean",            
                 "vtiq_ex2_QM_median",           "vtiq_ex2_QM_max",             
                 "fatdist_ex1_QM_first",         "fatdist_ex1_QM_mean",         
                 "fatdist_ex1_QM_median",        "fatdist_ex1_QM_max",          
                 "vtiq_fat_ratio_ex1_QM_first",  "vtiq_fat_ratio_ex1_QM_mean",  
                 "vtiq_fat_ratio_ex1_QM_median", "vtiq_fat_ratio_ex1_QM_max",   
                 "qual_ex1_QM_first",            "maxcount",                    
                 "X._i",                         "qual_ex1_QM_mean",            
                 "qual_ex1_QM_median",           "qual_ex1_QM_max",             
                 "final_birads_num",             "birads_b_num",                
                 "birads_g_num",                 "birads_d_num",                
                 "tissue_b_num",                 "shape_b_num",                 
                 "orientation_b_num",            "margin_b_num",                
                 "margin_indistinct_b_num",      "margin_angular_b_num",        
                 "margin_microlobulated_b_num",  "margin_speculated_b_num",     
                 "echo_b_num",                   "posterior_b_num",             
                 "calcifications_b_num",         "in_mass_b_num",               
                 "outside_mass_b_num",           "intraductal_b_num",           
                 "tissue_d_num",                 "shape_d_num",                 
                 "orientation_d_num",            "margin_d_num",                
                 "margin_indistinct_d_num",      "margin_angular_d_num",        
                 "margin_microlobulated_d_num",  "margin_speculated_d_num",     
                 "echo_d_num",                   "posterior_d_num",             
                 "calcifications_d_num",         "in_mass_d_num",               
                 "outside_mass_d_num",           "intraductal_d_num",           
                 "tissue_g_num",                 "shape_g_num",                 
                 "orientation_g_num",            "margin_g_num",                
                 "margin_indistinct_g_num",      "margin_angular_g_num",        
                 "margin_microlobulated_g_num",  "margin_speculated_g_num",     
                 "echo_g_num",                   "posterior_g_num",             
                 "calcifications_g_num",         "in_mass_g_num",               
                 "outside_mass_g_num",           "intraductal_g_num",           
                 "flag",                         "QM_bad",                      
                 "QM_all3bad",                   "strain", 
                
                
                "diagnostics_Configuration_Settings_B_clahe_TPS",
                "diagnostics_Configuration_EnabledImageTypes_B_clahe_TPS",
                "diagnostics_Image.original_Hash_B_clahe_TPS",
                "diagnostics_Image.original_Dimensionality_B_clahe_TPS",
                "diagnostics_Image.original_Spacing_B_clahe_TPS",
                "diagnostics_Image.original_Size_B_clahe_TPS",
                "diagnostics_Image.original_Mean_B_clahe_TPS",
                "diagnostics_Image.original_Minimum_B_clahe_TPS",
                "diagnostics_Image.original_Maximum_B_clahe_TPS",
                "diagnostics_Mask.original_Hash_B_clahe_TPS",
                "diagnostics_Mask.original_Spacing_B_clahe_TPS",
                "diagnostics_Mask.original_Size_B_clahe_TPS",
                "diagnostics_Mask.original_BoundingBox_B_clahe_TPS",
                "diagnostics_Mask.original_VoxelNum_B_clahe_TPS",
                "diagnostics_Mask.original_VolumeNum_B_clahe_TPS",
                "diagnostics_Mask.original_CenterOfMassIndex_B_clahe_TPS",
                "diagnostics_Mask.original_CenterOfMass_B_clahe_TPS",
                "diagnostics_Image.interpolated_Spacing_B_clahe_TPS",
                "diagnostics_Image.interpolated_Size_B_clahe_TPS",
                "diagnostics_Image.interpolated_Mean_B_clahe_TPS",
                "diagnostics_Image.interpolated_Minimum_B_clahe_TPS",
                "diagnostics_Image.interpolated_Maximum_B_clahe_TPS",
                "diagnostics_Mask.interpolated_Spacing_B_clahe_TPS",
                "diagnostics_Mask.interpolated_Size_B_clahe_TPS",
                "diagnostics_Mask.interpolated_BoundingBox_B_clahe_TPS",
                "diagnostics_Mask.interpolated_VoxelNum_B_clahe_TPS",
                "diagnostics_Mask.interpolated_VolumeNum_B_clahe_TPS",
                "diagnostics_Mask.interpolated_CenterOfMassIndex_B_clahe_TPS",
                "diagnostics_Mask.interpolated_CenterOfMass_B_clahe_TPS",
                "diagnostics_Mask.interpolated_Mean_B_clahe_TPS",
                "diagnostics_Mask.interpolated_Minimum_B_clahe_TPS",
                "diagnostics_Mask.interpolated_Maximum_B_clahe_TPS",
                "original_shape_Elongation_B_clahe_TPS",
                "original_shape_Flatness_B_clahe_TPS",
                "original_shape_LeastAxisLength_B_clahe_TPS",
                "original_shape_MajorAxisLength_B_clahe_TPS",
                "original_shape_Maximum2DDiameterColumn_B_clahe_TPS",
                "original_shape_Maximum2DDiameterRow_B_clahe_TPS",
                "original_shape_Maximum2DDiameterSlice_B_clahe_TPS",
                "original_shape_Maximum3DDiameter_B_clahe_TPS",
                "original_shape_MeshVolume_B_clahe_TPS",
                "original_shape_MinorAxisLength_B_clahe_TPS",
                "original_shape_Sphericity_B_clahe_TPS",
                "original_shape_SurfaceArea_B_clahe_TPS",
                "original_shape_SurfaceVolumeRatio_B_clahe_TPS",
                "original_shape_VoxelVolume_B_clahe_TPS",
                "original_firstorder_10Percentile_B_clahe_TPS",
                "original_firstorder_90Percentile_B_clahe_TPS",
                "original_firstorder_Energy_B_clahe_TPS",
                "original_firstorder_Entropy_B_clahe_TPS",
                "original_firstorder_InterquartileRange_B_clahe_TPS",
                "original_firstorder_Kurtosis_B_clahe_TPS",
                "original_firstorder_Maximum_B_clahe_TPS",
                "original_firstorder_MeanAbsoluteDeviation_B_clahe_TPS",
                "original_firstorder_Mean_B_clahe_TPS",
                "original_firstorder_Median_B_clahe_TPS",
                "original_firstorder_Minimum_B_clahe_TPS",
                "original_firstorder_Range_B_clahe_TPS",
                "original_firstorder_RobustMeanAbsoluteDeviation_B_clahe_TPS",
                "original_firstorder_RootMeanSquared_B_clahe_TPS",
                "original_firstorder_Skewness_B_clahe_TPS",
                "original_firstorder_TotalEnergy_B_clahe_TPS",
                "original_firstorder_Uniformity_B_clahe_TPS",
                "original_firstorder_Variance_B_clahe_TPS",
                "original_glcm_Autocorrelation_B_clahe_TPS",
                "original_glcm_ClusterProminence_B_clahe_TPS",
                "original_glcm_ClusterShade_B_clahe_TPS",
                "original_glcm_ClusterTendency_B_clahe_TPS",
                "original_glcm_Contrast_B_clahe_TPS",
                "original_glcm_Correlation_B_clahe_TPS",
                "original_glcm_DifferenceAverage_B_clahe_TPS",
                "original_glcm_DifferenceEntropy_B_clahe_TPS",
                "original_glcm_DifferenceVariance_B_clahe_TPS",
                "original_glcm_Id_B_clahe_TPS",
                "original_glcm_Idm_B_clahe_TPS",
                "original_glcm_Idmn_B_clahe_TPS",
                "original_glcm_Idn_B_clahe_TPS",
                "original_glcm_Imc1_B_clahe_TPS",
                "original_glcm_Imc2_B_clahe_TPS",
                "original_glcm_InverseVariance_B_clahe_TPS",
                "original_glcm_JointAverage_B_clahe_TPS",
                "original_glcm_JointEnergy_B_clahe_TPS",
                "original_glcm_JointEntropy_B_clahe_TPS",
                "original_glcm_MCC_B_clahe_TPS",
                "original_glcm_MaximumProbability_B_clahe_TPS",
                "original_glcm_SumAverage_B_clahe_TPS",
                "original_glcm_SumEntropy_B_clahe_TPS",
                "original_glcm_SumSquares_B_clahe_TPS",
                "original_gldm_DependenceEntropy_B_clahe_TPS",
                "original_gldm_DependenceNonUniformity_B_clahe_TPS",
                "original_gldm_DependenceNonUniformityNormalized_B_clahe_TPS",
                "original_gldm_DependenceVariance_B_clahe_TPS",
                "original_gldm_GrayLevelNonUniformity_B_clahe_TPS",
                "original_gldm_GrayLevelVariance_B_clahe_TPS",
                "original_gldm_HighGrayLevelEmphasis_B_clahe_TPS",
                "original_gldm_LargeDependenceEmphasis_B_clahe_TPS",
                "original_gldm_LargeDependenceHighGrayLevelEmphasis_B_clahe_TPS",
                "original_gldm_LargeDependenceLowGrayLevelEmphasis_B_clahe_TPS",
                "original_gldm_LowGrayLevelEmphasis_B_clahe_TPS",
                "original_gldm_SmallDependenceEmphasis_B_clahe_TPS",
                "original_gldm_SmallDependenceHighGrayLevelEmphasis_B_clahe_TPS",
                "original_gldm_SmallDependenceLowGrayLevelEmphasis_B_clahe_TPS",
                "original_glrlm_GrayLevelNonUniformity_B_clahe_TPS",
                "original_glrlm_GrayLevelNonUniformityNormalized_B_clahe_TPS",
                "original_glrlm_GrayLevelVariance_B_clahe_TPS",
                "original_glrlm_HighGrayLevelRunEmphasis_B_clahe_TPS",
                "original_glrlm_LongRunEmphasis_B_clahe_TPS",
                "original_glrlm_LongRunHighGrayLevelEmphasis_B_clahe_TPS",
                "original_glrlm_LongRunLowGrayLevelEmphasis_B_clahe_TPS",
                "original_glrlm_LowGrayLevelRunEmphasis_B_clahe_TPS",
                "original_glrlm_RunEntropy_B_clahe_TPS",
                "original_glrlm_RunLengthNonUniformity_B_clahe_TPS",
                "original_glrlm_RunLengthNonUniformityNormalized_B_clahe_TPS",
                "original_glrlm_RunPercentage_B_clahe_TPS",
                "original_glrlm_RunVariance_B_clahe_TPS",
                "original_glrlm_ShortRunEmphasis_B_clahe_TPS",
                "original_glrlm_ShortRunHighGrayLevelEmphasis_B_clahe_TPS",
                "original_glrlm_ShortRunLowGrayLevelEmphasis_B_clahe_TPS",
                "original_glszm_GrayLevelNonUniformity_B_clahe_TPS",
                "original_glszm_GrayLevelNonUniformityNormalized_B_clahe_TPS",
                "original_glszm_GrayLevelVariance_B_clahe_TPS",
                "original_glszm_HighGrayLevelZoneEmphasis_B_clahe_TPS",
                "original_glszm_LargeAreaEmphasis_B_clahe_TPS",
                "original_glszm_LargeAreaHighGrayLevelEmphasis_B_clahe_TPS",
                "original_glszm_LargeAreaLowGrayLevelEmphasis_B_clahe_TPS",
                "original_glszm_LowGrayLevelZoneEmphasis_B_clahe_TPS",
                "original_glszm_SizeZoneNonUniformity_B_clahe_TPS",
                "original_glszm_SizeZoneNonUniformityNormalized_B_clahe_TPS",
                "original_glszm_SmallAreaEmphasis_B_clahe_TPS",
                "original_glszm_SmallAreaHighGrayLevelEmphasis_B_clahe_TPS",
                "original_glszm_SmallAreaLowGrayLevelEmphasis_B_clahe_TPS",
                "original_glszm_ZoneEntropy_B_clahe_TPS",
                "original_glszm_ZonePercentage_B_clahe_TPS",
                "original_glszm_ZoneVariance_B_clahe_TPS",
                "original_ngtdm_Busyness_B_clahe_TPS",
                "original_ngtdm_Coarseness_B_clahe_TPS",
                "original_ngtdm_Complexity_B_clahe_TPS",
                "original_ngtdm_Contrast_B_clahe_TPS",
                "original_ngtdm_Strength_B_clahe_TPS", "Name_B_clahe_TPS" )      




colnames(db)<-colname
summary(db)

#### (3) define variable types ####

sapply(db, class)

colnames(db)

#relevel the categorical variables
db$palpability <- as.factor(ifelse(db$palpability=="0", "no","yes"))
levels(db$palpability)
db$palpability <- relevel(db$palpability, ref = "no")
#defines reference als 0 for no increased risk
levels(db$palpability)

db$ultrasound_tissue <- as.factor(ifelse(db$ultrasound_tissue=="1", "homogeneous_fat", 
                                            ifelse(db$ultrasound_tissue=="2", "homogeneous_fibroglandular", "heterogeneous")))
levels(db$ultrasound_tissue)
db$ultrasound_tissue <- relevel(db$ultrasound_tissue, ref = "homogeneous_fat")
levels(db$ultrasound_tissue)

db$ultrasound_shape <- as.factor(ifelse(db$ultrasound_shape=="1", "oval", 
                                         ifelse(db$ultrasound_shape=="2", "round", "irregular")))
levels(db$ultrasound_shape)
db$ultrasound_shape <- relevel(db$ultrasound_shape, ref = "oval")
levels(db$ultrasound_shape)

db$ultrasound_orientation <- as.factor(ifelse(db$ultrasound_orientation=="1", "parallel","not_parallel"))
levels(db$ultrasound_orientation)
db$ultrasound_orientation <- relevel(db$ultrasound_orientation, ref = "parallel")
levels(db$ultrasound_orientation)

db$ultrasound_margin <- as.factor(ifelse(db$ultrasound_margin=="1", "cicumscribed","not_circumscribed"))
levels(db$ultrasound_margin)
db$ultrasound_margin <- relevel(db$ultrasound_margin, ref = "cicumscribed")
levels(db$ultrasound_margin)

db$ultrasound_margin_indistinct <- as.factor(ifelse(db$ultrasound_margin_indistinct=="1", "indistinct","not_indistinct"))
levels(db$ultrasound_margin_indistinct)
db$ultrasound_margin_indistinct <- relevel(db$ultrasound_margin_indistinct, ref = "not_indistinct")
levels(db$ultrasound_margin_indistinct)

db$ultrasound_margin_angular <- as.factor(ifelse(db$ultrasound_margin_angular=="1", "angular","not_angular"))
levels(db$ultrasound_margin_angular)
db$ultrasound_margin_angular <- relevel(db$ultrasound_margin_angular, ref = "not_angular")
levels(db$ultrasound_margin_angular)

db$ultrasound_margin_spiculated <- as.factor(ifelse(db$ultrasound_margin_spiculated=="1", "spiculated","not_spiculated"))
levels(db$ultrasound_margin_spiculated)
db$ultrasound_margin_spiculated <- relevel(db$ultrasound_margin_spiculated, ref = "not_spiculated")
levels(db$ultrasound_margin_spiculated)

db$ultrasound_margin_microlobulated <- as.factor(ifelse(db$ultrasound_margin_microlobulated=="1", "microlobulated","not_microlobulated"))
levels(db$ultrasound_margin_microlobulated)
db$ultrasound_margin_microlobulated <- relevel(db$ultrasound_margin_microlobulated, ref = "not_microlobulated")
levels(db$ultrasound_margin_microlobulated)

db$ultrasound_calcification<- as.factor(ifelse(db$ultrasound_calcification=="1", "yes","no"))
levels(db$ultrasound_calcification)
db$ultrasound_calcification<- relevel(db$ultrasound_calcification, ref = "no")
levels(db$ultrasound_calcification)



db$swe1_quality <- as.factor(ifelse(db$swe1_quality=="1", "high", 
                                        ifelse(db$swe1_quality=="2", "intermediate", "low")))
levels(db$swe1_quality)
db$swe1_quality <- relevel(db$swe1_quality, ref = "high")
levels(db$swe1_quality)

db$swe2_quality <- as.factor(ifelse(db$swe2_quality=="1", "high", 
                                    ifelse(db$swe2_quality=="2", "intermediate", "low")))
levels(db$swe2_quality)
db$swe2_quality <- relevel(db$swe2_quality, ref = "high")
levels(db$swe2_quality)

db$swe3_quality <- as.factor(ifelse(db$swe3_quality=="1", "high", 
                                    ifelse(db$swe3_quality=="2", "intermediate", "low")))
levels(db$swe3_quality)
db$swe3_quality <- relevel(db$swe3_quality, ref = "high")
levels(db$swe3_quality)

db$swe1_position <- as.factor(ifelse(db$swe1_position=="1", "in_lesion","rim"))
levels(db$swe1_position)
db$swe1_position <- relevel(db$swe1_position, ref = "in_lesion")
levels(db$swe1_position)

db$swe2_position <- as.factor(ifelse(db$swe2_position=="1", "in_lesion","rim"))
levels(db$swe2_position)
db$swe2_position <- relevel(db$swe2_position, ref = "in_lesion")
levels(db$swe2_position)

db$swe3_position <- as.factor(ifelse(db$swe3_position=="1", "in_lesion","rim"))
levels(db$swe3_position)
db$swe3_position <- relevel(db$swe3_position, ref = "in_lesion")
levels(db$swe3_position)


db$outcome<-as.factor(ifelse(db$outcome==1, "Benign", "Malignant"))
levels(db$outcome) #caret uses the first level of the outcome variable as reference during the training process - so first level should be the positive outcome 
db$outcome <- factor(db$outcome, levels=rev(levels(db$outcome)))
levels(db$outcome)
#Malignant als Outcome-Variable, daher anders als Predictive Variables.


#make numeric variables numeric
db$age<-as.numeric(db$age)
db$ultrasound_axis<-as.numeric(db$ultrasound_axis)
db$ultrasound_perpendicular<-as.numeric(db$ultrasound_perpendicular)
db$ultrasound_orthogonal<-as.numeric(db$ultrasound_orthogonal)
db$swe1_lesion<-as.numeric(db$swe1_lesion)
db$swe2_lesion<-as.numeric(db$swe2_lesion)
db$swe3_lesion<-as.numeric(db$swe3_lesion)

##radiomics features
db$diagnostics_Image.original_Mean_B_clahe_TPS<-as.numeric(db$diagnostics_Image.original_Mean_B_clahe_TPS)
db$diagnostics_Image.original_Minimum_B_clahe_TPS<-as.numeric(db$diagnostics_Image.original_Minimum_B_clahe_TPS)
db$diagnostics_Image.original_Maximum_B_clahe_TPS<-as.numeric(db$diagnostics_Image.original_Maximum_B_clahe_TPS)
db$diagnostics_Mask.original_VoxelNum_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_VoxelNum_B_clahe_TPS)
db$diagnostics_Mask.original_VolumeNum_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_VolumeNum_B_clahe_TPS)
db$diagnostics_Image.interpolated_Mean_B_clahe_TPS<-as.numeric(db$diagnostics_Image.interpolated_Mean_B_clahe_TPS)
db$diagnostics_Image.interpolated_Minimum_B_clahe_TPS<-as.numeric(db$diagnostics_Image.interpolated_Minimum_B_clahe_TPS)
db$diagnostics_Image.interpolated_Maximum_B_clahe_TPS<-as.numeric(db$diagnostics_Image.interpolated_Maximum_B_clahe_TPS)
db$diagnostics_Mask.interpolated_Spacing_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_Spacing_B_clahe_TPS)
db$diagnostics_Mask.interpolated_Size_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_Size_B_clahe_TPS)
db$diagnostics_Mask.interpolated_BoundingBox_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_BoundingBox_B_clahe_TPS)
db$diagnostics_Mask.interpolated_VoxelNum_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_VoxelNum_B_clahe_TPS)
db$diagnostics_Mask.interpolated_VolumeNum_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_VolumeNum_B_clahe_TPS)
db$diagnostics_Mask.interpolated_CenterOfMassIndex_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_CenterOfMassIndex_B_clahe_TPS)
db$diagnostics_Mask.interpolated_CenterOfMass_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_CenterOfMass_B_clahe_TPS)
db$diagnostics_Mask.interpolated_Mean_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_Mean_B_clahe_TPS)
db$diagnostics_Mask.interpolated_Minimum_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_Minimum_B_clahe_TPS)
db$diagnostics_Mask.interpolated_Maximum_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.interpolated_Maximum_B_clahe_TPS)
db$original_shape_Elongation_B_clahe_TPS<-as.numeric(db$original_shape_Elongation_B_clahe_TPS)
db$original_shape_Flatness_B_clahe_TPS<-as.numeric(db$original_shape_Flatness_B_clahe_TPS)
db$original_shape_LeastAxisLength_B_clahe_TPS<-as.numeric(db$original_shape_LeastAxisLength_B_clahe_TPS)
db$original_shape_MajorAxisLength_B_clahe_TPS<-as.numeric(db$original_shape_MajorAxisLength_B_clahe_TPS)
db$original_shape_Maximum2DDiameterColumn_B_clahe_TPS<-as.numeric(db$original_shape_Maximum2DDiameterColumn_B_clahe_TPS)
db$original_shape_Maximum2DDiameterRow_B_clahe_TPS<-as.numeric(db$original_shape_Maximum2DDiameterRow_B_clahe_TPS)
db$original_shape_Maximum2DDiameterSlice_B_clahe_TPS<-as.numeric(db$original_shape_Maximum2DDiameterSlice_B_clahe_TPS)
db$original_shape_Maximum3DDiameter_B_clahe_TPS<-as.numeric(db$original_shape_Maximum3DDiameter_B_clahe_TPS)
db$original_shape_MeshVolume_B_clahe_TPS<-as.numeric(db$original_shape_MeshVolume_B_clahe_TPS)
db$original_shape_MinorAxisLength_B_clahe_TPS<-as.numeric(db$original_shape_MinorAxisLength_B_clahe_TPS)
db$original_shape_Sphericity_B_clahe_TPS<-as.numeric(db$original_shape_Sphericity_B_clahe_TPS)
db$original_shape_SurfaceArea_B_clahe_TPS<-as.numeric(db$original_shape_SurfaceArea_B_clahe_TPS)
db$original_shape_SurfaceVolumeRatio_B_clahe_TPS<-as.numeric(db$original_shape_SurfaceVolumeRatio_B_clahe_TPS)
db$original_shape_VoxelVolume_B_clahe_TPS<-as.numeric(db$original_shape_VoxelVolume_B_clahe_TPS)
db$original_firstorder_10Percentile_B_clahe_TPS<-as.numeric(db$original_firstorder_10Percentile_B_clahe_TPS)
db$original_firstorder_90Percentile_B_clahe_TPS<-as.numeric(db$original_firstorder_90Percentile_B_clahe_TPS)
db$original_firstorder_Energy_B_clahe_TPS<-as.numeric(db$original_firstorder_Energy_B_clahe_TPS)
db$original_firstorder_Entropy_B_clahe_TPS<-as.numeric(db$original_firstorder_Entropy_B_clahe_TPS)
db$original_firstorder_InterquartileRange_B_clahe_TPS<-as.numeric(db$original_firstorder_InterquartileRange_B_clahe_TPS)
db$original_firstorder_Kurtosis_B_clahe_TPS<-as.numeric(db$original_firstorder_Kurtosis_B_clahe_TPS)
db$original_firstorder_Maximum_B_clahe_TPS<-as.numeric(db$original_firstorder_Maximum_B_clahe_TPS)
db$original_firstorder_MeanAbsoluteDeviation_B_clahe_TPS<-as.numeric(db$original_firstorder_MeanAbsoluteDeviation_B_clahe_TPS)
db$original_firstorder_Mean_B_clahe_TPS<-as.numeric(db$original_firstorder_Mean_B_clahe_TPS)
db$original_firstorder_Median_B_clahe_TPS<-as.numeric(db$original_firstorder_Median_B_clahe_TPS)
db$original_firstorder_Minimum_B_clahe_TPS<-as.numeric(db$original_firstorder_Minimum_B_clahe_TPS)
db$original_firstorder_Range_B_clahe_TPS<-as.numeric(db$original_firstorder_Range_B_clahe_TPS)
db$original_firstorder_RobustMeanAbsoluteDeviation_B_clahe_TPS<-as.numeric(db$original_firstorder_RobustMeanAbsoluteDeviation_B_clahe_TPS)
db$original_firstorder_RootMeanSquared_B_clahe_TPS<-as.numeric(db$original_firstorder_RootMeanSquared_B_clahe_TPS)
db$original_firstorder_Skewness_B_clahe_TPS<-as.numeric(db$original_firstorder_Skewness_B_clahe_TPS)
db$original_firstorder_TotalEnergy_B_clahe_TPS<-as.numeric(db$original_firstorder_TotalEnergy_B_clahe_TPS)
db$original_firstorder_Uniformity_B_clahe_TPS<-as.numeric(db$original_firstorder_Uniformity_B_clahe_TPS)
db$original_firstorder_Variance_B_clahe_TPS<-as.numeric(db$original_firstorder_Variance_B_clahe_TPS)
db$original_glcm_Autocorrelation_B_clahe_TPS<-as.numeric(db$original_glcm_Autocorrelation_B_clahe_TPS)
db$original_glcm_ClusterProminence_B_clahe_TPS<-as.numeric(db$original_glcm_ClusterProminence_B_clahe_TPS)
db$original_glcm_ClusterShade_B_clahe_TPS<-as.numeric(db$original_glcm_ClusterShade_B_clahe_TPS)
db$original_glcm_ClusterTendency_B_clahe_TPS<-as.numeric(db$original_glcm_ClusterTendency_B_clahe_TPS)
db$original_glcm_Contrast_B_clahe_TPS<-as.numeric(db$original_glcm_Contrast_B_clahe_TPS)
db$original_glcm_Correlation_B_clahe_TPS<-as.numeric(db$original_glcm_Correlation_B_clahe_TPS)
db$original_glcm_DifferenceAverage_B_clahe_TPS<-as.numeric(db$original_glcm_DifferenceAverage_B_clahe_TPS)
db$original_glcm_DifferenceEntropy_B_clahe_TPS<-as.numeric(db$original_glcm_DifferenceEntropy_B_clahe_TPS)
db$original_glcm_DifferenceVariance_B_clahe_TPS<-as.numeric(db$original_glcm_DifferenceVariance_B_clahe_TPS)
db$original_glcm_Id_B_clahe_TPS<-as.numeric(db$original_glcm_Id_B_clahe_TPS)
db$original_glcm_Idm_B_clahe_TPS<-as.numeric(db$original_glcm_Idm_B_clahe_TPS)
db$original_glcm_Idmn_B_clahe_TPS<-as.numeric(db$original_glcm_Idmn_B_clahe_TPS)
db$original_glcm_Idn_B_clahe_TPS<-as.numeric(db$original_glcm_Idn_B_clahe_TPS)
db$original_glcm_Imc1_B_clahe_TPS<-as.numeric(db$original_glcm_Imc1_B_clahe_TPS)
db$original_glcm_Imc2_B_clahe_TPS<-as.numeric(db$original_glcm_Imc2_B_clahe_TPS)
db$original_glcm_InverseVariance_B_clahe_TPS<-as.numeric(db$original_glcm_InverseVariance_B_clahe_TPS)
db$original_glcm_JointAverage_B_clahe_TPS<-as.numeric(db$original_glcm_JointAverage_B_clahe_TPS)
db$original_glcm_JointEnergy_B_clahe_TPS<-as.numeric(db$original_glcm_JointEnergy_B_clahe_TPS)
db$original_glcm_JointEntropy_B_clahe_TPS<-as.numeric(db$original_glcm_JointEntropy_B_clahe_TPS)
db$original_glcm_MCC_B_clahe_TPS<-as.numeric(db$original_glcm_MCC_B_clahe_TPS)
db$original_glcm_MaximumProbability_B_clahe_TPS<-as.numeric(db$original_glcm_MaximumProbability_B_clahe_TPS)
db$original_glcm_SumAverage_B_clahe_TPS<-as.numeric(db$original_glcm_SumAverage_B_clahe_TPS)
db$original_glcm_SumEntropy_B_clahe_TPS<-as.numeric(db$original_glcm_SumEntropy_B_clahe_TPS)
db$original_glcm_SumSquares_B_clahe_TPS<-as.numeric(db$original_glcm_SumSquares_B_clahe_TPS)
db$original_gldm_DependenceEntropy_B_clahe_TPS<-as.numeric(db$original_gldm_DependenceEntropy_B_clahe_TPS)
db$original_gldm_DependenceNonUniformity_B_clahe_TPS<-as.numeric(db$original_gldm_DependenceNonUniformity_B_clahe_TPS)
db$original_gldm_DependenceNonUniformityNormalized_B_clahe_TPS<-as.numeric(db$original_gldm_DependenceNonUniformityNormalized_B_clahe_TPS)
db$original_gldm_DependenceVariance_B_clahe_TPS<-as.numeric(db$original_gldm_DependenceVariance_B_clahe_TPS)
db$original_gldm_GrayLevelNonUniformity_B_clahe_TPS<-as.numeric(db$original_gldm_GrayLevelNonUniformity_B_clahe_TPS)
db$original_gldm_GrayLevelVariance_B_clahe_TPS<-as.numeric(db$original_gldm_GrayLevelVariance_B_clahe_TPS)
db$original_gldm_HighGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_HighGrayLevelEmphasis_B_clahe_TPS)
db$original_gldm_LargeDependenceEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_LargeDependenceEmphasis_B_clahe_TPS)
db$original_gldm_LargeDependenceHighGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_LargeDependenceHighGrayLevelEmphasis_B_clahe_TPS)
db$original_gldm_LargeDependenceLowGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_LargeDependenceLowGrayLevelEmphasis_B_clahe_TPS)
db$original_gldm_LowGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_LowGrayLevelEmphasis_B_clahe_TPS)
db$original_gldm_SmallDependenceEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_SmallDependenceEmphasis_B_clahe_TPS)
db$original_gldm_SmallDependenceHighGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_SmallDependenceHighGrayLevelEmphasis_B_clahe_TPS)
db$original_gldm_SmallDependenceLowGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_gldm_SmallDependenceLowGrayLevelEmphasis_B_clahe_TPS)
db$original_glrlm_GrayLevelNonUniformity_B_clahe_TPS<-as.numeric(db$original_glrlm_GrayLevelNonUniformity_B_clahe_TPS)
db$original_glrlm_GrayLevelNonUniformityNormalized_B_clahe_TPS<-as.numeric(db$original_glrlm_GrayLevelNonUniformityNormalized_B_clahe_TPS)
db$original_glrlm_GrayLevelVariance_B_clahe_TPS<-as.numeric(db$original_glrlm_GrayLevelVariance_B_clahe_TPS)
db$original_glrlm_HighGrayLevelRunEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_HighGrayLevelRunEmphasis_B_clahe_TPS)
db$original_glrlm_LongRunEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_LongRunEmphasis_B_clahe_TPS)
db$original_glrlm_LongRunHighGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_LongRunHighGrayLevelEmphasis_B_clahe_TPS)
db$original_glrlm_LongRunLowGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_LongRunLowGrayLevelEmphasis_B_clahe_TPS)
db$original_glrlm_LowGrayLevelRunEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_LowGrayLevelRunEmphasis_B_clahe_TPS)
db$original_glrlm_RunEntropy_B_clahe_TPS<-as.numeric(db$original_glrlm_RunEntropy_B_clahe_TPS)
db$original_glrlm_RunLengthNonUniformity_B_clahe_TPS<-as.numeric(db$original_glrlm_RunLengthNonUniformity_B_clahe_TPS)
db$original_glrlm_RunLengthNonUniformityNormalized_B_clahe_TPS<-as.numeric(db$original_glrlm_RunLengthNonUniformityNormalized_B_clahe_TPS)
db$original_glrlm_RunPercentage_B_clahe_TPS<-as.numeric(db$original_glrlm_RunPercentage_B_clahe_TPS)
db$original_glrlm_RunVariance_B_clahe_TPS<-as.numeric(db$original_glrlm_RunVariance_B_clahe_TPS)
db$original_glrlm_ShortRunEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_ShortRunEmphasis_B_clahe_TPS)
db$original_glrlm_ShortRunHighGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_ShortRunHighGrayLevelEmphasis_B_clahe_TPS)
db$original_glrlm_ShortRunLowGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glrlm_ShortRunLowGrayLevelEmphasis_B_clahe_TPS)
db$original_glszm_GrayLevelNonUniformity_B_clahe_TPS<-as.numeric(db$original_glszm_GrayLevelNonUniformity_B_clahe_TPS)
db$original_glszm_GrayLevelNonUniformityNormalized_B_clahe_TPS<-as.numeric(db$original_glszm_GrayLevelNonUniformityNormalized_B_clahe_TPS)
db$original_glszm_GrayLevelVariance_B_clahe_TPS<-as.numeric(db$original_glszm_GrayLevelVariance_B_clahe_TPS)
db$original_glszm_HighGrayLevelZoneEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_HighGrayLevelZoneEmphasis_B_clahe_TPS)
db$original_glszm_LargeAreaEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_LargeAreaEmphasis_B_clahe_TPS)
db$original_glszm_LargeAreaHighGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_LargeAreaHighGrayLevelEmphasis_B_clahe_TPS)
db$original_glszm_LargeAreaLowGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_LargeAreaLowGrayLevelEmphasis_B_clahe_TPS)
db$original_glszm_LowGrayLevelZoneEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_LowGrayLevelZoneEmphasis_B_clahe_TPS)
db$original_glszm_SizeZoneNonUniformity_B_clahe_TPS<-as.numeric(db$original_glszm_SizeZoneNonUniformity_B_clahe_TPS)
db$original_glszm_SizeZoneNonUniformityNormalized_B_clahe_TPS<-as.numeric(db$original_glszm_SizeZoneNonUniformityNormalized_B_clahe_TPS)
db$original_glszm_SmallAreaEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_SmallAreaEmphasis_B_clahe_TPS)
db$original_glszm_SmallAreaHighGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_SmallAreaHighGrayLevelEmphasis_B_clahe_TPS)
db$original_glszm_SmallAreaLowGrayLevelEmphasis_B_clahe_TPS<-as.numeric(db$original_glszm_SmallAreaLowGrayLevelEmphasis_B_clahe_TPS)
db$original_glszm_ZoneEntropy_B_clahe_TPS<-as.numeric(db$original_glszm_ZoneEntropy_B_clahe_TPS)
db$original_glszm_ZonePercentage_B_clahe_TPS<-as.numeric(db$original_glszm_ZonePercentage_B_clahe_TPS)
db$original_glszm_ZoneVariance_B_clahe_TPS<-as.numeric(db$original_glszm_ZoneVariance_B_clahe_TPS)
db$original_ngtdm_Busyness_B_clahe_TPS<-as.numeric(db$original_ngtdm_Busyness_B_clahe_TPS)
db$original_ngtdm_Coarseness_B_clahe_TPS<-as.numeric(db$original_ngtdm_Coarseness_B_clahe_TPS)
db$original_ngtdm_Complexity_B_clahe_TPS<-as.numeric(db$original_ngtdm_Complexity_B_clahe_TPS)
db$original_ngtdm_Contrast_B_clahe_TPS<-as.numeric(db$original_ngtdm_Contrast_B_clahe_TPS)
db$original_ngtdm_Strength_B_clahe_TPS<-as.numeric(db$original_ngtdm_Strength_B_clahe_TPS)



#with NA values
#db$diagnostics_Configuration_Settings_B_clahe_TPS<-as.numeric(db$diagnostics_Configuration_Settings_B_clahe_TPS)
#db$diagnostics_Configuration_EnabledImageTypes_B_clahe_TPS<-as.numeric(db$diagnostics_Configuration_EnabledImageTypes_B_clahe_TPS)
#db$diagnostics_Image.original_Hash_B_clahe_TPS<-as.numeric(db$diagnostics_Image.original_Hash_B_clahe_TPS)
#db$diagnostics_Image.original_Dimensionality_B_clahe_TPS<-as.numeric(db$diagnostics_Image.original_Dimensionality_B_clahe_TPS)
#db$diagnostics_Image.original_Spacing_B_clahe_TPS<-as.numeric(db$diagnostics_Image.original_Spacing_B_clahe_TPS)
#db$diagnostics_Image.original_Size_B_clahe_TPS<-as.numeric(db$diagnostics_Image.original_Size_B_clahe_TPS)
#db$diagnostics_Mask.original_Hash_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_Hash_B_clahe_TPS)
#db$diagnostics_Mask.original_Spacing_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_Spacing_B_clahe_TPS)
#db$diagnostics_Mask.original_Size_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_Size_B_clahe_TPS)
#db$diagnostics_Mask.original_BoundingBox_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_BoundingBox_B_clahe_TPS)
#db$diagnostics_Mask.original_CenterOfMassIndex_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_CenterOfMassIndex_B_clahe_TPS)
#db$diagnostics_Mask.original_CenterOfMass_B_clahe_TPS<-as.numeric(db$diagnostics_Mask.original_CenterOfMass_B_clahe_TPS)
#db$diagnostics_Image.interpolated_Spacing_B_clahe_TPS<-as.numeric(db$diagnostics_Image.interpolated_Spacing_B_clahe_TPS)
#db$diagnostics_Image.interpolated_Size_B_clahe_TPS<-as.numeric(db$diagnostics_Image.interpolated_Size_B_clahe_TPS)
#db$Name_B_clahe_TPS<-as.numeric(db$Name_B_clahe_TPS)

#na_only_vars <- names(db[colSums(is.na(db)) == nrow(db)])
# Print the filtered variables
#print(na_only_vars)


#remove BI-RADS 3 patients
#db <- subset(db, !db$birads==1)

#choose validation set
my_seed = 2023
set.seed(my_seed)

CrossTable(db$siteno==1)
Table(db$siteno==1 & db$outcome=="Malignant")
#-> only site 1 has >100 cancers

db_train <- subset(db, !db$siteno==1)
db_validation <- subset(db, db$siteno==1)


CrossTable(db$outcome) 
CrossTable(db_train$outcome) 
CrossTable(db_validation$outcome) 


#### (4) define blueprint for data pre-processing ####
library(recipes)
names(db)
predictors<- c("age", 
               #"palpability"   ,        #"ultrasound_tissue"  ,               "ultrasound_shape"   ,   #"ultrasound_orientation"     ,      
 #              "ultrasound_margin"   ,  #"ultrasound_calcification",
       #        "swe1_quality", "swe1_position", "swe1_lesion" ,
 #              "swe2_quality", "swe2_position", "swe2_lesion" ,
  #             "swe3_quality", "swe3_position", "swe3_lesion" 
   
   "diagnostics_Image.original_Mean_B_clahe_TPS",
   "diagnostics_Image.original_Minimum_B_clahe_TPS",
   "diagnostics_Image.original_Maximum_B_clahe_TPS",
   "diagnostics_Mask.original_VoxelNum_B_clahe_TPS",
   "diagnostics_Mask.original_VolumeNum_B_clahe_TPS",
   "diagnostics_Image.interpolated_Mean_B_clahe_TPS",
   "diagnostics_Image.interpolated_Minimum_B_clahe_TPS",
   "diagnostics_Image.interpolated_Maximum_B_clahe_TPS",
   "diagnostics_Mask.interpolated_VoxelNum_B_clahe_TPS",
   "diagnostics_Mask.interpolated_VolumeNum_B_clahe_TPS",
   "diagnostics_Mask.interpolated_Mean_B_clahe_TPS",
   "diagnostics_Mask.interpolated_Minimum_B_clahe_TPS",
   "diagnostics_Mask.interpolated_Maximum_B_clahe_TPS",
   "original_shape_Elongation_B_clahe_TPS",
   "original_shape_Flatness_B_clahe_TPS",
   "original_shape_LeastAxisLength_B_clahe_TPS",
   "original_shape_MajorAxisLength_B_clahe_TPS",
   "original_shape_Maximum2DDiameterColumn_B_clahe_TPS",
   "original_shape_Maximum2DDiameterRow_B_clahe_TPS",
   "original_shape_Maximum2DDiameterSlice_B_clahe_TPS",
   "original_shape_Maximum3DDiameter_B_clahe_TPS",
   "original_shape_MeshVolume_B_clahe_TPS",
   "original_shape_MinorAxisLength_B_clahe_TPS",
   "original_shape_Sphericity_B_clahe_TPS",
   "original_shape_SurfaceArea_B_clahe_TPS",
   "original_shape_SurfaceVolumeRatio_B_clahe_TPS",
   "original_shape_VoxelVolume_B_clahe_TPS",
   "original_firstorder_10Percentile_B_clahe_TPS",
   "original_firstorder_90Percentile_B_clahe_TPS",
   "original_firstorder_Energy_B_clahe_TPS",
   "original_firstorder_Entropy_B_clahe_TPS",
   "original_firstorder_InterquartileRange_B_clahe_TPS",
   "original_firstorder_Kurtosis_B_clahe_TPS",
   "original_firstorder_Maximum_B_clahe_TPS",
   "original_firstorder_MeanAbsoluteDeviation_B_clahe_TPS",
   "original_firstorder_Mean_B_clahe_TPS",
   "original_firstorder_Median_B_clahe_TPS",
   "original_firstorder_Minimum_B_clahe_TPS",
   "original_firstorder_Range_B_clahe_TPS",
   "original_firstorder_RobustMeanAbsoluteDeviation_B_clahe_TPS",
   "original_firstorder_RootMeanSquared_B_clahe_TPS",
   "original_firstorder_Skewness_B_clahe_TPS",
   "original_firstorder_TotalEnergy_B_clahe_TPS",
   "original_firstorder_Uniformity_B_clahe_TPS",
   "original_firstorder_Variance_B_clahe_TPS",
   "original_glcm_Autocorrelation_B_clahe_TPS",
   "original_glcm_ClusterProminence_B_clahe_TPS",
   "original_glcm_ClusterShade_B_clahe_TPS",
   "original_glcm_ClusterTendency_B_clahe_TPS",
   "original_glcm_Contrast_B_clahe_TPS",
   "original_glcm_Correlation_B_clahe_TPS",
   "original_glcm_DifferenceAverage_B_clahe_TPS",
   "original_glcm_DifferenceEntropy_B_clahe_TPS",
   "original_glcm_DifferenceVariance_B_clahe_TPS",
   "original_glcm_Id_B_clahe_TPS",
   "original_glcm_Idm_B_clahe_TPS",
   "original_glcm_Idmn_B_clahe_TPS",
   "original_glcm_Idn_B_clahe_TPS",
   "original_glcm_Imc1_B_clahe_TPS",
   "original_glcm_Imc2_B_clahe_TPS",
   "original_glcm_InverseVariance_B_clahe_TPS",
   "original_glcm_JointAverage_B_clahe_TPS",
   "original_glcm_JointEnergy_B_clahe_TPS",
   "original_glcm_JointEntropy_B_clahe_TPS",
   "original_glcm_MCC_B_clahe_TPS",
   "original_glcm_MaximumProbability_B_clahe_TPS",
   "original_glcm_SumAverage_B_clahe_TPS",
   "original_glcm_SumEntropy_B_clahe_TPS",
   "original_glcm_SumSquares_B_clahe_TPS",
   "original_gldm_DependenceEntropy_B_clahe_TPS",
   "original_gldm_DependenceNonUniformity_B_clahe_TPS",
   "original_gldm_DependenceNonUniformityNormalized_B_clahe_TPS",
   "original_gldm_DependenceVariance_B_clahe_TPS",
   "original_gldm_GrayLevelNonUniformity_B_clahe_TPS",
   "original_gldm_GrayLevelVariance_B_clahe_TPS",
   "original_gldm_HighGrayLevelEmphasis_B_clahe_TPS",
   "original_gldm_LargeDependenceEmphasis_B_clahe_TPS",
   "original_gldm_LargeDependenceHighGrayLevelEmphasis_B_clahe_TPS",
   "original_gldm_LargeDependenceLowGrayLevelEmphasis_B_clahe_TPS",
   "original_gldm_LowGrayLevelEmphasis_B_clahe_TPS",
   "original_gldm_SmallDependenceEmphasis_B_clahe_TPS",
   "original_gldm_SmallDependenceHighGrayLevelEmphasis_B_clahe_TPS",
   "original_gldm_SmallDependenceLowGrayLevelEmphasis_B_clahe_TPS",
   "original_glrlm_GrayLevelNonUniformity_B_clahe_TPS",
   "original_glrlm_GrayLevelNonUniformityNormalized_B_clahe_TPS",
   "original_glrlm_GrayLevelVariance_B_clahe_TPS",
   "original_glrlm_HighGrayLevelRunEmphasis_B_clahe_TPS",
   "original_glrlm_LongRunEmphasis_B_clahe_TPS",
   "original_glrlm_LongRunHighGrayLevelEmphasis_B_clahe_TPS",
   "original_glrlm_LongRunLowGrayLevelEmphasis_B_clahe_TPS",
   "original_glrlm_LowGrayLevelRunEmphasis_B_clahe_TPS",
   "original_glrlm_RunEntropy_B_clahe_TPS",
   "original_glrlm_RunLengthNonUniformity_B_clahe_TPS",
   "original_glrlm_RunLengthNonUniformityNormalized_B_clahe_TPS",
   "original_glrlm_RunPercentage_B_clahe_TPS",
   "original_glrlm_RunVariance_B_clahe_TPS",
   "original_glrlm_ShortRunEmphasis_B_clahe_TPS",
   "original_glrlm_ShortRunHighGrayLevelEmphasis_B_clahe_TPS",
   "original_glrlm_ShortRunLowGrayLevelEmphasis_B_clahe_TPS",
   "original_glszm_GrayLevelNonUniformity_B_clahe_TPS",
   "original_glszm_GrayLevelNonUniformityNormalized_B_clahe_TPS",
   "original_glszm_GrayLevelVariance_B_clahe_TPS",
   "original_glszm_HighGrayLevelZoneEmphasis_B_clahe_TPS",
   "original_glszm_LargeAreaEmphasis_B_clahe_TPS",
   "original_glszm_LargeAreaHighGrayLevelEmphasis_B_clahe_TPS",
   "original_glszm_LargeAreaLowGrayLevelEmphasis_B_clahe_TPS",
   "original_glszm_LowGrayLevelZoneEmphasis_B_clahe_TPS",
   "original_glszm_SizeZoneNonUniformity_B_clahe_TPS",
   "original_glszm_SizeZoneNonUniformityNormalized_B_clahe_TPS",
   "original_glszm_SmallAreaEmphasis_B_clahe_TPS",
   "original_glszm_SmallAreaHighGrayLevelEmphasis_B_clahe_TPS",
   "original_glszm_SmallAreaLowGrayLevelEmphasis_B_clahe_TPS",
   "original_glszm_ZoneEntropy_B_clahe_TPS",
   "original_glszm_ZonePercentage_B_clahe_TPS",
   "original_glszm_ZoneVariance_B_clahe_TPS",
   "original_ngtdm_Busyness_B_clahe_TPS",
   "original_ngtdm_Coarseness_B_clahe_TPS",
   "original_ngtdm_Complexity_B_clahe_TPS",
   "original_ngtdm_Contrast_B_clahe_TPS",
   "original_ngtdm_Strength_B_clahe_TPS"
 
               )

outcome<-"outcome"

formula<-as.formula(paste(outcome, paste(predictors, collapse = "+"),sep="~"))

set.seed(my_seed)

recipe<-recipe(formula,db_train) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%                   # impute with KNN (von missing values). missing neighbours: anhand Durchschnitt der ähnlichsten anderen Patienten (hier 5)
  step_BoxCox(all_numeric(),-all_outcomes()) %>%                        # boxcox transformation for all numeric features, d.h. Variablen werden auf einheitl. Skalenniveau skaliert (dass nicht große Variablen (bspw. Einkommen) ein größeres Gewicht bekommen.)
  step_other(all_nominal(), threshold = .05, other = "other") %>%        # (all nominal selects all factors): lumping: If it's less than one then factor levels whose rate of occurrence in the training set are below threshold will be "othered". If it's greater or equal to one then it's treated as a frequency and factor levels that occur less then threshold times will be "othered".
  step_zv(all_predictors(),-all_outcomes()) %>%                         # remove zero variance variables
  step_nzv(all_predictors(),-all_outcomes())%>%                         # remove near zero variance variables
  step_normalize(all_numeric(),-all_outcomes())%>%                      # normalize all numeric features 
  step_dummy(all_nominal(),-all_outcomes()) %>%                         # one hot encoding for factor variables  => d.h. logische Schlussfolgerung bei categoriellen Variablen
  step_corr(all_predictors(),-all_outcomes(), threshold = 0.9)          # remove variables that have large absolute correlations with other variables. The step will try to remove the minimum number of columns so that all the resulting absolute correlations are less than this value.

#der juice(prep) gibt eine jetzt auf Normalverteilung skalierte Tabelle an


#examine what modifications are done on the dataset 
set.seed(my_seed)
prep<-prep(recipe, db_train)
view(juice(prep))

#Examining all steps in the blue print
tidy(prep)

#Examining mean and standard deviation for normalizing AGE
tidy(prep, number= 8)

#examine the development dataset after prep-rocessing 
prep[["template"]]


#### (5) Algorithm training ####

### define performance metrics and training parameters ####

## define performance metrics
MySummary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}


## define training parameters 
cv_grid <- trainControl(
  method = "repeatedcv", 
  number = 10, #Unterteilung des Testsets in 10 Teile
  repeats = 3,  #10-fold cross-validation wird 3x durchgerechnet. Damit wird weniger dem Zufall überlassen, dass gerade eine optimale Performance erreicht wird
  search = "grid", 
  verboseIter= TRUE,
  classProbs = TRUE,
  returnResamp = "final",
  savePredictions = "final", 
  summaryFunction = MySummary,
  selectionFunction = "tolerance",
  allowParallel=TRUE
)

cv_random <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 3,
  search = "random" ,
  verboseIter= TRUE,
  classProbs = TRUE,
  returnResamp = "final",
  savePredictions = "final", 
  summaryFunction = MySummary,
  selectionFunction = "tolerance",
  allowParallel=TRUE
)

### GLM - elastic net ####

hyper_grid_glm <- expand.grid(
  alpha = seq(from=0.01, to= 1, by=0.005), 
  lambda = seq(from=0.01, to= 1, by=0.005) 
)

library(doParallel)
no_cores<- detectCores()-2

cl<-makePSOCKcluster(no_cores)
registerDoParallel(cl)

set.seed(my_seed)
cv_glm <- caret::train(recipe,
                   data=db_train,
                   method="glmnet",
                   metric="Kappa",
                   tuneLength = 30, #allows system to tune algorithm automatically. It indicates the number of different values to try for each tunning parameter. For example, mtry for randomForest. Suppose, tuneLength = 5, it means try 5 different mtry values and find the optimal mtry value based on these 5 values
                   trControl=cv_random
                   #tuneGrid = hyper_grid_glm
)

stopCluster(cl)
registerDoSEQ()


cv_glm$bestTune
cv_glm$results[c(25),]

#alpha      lambda  Accuracy     Kappa       ROC      Sens      Spec       AUC Precision    Recall
#0.4549702 0.008100243 0.8826207 0.6877192 0.9325435 0.7353333 0.9355721 0.7979705 0.8075674 0.7353333
#       F   AccuracySD    KappaSD      ROCSD    SensSD     SpecSD      AUCSD PrecisionSD  RecallSD        FSD
#0.7651905  0.0343082 0.09782109 0.02192669 0.1083226 0.02721751 0.05458847   0.0737571 0.1083226 0.07755472
ggplot(cv_glm)





### XGBoost ####
#Hyperparameters
#max_depth:Controls the maximum depth of the trees. Deeper trees have more terminal nodes and fit more data. Convergence also requires less trees if we grow them deep. However, if the trees are to deep we will be using a lot of information from the first trees and the final trees of the algorithm will have less importance on the loss function. The Boosting benefits from using information from many trees. Therefore it is intuitive that huge trees are not desirable. Smaller trees also grow faster and because the Boosting grow new trees in a pseudo-residual and we do not require any amazing adjustment for an individual tree.
#eta: range [0,1] Learning (or shrinkage) parameter. It controls how much information from a new tree will be used in the Boosting. This parameter must be bigger than 0 and limited to 1. If it is close to zero we will use only a small piece of information from each new tree. If we set eta to 1 we will use all information from the new tree. Big values of eta result in a faster convergence and more over-fitting problems. Small values may need to many trees to converge.
#gamma: range [0,endless] Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be. 
#colsample_bytree: range [0,1] colsample_bytree is the subsample ratio of columns when constructing each tree. Subsampling occurs once for every tree constructed.
#min child weight: Minimum sum of instance weight (hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning. 
#subsample: range [0,1] Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees. and this will prevent overfitting. Subsampling will occur once in every boosting iteration.

hyper_grid_xgboost <- expand.grid(
  nrounds = seq(from=5, to= 100, by=5), 
  max_depth = seq(from=5, to= 50, by=5),
  eta = seq(from=0.1, to= 1, by=0.1), 
  gamma = seq(from=0.5, to= 10, by=0.5), 
  colsample_bytree = seq(from=0.1, to= 1, by=0.1), 
  min_child_weight = seq(from=2, to= 10, by=1), 
  subsample = 1
)

library(doParallel)
no_cores<- detectCores()-2

cl<-makePSOCKcluster(no_cores)
registerDoParallel(cl)

set.seed(my_seed)
cv_xgboost <- caret::train(recipe,
                       data=db_train,
                       method="xgbTree",
                       metric="Kappa",
                       tuneLength = 30, #allows system to tune algorithm automatically. It indicates the number of different values to try for each tunning parameter. For example, mtry for randomForest. Suppose, tuneLength = 5, it means try 5 different mtry values and find the optimal mtry value based on these 5 values
                       trControl=cv_random
                       #tuneGrid = hyper_grid_xgboost
)

stopCluster(cl)
registerDoSEQ()

cv_xgboost$bestTune
cv_xgboost$results[c("9"),]

#       eta max_depth    gamma colsample_bytree min_child_weight subsample nrounds  Accuracy     Kappa
# 0.2213521         1 4.514488         0.574745                2 0.9475944     121 0.8826761 0.6813555
#     ROC      Sens      Spec       AUC Precision    Recall         F AccuracySD    KappaSD      ROCSD
#0.930885 0.7065556 0.9460126 0.7905473 0.8270521 0.7065556 0.7574637 0.03285075 0.09626776 0.02389728
#SensSD     SpecSD     AUCSD PrecisionSD  RecallSD       FSD
#0.1107593 0.02255929 0.0532228  0.06534975 0.1107593 0.0772669

ggplot(cv_xgboost)



#### (6) Resampling performance testing ####

### ROC ####


roc_glm_train = roc(as.vector(db_train$outcome),as.matrix(predict(cv_glm , db_train, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_glm_train = pROC::auc(roc_glm_train) #Calculate the area under the ROC curve
auc_CI_glm_train = pROC::ci.auc(roc_glm_train, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_xgboost_train = roc(as.vector(db_train$outcome),as.matrix(predict(cv_xgboost , db_train, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_xgboost_train = pROC::auc(roc_xgboost_train) #Calculate the area under the ROC curve
auc_CI_xgboost_train = pROC::ci.auc(roc_xgboost_train, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_b_xgboost_train = roc(as.vector(db_b_clahe_train$outcome),as.matrix(predict(cv_b_clahe_xgboost , db_b_clahe_train, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_b_xgboost_train = pROC::auc(roc_b_xgboost_train) #Calculate the area under the ROC curve
auc_CI_b_xgboost_train = pROC::ci.auc(roc_b_xgboost_train, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


### metrics ####

#glm - elastic net
cv_glm$bestTune
cv_glm$results[c(216),]
cv_glm[["resample"]][["ROC"]] #AUROC
range(cv_glm[["resample"]][["ROC"]])
mean((cv_glm[["resample"]][["ROC"]]))
CI(cv_glm[["resample"]][["ROC"]], ci=0.95)
sd(cv_glm[["resample"]][["ROC"]])

cv_glm[["resample"]][["Kappa"]]
mean(cv_glm[["resample"]][["Kappa"]])
CI(cv_glm[["resample"]][["Kappa"]], ci=0.95)

cv_glm[["resample"]][["Accuracy"]]
CI(cv_glm[["resample"]][["Accuracy"]], ci=0.95)

cv_glm[["resample"]][["Sens"]]
CI(cv_glm[["resample"]][["Sens"]], ci=0.95)

cv_glm[["resample"]][["Spec"]]
CI(cv_glm[["resample"]][["Spec"]], ci=0.95)

cv_glm[["resample"]][["Precision"]] #positive-predictive value
CI(cv_glm[["resample"]][["Precision"]], ci=0.95)

#xgboost
cv_xgboost$bestTune
cv_xgboost[["resample"]][["ROC"]] #AUROC
CI(cv_xgboost[["resample"]][["ROC"]], ci=0.95)
range(cv_xgboost[["resample"]][["ROC"]])

cv_xgboost[["resample"]][["Kappa"]]
CI(cv_xgboost[["resample"]][["Kappa"]], ci=0.95)

cv_xgboost[["resample"]][["Accuracy"]]
CI(cv_xgboost[["resample"]][["Accuracy"]], ci=0.95)

cv_xgboost[["resample"]][["Sens"]]
CI(cv_xgboost[["resample"]][["Sens"]], ci=0.95)

cv_xgboost[["resample"]][["Spec"]]
CI(cv_xgboost[["resample"]][["Spec"]], ci=0.95)

cv_xgboost[["resample"]][["Precision"]] #positive-predictive value
CI(cv_xgboost[["resample"]][["Precision"]], ci=0.95)



### confusion matrices ####

####26-01-2024
confusionMatrix(as.factor(predict(cv_glm , db_train)), factor(db_train$outcome), positive="Malignant") #confusion matrix

glm_binary_prediction_train <-
  ifelse(predict(cv_glm , db_train, type="prob")$"Malignant" 
         >= glm_cutoff,
         "Malignant",
         "Benign")

confusionMatrix(as.factor(glm_binary_prediction_train), factor(db_train$outcome), positive="Malignant") #confusion matrix

##
confusionMatrix(as.factor(predict(cv_xgboost , db_train)), factor(db_train$outcome), positive="Malignant") #confusion matrix

xgboost_binary_prediction_train <-
  ifelse(predict(cv_xgboost , db_train, type="prob")$"Malignant" 
         >= xgboost_cutoff,
         "Malignant",
         "Benign")

confusionMatrix(as.factor(xgboost_binary_prediction_train), factor(db_train$outcome), positive="Malignant") #confusion matrix



##glm - elastic net
confusionMatrix(as.factor(cv_glm[["pred"]][["pred"]]), factor(cv_glm[["pred"]][["obs"]]), positive="Malignant") #confusion matrix

# set cutoff 
glm_cutoff <- as.data.frame(
  cbind(cv_glm[["pred"]][["Malignant"]], cv_glm[["pred"]][["Benign"]], factor(cv_glm[["pred"]][["obs"]]))
)
colnames(glm_cutoff) <- c("prediction_malignant","prediction_benign", "actual")
require(Rfast)
glm_cutoff <- subset(glm_cutoff, glm_cutoff$actual ==1) # 1 == actual malign
glm_cutoff = (Rfast::nth(glm_cutoff$prediction_malignant, 3, descending = FALSE)) # die Zahl (5) bedeutet, dass der 5.-niedrigste Wert der malignen Befunde als Cutoff gewählt wird, die 4 davor werden als verpasst toleriert
detach(package:Rfast)

glm_binary_prediction_training <-
  ifelse(cv_glm[["pred"]][["Malignant"]] 
         >= glm_cutoff ,
         "Malignant",
         "Benign")

confusionMatrix(as.factor(glm_binary_prediction_training), factor(cv_glm[["pred"]][["obs"]]), positive="Malignant") #confusion matrix


##xgboost 
confusionMatrix(as.factor(cv_xgboost[["pred"]][["pred"]]), factor(cv_xgboost[["pred"]][["obs"]]), positive="Malignant") #confusion matrix

#set cutoff
xgboost_cutoff <- as.data.frame(
  cbind(cv_xgboost[["pred"]][["Malignant"]], cv_xgboost[["pred"]][["Benign"]], factor(cv_xgboost[["pred"]][["obs"]]))
)
colnames(xgboost_cutoff) <- c("prediction_malignant","prediction_benign", "actual")
require(Rfast)
xgboost_cutoff <- subset(xgboost_cutoff, xgboost_cutoff$actual ==1) 
xgboost_cutoff = (Rfast::nth(xgboost_cutoff$prediction_malignant, 4, descending = FALSE))
detach(package:Rfast)

xgboost_binary_prediction_training <-
  ifelse(cv_xgboost[["pred"]][["Malignant"]] 
         >= xgboost_cutoff ,
         "Malignant",
         "Benign")

confusionMatrix(as.factor(xgboost_binary_prediction_training), factor(cv_xgboost[["pred"]][["obs"]]), positive="Malignant") #confusion matrix



cm_a <- 233
cm_b <- 398
cm_c <- 0
cm_d <- 230

cm_a_c <- cm_a + cm_c
cm_b_d <- cm_b + cm_d
cm_a_b <- cm_a + cm_b
cm_c_d <- cm_c + cm_d

cat("Sensitivity:", cm_a, "of", cm_a_c, "\n")
cat("Specificity:", cm_d, "of", cm_b_d, "\n")
cat("PPV:", cm_a, "of", cm_a_b, "\n")
cat("NPV:", cm_d, "of", cm_c_d, "\n")

print

#ci sensitivity
ci_train_sensitivity <- binom.test(cm_a, cm_a_c, p = 1,
                                   conf.level = 0.95)

print(ci_train_sensitivity)

#ci specificity
ci_train_specifity<- binom.test(cm_d, cm_b_d, p = 1,
                                conf.level = 0.95)

print(ci_train_specifity)

#ci ppv
ci_train_ppv<- binom.test(cm_a, cm_a_b, p = 1,
                          conf.level = 0.95)

print(ci_train_ppv)

#ci npv
ci_train_npv<- binom.test(cm_d, cm_c_d, p = 1,
                          conf.level = 0.95)

print(ci_train_npv)


#### calibration ####
## calibration plots 

##glm 
glm_calplot_cv <- calibration(factor(cv_glm[["pred"]][["obs"]]) ~ cv_glm[["pred"]][["Malignant"]], data = cv_glm, cuts=10)
xyplot(glm_calplot_cv, auto.key = list(columns = 2), 
       xlab=list(
         label="Predicted Probability",
         cex=1.5),
       ylab=list(
         label="Observed Probability",
         cex=1.5),
       scales=list(cex=1.5)
) 

ggplot(glm_calplot_cv)



##mars 
mars_calplot_cv <- calibration(factor(cv_mars[["pred"]][["obs"]]) ~ cv_mars[["pred"]][["Malignant"]], data = cv_mars, cuts=10)
xyplot(mars_calplot_cv, auto.key = list(columns = 2), 
       xlab=list(
         label="Predicted Probability",
         cex=1.5),
       ylab=list(
         label="Observed Probability",
         cex=1.5),
       scales=list(cex=1.5)
) 

ggplot(mars_calplot_cv)


##xgboost 
xgboost_calplot_cv <- calibration(factor(cv_xgboost[["pred"]][["obs"]]) ~ cv_xgboost[["pred"]][["Malignant"]], data = cv_xgboost, cuts=10)
xyplot(xgboost_calplot_cv, auto.key = list(columns = 2), 
       xlab=list(
         label="Predicted Probability",
         cex=1.5),
       ylab=list(
         label="Observed Probability",
         cex=1.5),
       scales=list(cex=1.5)
) 

setwd("C:/Users/he_ta/Desktop/Doktorarbeit/2. Programming/4. R Codes/")

ggplot(xgboost_calplot_cv)

trellis.device(png, filename = "b_clahe_xgboost_calplot.png", width = 800, height = 600)
print(glm_calplot_cv)
dev.off()  # Close the device


## calibration scores 

Spiegelhalter_z = function(y, prob){
  alpha = 0.05
  z_score = sum((y-prob)*(1-2*prob))/sqrt(sum(((1-2*prob)^2)*prob*(1-prob)))
  print(z_score)
  if (abs(z_score) > qnorm(1-alpha/2)){
    print('reject null. NOT calibrated')
  } else{
    print('fail to reject. calibrated')
  }
  cat('z score: ', z_score, '\n')
  cat('p value: ', 1-pnorm(abs(z_score)), '\n')
  return(z_score)
}


Brier = function(y, prob){
  a = mean((prob-y)^2)
  print(a)
} 
## Lower scores approaching 0 are better.


##glm elastic net
Spiegelhalter_z (unfactor(revalue(cv_glm[["pred"]][["obs"]], c("Malignant"=1, "Benign"=0))), cv_glm[["pred"]][["Malignant"]])
Brier (unfactor(revalue(cv_glm[["pred"]][["obs"]], c("Malignant"=1, "Benign"=0))), cv_glm[["pred"]][["Malignant"]])

##xgboost 
Spiegelhalter_z (unfactor(revalue(cv_xgboost[["pred"]][["obs"]], c("Malignant"=1, "Benign"=0))), cv_xgboost[["pred"]][["Malignant"]])
Brier (unfactor(revalue(cv_xgboost[["pred"]][["obs"]], c("Malignant"=1, "Benign"=0))), cv_xgboost[["pred"]][["Malignant"]])




#### (7) (External) validation ####

#predict probabilities 
prob_predictions =predict(cv_xgboost , db_validation, type = "prob")

db_validation['Prob_1'] = prob_predictions[1]  # Assuming Prob_1 is the first column
db_validation['Prob_2'] = prob_predictions[2]  # Assuming Prob_2 is the second column

db_validation$Prob_1


#predict classes 
xgboost_binary_prediction = predict(cv_xgboost , db_validation)
db_validation['xgboost_binary_prediction'] = xgboost_binary_prediction
db_validation$xgboost_binary_prediction


selected_columns <- db_validation[c('VTIQ.Number', 'Prob_1','xgboost_binary_prediction')]
print(selected_columns)

options(max.print = 10000)
print(selected_columns)

#apply pre-processing steps to validation set manually (done automatically by "predict")

test <- bake(prep, db_validation)
names(test)
names(predictors)


### ROC ####


roc_glm_validation = roc(as.vector(db_validation$outcome),as.matrix(predict(cv_glm , db_validation, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_glm_validation = pROC::auc(roc_glm_validation) #Calculate the area under the ROC curve
auc_CI_glm_validation = pROC::ci.auc(roc_glm_validation, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_xgboost_validation = roc(as.vector(db_validation$outcome),as.matrix(predict(cv_xgboost , db_validation, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_xgboost_validation = pROC::auc(roc_xgboost_validation) #Calculate the area under the ROC curve
auc_CI_xgboost_validation = pROC::ci.auc(roc_xgboost_validation, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_b_xgboost_validation = roc(as.vector(db_b_clahe_validation$outcome),as.matrix(predict(cv_b_clahe_xgboost , db_b_clahe_validation, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_b_xgboost_validation = pROC::auc(roc_b_xgboost_validation) #Calculate the area under the ROC curve
auc_CI_b_xgboost_validation = pROC::ci.auc(roc_b_xgboost_validation, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


plot.roc(roc_glm_validation, ylim=c(0,1), xlim=c(1,0), cex.lab=1.8, cex.axis=1.5, cex.main=1.8, cex.sub=1.8, 
         legacy.axes=TRUE) #Plot the ROC curves
lines(roc_glm_validation, col="blue")
lines(roc_xgboost_validation, col="red")
lines(roc_mars_validation, col="orange")
lines(roc_svm_validation, col="black")
lines(roc_nn_validation, col="grey60")
legend("bottomright", legend=c("LR with Elastic Net Penalty", "XGBoost Tree", "MARS", "SVM", "neural network"), col=c("blue", "red","orange", "black", "grey60"), lwd=2, cex=1.3)

plot.roc(roc_glm_validation, ylim=c(0,1), xlim=c(1,0), cex.lab=1.8, cex.axis=1.5, cex.main=1.8, cex.sub=1.8, 
         legacy.axes=TRUE) #Plot the ROC curves
lines(roc_glm_validation, col="black")
lines(roc_nn_validation, col="grey70")
legend("bottomright", legend=c("LR with Elastic Net Penalty", "neural network"), col=c("black", "grey70"), lwd=2, cex=1.3)

plot.roc(roc_glm_validation, ylim=c(0,1), xlim=c(1,0), cex.lab=1.8, cex.axis=1.5, cex.main=1.8, cex.sub=1.8, 
         legacy.axes=TRUE) #Plot the ROC curves
lines(roc_glm_validation, col="black")
lines(roc_nn_validation, col="grey70")
lines(roc_swe, col="blue")
lines(roc_swe_max, col="red")

legend("bottomright", legend=c("LR with Elastic Net Penalty", "neural network", "Shear-Wave Elastography - mean", "Shear-Wave Elastography - maximum"), col=c("black", "grey70", "blue", "red"), lwd=2, cex=1.3)


plot.roc(roc_glm_validation, ylim=c(0,1), xlim=c(1,0), cex.lab=1.8, cex.axis=1.5, cex.main=1.8, cex.sub=1.8, 
         legacy.axes=TRUE) #Plot the ROC curves
lines(roc_glm_validation, col="black")
lines(roc_nn_validation, col="grey70")
lines(roc_swe, col="grey30")

legend("bottomright", legend=c("LR with Elastic Net Penalty", "neural network", "Shear-Wave Elastography", "Shear-Wave Elastography - maximum"), col=c("black", "grey70", "grey30"), lwd=2, cex=1.3)


#### confusion matrices ####

#set cutoff


#glm
confusionMatrix(as.factor(predict(cv_glm , db_validation)), factor(db_validation$outcome), positive="Malignant") #confusion matrix

glm_binary_prediction_validation <-
  ifelse(predict(cv_glm , db_validation, type="prob")$"Malignant" 
         >= glm_cutoff,
         "Malignant",
         "Benign")

confusionMatrix(as.factor(glm_binary_prediction_validation), factor(db_validation$outcome), positive="Malignant") #confusion matrix

factor(glm_binary_prediction_validation)
factor(db_validation$outcome)

cm_a <- 114
cm_b <- 151
cm_c <- 2
cm_d <- 76

cm_a_c <- cm_a + cm_c
cm_b_d <- cm_b + cm_d
cm_a_b <- cm_a + cm_b
cm_c_d <- cm_c + cm_d



#ci sensitivity
ci_external_sensitivity <- binom.test(cm_a, cm_a_c, p = 1,
                                      conf.level = 0.95)

print(ci_external_sensitivity)

#ci specificity
ci_external_specifity<- binom.test(cm_d, cm_b_d, p = 1,
                                   conf.level = 0.95)

print(ci_external_specifity)

#ci ppv
ci_external_ppv<- binom.test(cm_a, cm_a_b, p = 1,
                             conf.level = 0.95)

print(ci_external_ppv)

#ci npv
ci_external_npv<- binom.test(cm_d, cm_c_d, p = 1,
                             conf.level = 0.95)

print(ci_external_npv)

#xgboost
confusionMatrix(as.factor(predict(cv_xgboost , db_validation)), factor(db_validation$outcome), positive="Malignant") #confusion matrix

xgboost_binary_prediction_validation <-
  ifelse(predict(cv_xgboost , db_validation, type="prob")$"Malignant" 
         >= xgboost_cutoff,
         "Malignant",
         "Benign")

confusionMatrix(as.factor(xgboost_binary_prediction_validation), factor(db_validation$outcome), positive="Malignant") #confusion matrix




#### calibration ####

## calibration plots 

##glm 
glm_calplot_validation <- calibration(factor(db_validation$outcome) ~ as.matrix(predict(cv_glm , db_validation, type = "prob")$"Malignant"), data = db_validation, cuts=10)
xyplot(glm_calplot_validation, auto.key = list(columns = 2), 
       xlab=list(
         label="Predicted Probability",
         cex=1.5),
       ylab=list(
         label="Observed Probability",
         cex=1.5),
       scales=list(cex=1.5)
) 


ggplot(glm_calplot_validation)


##mars 
mars_calplot_validation <- calibration(factor(db_validation$outcome) ~ 
  as.matrix(predict(cv_mars , db_validation, type = "prob")$"Malignant"), 
  data = db_validation, cuts=10)

xyplot(mars_calplot_validation, auto.key = list(columns = 2), 
       xlab=list(
         label="Predicted Probability",
         cex=1.5),
       ylab=list(
         label="Observed Probability",
         cex=1.5),
       scales=list(cex=1.5)
) 

ggplot(mars_calplot_validation)


##xgboost 
xgboost_calplot_validation <- calibration(factor(db_validation$outcome) ~ as.matrix(predict(cv_xgboost , db_validation, type = "prob")$"Malignant"), data = db_validation, cuts=10)
xyplot(xgboost_calplot_validation, auto.key = list(columns = 2), 
       xlab=list(
         label="Predicted Probability",
         cex=1.5),
       ylab=list(
         label="Observed Probability",
         cex=1.5),
       scales=list(cex=1.5)
) 

ggplot(xgboost_calplot_validation)

## calibration scores 

##glm elastic net
Spiegelhalter_z (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_glm , db_validation, type = "prob")$"Malignant"))
Brier (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_glm , db_validation, type = "prob")$"Malignant"))

##xgboost 
Spiegelhalter_z (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_xgboost , db_validation, type = "prob")$"Malignant"))
Brier (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_xgboost , db_validation, type = "prob")$"Malignant"))

##mars 
Spiegelhalter_z (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_mars , db_validation, type = "prob")$"Malignant"))
Brier (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_mars , db_validation, type = "prob")$"Malignant"))

##svm 
Spiegelhalter_z (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_svm , db_validation, type = "prob")$"Malignant"))
Brier (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_svm , db_validation, type = "prob")$"Malignant"))

##nn
Spiegelhalter_z (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_nn , db_validation, type = "prob")$"Malignant"))
Brier (unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), as.matrix(predict(cv_nn , db_validation, type = "prob")$"Malignant"))




#### (8) Explainable Artificial Intelligence  ####

### glm - elastic net ####

##agnostic model explanation 
xai_glm_explainer <- DALEX::explain(cv_glm, label = "Malignant", 
                                      data = select(db_validation, c(predictors,outcome)),   
                                      y = unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), 
                                      verbose = FALSE)

xai_glm_feature_importance <- model_parts(xai_glm_explainer, 
                       type="variable_importance")

plot(xai_glm_feature_importance, max_vars=10, show_boxplots= TRUE, title="Logistic Regression with Elastic Net Penalty", subtitle = "")


###
#importance_values <- xai_glm_explainer$data

importance_values <- as.vector(xai_glm_explainer$data)

sorted_importance <- sort(importance_values, decreasing = TRUE)


xai_glm_feature_importance <- model_parts(xai_glm_explainer, 
                       type="variable_importance")

plot(xai_glm_feature_importance, max_vars=10, show_boxplots= TRUE, title="Logistic Regression with Elastic Net Penalty", subtitle = "")

new_data_frame()

# Accumulated Local Dependence Profile
xai_glm_ld <- model_profile(xai_glm_explainer, variables =  c("ultrasound_shape", "swe1_quality", "ultrasound_tissue"), type = "accumulated")
plot(xai_glm_ld , geom = "profiles")


# partial dependencies for categorial variables 
xai_glm_pd  <- ingredients::accumulated_dependency(xai_glm_explainer, variable_type = "categorical")
plot(xai_glm_pd)


## model-specific explanations 
glm_model_coef <- coef(cv_glm$finalModel, cv_glm$finalModel$lambdaOpt)


###  XGBoost Tree ####

##agnostic model explanation 


xai_xgboost_explainer <- DALEX::explain(cv_xgboost, label = "Malignant", 
                                    data = select(db_validation, c(predictors,outcome)),   
                                    y = unfactor(revalue(db_validation$outcome, c("Malignant"=1, "Benign"=0))), 
                                    verbose = FALSE)

xai_xgboost_feature_importance <- model_parts(xai_xgboost_explainer, 
                                          type="variable_importance")

plot(xai_xgboost_feature_importance, max_vars = 10, show_boxplots = TRUE, title = "Extreme Gradient Boosting Tree", subtitle = "")

########

xai_xgboost_explainer <- DALEX::explain(cv_xgboost, label = "Malignant", 
                                    data = select(db_validation, predictors),   
                                    y = select(db_validation, outcome), 
                                    verbose = FALSE)

xai_xgboost_feature_importance <- model_parts(xai_xgboost_explainer, 
                                          type="variable_importance")

plot(xai_xgboost_feature_importance, max_vars=10, show_boxplots= TRUE, title="Extreme Gradient Boosting Tree", subtitle = "")

# partial dependencies for categorial variables 
xai_xgboost_cat  <- ingredients::accumulated_dependency(xai_xgboost_explainer, variable_type = "categorical")
plot(xai_xgboost_cat)

## SHAP values
# shap.prep() returns the long-format SHAP data from either model or
shap_long <- shap.prep(xgb_model = cv_xgboost$finalModel, X_train = as.matrix(select(prep[["template"]], !outcome)))

## SHAP summary plot
#create list names "new_labels" to customize variable labels for summary plot
prep[["template"]]
new_labels<-list(
  age = "Patient Age", 
  palpability = "Clinical Palpability", 
  ultrasound_shape_round = "Shape of mass - round",
  Shape_Oval   ="Shape of mass - oval",           
  Shape_Lobular   ="Shape of mass - lobular",    
  Shape_Irregular   ="Shape of mass - irregular",    
  Margin_Circumscribed    ="Margin of mass - circumscribed",
  Margin_microbulated    ="Margin of mass - microlobulated",
  Margin_Obscured   = "Margin of mass - obscured"   ,       
  Margin_Ill.defined = "Margin of mass - ill defined" ,         
  Margin_Spiculated  ="Margin of mass - spiculated ",
  Density_high =  "Density of mass - high ",     
  Density_iso  =  "Density of mass - iso"  ,    
  Density_low  ="Density of mass - low",
  Density_fat.containing = "Density of mass - fat containing"    
)   
new_labels

shap.plot.summary(shap_long)  + 
  theme(axis.text.x=element_text(size = 15),
        axis.title.x=element_text(size = 15),
        axis.text.y=element_text(size = 13),
        axis.title.y=element_text(size = 15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))


# option of dilute is offered to make plot faster if there are many observations
shap.plot.summary(shap_long, x_bound  = 1.5, dilute = 10)



### MARS ####
##agnostic model explanation 
xai_mars_explainer <- DALEX::explain(cv_mars, label = "Malignant", 
                                    data = select(db_validation, all_of(c(predictors, outcome))),   
                                    y = select(db_validation, outcome), 
                                    verbose = FALSE)

xai_mars_feature_importance <- model_parts(xai_mars_explainer, 
                                          type="variable_importance")

plot(xai_mars_feature_importance, max_vars=10, show_boxplots= TRUE, title="Multivariate Adaptive Regression Spline", subtitle = "")

# partial dependencies for categorial variables 
xai_mars_cat  <- ingredients::accumulated_dependency(xai_mars_explainer, variable_type = "categorical")
plot(xai_mars_cat)


### SVM ####
##agnostic model explanation 
xai_svm_explainer <- DALEX::explain(cv_svm, label = "Malignant", 
                                    data = select(db_validation, predictors),   
                                    y = select(db_validation, outcome), 
                                    verbose = FALSE)

xai_svm_feature_importance <- model_parts(xai_svm_explainer, 
                                          type="variable_importance")

plot(xai_svm_feature_importance, max_vars=10, show_boxplots= TRUE, title="Support Vector Machine", subtitle = "")

# partial dependencies for categorial variables 
xai_svm_cat  <- ingredients::accumulated_dependency(xai_svm_explainer, variable_type = "categorical")
plot(xai_svm_cat)


### neural network ####
##agnostic model explanation 
xai_nn_explainer <- DALEX::explain(cv_nn, label = "Malignant", 
                                    data = select(db_validation, predictors),   
                                    y = select(db_validation, outcome), 
                                    verbose = FALSE)

xai_nn_feature_importance <- model_parts(xai_nn_explainer, 
                                          type="variable_importance")

plot(xai_nn_feature_importance, max_vars=10, show_boxplots= TRUE, title="Neural Network", subtitle = "")

# partial dependencies for categorial variables 
xai_glm_cat  <- ingredients::accumulated_dependency(xai_glm_explainer, variable_type = "categorical")
plot(xai_glm_cat)

## LIME
explainer_nn <- lime(select(db_train, predictors), cv_nn,  n_bins = 1)

class(explainer_nn)
## [1] "data_frame_explainer" "explainer"            "list"

summary(explainer_nn)

explanation_nn <- lime::explain(
  x = select(db_validation, predictors), 
  explainer = explainer_nn, 
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = .75,
  n_features = 14, 
  feature_select = "highest_weights",
  labels = "Malignant"
)

plot_features(explanation_nn)

plot_explanations(explanation_nn)

plot_explanations(explanation_nn) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size = 15),
        axis.title.y=element_text(size = 15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)
  )



#### (10) human performance ####

##b mode experts
human_usexperts <- db_validation$final_birads_num

roc_human_usexperts = roc(as.vector(db_validation$outcome),as.matrix(human_usexperts)) #Conduct the ROC analyses
auc_human_usexperts = pROC::auc(roc_human_usexperts) #Calculate the area under the ROC curve
auc_CI_human_usexperts = pROC::ci.auc(roc_human_usexperts, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



human_usexperts<-as.factor(ifelse(human_usexperts==1, "Benign", "Malignant"))
levels(human_usexperts) #caret uses the first level of the outcome variable as reference during the training process - so first level should be the positive outcome 
human_usexperts <- factor(human_usexperts, levels=rev(levels(human_usexperts)))
levels(human_usexperts)

#strain experts
strain_ratio <- (db$strainelasto/ db$strainbmode)
str_value <-  as.factor(ifelse(strain_ratio  <1, "Benign", "Malignant"))

roc_str_value = roc(as.vector(db$outcome),as.matrix(str_value)) #Conduct the ROC analyses
auc_str_value = pROC::auc(roc_str_value) #Calculate the area under the ROC curve
auc_CI_str_value = pROC::ci.auc(roc_str_value, method="bootstr_valuerap", boot.str_valueratified=TRUE) #Calculate the area under the ROC curve

print(auc_str_value)
print(auc_CI_str_value)


##strain confusion matrix
strain <- as.factor(
  ifelse(db$strain  ==1, "Benign", "Malignant"))

strain_binary_prediction <- factor(strain, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(strain_binary_prediction), factor(db$outcome), positive="Malignant") #confusion matrix



##AUC US Experts
human_usexperts_full_cohort <- db$final_birads_num

roc_human_usexperts_full_cohort = roc(as.vector(db$outcome),as.matrix(human_usexperts_full_cohort)) #Conduct the ROC analyses
auc_human_usexperts_full_cohort = pROC::auc(roc_human_usexperts_full_cohort) #Calculate the area under the ROC curve
auc_CI_human_usexperts_full_cohort = pROC::ci.auc(roc_human_usexperts_full_cohort, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

print(auc_human_usexperts_full_cohort)
print(auc_CI_human_usexperts_full_cohort)

#US Experts confusion matrix
human_usexperts_binary_prediction <- as.factor(
  ifelse(human_usexperts_full_cohort  ==1, "Benign", "Malignant"))

human_usexperts_binary_prediction <- factor(human_usexperts_binary_prediction, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_usexperts_binary_prediction), factor(db$outcome), positive="Malignant") #confusion matrix


#US Experts single BIRADS score
#for b-mode dataset
barr_birads <- db_validation$birads_b_num

#for str dataset
#barr_birads <- db_validation$birads_STR_num

roc_barr = roc(as.vector(db_validation$outcome),as.matrix(barr_birads)) #Conduct the ROC analyses
auc_barr = pROC::auc(roc_barr) #Calculate the area under the ROC curve
auc_CI_barr = pROC::ci.auc(roc_barr, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



binary_barr <- as.factor(
  ifelse(barr_birads  ==1, "Benign", "Malignant"))

binary_barr <- factor(binary_barr, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(binary_barr), factor(db_validation$outcome), positive="Malignant") #confusion matrix



##

golatta_birads <- db_validation$birads_g_num

roc_golatta = roc(as.vector(db_validation$outcome),as.matrix(golatta_birads)) #Conduct the ROC analyses
auc_golatta = pROC::auc(roc_golatta) #Calculate the area under the ROC curve
auc_CI_golatta = pROC::ci.auc(roc_golatta, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


binary_golatta <- as.factor(
  ifelse(golatta_birads  ==1, "Benign", "Malignant"))

binary_golatta <- factor(binary_golatta, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(binary_golatta), factor(db_validation$outcome), positive="Malignant") #confusion matrix


duda_birads <- db_validation$birads_d_num

roc_duda = roc(as.vector(db_validation$outcome),as.matrix(duda_birads)) #Conduct the ROC analyses
auc_duda = pROC::auc(roc_duda) #Calculate the area under the ROC curve
auc_CI_duda = pROC::ci.auc(roc_duda, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


binary_duda <- as.factor(
  ifelse(duda_birads  ==1, "Benign", "Malignant"))

binary_duda <- factor(binary_duda, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(binary_duda), factor(db_validation$outcome), positive="Malignant") #confusion matrix




plot.roc(roc_golatta, ylim=c(0,1), xlim=c(1,0), cex.lab=1.8, cex.axis=1.5, cex.main=1.8, cex.sub=1.8, 
         legacy.axes=TRUE) #Plot the ROC curves
lines(roc_golatta, col="blue")
lines(roc_barr, col="red")
lines(roc_duda, col="orange")
lines(roc_xgboost_validation , col="black")

lines(roc_human_usexperts, col="grey60")
legend("bottomright", legend=c("expert 1", "expert 2", "expert 3", "xgboost", "expert agreement"), col=c("blue", "red","orange", "black", "grey60"), lwd=2, cex=1.3)


ggroc(roc_duda)
install.packages("ggroc")

library(pROC)

# Create a list of ROC objects (roc_golatta, roc_barr, roc_duda, etc.)
roc_list <- list(
  golatta = roc_golatta,
  barr = roc_barr,
  duda = roc_duda,
  xgboost = roc_xgboost_validation,
  human_usexperts = roc_human_usexperts
)

roc_list <- list(
  expert_1 = roc_golatta,
  expert_2 = roc_barr,
  expert_3 = roc_duda,
  expert_agreement = roc_human_usexperts, 
  xgboost = roc_xgboost_validation
  
)

custom_aesthetics <- data.frame(
  Model = names(roc_list),
  colour = c("blue", "red", "orange", "black", "grey60"),
  linetype = c("solid", "dashed", "dotted", "solid", "solid"),
  size = c(1, 1, 1, 1, 1)
)

ggroc_plot <- ggroc(roc_list, 
                    aes = list(
                      colour = custom_aesthetics$color,
                      linetype = custom_aesthetics$linetype,
                      size = custom_aesthetics$size
                    ),
                    legacy.axes = FALSE) 

ggroc(roc_list)


##AUC strain

st <- (db$strainelasto2/ db$strainbmode2)

roc_st = roc(as.vector(db$outcome),as.matrix(st)) #Conduct the ROC analyses
auc_st = pROC::auc(roc_st) #Calculate the area under the ROC curve
auc_CI_st = pROC::ci.auc(roc_st, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

print(auc_st)
print(auc_CI_st)

str_value <- (db$strainelasto/ db$strainbmode)

roc_str_value = roc(as.vector(db$outcome),as.numeric(str_value)) #Conduct the ROC analyses
auc_str_value = pROC::auc(roc_str_value) #Calculate the area under the ROC curve
auc_CI_str_value = pROC::ci.auc(roc_str_value, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

print(auc_str_value)
print(auc_CI_str_value)



roc_str_value = roc(as.vector(db$outcome),as.numeric(strain_ratio)) #Conduct the ROC analyses
auc_str_value = pROC::auc(roc_str_value) #Calculate the area under the ROC curve
auc_CI_str_value = pROC::ci.auc(roc_str_value, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

print(auc_str_value)
print(auc_CI_str_value)



strain_ratio <- as.factor(
  ifelse(str_value < 1, "Benign", "Malignant"))

strain_binary_prediction <- factor(strain_ratio, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(strain_binary_prediction), factor(db$outcome), positive="Malignant") #confusion matrix









#development
human_usexperts_development <- db_train$final_birads_num

human_usexperts_binary_prediction_development <- as.factor(
  ifelse(human_usexperts_development  ==1, "Benign", "Malignant"))

human_usexperts_binary_prediction_development <- factor(human_usexperts_binary_prediction_development, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_usexperts_binary_prediction_development), factor(db_train$outcome), positive="Malignant") #confusion matrix



swe_golatta_binary_prediction <- as.factor(
  ifelse(
    gtools::na.replace(((db_validation$swe1_lesion +  db_validation$swe2_lesion + db_validation$swe3_lesion)/3), median, na.rm = TRUE)
    < 2.55, 
    "Benign", "Malignant"))

confusionMatrix(as.factor(swe_golatta_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix


swe_berg_binary_prediction <- as.factor(
  ifelse(
    (db_validation$swe1_lesion +  db_validation$swe2_lesion + db_validation$swe3_lesion)/3 
    < 1.8, 
    "Benign", "Malignant"))

confusionMatrix(as.factor(swe_berg_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix

swe_berg_binary_prediction <- as.factor(
  ifelse(
    gtools::na.replace(rowMaxs(cbind(as.matrix(db_validation$swe1_lesion), as.matrix(db_validation$swe2_lesion), as.matrix(db_validation$swe3_lesion)), value=TRUE), median, na.rm=TRUE)
    <= 5.2, 
    "Benign", "Malignant"))

confusionMatrix(as.factor(swe_berg_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix



human <- db_validation$birads

roc_human = roc(as.vector(db_validation$outcome),as.matrix(human)) #Conduct the ROC analyses
auc_human = pROC::auc(roc_human) #Calculate the area under the ROC curve
auc_CI_human = pROC::ci.auc(roc_human, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



swe <- (db_validation$swe1_lesion + db_validation$swe1_lesion + db_validation$swe1_lesion)/3

roc_swe = roc(as.vector(db_validation$outcome),as.matrix(swe)) #Conduct the ROC analyses
auc_swe = pROC::auc(roc_swe) #Calculate the area under the ROC curve
auc_CI_swe = pROC::ci.auc(roc_swe, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

swe_max <- rowMaxs(cbind(as.matrix(db_validation$swe1_lesion), as.matrix(db_validation$swe2_lesion), as.matrix(db_validation$swe3_lesion)), value=TRUE)

roc_swe_max = roc(as.vector(db_validation$outcome),as.matrix(swe_max)) #Conduct the ROC analyses
auc_swe_max = pROC::auc(roc_swe_max) #Calculate the area under the ROC curve
auc_CI_swe_max = pROC::ci.auc(roc_swe_max, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

st <- (db_validation$strainelasto2/ db_validation$strainbmode2)

roc_st = roc(as.vector(db_validation$outcome),as.matrix(st)) #Conduct the ROC analyses
auc_st = pROC::auc(roc_st) #Calculate the area under the ROC curve
auc_CI_st = pROC::ci.auc(roc_st, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



human_binary_prediction <- as.factor(
  ifelse(human  ==1, "Benign", "Malignant"))

human_binary_prediction <- factor(human_binary_prediction, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix


human_usexperts_binary_prediction <- as.factor(
   ifelse(human_usexperts  ==1, "Benign", "Malignant"))

human_usexperts_binary_prediction <- factor(human_usexperts_binary_prediction, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_usexperts_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix

#development
human_usexperts_development <- db_train$final_birads_num

human_usexperts_binary_prediction_development <- as.factor(
  ifelse(human_usexperts_development  ==1, "Benign", "Malignant"))

human_usexperts_binary_prediction_development <- factor(human_usexperts_binary_prediction_development, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_usexperts_binary_prediction_development), factor(db_train$outcome), positive="Malignant") #confusion matrix



swe_golatta_binary_prediction <- as.factor(
   ifelse(
      gtools::na.replace(((db_validation$swe1_lesion +  db_validation$swe2_lesion + db_validation$swe3_lesion)/3), median, na.rm = TRUE)
      < 2.55, 
      "Benign", "Malignant"))

confusionMatrix(as.factor(swe_golatta_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix


swe_berg_binary_prediction <- as.factor(
   ifelse(
      (db_validation$swe1_lesion +  db_validation$swe2_lesion + db_validation$swe3_lesion)/3 
      < 1.8, 
      "Benign", "Malignant"))

confusionMatrix(as.factor(swe_berg_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix

swe_berg_binary_prediction <- as.factor(
   ifelse(
      gtools::na.replace(rowMaxs(cbind(as.matrix(db_validation$swe1_lesion), as.matrix(db_validation$swe2_lesion), as.matrix(db_validation$swe3_lesion)), value=TRUE), median, na.rm=TRUE)
      <= 5.2, 
      "Benign", "Malignant"))

confusionMatrix(as.factor(swe_berg_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix



strain <- as.factor(
  ifelse(db_validation$strain  ==1, "Benign", "Malignant"))

strain_binary_prediction <- factor(strain, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(strain_binary_prediction), factor(db_validation$outcome), positive="Malignant") #confusion matrix



#reduction in biopsies
#golatta
(285 - 227) / 285

#berg
(285 - 227) / 285

#glm
(285 - 227) / 285

#nn
(285 - 205) / 285

#reduction in benign biopsies
#golatta
(159 - 121) / 159

#berg
(159 - 29) / 159

#glm
(159 - 101) / 159

#nn
(159 - 79) / 159



## intra rater reliability 
swe_1_2 <- cor.test(db$swe1_lesion, db$swe2_lesion, 
                method = "pearson")

swe_1_3 <- cor.test(db$swe1_lesion, db$swe3_lesion, 
                    method = "pearson")

swe_2_3 <- cor.test(db$swe2_lesion, db$swe3_lesion, 
                    method = "pearson")



#### (11) algorithm ensemble ####


#### (12) compare  Performance  ####

###AUC ####

# ML vs. human
roc.test(roc_glm_validation, roc_human, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_xgboost_validation, roc_human, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_mars_validation, roc_human, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_svm_validation, roc_human, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_nn_validation, roc_human, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)


roc.test(roc_glm_validation, roc_human_usexperts_full_cohort, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_xgboost_validation, roc_human_usexperts_full_cohort, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_mars_validation, roc_human_usexperts_full_cohort, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_svm_validation, roc_human_usexperts_full_cohort, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
#roc.test(roc_nn_validation, roc_human_usexperts_full_cohort, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_xgboost_validation, roc_human_usexperts, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_glm_validation, roc_human_usexperts, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)


roc.test(roc_glm_validation, roc_swe, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_nn_validation, roc_swe, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_glm_validation, roc_swe_max, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_nn_validation, roc_swe_max, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_swe, roc_swe_max, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)



# ML vs. ML 
roc.test(roc_glm_validation, roc_xgboost_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_glm_validation, roc_mars_validation, method="bootstrap", 
         alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)


roc.test(roc_glm_validation, roc_b_xgboost_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)



roc.test(roc_glm_validation, roc_svm_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_glm_validation, roc_nn_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_xgboost_validation, roc_mars_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_xgboost_validation, roc_svm_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_xgboost_validation, roc_nn_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_mars_validation, roc_svm_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_mars_validation, roc_nn_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_svm_validation, roc_nn_validation, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)


### Sensitivity, Specificity, NPV, PPV ####
#use proportion difference to compare PPVs 
# bmode 126 of 285
# SWE 123 of 244
# iSWE 126 of 227
# iSWE 126 of 205


# SWE  vs bmode
prop.test(x = c(123, 126), n = c(244, 285),
          alternative = "greater")

# iSWE glm vs bmode
prop.test(x = c(126, 126), n = c(227, 285),
          alternative = "greater")

# iSWE nn vs bmode
prop.test(x = c(126, 126), n = c(205, 285),
          alternative = "greater")

# iSWE glm vs SWE
prop.test(x = c(126, 123), n = c(227, 244),
          alternative = "greater")


#use proportion difference to compare rate of unncessary biopsies  
# bmode 159 of 159
# SWE 121 of 159
# iSWE 101 of 159
# iSWE 79 of 159


# SWE  vs bmode
prop.test(x = c(121, 159), n = c(159, 159),
          alternative = "less")

# iSWE glm vs bmode
prop.test(x = c(101, 159), n = c(159, 159),
          alternative = "less")

# iSWE nn vs bmode
prop.test(x = c(79, 159), n = c(159, 159),
          alternative = "less")




##use mcnemar test to compare different models

mcnemar.test( predict(cv_glm , db_validation),
              (human_binary_prediction),
              correct = TRUE)

mcnemar.test( predict(cv_mars , db_validation),
              (human_usexperts_binary_prediction),
              correct = TRUE)

mcnemar.test( predict(cv_nn , db_validation),
              (human_binary_prediction),
              correct = TRUE)

mcnemar.test( swe_golatta_binary_prediction,
              (human_binary_prediction),
              correct = TRUE)

mcnemar.test( swe_berg_binary_prediction,
              (human_binary_prediction),
              correct = TRUE)

mcnemar.test( swe_golatta_binary_prediction,
              predict(cv_glm , db_validation),
              correct = TRUE)

mcnemar.test( swe_berg_binary_prediction,
              predict(cv_glm , db_validation),
              correct = TRUE)


mcnemar.test( swe_golatta_binary_prediction,
              predict(cv_nn , db_validation),
              correct = TRUE)

mcnemar.test( swe_berg_binary_prediction,
              predict(cv_nn , db_validation),
              correct = TRUE)

mcnemar.test( predict(cv_glm , db_validation), 
              predict(cv_nn , db_validation),
              correct = TRUE)


binom.test(79, 159, p = 1,
           alternative = c("two.sided"),
           conf.level = 0.95)



#### (12.1) subgroup analysis ####

## subgroup analysis 

db_validation_3 <- subset(db_validation, (db_validation$birads==1))
db_validation_4a <- subset(db_validation, (db_validation$birads==2))
db_validation_4b <- subset(db_validation, (db_validation$birads==3))
db_validation_4c <- subset(db_validation, (db_validation$birads==4))



#outcome
CrossTable(db_validation_4c$outcome)
CrossTable(db_validation_4a$outcome)
CrossTable(db_validation_4b$outcome)

CrossTable(db_train_4c$outcome)
CrossTable(db_train_4a$outcome)
CrossTable(db_train_4b$outcome)

#



#### AUC ####

##human performance 
human_usexperts_3 <- db_validation_3$final_birads_num
human_usexperts_4a <- db_validation_4a$final_birads_num
human_usexperts_4b <- db_validation_4b$final_birads_num
human_usexperts_4c <- db_validation_4c$final_birads_num



roc_human_usexperts_validation_3 = roc(as.vector(db_validation_3$outcome),as.matrix(human_usexperts_3)) #Conduct the ROC analyses
auc_human_usexperts_validation_3 = pROC::auc(roc_human_usexperts_validation_3) #Calculate the area under the ROC curve
auc_CI_human_usexperts_validation_3 = pROC::ci.auc(roc_human_usexperts_validation_3, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_human_usexperts_validation_4a = roc(as.vector(db_validation_4a$outcome),as.matrix(human_usexperts_4a)) #Conduct the ROC analyses
auc_human_usexperts_validation_4a = pROC::auc(roc_human_usexperts_validation_4a) #Calculate the area under the ROC curve
auc_CI_human_usexperts_validation_4a = pROC::ci.auc(roc_human_usexperts_validation_4a, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_human_usexperts_validation_4b = roc(as.vector(db_validation_4b$outcome),as.matrix(human_usexperts_4b)) #Conduct the ROC analyses
auc_human_usexperts_validation_4b = pROC::auc(roc_human_usexperts_validation_4b) #Calculate the area under the ROC curve
auc_CI_human_usexperts_validation_4b = pROC::ci.auc(roc_human_usexperts_validation_4b, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_human_usexperts_validation_4c = roc(as.vector(db_validation_4c$outcome),as.matrix(human_usexperts_4c)) #Conduct the ROC analyses
auc_human_usexperts_validation_4c = pROC::auc(roc_human_usexperts_validation_4c) #Calculate the area under the ROC curve
auc_CI_human_usexperts_validation_4c = pROC::ci.auc(roc_human_usexperts_validation_4c, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


## ich bekomme einen Type Error => predict => kein factor

roc_glm_validation_4a = roc(as.vector(db_validation_4a$outcome),as.matrix(predict(cv_glm , db_validation_4a, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_glm_validation_4a = pROC::auc(roc_glm_validation_4a) #Calculate the area under the ROC curve
auc_CI_glm_validation_4a = pROC::ci.auc(roc_glm_validation_4a, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_glm_validation_4b = roc(as.vector(db_validation_4b$outcome),as.matrix(predict(cv_glm , db_validation_4b, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_glm_validation_4b = pROC::auc(roc_glm_validation_4b) #Calculate the area under the ROC curve
auc_CI_glm_validation_4b = pROC::ci.auc(roc_glm_validation_4b, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_glm_validation_4c = roc(as.vector(db_validation_4c$outcome),as.matrix(predict(cv_glm , db_validation_4c, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_glm_validation_4c = pROC::auc(roc_glm_validation_4c) #Calculate the area under the ROC curve
auc_CI_glm_validation_4c = pROC::ci.auc(roc_glm_validation_4c, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



roc_xgboost_validation_4a = roc(as.vector(db_validation_4a$outcome),as.matrix(predict(cv_xgboost , db_validation_4a, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_xgboost_validation_4a = pROC::auc(roc_xgboost_validation_4a) #Calculate the area under the ROC curve
auc_CI_xgboost_validation_4a = pROC::ci.auc(roc_xgboost_validation_4a, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_xgboost_validation_4c = roc(as.vector(db_validation_4c$outcome),as.matrix(predict(cv_xgboost , db_validation_4c, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_xgboost_validation_4c = pROC::auc(roc_xgboost_validation_4c) #Calculate the area under the ROC curve
auc_CI_xgboost_validation_4c = pROC::ci.auc(roc_xgboost_validation_4c, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_xgboost_validation_4b = roc(as.vector(db_validation_4b$outcome),as.matrix(predict(cv_xgboost , db_validation_4b, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_xgboost_validation_4b = pROC::auc(roc_xgboost_validation_4b) #Calculate the area under the ROC curve
auc_CI_xgboost_validation_4b = pROC::ci.auc(roc_xgboost_validation_4b, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



roc_svm_validation_4a = roc(as.vector(db_validation_4a$outcome),as.matrix(predict(cv_svm , db_validation_4a, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_svm_validation_4a = pROC::auc(roc_svm_validation_4a) #Calculate the area under the ROC curve
auc_CI_svm_validation_4a = pROC::ci.auc(roc_svm_validation_4a, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_svm_validation_4c = roc(as.vector(db_validation_4c$outcome),as.matrix(predict(cv_svm , db_validation_4c, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_svm_validation_4c = pROC::auc(roc_svm_validation_4c) #Calculate the area under the ROC curve
auc_CI_svm_validation_4c = pROC::ci.auc(roc_svm_validation_4c, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_svm_validation_4b = roc(as.vector(db_validation_4b$outcome),as.matrix(predict(cv_svm , db_validation_4b, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_svm_validation_4b = pROC::auc(roc_svm_validation_4b) #Calculate the area under the ROC curve
auc_CI_svm_validation_4b = pROC::ci.auc(roc_svm_validation_4b, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



roc_nn_validation_4a = roc(as.vector(db_validation_4a$outcome),as.matrix(predict(cv_nn , db_validation_4a, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_nn_validation_4a = pROC::auc(roc_nn_validation_4a) #Calculate the area under the ROC curve
auc_CI_nn_validation_4a = pROC::ci.auc(roc_nn_validation_4a, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_nn_validation_4c = roc(as.vector(db_validation_4c$outcome),as.matrix(predict(cv_nn , db_validation_4c, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_nn_validation_4c = pROC::auc(roc_nn_validation_4c) #Calculate the area under the ROC curve
auc_CI_nn_validation_4c = pROC::ci.auc(roc_nn_validation_4c, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_nn_validation_4b = roc(as.vector(db_validation_4b$outcome),as.matrix(predict(cv_nn , db_validation_4b, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_nn_validation_4b = pROC::auc(roc_nn_validation_4b) #Calculate the area under the ROC curve
auc_CI_nn_validation_4b = pROC::ci.auc(roc_nn_validation_4b, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



swe_4a <- (db_validation_4a$swe1_lesion + db_validation_4a$swe1_lesion + db_validation_4a$swe1_lesion)/3
swe_4b <- (db_validation_4b$swe1_lesion + db_validation_4b$swe1_lesion + db_validation_4b$swe1_lesion)/3
swe_4c <- (db_validation_4c$swe1_lesion + db_validation_4c$swe1_lesion + db_validation_4c$swe1_lesion)/3



roc_swe_validation_4a = roc(as.vector(db_validation_4a$outcome),as.matrix(swe_4a)) #Conduct the ROC analyses
auc_swe_validation_4a = pROC::auc(roc_swe_validation_4a) #Calculate the area under the ROC curve
auc_CI_swe_validation_4a = pROC::ci.auc(roc_swe_validation_4a, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_swe_validation_4b = roc(as.vector(db_validation_4b$outcome),as.matrix(swe_4b)) #Conduct the ROC analyses
auc_swe_validation_4b = pROC::auc(roc_swe_validation_4b) #Calculate the area under the ROC curve
auc_CI_swe_validation_4b = pROC::ci.auc(roc_swe_validation_4b, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve

roc_swe_validation_4c = roc(as.vector(db_validation_4c$outcome),as.matrix(swe_4c)) #Conduct the ROC analyses
auc_swe_validation_4c = pROC::auc(roc_swe_validation_4c) #Calculate the area under the ROC curve
auc_CI_swe_validation_4c = pROC::ci.auc(roc_swe_validation_4c, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve




#validation
glm_binary_prediction_validation_4a <-
   ifelse(predict(cv_glm , db_validation_4a, type = "prob")$"Malignant" 
          >= glm_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(glm_binary_prediction_validation_4a), factor(db_validation_4a$outcome), positive="Malignant") #confusion matrix


glm_binary_prediction_validation_4c <-
   ifelse(predict(cv_glm , db_validation_4c, type = "prob")$"Malignant" 
          >= glm_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(glm_binary_prediction_validation_4c), factor(db_validation_4c$outcome), positive="Malignant") #confusion matrix


glm_binary_prediction_validation_4b <-
   ifelse(predict(cv_glm , db_validation_4b, type = "prob")$"Malignant" 
          >= glm_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(glm_binary_prediction_validation_4b), factor(db_validation_4b$outcome), positive="Malignant") #confusion matrix





xgboost_binary_prediction_validation_4a <-
   ifelse(predict(cv_xgboost , db_validation_4a, type = "prob")$"Malignant" 
          >= xgboost_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(xgboost_binary_prediction_validation_4a), factor(db_validation_4a$outcome), positive="Malignant") #confusion matrix


xgboost_binary_prediction_validation_4c <-
   ifelse(predict(cv_xgboost , db_validation_4c, type = "prob")$"Malignant" 
          >= xgboost_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(xgboost_binary_prediction_validation_4c), factor(db_validation_4c$outcome), positive="Malignant") #confusion matrix


xgboost_binary_prediction_validation_4b <-
   ifelse(predict(cv_xgboost , db_validation_4b, type = "prob")$"Malignant" 
          >= xgboost_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(xgboost_binary_prediction_validation_4b), factor(db_validation_4b$outcome), positive="Malignant") #confusion matrix





svm_binary_prediction_validation_4a <-
   ifelse(predict(cv_svm , db_validation_4a, type = "prob")$"Malignant" 
          >= svm_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(svm_binary_prediction_validation_4a), factor(db_validation_4a$outcome), positive="Malignant") #confusion matrix


svm_binary_prediction_validation_4c <-
   ifelse(predict(cv_svm , db_validation_4c, type = "prob")$"Malignant" 
          >= svm_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(svm_binary_prediction_validation_4c), factor(db_validation_4c$outcome), positive="Malignant") #confusion matrix


svm_binary_prediction_validation_4b <-
   ifelse(predict(cv_svm , db_validation_4b, type = "prob")$"Malignant" 
          >= svm_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(svm_binary_prediction_validation_4b), factor(db_validation_4b$outcome), positive="Malignant") #confusion matrix





nn_binary_prediction_validation_4a <-
   ifelse(predict(cv_nn , db_validation_4a, type = "prob")$"Malignant" 
          >= nn_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(nn_binary_prediction_validation_4a), factor(db_validation_4a$outcome), positive="Malignant") #confusion matrix


nn_binary_prediction_validation_4c <-
   ifelse(predict(cv_nn , db_validation_4c, type = "prob")$"Malignant" 
          >= nn_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(nn_binary_prediction_validation_4c), factor(db_validation_4c$outcome), positive="Malignant") #confusion matrix


nn_binary_prediction_validation_4b <-
   ifelse(predict(cv_nn , db_validation_4b, type = "prob")$"Malignant" 
          >= nn_cutoff ,
          "Malignant",
          "Benign")

confusionMatrix(as.factor(nn_binary_prediction_validation_4b), factor(db_validation_4b$outcome), positive="Malignant") #confusion matrix







human_4a <- db_validation_4a$birads
human_4b <- db_validation_4b$birads
human_4c <- db_validation_4c$birads


human_binary_prediction_4a <- as.factor(
   ifelse(human_4a  ==1, "Benign", "Malignant"))

human_binary_prediction_4a <- factor(human_binary_prediction_4a, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_binary_prediction_4a), factor(db_validation_4a$outcome), positive="Malignant") #confusion matrix


human_binary_prediction_4b <- as.factor(
   ifelse(human_4b  ==1, "Benign", "Malignant"))

human_binary_prediction_4b <- factor(human_binary_prediction_4b, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_binary_prediction_4b), factor(db_validation_4b$outcome), positive="Malignant") #confusion matrix


human_binary_prediction_4c <- as.factor(
   ifelse(human_4c  ==1, "Benign", "Malignant"))

human_binary_prediction_4c <- factor(human_binary_prediction_4c, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(human_binary_prediction_4c), factor(db_validation_4c$outcome), positive="Malignant") #confusion matrix




swe_golatta_binary_prediction_4a <- as.factor(
   ifelse(
      gtools::na.replace(((db_validation_4a$swe1_lesion +  db_validation_4a$swe2_lesion + db_validation_4a$swe3_lesion)/3), median, na.rm = TRUE)
      < 2.55, 
      "Benign", "Malignant"))

confusionMatrix(as.factor(swe_golatta_binary_prediction_4a), factor(db_validation_4a$outcome), positive="Malignant") #confusion matrix



swe_golatta_binary_prediction_4b <- as.factor(
   ifelse(
      gtools::na.replace(((db_validation_4b$swe1_lesion +  db_validation_4b$swe2_lesion + db_validation_4b$swe3_lesion)/3), median, na.rm = TRUE)
      < 2.55, 
      "Benign", "Malignant"))

confusionMatrix(as.factor(swe_golatta_binary_prediction_4b), factor(db_validation_4b$outcome), positive="Malignant") #confusion matrix



swe_golatta_binary_prediction_4c <- as.factor(
   ifelse(
      gtools::na.replace(((db_validation_4c$swe1_lesion +  db_validation_4c$swe2_lesion + db_validation_4c$swe3_lesion)/3), median, na.rm = TRUE)
      < 2.55, 
      "Benign", "Malignant"))

confusionMatrix(as.factor(swe_golatta_binary_prediction_4c), factor(db_validation_4c$outcome), positive="Malignant") #confusion matrix




###pictures
images_swe_fp <- subset(db_validation, 
                 (db_validation$outcome == "Benign" 
                 & (gtools::na.replace(((db_validation$swe1_lesion +  db_validation$swe2_lesion + db_validation$swe3_lesion)/3), median, na.rm = TRUE) >= 2.55 )
                 & (predict(cv_nn , db_validation,type="prob")$"Malignant" < nn_cutoff))
                 )


images_swe_fn <- subset(db_validation, 
                        (db_validation$outcome == "Malignant" 
                         & (gtools::na.replace(((db_validation$swe1_lesion +  db_validation$swe2_lesion + db_validation$swe3_lesion)/3), median, na.rm = TRUE) < 2.55 )
                         & (predict(cv_nn , db_validation,type="prob")$"Malignant" >= nn_cutoff))
)



#### (13) traditional LR ####

db_lr <- db
db$BIRADS <- NULL

names(db)
model_logreg <- glm (
  outcome ~  Age +  factor(Shape) + factor(Margin) +    factor (Density), 
  data=db_lr, family = binomial
) 

summary(model_logreg)

#95% CI for coefficients
confint(model_logreg)
#OR + 95% CIs
exp(cbind(OR = coef(model_logreg), confint(model_logreg)))

#### (14) compare dataset ####

###dataset size

## total case count
case_count_total <- sum(db$outcome == "Malignant",db$outcome == "Benign")
print(case_count_total)

##development set size
case_count_total_train <- sum(db_train$outcome == "Malignant",db_train$outcome == "Benign")
print(case_count_total_train)

##validation set size
case_count_total_validation <- sum(db_validation$outcome == "Malignant",db_validation$outcome == "Benign")
print(case_count_total_validation)

##malignant cases
malignant_count_total <- sum(db$outcome == "Malignant")
print(malignant_count_total)


malignant_count_percentage <- malignant_count_total/case_count_total
print(malignant_count_percentage)

malignant_count_training <- sum(db_train$outcome == "Malignant")
print(malignant_count_training)

malignant_count_validation <- sum(db_validation$outcome == "Malignant")
print(malignant_count_validation)

##birads distribution

#birads 3
birads_3_count <- sum(db$birads == "1")
print(birads_3_count)

birads_3_count_percentage <- birads_3_count/case_count_total
print(birads_3_count_percentage)

#birads 4a
birads_4a_count <- sum(db$birads == "2")
print(birads_4a_count)

birads_4a_count_percentage <- birads_4a_count/case_count_total
print(birads_4a_count_percentage)


#birads 4b

birads_4b_count <- sum(db$birads == "3")
print(birads_4b_count)

birads_4b_count_percentage <- birads_4b_count/case_count_total
print(birads_4b_count_percentage)


#birads 4c
birads_4c_count <- sum(db$birads == "4")
print(birads_4c_count)

birads_4c_count_percentage <- birads_4c_count/case_count_total
print(birads_4c_count_percentage)

#### compare development and validation set ####
mean(db$age)
sd(db$age)

mean(db_train$age)
sd(db_train$age)

mean(db_validation$age)
sd(db_validation$age)

t.test(db_train$age, db_validation$age)
sd(db_train$age)
sd(db_validation$age)


table(is.na(db$birads))
table(is.na(db_train$birads))
table(is.na(db_validation$birads))

11/857
9/572
2/285

t.test(db_train$swe1_lesion, db_validation$swe1_lesion)
sd(db_train$swe1_lesion, na.rm=TRUE)
sd(db_validation$swe1_lesion, na.rm=TRUE)
mean(db$swe1_lesion, na.rm=TRUE)
sd(db$swe1_lesion, na.rm=TRUE)

t.test(db_train$swe2_lesion, db_validation$swe2_lesion)
sd(db_train$swe2_lesion, na.rm=TRUE)
sd(db_validation$swe2_lesion, na.rm=TRUE)
mean(db$swe2_lesion, na.rm=TRUE)
sd(db$swe2_lesion, na.rm=TRUE)

t.test(db_train$swe3_lesion, db_validation$swe3_lesion)
sd(db_train$swe3_lesion, na.rm=TRUE)
sd(db_validation$swe3_lesion, na.rm=TRUE)
mean(db$swe3_lesion, na.rm=TRUE)
sd(db$swe3_lesion, na.rm=TRUE)


t.test(db_train$axis, db_validation$axis)
sd(db_train$axis)
sd(db_validation$axis)

t.test(db_train$perpend, db_validation$perpend)
sd(db_train$perpend)
sd(db_validation$perpend)

t.test(db_train$ortho, db_validation$ortho)
sd(db_train$ortho)
sd(db_validation$ortho)

CrossTable(db$outcome, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

missing_db <- table(is.na(db$outcome))["TRUE"]
missing_db_train <- table(is.na(db_train$outcome))["TRUE"]
missing_db_validation <- table(is.na(db_validation$outcome))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort
percentage_missing_db_train <- missing_db_train/number_total_cohort
percentage_missing_db_validation <- missing_db_validation/number_total_cohort

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)


#### palpability
CrossTable(db$palpability, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

#build the contingency table 
contingency_table <- table(db$palpability, db$site == "1")
print(contingency_table)

#seperate the column values by study centre
col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
row_totals <- rowSums(contingency_table)
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)




#hier hakts, ich brauche einen numerischen Wert für row_totals, damit einen prozentualen Anteil berechnen kann.
number_negative_palpability <- cat("total cohort negative palpability:", row_totals["no"], "\n")
percentage_negative_palpability_total_cohort <- number_negative_palpability/ number_total_cohort 
print(percentage_negative_palpability_total_cohort)

cat("total cohort positive palpability:", row_totals["yes"], "\n")

negative_train_palpability <- contingency_table[1, 1]
cat("train negative palpability:", negative_train_palpability, "\n")
train_negative_percentage <- negative_train/col_totals_train
cat("train negative palpability percentage:", train_negative_percentage)


##this parts works

negative_train_palpability <- contingency_table[1, 1]
train_negative_percentage <- negative_train_palpability/col_totals_train
cat("train negative palpability:", negative_train_palpability, ", train negative palpability percentage:", train_negative_percentage)


positive_train_palpability <- contingency_table[2, 1]
train_positive_percentage <- positive_train_palpability/col_totals_train
cat("train positive palpability:", positive_train_palpability, ", train positive palpability percentage:", train_positive_percentage)


negative_validation_palpability <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_palpability/col_totals_validation
cat("train negative palpability:", negative_validation_palpability, ", train negative palpability percentage:", train_negative_percentage)

positive_validation_palpability <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_palpability/col_totals_validation
cat("train positive palpability:", positive_validation_palpability, ", train positive palpability percentage:", train_positive_percentage)


#calculate the number of missing values

missing_db <- table(is.na(db$palpability))["TRUE"]
missing_db_train <- table(is.na(db_train$palpability))["TRUE"]
missing_db_validation <- table(is.na(db_validation$palpability))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort
percentage_missing_db_train <- missing_db_train/number_total_cohort
percentage_missing_db_validation <- missing_db_validation/number_total_cohort

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)

####

CrossTable(db$ultrasound_orientation, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

#copy and repeat this center code
contingency_table <- table(db$ultrasound_orientation, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


negative_train_ultrasound_orientation <- contingency_table[1, 1]
train_negative_percentage <- negative_train_ultrasound_orientation/col_totals_train
cat("train negative ultrasound_orientation:", negative_train_ultrasound_orientation, ", train negative ultrasound_orientation percentage:", train_negative_percentage)


positive_train_ultrasound_orientation <- contingency_table[2, 1]
train_positive_percentage <- positive_train_ultrasound_orientation/col_totals_train
cat("train positive ultrasound_orientation:", positive_train_ultrasound_orientation, ", train positive ultrasound_orientation percentage:", train_positive_percentage)


negative_validation_ultrasound_orientation <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_ultrasound_orientation/col_totals_validation
cat("train negative ultrasound_orientation:", negative_validation_ultrasound_orientation, ", train negative ultrasound_orientation percentage:", train_negative_percentage)

positive_validation_ultrasound_orientation <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_ultrasound_orientation/col_totals_validation
cat("train positive ultrasound_orientation:", positive_validation_ultrasound_orientation, ", train positive ultrasound_orientation percentage:", train_positive_percentage)


missing_db <- table(is.na(db$ultrasound_orientation))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_orientation))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_orientation))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort*100
percentage_missing_db_train <- missing_db_train/number_total_cohort*100
percentage_missing_db_validation <- missing_db_validation/number_total_cohort*100

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)


##ultrasound margin


CrossTable(db$ultrasound_margin, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

#copy and repeat this center code
contingency_table <- table(db$ultrasound_margin, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


negative_train_ultrasound_margin <- contingency_table[1, 1]
train_negative_percentage <- negative_train_ultrasound_margin/col_totals_train
cat("train negative ultrasound_margin:", negative_train_ultrasound_margin, ", train negative ultrasound_margin percentage:", train_negative_percentage)


positive_train_ultrasound_margin <- contingency_table[2, 1]
train_positive_percentage <- positive_train_ultrasound_margin/col_totals_train
cat("train positive ultrasound_margin:", positive_train_ultrasound_margin, ", train positive ultrasound_margin percentage:", train_positive_percentage)


negative_validation_ultrasound_margin <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_ultrasound_margin/col_totals_validation
cat("train negative ultrasound_margin:", negative_validation_ultrasound_margin, ", train negative ultrasound_margin percentage:", train_negative_percentage)

positive_validation_ultrasound_margin <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_ultrasound_margin/col_totals_validation
cat("train positive ultrasound_margin:", positive_validation_ultrasound_margin, ", train positive ultrasound_margin percentage:", train_positive_percentage)


missing_db <- table(is.na(db$ultrasound_margin))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_margin))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_margin))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort
percentage_missing_db_train <- missing_db_train/number_total_cohort
percentage_missing_db_validation <- missing_db_validation/number_total_cohort

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)



CrossTable(db$ultrasound_margin_indistinct, db$site=="1", chisq = TRUE, prop.chisq = TRUE)


#copy and repeat this center code
contingency_table <- table(db$ultrasound_margin_indistinct, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


negative_train_ultrasound_margin_indistinct <- contingency_table[1, 1]
train_negative_percentage <- negative_train_ultrasound_margin_indistinct/col_totals_train
cat("train negative ultrasound_margin_indistinct:", negative_train_ultrasound_margin_indistinct, ", train negative ultrasound_margin_indistinct percentage:", train_negative_percentage)


positive_train_ultrasound_margin_indistinct <- contingency_table[2, 1]
train_positive_percentage <- positive_train_ultrasound_margin_indistinct/col_totals_train
cat("train positive ultrasound_margin_indistinct:", positive_train_ultrasound_margin_indistinct, ", train positive ultrasound_margin_indistinct percentage:", train_positive_percentage)


negative_validation_ultrasound_margin_indistinct <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_ultrasound_margin_indistinct/col_totals_validation
cat("validation negative ultrasound_margin_indistinct:", negative_validation_ultrasound_margin_indistinct, ", train negative ultrasound_margin_indistinct percentage:", train_negative_percentage)

positive_validation_ultrasound_margin_indistinct <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_ultrasound_margin_indistinct/col_totals_validation
cat("validation positive ultrasound_margin_indistinct:", positive_validation_ultrasound_margin_indistinct, ", train positive ultrasound_margin_indistinct percentage:", train_positive_percentage)


missing_db <- table(is.na(db$ultrasound_margin_indistinct))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_margin_indistinct))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_margin_indistinct))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort
percentage_missing_db_train <- missing_db_train/number_total_cohort
percentage_missing_db_validation <- missing_db_validation/number_total_cohort

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)




CrossTable(db$ultrasound_margin_angular, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

#copy and repeat this center code
contingency_table <- table(db$ultrasound_margin_angular, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


negative_train_ultrasound_margin_angular <- contingency_table[1, 1]
train_negative_percentage <- negative_train_ultrasound_margin_angular/col_totals_train
cat("train negative ultrasound_margin_angular:", negative_train_ultrasound_margin_angular, ", train negative ultrasound_margin_angular percentage:", train_negative_percentage)


positive_train_ultrasound_margin_angular <- contingency_table[2, 1]
train_positive_percentage <- positive_train_ultrasound_margin_angular/col_totals_train
cat("train positive ultrasound_margin_angular:", positive_train_ultrasound_margin_angular, ", train positive ultrasound_margin_angular percentage:", train_positive_percentage)


negative_validation_ultrasound_margin_angular <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_ultrasound_margin_angular/col_totals_validation
cat("validation negative ultrasound_margin_angular:", negative_validation_ultrasound_margin_angular, ", train negative ultrasound_margin_angular percentage:", train_negative_percentage)

positive_validation_ultrasound_margin_angular <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_ultrasound_margin_angular/col_totals_validation
cat("validation positive ultrasound_margin_angular:", positive_validation_ultrasound_margin_angular, ", train positive ultrasound_margin_angular percentage:", train_positive_percentage)


missing_db <- table(is.na(db$ultrasound_margin_angular))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_margin_angular))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_margin_angular))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort*100
percentage_missing_db_train <- missing_db_train/number_total_cohort*100
percentage_missing_db_validation <- missing_db_validation/number_total_cohort*100

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)



CrossTable(db$ultrasound_margin_microlobulated, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

#copy and repeat this center code
contingency_table <- table(db$ultrasound_margin_microlobulated, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


negative_train_ultrasound_margin_microlobulated <- contingency_table[1, 1]
train_negative_percentage <- negative_train_ultrasound_margin_microlobulated/col_totals_train
cat("train negative ultrasound_margin_microlobulated:", negative_train_ultrasound_margin_microlobulated, ", train negative ultrasound_margin_microlobulated percentage:", train_negative_percentage)


positive_train_ultrasound_margin_microlobulated <- contingency_table[2, 1]
train_positive_percentage <- positive_train_ultrasound_margin_microlobulated/col_totals_train
cat("train positive ultrasound_margin_microlobulated:", positive_train_ultrasound_margin_microlobulated, ", train positive ultrasound_margin_microlobulated percentage:", train_positive_percentage)


negative_validation_ultrasound_margin_microlobulated <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_ultrasound_margin_microlobulated/col_totals_validation
cat("validation negative ultrasound_margin_microlobulated:", negative_validation_ultrasound_margin_microlobulated, ", train negative ultrasound_margin_microlobulated percentage:", train_negative_percentage)

positive_validation_ultrasound_margin_microlobulated <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_ultrasound_margin_microlobulated/col_totals_validation
cat("validation positive ultrasound_margin_microlobulated:", positive_validation_ultrasound_margin_microlobulated, ", train positive ultrasound_margin_microlobulated percentage:", train_positive_percentage)


missing_db <- table(is.na(db$ultrasound_margin_microlobulated))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_margin_microlobulated))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_margin_microlobulated))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort*100
percentage_missing_db_train <- missing_db_train/number_total_cohort*100
percentage_missing_db_validation <- missing_db_validation/number_total_cohort*100

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)



CrossTable(db$ultrasound_margin_spiculated, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

#copy and repeat this center code
contingency_table <- table(db$ultrasound_margin_spiculated, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


negative_train_ultrasound_margin_spiculated <- contingency_table[1, 1]
train_negative_percentage <- negative_train_ultrasound_margin_spiculated/col_totals_train
cat("train negative ultrasound_margin_spiculated:", negative_train_ultrasound_margin_spiculated, ", train negative ultrasound_margin_spiculated percentage:", train_negative_percentage)


positive_train_ultrasound_margin_spiculated <- contingency_table[2, 1]
train_positive_percentage <- positive_train_ultrasound_margin_spiculated/col_totals_train
cat("train positive ultrasound_margin_spiculated:", positive_train_ultrasound_margin_spiculated, ", train positive ultrasound_margin_spiculated percentage:", train_positive_percentage)


negative_validation_ultrasound_margin_spiculated <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_ultrasound_margin_spiculated/col_totals_validation
cat("validation negative ultrasound_margin_spiculated:", negative_validation_ultrasound_margin_spiculated, ", train negative ultrasound_margin_spiculated percentage:", train_negative_percentage)

positive_validation_ultrasound_margin_spiculated <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_ultrasound_margin_spiculated/col_totals_validation
cat("validation positive ultrasound_margin_spiculated:", positive_validation_ultrasound_margin_spiculated, ", train positive ultrasound_margin_spiculated percentage:", train_positive_percentage)


missing_db <- table(is.na(db$ultrasound_margin_spiculated))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_margin_spiculated))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_margin_spiculated))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort
percentage_missing_db_train <- missing_db_train/number_total_cohort
percentage_missing_db_validation <- missing_db_validation/number_total_cohort

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)



CrossTable(db$ultrasound_calcification, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

#copy and repeat this center code
contingency_table <- table(db$ultrasound_calcification, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


negative_train_ultrasound_calcification <- contingency_table[1, 1]
train_negative_percentage <- negative_train_ultrasound_calcification/col_totals_train
cat("train negative ultrasound_calcification:", negative_train_ultrasound_calcification, ", train negative ultrasound_calcification percentage:", train_negative_percentage)


positive_train_ultrasound_calcification <- contingency_table[2, 1]
train_positive_percentage <- positive_train_ultrasound_calcification/col_totals_train
cat("train positive ultrasound_calcification:", positive_train_ultrasound_calcification, ", train positive ultrasound_calcification percentage:", train_positive_percentage)


negative_validation_ultrasound_calcification <- contingency_table[1, 2]
train_negative_percentage <- negative_validation_ultrasound_calcification/col_totals_validation
cat("validation negative ultrasound_calcification:", negative_validation_ultrasound_calcification, ", train negative ultrasound_calcification percentage:", train_negative_percentage)

positive_validation_ultrasound_calcification <- contingency_table[2, 2]
train_positive_percentage <- positive_validation_ultrasound_calcification/col_totals_validation
cat("validation positive ultrasound_calcification:", positive_validation_ultrasound_calcification, ", train positive ultrasound_calcification percentage:", train_positive_percentage)


missing_db <- table(is.na(db$ultrasound_calcification))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_calcification))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_calcification))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort*100
percentage_missing_db_train <- missing_db_train/number_total_cohort*100
percentage_missing_db_validation <- missing_db_validation/number_total_cohort*100

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)

###ultrasound tissue
CrossTable(db$ultrasound_tissue=="homogeneous_fat", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$ultrasound_tissue=="heterogeneous", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$ultrasound_tissue=="homogeneous_fibroglandular", db$site=="1", chisq = TRUE, prop.chisq = TRUE)

contingency_table <- table(db$ultrasound_tissue, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


homogeneous_fat_train <- contingency_table[1, 1]
homogeneous_fat_train_percentage <- homogeneous_fat_train/col_totals_train
cat("homogeneous_fat_train:", homogeneous_fat_train, ", homogeneous_fat_train percentage:", homogeneous_fat_train_percentage)

heterogenous_fat_train <- contingency_table[2, 1]
heterogenous_fat_train_percentage <- heterogenous_fat_train/col_totals_train
cat("heterogenous_fat_train:", heterogenous_fat_train, ", heterogenous_fat_train percentage:", heterogenous_fat_train_percentage)

homogeneous_fibroglandular_train <- contingency_table[3, 1]
homogeneous_fibroglandular_train_percentage <- homogeneous_fibroglandular_train/col_totals_train
cat("homogeneous_fibroglandular_train:", homogeneous_fibroglandular_train, ", homogeneous_fibroglandular_train percentage:", homogeneous_fibroglandular_train_percentage)


homogeneous_fat_validation <- contingency_table[1, 2]
homogeneous_fat_validation_percentage <- homogeneous_fat_validation/col_totals_validation
cat("homogeneous_fat_validation:", homogeneous_fat_validation, ", homogeneous_fat_validation percentage:", homogeneous_fat_validation_percentage)

heterogenous_fat_validation <- contingency_table[2, 2]
heterogenous_fat_validation_percentage <- heterogenous_fat_validation/col_totals_validation
cat("heterogenous_fat_validation:", heterogenous_fat_validation, ", heterogenous_fat_validation percentage:", heterogenous_fat_validation_percentage)

homogeneous_fibroglandular_validation <- contingency_table[3, 2]
homogeneous_fibroglandular_validation_percentage <- homogeneous_fibroglandular_validation/col_totals_validation
cat("homogeneous_fibroglandular_validation:", homogeneous_fibroglandular_validation, ", homogeneous_fibroglandular_validation percentage:", homogeneous_fibroglandular_validation_percentage)


missing_db <- table(is.na(db$ultrasound_tissue))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_tissue))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_tissue))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort*100
percentage_missing_db_train <- missing_db_train/number_total_cohort*100
percentage_missing_db_validation <- missing_db_validation/number_total_cohort*100

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)




### ultrasound shape 

CrossTable(db$ultrasound_shape=="oval", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$ultrasound_shape=="round", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$ultrasound_shape=="irregular", db$site=="1", chisq = TRUE, prop.chisq = TRUE)

contingency_table <- table(db$ultrasound_shape, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


oval_shape_train <- contingency_table[1, 1]
oval_shape_train_percentage <- oval_shape_train/col_totals_train
cat("oval_shape_train:", oval_shape_train, ", oval_shape_train percentage:", oval_shape_train_percentage)

round_shape_train <- contingency_table[2, 1]
round_shape_train_percentage <- round_shape_train/col_totals_train
cat("round_shape_train:", round_shape_train, ", round_shape_train percentage:", round_shape_train_percentage)

irregular_shape_train <- contingency_table[3, 1]
irregular_shape_train_percentage <- irregular_shape_train/col_totals_train
cat("irregular_shape_train:", irregular_shape_train, ", irregular_shape_train percentage:", irregular_shape_train_percentage)



####


oval_shape_validation <- contingency_table[1, 2]
oval_shape_validation_percentage <- oval_shape_validation/col_totals_validation
cat("oval_shape_validation:", oval_shape_validation, ", oval_shape_validation percentage:", oval_shape_validation_percentage)

round_shape_validation <- contingency_table[2, 2]
round_shape_validation_percentage <- round_shape_validation/col_totals_validation
cat("round_shape_validation:", round_shape_validation, ", round_shape_validation percentage:", round_shape_validation_percentage)

irregular_shape_validation <- contingency_table[3, 2]
irregular_shape_validation_percentage <- irregular_shape_validation/col_totals_validation
cat("irregular_shape_validation:", irregular_shape_validation, ", irregular_shape_validation percentage:", irregular_shape_validation_percentage)

missing_db <- table(is.na(db$ultrasound_shape))["TRUE"]
missing_db_train <- table(is.na(db_train$ultrasound_shape))["TRUE"]
missing_db_validation <- table(is.na(db_validation$ultrasound_shape))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort*100
percentage_missing_db_train <- missing_db_train/number_total_cohort*100
percentage_missing_db_validation <- missing_db_validation/number_total_cohort*100

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)

###


#echogenity
CrossTable(db$echo, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

contingency_table <- table(db$echo, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)

##echos train
echo_anechoic_train <- contingency_table[1, 1]
echo_anechoic_train_percentage <- echo_anechoic_train/col_totals_train
cat("echo_anechoic_train:", echo_anechoic_train, ", echo_anechoic_train percentage:", echo_anechoic_train_percentage)

echo_hyper_train <- contingency_table[2, 1]
echo_hyper_train_percentage <- echo_hyper_train/col_totals_train
cat("echo_hyper_train:", echo_hyper_train, ", echo_hyper_train percentage:", echo_hyper_train_percentage)

echo_comp_cyst_sol_train <- contingency_table[3, 1]
echo_comp_cyst_sol_train_percentage <- echo_comp_cyst_sol_train/col_totals_train
cat("echo_comp_cyst_sol_train:", echo_comp_cyst_sol_train, ", echo_comp_cyst_sol_train percentage:", echo_comp_cyst_sol_train_percentage)

echo_hypo_train <- contingency_table[4, 1]
echo_hypo_train_percentage <- echo_hypo_train/col_totals_train
cat("echo_hypo_train:", echo_hypo_train, ", echo_hypo_train percentage:", echo_hypo_train_percentage)

echo_iso_train <- contingency_table[5, 1]
echo_iso_train_percentage <- echo_iso_train/col_totals_train
cat("echo_iso_train:", echo_iso_train, ", echo_iso_train percentage:", echo_iso_train_percentage)

echo_hetero_train <- contingency_table[6, 1]
echo_hetero_train_percentage <- echo_hetero_train/col_totals_train
cat("echo_hetero_train:", echo_hetero_train, ", echo_hetero_train percentage:", echo_hetero_train_percentage)


##echos validation
echo_anechoic_validation <- contingency_table[1, 2]
echo_anechoic_validation_percentage <- echo_anechoic_validation/col_totals_validation
cat("echo_anechoic_validation:", echo_anechoic_validation, ", echo_anechoic_validation percentage:", echo_anechoic_validation_percentage)

echo_hyper_validation <- contingency_table[2, 2]
echo_hyper_validation_percentage <- echo_hyper_validation/col_totals_validation
cat("echo_hyper_validation:", echo_hyper_validation, ", echo_hyper_validation percentage:", echo_hyper_validation_percentage)

echo_comp_cyst_sol_validation <- contingency_table[3, 2]
echo_comp_cyst_sol_validation_percentage <- echo_comp_cyst_sol_validation/col_totals_validation
cat("echo_comp_cyst_sol_validation:", echo_comp_cyst_sol_validation, ", echo_comp_cyst_sol_validation percentage:", echo_comp_cyst_sol_validation_percentage)

echo_hypo_validation <- contingency_table[4, 2]
echo_hypo_validation_percentage <- echo_hypo_validation/col_totals_validation
cat("echo_hypo_validation:", echo_hypo_validation, ", echo_hypo_validation percentage:", echo_hypo_validation_percentage)

echo_iso_validation <- contingency_table[5, 2]
echo_iso_validation_percentage <- echo_iso_validation/col_totals_validation
cat("echo_iso_validation:", echo_iso_validation, ", echo_iso_validation percentage:", echo_iso_validation_percentage)

echo_hetero_validation <- contingency_table[6, 2]
echo_hetero_validation_percentage <- echo_hetero_validation/col_totals_validation
cat("echo_hetero_validation:", echo_hetero_validation, ", echo_hetero_validation percentage:", echo_hetero_validation_percentage)


missing_db <- table(is.na(db$echo))["TRUE"]
missing_db_train <- table(is.na(db_train$echo))["TRUE"]
missing_db_validation <- table(is.na(db_validation$echo))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort
percentage_missing_db_train <- missing_db_train/number_total_cohort
percentage_missing_db_validation <- missing_db_validation/number_total_cohort

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)



CrossTable(db$echo, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$echo_anechoic, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$echo_hyper, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$echo_comp_cyst_sol, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$echo_hypo, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$echo_iso, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$echo_hetero, db$site=="1", chisq = TRUE, prop.chisq = TRUE)


####posterior
CrossTable(db$post, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$posterior_none, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$posterior_enhancement, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$posterior_shadowing, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$posterior_combined, db$site=="1", chisq = TRUE, prop.chisq = TRUE)



contingency_table <- table(db$post, db$site == "1")
row_totals <- rowSums(contingency_table)
print(contingency_table)

col_totals<- colSums(contingency_table)
col_totals_train <- col_totals["FALSE"]
col_totals_validation <- col_totals["TRUE"]
number_total_cohort <- sum(row_totals)


posterior_none_train <- contingency_table[1, 1]
posterior_none_train_percentage <- posterior_none_train/col_totals_train*100
cat("posterior_none_train:", posterior_none_train, ", posterior_none_train percentage:", posterior_none_train_percentage)

posterior_enhancement_train <- contingency_table[2, 1]
posterior_enhancement_train_percentage <- posterior_enhancement_train/col_totals_train*100
cat("posterior_enhancement_train:", posterior_enhancement_train, ", posterior_enhancement_train percentage:", posterior_enhancement_train_percentage)

posterior_shadowing_train <- contingency_table[3, 1]
posterior_shadowing_train_percentage <- posterior_shadowing_train/col_totals_train*100
cat("posterior_shadowing_train:", posterior_shadowing_train, ", posterior_shadowing_train percentage:", posterior_shadowing_train_percentage)

posterior_combined_train <- contingency_table[4, 1]
posterior_combined_train_percentage <- posterior_combined_train/col_totals_train*100
cat("posterior_combined_train:", posterior_combined_train, ", posterior_combined_train percentage:", posterior_combined_train_percentage)


####


posterior_none_validation <- contingency_table[1, 2]
posterior_none_validation_percentage <- posterior_none_validation/col_totals_validation*100
cat("posterior_none_validation:", posterior_none_validation, ", posterior_none_validation percentage:", posterior_none_validation_percentage)

posterior_enhancement_validation <- contingency_table[2, 2]
posterior_enhancement_validation_percentage <- posterior_enhancement_validation/col_totals_validation*100
cat("posterior_enhancement_validation:", posterior_enhancement_validation, ", posterior_enhancement_validation percentage:", posterior_enhancement_validation_percentage)

posterior_shadowing_validation <- contingency_table[3, 2]
posterior_shadowing_validation_percentage <- posterior_shadowing_validation/col_totals_validation*100
cat("posterior_shadowing_validation:", posterior_shadowing_validation, ", posterior_shadowing_validation percentage:", posterior_shadowing_validation_percentage)

posterior_combined_validation <- contingency_table[4, 2]
posterior_combined_validation_percentage <- posterior_combined_validation/col_totals_validation*100
cat("posterior_combined_validation:", posterior_combined_validation, ", posterior_combined_validation percentage:", posterior_combined_validation_percentage)



missing_db <- table(is.na(db$post))["TRUE"]
missing_db_train <- table(is.na(db_train$post))["TRUE"]
missing_db_validation <- table(is.na(db_validation$post))["TRUE"]

percentage_missing_db <- missing_db/number_total_cohort*100
percentage_missing_db_train <- missing_db_train/number_total_cohort*100
percentage_missing_db_validation <- missing_db_validation/number_total_cohort*100

cat("number of missing values total cohort:", missing_db, ", percentage:", percentage_missing_db)
cat("number of missing values training cohort:", missing_db_train, ", percentage:", percentage_missing_db_train)
cat("number of missing values validation cohort:",missing_db_validation, ", percentage:", percentage_missing_db_validation)




CrossTable(db$swe1_quality=="low", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$swe1_quality=="intermediate", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$swe1_quality=="high", db$site=="1", chisq = TRUE, prop.chisq = TRUE)

CrossTable(db$swe1_position, db$site=="1", chisq = TRUE, prop.chisq = TRUE)


CrossTable(db$swe2_quality=="low", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$swe2_quality=="intermediate", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$swe2_quality=="high", db$site=="1", chisq = TRUE, prop.chisq = TRUE)

CrossTable(db$swe2_position, db$site=="1", chisq = TRUE, prop.chisq = TRUE)


CrossTable(db$swe3_quality=="low", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$swe3_quality=="intermediate", db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$swe3_quality=="high", db$site=="1", chisq = TRUE, prop.chisq = TRUE)

CrossTable(db$swe3_position, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

CrossTable(db$outcome, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

CrossTable(db$patho1, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

CrossTable(db$patho2, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

missing_patho <- table(is.na(db$patho1) & is.na(db$patho2))
missing_patho 

CrossTable(db$strainmeasure, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$strainelasto, db$site=="1", chisq = TRUE, prop.chisq = TRUE)
CrossTable(db$strainbmode, db$site=="1", chisq = TRUE, prop.chisq = TRUE)



CrossTable(db$ratio<1, db$site=="1", chisq = TRUE, prop.chisq = TRUE)


Table(db$strainratio)
Table(db$site)
contingency_table <- as.data.frame(contingency_table)
row_totals <- rowSums(contingency_table)
print(contingency_table)


CrossTable(db$strainbmode2, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

Table (db$siteno)

mal_age <- subset(db, db$outcome=="Malignant")
range(mal_age$age)

#### (14) save and load model ####
saveRDS(cv_nn, "cv_nn.rds")
my_model <- readRDS("cv_nn.rds")

setwd("C:/Users/he_ta/Desktop/Doktorarbeit/2. Programming/4. R Codes/")

#save specific model
saveRDS(cv_xgboost, "b_clahe_cv_xgboost.rds")
#read specific model
cv_b_clahe_xgboost <- readRDS("b_clahe_cv_xgboost.rds")

#save subset from database
csv_file_path <- "C:/Users/he_ta/Desktop/Doktorarbeit/2. Programming/4. R Codes/b_clahe_db_validation.csv"
write.csv(db_validation, file = csv_file_path, row.names = FALSE)

#read subset from database
db_b_clahe_validation <- read.csv("b_clahe_db_validation.csv", stringsAsFactors = F)





#### (15) sample size ####
help(package = "rpact")

#calculate sample size for testing rates in one sample 
#groups: The number of treatment groups (1 or 2), default is 2
#thetaH0: The null hypothesis value 
#pi1: assumed probability in the active treatment group if two treatment groups are considered, or the alternative probability for a one treatment group design
# normalApproximation: If FALSE, the sample size for the case of one treatment group is calculated exactly using the binomial distribution, default is TRUE. 

getSampleSizeRates(groups=1, thetaH0 = 0.25, pi1=0.02, normalApproximation=FALSE)




row_index <- 250
single_row <- db_validation[row_index, ]

# Print the selected row
print(single_row)


order(single_row, decreasing = TRUE)

type(single_row)





