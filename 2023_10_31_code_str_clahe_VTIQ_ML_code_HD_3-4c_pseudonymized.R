#### (1) Install and load relevant packages ####



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

#set working directory
setwd("C:/User")


#### (2) Load data  ####

#load csv file from working directory
db <- read.csv(".csv", stringsAsFactors = F)
db_original <- read.csv(".csv", stringsAsFactors = F)

#display the names of the columns
colnames(db)

#multiple replacement of column names as used in VTIQ codebook

#replace >10m/s lesion meassure with 10 for lesion measure
db$lesion [db$lesionresp==1] <- 10
#replace >10m/s lesion meassure with 10 for lesion measure
db$lesion2 [db$lesionresp2==1] <- 10
#replace >10m/s lesion meassure with 10 for lesion measure
db$lesion3 [db$lesionresp3==1] <- 10

#replace schallkopfüberschreitend in strain_elasto with 50
db$strainelasto [db$elastresp==1] <- 50

#replace " " in with NA
db$strainbmode [(db$strainbmode== " ")] <- NA
db$strainelasto [(db$strainelasto== " ")] <- NA
db$lesion [(db$lesion== " ")] <- NA
db$lesion2 [(db$lesion2== " ")] <- NA
db$lesion3 [(db$lesion3== " ")] <- NA

#modification of integer characters to numeric values as provided in VTIQ Codebook
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




##replacement of column names with adapted names including clinical variables (146-291) and radiomics features (from 293)
  
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
                
                
                "diagnostics_Configuration_Settings_STR_clahe",
                "diagnostics_Configuration_EnabledImageTypes_STR_clahe",
                "diagnostics_Image.original_Hash_STR_clahe",
                "diagnostics_Image.original_Dimensionality_STR_clahe",
                "diagnostics_Image.original_Spacing_STR_clahe",
                "diagnostics_Image.original_Size_STR_clahe",
                "diagnostics_Image.original_Mean_STR_clahe",
                "diagnostics_Image.original_Minimum_STR_clahe",
                "diagnostics_Image.original_Maximum_STR_clahe",
                "diagnostics_Mask.original_Hash_STR_clahe",
                "diagnostics_Mask.original_Spacing_STR_clahe",
                "diagnostics_Mask.original_Size_STR_clahe",
                "diagnostics_Mask.original_BoundingBox_STR_clahe",
                "diagnostics_Mask.original_VoxelNum_STR_clahe",
                "diagnostics_Mask.original_VolumeNum_STR_clahe",
                "diagnostics_Mask.original_CenterOfMassIndex_STR_clahe",
                "diagnostics_Mask.original_CenterOfMass_STR_clahe",
                "diagnostics_Image.interpolated_Spacing_STR_clahe",
                "diagnostics_Image.interpolated_Size_STR_clahe",
                "diagnostics_Image.interpolated_Mean_STR_clahe",
                "diagnostics_Image.interpolated_Minimum_STR_clahe",
                "diagnostics_Image.interpolated_Maximum_STR_clahe",
                "diagnostics_Mask.interpolated_Spacing_STR_clahe",
                "diagnostics_Mask.interpolated_Size_STR_clahe",
                "diagnostics_Mask.interpolated_BoundingBox_STR_clahe",
                "diagnostics_Mask.interpolated_VoxelNum_STR_clahe",
                "diagnostics_Mask.interpolated_VolumeNum_STR_clahe",
                "diagnostics_Mask.interpolated_CenterOfMassIndex_STR_clahe",
                "diagnostics_Mask.interpolated_CenterOfMass_STR_clahe",
                "diagnostics_Mask.interpolated_Mean_STR_clahe",
                "diagnostics_Mask.interpolated_Minimum_STR_clahe",
                "diagnostics_Mask.interpolated_Maximum_STR_clahe",
                "original_shape_Elongation_STR_clahe",
                "original_shape_Flatness_STR_clahe",
                "original_shape_LeastAxisLength_STR_clahe",
                "original_shape_MajorAxisLength_STR_clahe",
                "original_shape_Maximum2DDiameterColumn_STR_clahe",
                "original_shape_Maximum2DDiameterRow_STR_clahe",
                "original_shape_Maximum2DDiameterSlice_STR_clahe",
                "original_shape_Maximum3DDiameter_STR_clahe",
                "original_shape_MeshVolume_STR_clahe",
                "original_shape_MinorAxisLength_STR_clahe",
                "original_shape_Sphericity_STR_clahe",
                "original_shape_SurfaceArea_STR_clahe",
                "original_shape_SurfaceVolumeRatio_STR_clahe",
                "original_shape_VoxelVolume_STR_clahe",
                "original_firstorder_10Percentile_STR_clahe",
                "original_firstorder_90Percentile_STR_clahe",
                "original_firstorder_Energy_STR_clahe",
                "original_firstorder_Entropy_STR_clahe",
                "original_firstorder_InterquartileRange_STR_clahe",
                "original_firstorder_Kurtosis_STR_clahe",
                "original_firstorder_Maximum_STR_clahe",
                "original_firstorder_MeanAbsoluteDeviation_STR_clahe",
                "original_firstorder_Mean_STR_clahe",
                "original_firstorder_Median_STR_clahe",
                "original_firstorder_Minimum_STR_clahe",
                "original_firstorder_Range_STR_clahe",
                "original_firstorder_RobustMeanAbsoluteDeviation_STR_clahe",
                "original_firstorder_RootMeanSquared_STR_clahe",
                "original_firstorder_Skewness_STR_clahe",
                "original_firstorder_TotalEnergy_STR_clahe",
                "original_firstorder_Uniformity_STR_clahe",
                "original_firstorder_Variance_STR_clahe",
                "original_glcm_Autocorrelation_STR_clahe",
                "original_glcm_ClusterProminence_STR_clahe",
                "original_glcm_ClusterShade_STR_clahe",
                "original_glcm_ClusterTendency_STR_clahe",
                "original_glcm_Contrast_STR_clahe",
                "original_glcm_Correlation_STR_clahe",
                "original_glcm_DifferenceAverage_STR_clahe",
                "original_glcm_DifferenceEntropy_STR_clahe",
                "original_glcm_DifferenceVariance_STR_clahe",
                "original_glcm_Id_STR_clahe",
                "original_glcm_Idm_STR_clahe",
                "original_glcm_Idmn_STR_clahe",
                "original_glcm_Idn_STR_clahe",
                "original_glcm_Imc1_STR_clahe",
                "original_glcm_Imc2_STR_clahe",
                "original_glcm_InverseVariance_STR_clahe",
                "original_glcm_JointAverage_STR_clahe",
                "original_glcm_JointEnergy_STR_clahe",
                "original_glcm_JointEntropy_STR_clahe",
                "original_glcm_MCC_STR_clahe",
                "original_glcm_MaximumProbability_STR_clahe",
                "original_glcm_SumAverage_STR_clahe",
                "original_glcm_SumEntropy_STR_clahe",
                "original_glcm_SumSquares_STR_clahe",
                "original_gldm_DependenceEntropy_STR_clahe",
                "original_gldm_DependenceNonUniformity_STR_clahe",
                "original_gldm_DependenceNonUniformityNormalized_STR_clahe",
                "original_gldm_DependenceVariance_STR_clahe",
                "original_gldm_GrayLevelNonUniformity_STR_clahe",
                "original_gldm_GrayLevelVariance_STR_clahe",
                "original_gldm_HighGrayLevelEmphasis_STR_clahe",
                "original_gldm_LargeDependenceEmphasis_STR_clahe",
                "original_gldm_LargeDependenceHighGrayLevelEmphasis_STR_clahe",
                "original_gldm_LargeDependenceLowGrayLevelEmphasis_STR_clahe",
                "original_gldm_LowGrayLevelEmphasis_STR_clahe",
                "original_gldm_SmallDependenceEmphasis_STR_clahe",
                "original_gldm_SmallDependenceHighGrayLevelEmphasis_STR_clahe",
                "original_gldm_SmallDependenceLowGrayLevelEmphasis_STR_clahe",
                "original_glrlm_GrayLevelNonUniformity_STR_clahe",
                "original_glrlm_GrayLevelNonUniformityNormalized_STR_clahe",
                "original_glrlm_GrayLevelVariance_STR_clahe",
                "original_glrlm_HighGrayLevelRunEmphasis_STR_clahe",
                "original_glrlm_LongRunEmphasis_STR_clahe",
                "original_glrlm_LongRunHighGrayLevelEmphasis_STR_clahe",
                "original_glrlm_LongRunLowGrayLevelEmphasis_STR_clahe",
                "original_glrlm_LowGrayLevelRunEmphasis_STR_clahe",
                "original_glrlm_RunEntropy_STR_clahe",
                "original_glrlm_RunLengthNonUniformity_STR_clahe",
                "original_glrlm_RunLengthNonUniformityNormalized_STR_clahe",
                "original_glrlm_RunPercentage_STR_clahe",
                "original_glrlm_RunVariance_STR_clahe",
                "original_glrlm_ShortRunEmphasis_STR_clahe",
                "original_glrlm_ShortRunHighGrayLevelEmphasis_STR_clahe",
                "original_glrlm_ShortRunLowGrayLevelEmphasis_STR_clahe",
                "original_glszm_GrayLevelNonUniformity_STR_clahe",
                "original_glszm_GrayLevelNonUniformityNormalized_STR_clahe",
                "original_glszm_GrayLevelVariance_STR_clahe",
                "original_glszm_HighGrayLevelZoneEmphasis_STR_clahe",
                "original_glszm_LargeAreaEmphasis_STR_clahe",
                "original_glszm_LargeAreaHighGrayLevelEmphasis_STR_clahe",
                "original_glszm_LargeAreaLowGrayLevelEmphasis_STR_clahe",
                "original_glszm_LowGrayLevelZoneEmphasis_STR_clahe",
                "original_glszm_SizeZoneNonUniformity_STR_clahe",
                "original_glszm_SizeZoneNonUniformityNormalized_STR_clahe",
                "original_glszm_SmallAreaEmphasis_STR_clahe",
                "original_glszm_SmallAreaHighGrayLevelEmphasis_STR_clahe",
                "original_glszm_SmallAreaLowGrayLevelEmphasis_STR_clahe",
                "original_glszm_ZoneEntropy_STR_clahe",
                "original_glszm_ZonePercentage_STR_clahe",
                "original_glszm_ZoneVariance_STR_clahe",
                "original_ngtdm_Busyness_STR_clahe",
                "original_ngtdm_Coarseness_STR_clahe",
                "original_ngtdm_Complexity_STR_clahe",
                "original_ngtdm_Contrast_STR_clahe",
                "original_ngtdm_Strength_STR_clahe", "Name_STR_clahe" )      




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
levels(db$outcome) 
db$outcome <- factor(db$outcome, levels=rev(levels(db$outcome)))
levels(db$outcome)


#make numeric variables numeric
db$age<-as.numeric(db$age)
db$ultrasound_axis<-as.numeric(db$ultrasound_axis)
db$ultrasound_perpendicular<-as.numeric(db$ultrasound_perpendicular)
db$ultrasound_orthogonal<-as.numeric(db$ultrasound_orthogonal)
db$swe1_lesion<-as.numeric(db$swe1_lesion)
db$swe2_lesion<-as.numeric(db$swe2_lesion)
db$swe3_lesion<-as.numeric(db$swe3_lesion)

##radiomics features
db$diagnostics_Image.original_Mean_STR_clahe<-as.numeric(db$diagnostics_Image.original_Mean_STR_clahe)
db$diagnostics_Image.original_Minimum_STR_clahe<-as.numeric(db$diagnostics_Image.original_Minimum_STR_clahe)
db$diagnostics_Image.original_Maximum_STR_clahe<-as.numeric(db$diagnostics_Image.original_Maximum_STR_clahe)
db$diagnostics_Mask.original_VoxelNum_STR_clahe<-as.numeric(db$diagnostics_Mask.original_VoxelNum_STR_clahe)
db$diagnostics_Mask.original_VolumeNum_STR_clahe<-as.numeric(db$diagnostics_Mask.original_VolumeNum_STR_clahe)
db$diagnostics_Image.interpolated_Mean_STR_clahe<-as.numeric(db$diagnostics_Image.interpolated_Mean_STR_clahe)
db$diagnostics_Image.interpolated_Minimum_STR_clahe<-as.numeric(db$diagnostics_Image.interpolated_Minimum_STR_clahe)
db$diagnostics_Image.interpolated_Maximum_STR_clahe<-as.numeric(db$diagnostics_Image.interpolated_Maximum_STR_clahe)
db$diagnostics_Mask.interpolated_Spacing_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_Spacing_STR_clahe)
db$diagnostics_Mask.interpolated_Size_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_Size_STR_clahe)
db$diagnostics_Mask.interpolated_BoundingBox_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_BoundingBox_STR_clahe)
db$diagnostics_Mask.interpolated_VoxelNum_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_VoxelNum_STR_clahe)
db$diagnostics_Mask.interpolated_VolumeNum_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_VolumeNum_STR_clahe)
db$diagnostics_Mask.interpolated_CenterOfMassIndex_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_CenterOfMassIndex_STR_clahe)
db$diagnostics_Mask.interpolated_CenterOfMass_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_CenterOfMass_STR_clahe)
db$diagnostics_Mask.interpolated_Mean_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_Mean_STR_clahe)
db$diagnostics_Mask.interpolated_Minimum_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_Minimum_STR_clahe)
db$diagnostics_Mask.interpolated_Maximum_STR_clahe<-as.numeric(db$diagnostics_Mask.interpolated_Maximum_STR_clahe)
db$original_shape_Elongation_STR_clahe<-as.numeric(db$original_shape_Elongation_STR_clahe)
db$original_shape_Flatness_STR_clahe<-as.numeric(db$original_shape_Flatness_STR_clahe)
db$original_shape_LeastAxisLength_STR_clahe<-as.numeric(db$original_shape_LeastAxisLength_STR_clahe)
db$original_shape_MajorAxisLength_STR_clahe<-as.numeric(db$original_shape_MajorAxisLength_STR_clahe)
db$original_shape_Maximum2DDiameterColumn_STR_clahe<-as.numeric(db$original_shape_Maximum2DDiameterColumn_STR_clahe)
db$original_shape_Maximum2DDiameterRow_STR_clahe<-as.numeric(db$original_shape_Maximum2DDiameterRow_STR_clahe)
db$original_shape_Maximum2DDiameterSlice_STR_clahe<-as.numeric(db$original_shape_Maximum2DDiameterSlice_STR_clahe)
db$original_shape_Maximum3DDiameter_STR_clahe<-as.numeric(db$original_shape_Maximum3DDiameter_STR_clahe)
db$original_shape_MeshVolume_STR_clahe<-as.numeric(db$original_shape_MeshVolume_STR_clahe)
db$original_shape_MinorAxisLength_STR_clahe<-as.numeric(db$original_shape_MinorAxisLength_STR_clahe)
db$original_shape_Sphericity_STR_clahe<-as.numeric(db$original_shape_Sphericity_STR_clahe)
db$original_shape_SurfaceArea_STR_clahe<-as.numeric(db$original_shape_SurfaceArea_STR_clahe)
db$original_shape_SurfaceVolumeRatio_STR_clahe<-as.numeric(db$original_shape_SurfaceVolumeRatio_STR_clahe)
db$original_shape_VoxelVolume_STR_clahe<-as.numeric(db$original_shape_VoxelVolume_STR_clahe)
db$original_firstorder_10Percentile_STR_clahe<-as.numeric(db$original_firstorder_10Percentile_STR_clahe)
db$original_firstorder_90Percentile_STR_clahe<-as.numeric(db$original_firstorder_90Percentile_STR_clahe)
db$original_firstorder_Energy_STR_clahe<-as.numeric(db$original_firstorder_Energy_STR_clahe)
db$original_firstorder_Entropy_STR_clahe<-as.numeric(db$original_firstorder_Entropy_STR_clahe)
db$original_firstorder_InterquartileRange_STR_clahe<-as.numeric(db$original_firstorder_InterquartileRange_STR_clahe)
db$original_firstorder_Kurtosis_STR_clahe<-as.numeric(db$original_firstorder_Kurtosis_STR_clahe)
db$original_firstorder_Maximum_STR_clahe<-as.numeric(db$original_firstorder_Maximum_STR_clahe)
db$original_firstorder_MeanAbsoluteDeviation_STR_clahe<-as.numeric(db$original_firstorder_MeanAbsoluteDeviation_STR_clahe)
db$original_firstorder_Mean_STR_clahe<-as.numeric(db$original_firstorder_Mean_STR_clahe)
db$original_firstorder_Median_STR_clahe<-as.numeric(db$original_firstorder_Median_STR_clahe)
db$original_firstorder_Minimum_STR_clahe<-as.numeric(db$original_firstorder_Minimum_STR_clahe)
db$original_firstorder_Range_STR_clahe<-as.numeric(db$original_firstorder_Range_STR_clahe)
db$original_firstorder_RobustMeanAbsoluteDeviation_STR_clahe<-as.numeric(db$original_firstorder_RobustMeanAbsoluteDeviation_STR_clahe)
db$original_firstorder_RootMeanSquared_STR_clahe<-as.numeric(db$original_firstorder_RootMeanSquared_STR_clahe)
db$original_firstorder_Skewness_STR_clahe<-as.numeric(db$original_firstorder_Skewness_STR_clahe)
db$original_firstorder_TotalEnergy_STR_clahe<-as.numeric(db$original_firstorder_TotalEnergy_STR_clahe)
db$original_firstorder_Uniformity_STR_clahe<-as.numeric(db$original_firstorder_Uniformity_STR_clahe)
db$original_firstorder_Variance_STR_clahe<-as.numeric(db$original_firstorder_Variance_STR_clahe)
db$original_glcm_Autocorrelation_STR_clahe<-as.numeric(db$original_glcm_Autocorrelation_STR_clahe)
db$original_glcm_ClusterProminence_STR_clahe<-as.numeric(db$original_glcm_ClusterProminence_STR_clahe)
db$original_glcm_ClusterShade_STR_clahe<-as.numeric(db$original_glcm_ClusterShade_STR_clahe)
db$original_glcm_ClusterTendency_STR_clahe<-as.numeric(db$original_glcm_ClusterTendency_STR_clahe)
db$original_glcm_Contrast_STR_clahe<-as.numeric(db$original_glcm_Contrast_STR_clahe)
db$original_glcm_Correlation_STR_clahe<-as.numeric(db$original_glcm_Correlation_STR_clahe)
db$original_glcm_DifferenceAverage_STR_clahe<-as.numeric(db$original_glcm_DifferenceAverage_STR_clahe)
db$original_glcm_DifferenceEntropy_STR_clahe<-as.numeric(db$original_glcm_DifferenceEntropy_STR_clahe)
db$original_glcm_DifferenceVariance_STR_clahe<-as.numeric(db$original_glcm_DifferenceVariance_STR_clahe)
db$original_glcm_Id_STR_clahe<-as.numeric(db$original_glcm_Id_STR_clahe)
db$original_glcm_Idm_STR_clahe<-as.numeric(db$original_glcm_Idm_STR_clahe)
db$original_glcm_Idmn_STR_clahe<-as.numeric(db$original_glcm_Idmn_STR_clahe)
db$original_glcm_Idn_STR_clahe<-as.numeric(db$original_glcm_Idn_STR_clahe)
db$original_glcm_Imc1_STR_clahe<-as.numeric(db$original_glcm_Imc1_STR_clahe)
db$original_glcm_Imc2_STR_clahe<-as.numeric(db$original_glcm_Imc2_STR_clahe)
db$original_glcm_InverseVariance_STR_clahe<-as.numeric(db$original_glcm_InverseVariance_STR_clahe)
db$original_glcm_JointAverage_STR_clahe<-as.numeric(db$original_glcm_JointAverage_STR_clahe)
db$original_glcm_JointEnergy_STR_clahe<-as.numeric(db$original_glcm_JointEnergy_STR_clahe)
db$original_glcm_JointEntropy_STR_clahe<-as.numeric(db$original_glcm_JointEntropy_STR_clahe)
db$original_glcm_MCC_STR_clahe<-as.numeric(db$original_glcm_MCC_STR_clahe)
db$original_glcm_MaximumProbability_STR_clahe<-as.numeric(db$original_glcm_MaximumProbability_STR_clahe)
db$original_glcm_SumAverage_STR_clahe<-as.numeric(db$original_glcm_SumAverage_STR_clahe)
db$original_glcm_SumEntropy_STR_clahe<-as.numeric(db$original_glcm_SumEntropy_STR_clahe)
db$original_glcm_SumSquares_STR_clahe<-as.numeric(db$original_glcm_SumSquares_STR_clahe)
db$original_gldm_DependenceEntropy_STR_clahe<-as.numeric(db$original_gldm_DependenceEntropy_STR_clahe)
db$original_gldm_DependenceNonUniformity_STR_clahe<-as.numeric(db$original_gldm_DependenceNonUniformity_STR_clahe)
db$original_gldm_DependenceNonUniformityNormalized_STR_clahe<-as.numeric(db$original_gldm_DependenceNonUniformityNormalized_STR_clahe)
db$original_gldm_DependenceVariance_STR_clahe<-as.numeric(db$original_gldm_DependenceVariance_STR_clahe)
db$original_gldm_GrayLevelNonUniformity_STR_clahe<-as.numeric(db$original_gldm_GrayLevelNonUniformity_STR_clahe)
db$original_gldm_GrayLevelVariance_STR_clahe<-as.numeric(db$original_gldm_GrayLevelVariance_STR_clahe)
db$original_gldm_HighGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_gldm_HighGrayLevelEmphasis_STR_clahe)
db$original_gldm_LargeDependenceEmphasis_STR_clahe<-as.numeric(db$original_gldm_LargeDependenceEmphasis_STR_clahe)
db$original_gldm_LargeDependenceHighGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_gldm_LargeDependenceHighGrayLevelEmphasis_STR_clahe)
db$original_gldm_LargeDependenceLowGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_gldm_LargeDependenceLowGrayLevelEmphasis_STR_clahe)
db$original_gldm_LowGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_gldm_LowGrayLevelEmphasis_STR_clahe)
db$original_gldm_SmallDependenceEmphasis_STR_clahe<-as.numeric(db$original_gldm_SmallDependenceEmphasis_STR_clahe)
db$original_gldm_SmallDependenceHighGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_gldm_SmallDependenceHighGrayLevelEmphasis_STR_clahe)
db$original_gldm_SmallDependenceLowGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_gldm_SmallDependenceLowGrayLevelEmphasis_STR_clahe)
db$original_glrlm_GrayLevelNonUniformity_STR_clahe<-as.numeric(db$original_glrlm_GrayLevelNonUniformity_STR_clahe)
db$original_glrlm_GrayLevelNonUniformityNormalized_STR_clahe<-as.numeric(db$original_glrlm_GrayLevelNonUniformityNormalized_STR_clahe)
db$original_glrlm_GrayLevelVariance_STR_clahe<-as.numeric(db$original_glrlm_GrayLevelVariance_STR_clahe)
db$original_glrlm_HighGrayLevelRunEmphasis_STR_clahe<-as.numeric(db$original_glrlm_HighGrayLevelRunEmphasis_STR_clahe)
db$original_glrlm_LongRunEmphasis_STR_clahe<-as.numeric(db$original_glrlm_LongRunEmphasis_STR_clahe)
db$original_glrlm_LongRunHighGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glrlm_LongRunHighGrayLevelEmphasis_STR_clahe)
db$original_glrlm_LongRunLowGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glrlm_LongRunLowGrayLevelEmphasis_STR_clahe)
db$original_glrlm_LowGrayLevelRunEmphasis_STR_clahe<-as.numeric(db$original_glrlm_LowGrayLevelRunEmphasis_STR_clahe)
db$original_glrlm_RunEntropy_STR_clahe<-as.numeric(db$original_glrlm_RunEntropy_STR_clahe)
db$original_glrlm_RunLengthNonUniformity_STR_clahe<-as.numeric(db$original_glrlm_RunLengthNonUniformity_STR_clahe)
db$original_glrlm_RunLengthNonUniformityNormalized_STR_clahe<-as.numeric(db$original_glrlm_RunLengthNonUniformityNormalized_STR_clahe)
db$original_glrlm_RunPercentage_STR_clahe<-as.numeric(db$original_glrlm_RunPercentage_STR_clahe)
db$original_glrlm_RunVariance_STR_clahe<-as.numeric(db$original_glrlm_RunVariance_STR_clahe)
db$original_glrlm_ShortRunEmphasis_STR_clahe<-as.numeric(db$original_glrlm_ShortRunEmphasis_STR_clahe)
db$original_glrlm_ShortRunHighGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glrlm_ShortRunHighGrayLevelEmphasis_STR_clahe)
db$original_glrlm_ShortRunLowGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glrlm_ShortRunLowGrayLevelEmphasis_STR_clahe)
db$original_glszm_GrayLevelNonUniformity_STR_clahe<-as.numeric(db$original_glszm_GrayLevelNonUniformity_STR_clahe)
db$original_glszm_GrayLevelNonUniformityNormalized_STR_clahe<-as.numeric(db$original_glszm_GrayLevelNonUniformityNormalized_STR_clahe)
db$original_glszm_GrayLevelVariance_STR_clahe<-as.numeric(db$original_glszm_GrayLevelVariance_STR_clahe)
db$original_glszm_HighGrayLevelZoneEmphasis_STR_clahe<-as.numeric(db$original_glszm_HighGrayLevelZoneEmphasis_STR_clahe)
db$original_glszm_LargeAreaEmphasis_STR_clahe<-as.numeric(db$original_glszm_LargeAreaEmphasis_STR_clahe)
db$original_glszm_LargeAreaHighGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glszm_LargeAreaHighGrayLevelEmphasis_STR_clahe)
db$original_glszm_LargeAreaLowGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glszm_LargeAreaLowGrayLevelEmphasis_STR_clahe)
db$original_glszm_LowGrayLevelZoneEmphasis_STR_clahe<-as.numeric(db$original_glszm_LowGrayLevelZoneEmphasis_STR_clahe)
db$original_glszm_SizeZoneNonUniformity_STR_clahe<-as.numeric(db$original_glszm_SizeZoneNonUniformity_STR_clahe)
db$original_glszm_SizeZoneNonUniformityNormalized_STR_clahe<-as.numeric(db$original_glszm_SizeZoneNonUniformityNormalized_STR_clahe)
db$original_glszm_SmallAreaEmphasis_STR_clahe<-as.numeric(db$original_glszm_SmallAreaEmphasis_STR_clahe)
db$original_glszm_SmallAreaHighGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glszm_SmallAreaHighGrayLevelEmphasis_STR_clahe)
db$original_glszm_SmallAreaLowGrayLevelEmphasis_STR_clahe<-as.numeric(db$original_glszm_SmallAreaLowGrayLevelEmphasis_STR_clahe)
db$original_glszm_ZoneEntropy_STR_clahe<-as.numeric(db$original_glszm_ZoneEntropy_STR_clahe)
db$original_glszm_ZonePercentage_STR_clahe<-as.numeric(db$original_glszm_ZonePercentage_STR_clahe)
db$original_glszm_ZoneVariance_STR_clahe<-as.numeric(db$original_glszm_ZoneVariance_STR_clahe)
db$original_ngtdm_Busyness_STR_clahe<-as.numeric(db$original_ngtdm_Busyness_STR_clahe)
db$original_ngtdm_Coarseness_STR_clahe<-as.numeric(db$original_ngtdm_Coarseness_STR_clahe)
db$original_ngtdm_Complexity_STR_clahe<-as.numeric(db$original_ngtdm_Complexity_STR_clahe)
db$original_ngtdm_Contrast_STR_clahe<-as.numeric(db$original_ngtdm_Contrast_STR_clahe)
db$original_ngtdm_Strength_STR_clahe<-as.numeric(db$original_ngtdm_Strength_STR_clahe)




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
   "diagnostics_Image.original_Mean_STR_clahe",
   "diagnostics_Image.original_Minimum_STR_clahe",
   "diagnostics_Image.original_Maximum_STR_clahe",
   "diagnostics_Mask.original_VoxelNum_STR_clahe",
   "diagnostics_Mask.original_VolumeNum_STR_clahe",
   "diagnostics_Image.interpolated_Mean_STR_clahe",
   "diagnostics_Image.interpolated_Minimum_STR_clahe",
   "diagnostics_Image.interpolated_Maximum_STR_clahe",
   "diagnostics_Mask.interpolated_VoxelNum_STR_clahe",
   "diagnostics_Mask.interpolated_VolumeNum_STR_clahe",
   "diagnostics_Mask.interpolated_Mean_STR_clahe",
   "diagnostics_Mask.interpolated_Minimum_STR_clahe",
   "diagnostics_Mask.interpolated_Maximum_STR_clahe",
   "original_shape_Elongation_STR_clahe",
   "original_shape_Flatness_STR_clahe",
   "original_shape_LeastAxisLength_STR_clahe",
   "original_shape_MajorAxisLength_STR_clahe",
   "original_shape_Maximum2DDiameterColumn_STR_clahe",
   "original_shape_Maximum2DDiameterRow_STR_clahe",
   "original_shape_Maximum2DDiameterSlice_STR_clahe",
   "original_shape_Maximum3DDiameter_STR_clahe",
   "original_shape_MeshVolume_STR_clahe",
   "original_shape_MinorAxisLength_STR_clahe",
   "original_shape_Sphericity_STR_clahe",
   "original_shape_SurfaceArea_STR_clahe",
   "original_shape_SurfaceVolumeRatio_STR_clahe",
   "original_shape_VoxelVolume_STR_clahe",
   "original_firstorder_10Percentile_STR_clahe",
   "original_firstorder_90Percentile_STR_clahe",
   "original_firstorder_Energy_STR_clahe",
   "original_firstorder_Entropy_STR_clahe",
   "original_firstorder_InterquartileRange_STR_clahe",
   "original_firstorder_Kurtosis_STR_clahe",
   "original_firstorder_Maximum_STR_clahe",
   "original_firstorder_MeanAbsoluteDeviation_STR_clahe",
   "original_firstorder_Mean_STR_clahe",
   "original_firstorder_Median_STR_clahe",
   "original_firstorder_Minimum_STR_clahe",
   "original_firstorder_Range_STR_clahe",
   "original_firstorder_RobustMeanAbsoluteDeviation_STR_clahe",
   "original_firstorder_RootMeanSquared_STR_clahe",
   "original_firstorder_Skewness_STR_clahe",
   "original_firstorder_TotalEnergy_STR_clahe",
   "original_firstorder_Uniformity_STR_clahe",
   "original_firstorder_Variance_STR_clahe",
   "original_glcm_Autocorrelation_STR_clahe",
   "original_glcm_ClusterProminence_STR_clahe",
   "original_glcm_ClusterShade_STR_clahe",
   "original_glcm_ClusterTendency_STR_clahe",
   "original_glcm_Contrast_STR_clahe",
   "original_glcm_Correlation_STR_clahe",
   "original_glcm_DifferenceAverage_STR_clahe",
   "original_glcm_DifferenceEntropy_STR_clahe",
   "original_glcm_DifferenceVariance_STR_clahe",
   "original_glcm_Id_STR_clahe",
   "original_glcm_Idm_STR_clahe",
   "original_glcm_Idmn_STR_clahe",
   "original_glcm_Idn_STR_clahe",
   "original_glcm_Imc1_STR_clahe",
   "original_glcm_Imc2_STR_clahe",
   "original_glcm_InverseVariance_STR_clahe",
   "original_glcm_JointAverage_STR_clahe",
   "original_glcm_JointEnergy_STR_clahe",
   "original_glcm_JointEntropy_STR_clahe",
   "original_glcm_MCC_STR_clahe",
   "original_glcm_MaximumProbability_STR_clahe",
   "original_glcm_SumAverage_STR_clahe",
   "original_glcm_SumEntropy_STR_clahe",
   "original_glcm_SumSquares_STR_clahe",
   "original_gldm_DependenceEntropy_STR_clahe",
   "original_gldm_DependenceNonUniformity_STR_clahe",
   "original_gldm_DependenceNonUniformityNormalized_STR_clahe",
   "original_gldm_DependenceVariance_STR_clahe",
   "original_gldm_GrayLevelNonUniformity_STR_clahe",
   "original_gldm_GrayLevelVariance_STR_clahe",
   "original_gldm_HighGrayLevelEmphasis_STR_clahe",
   "original_gldm_LargeDependenceEmphasis_STR_clahe",
   "original_gldm_LargeDependenceHighGrayLevelEmphasis_STR_clahe",
   "original_gldm_LargeDependenceLowGrayLevelEmphasis_STR_clahe",
   "original_gldm_LowGrayLevelEmphasis_STR_clahe",
   "original_gldm_SmallDependenceEmphasis_STR_clahe",
   "original_gldm_SmallDependenceHighGrayLevelEmphasis_STR_clahe",
   "original_gldm_SmallDependenceLowGrayLevelEmphasis_STR_clahe",
   "original_glrlm_GrayLevelNonUniformity_STR_clahe",
   "original_glrlm_GrayLevelNonUniformityNormalized_STR_clahe",
   "original_glrlm_GrayLevelVariance_STR_clahe",
   "original_glrlm_HighGrayLevelRunEmphasis_STR_clahe",
   "original_glrlm_LongRunEmphasis_STR_clahe",
   "original_glrlm_LongRunHighGrayLevelEmphasis_STR_clahe",
   "original_glrlm_LongRunLowGrayLevelEmphasis_STR_clahe",
   "original_glrlm_LowGrayLevelRunEmphasis_STR_clahe",
   "original_glrlm_RunEntropy_STR_clahe",
   "original_glrlm_RunLengthNonUniformity_STR_clahe",
   "original_glrlm_RunLengthNonUniformityNormalized_STR_clahe",
   "original_glrlm_RunPercentage_STR_clahe",
   "original_glrlm_RunVariance_STR_clahe",
   "original_glrlm_ShortRunEmphasis_STR_clahe",
   "original_glrlm_ShortRunHighGrayLevelEmphasis_STR_clahe",
   "original_glrlm_ShortRunLowGrayLevelEmphasis_STR_clahe",
   "original_glszm_GrayLevelNonUniformity_STR_clahe",
   "original_glszm_GrayLevelNonUniformityNormalized_STR_clahe",
   "original_glszm_GrayLevelVariance_STR_clahe",
   "original_glszm_HighGrayLevelZoneEmphasis_STR_clahe",
   "original_glszm_LargeAreaEmphasis_STR_clahe",
   "original_glszm_LargeAreaHighGrayLevelEmphasis_STR_clahe",
   "original_glszm_LargeAreaLowGrayLevelEmphasis_STR_clahe",
   "original_glszm_LowGrayLevelZoneEmphasis_STR_clahe",
   "original_glszm_SizeZoneNonUniformity_STR_clahe",
   "original_glszm_SizeZoneNonUniformityNormalized_STR_clahe",
   "original_glszm_SmallAreaEmphasis_STR_clahe",
   "original_glszm_SmallAreaHighGrayLevelEmphasis_STR_clahe",
   "original_glszm_SmallAreaLowGrayLevelEmphasis_STR_clahe",
   "original_glszm_ZoneEntropy_STR_clahe",
   "original_glszm_ZonePercentage_STR_clahe",
   "original_glszm_ZoneVariance_STR_clahe",
   "original_ngtdm_Busyness_STR_clahe",
   "original_ngtdm_Coarseness_STR_clahe",
   "original_ngtdm_Complexity_STR_clahe",
   "original_ngtdm_Contrast_STR_clahe",
   "original_ngtdm_Strength_STR_clahe"
 
               )

outcome<-"outcome"

formula<-as.formula(paste(outcome, paste(predictors, collapse = "+"),sep="~"))

set.seed(my_seed)

recipe<-recipe(formula,db_train) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%                  
  step_BoxCox(all_numeric(),-all_outcomes()) %>%                        
  step_other(all_nominal(), threshold = .05, other = "other") %>%       
  step_zv(all_predictors(),-all_outcomes()) %>%                         
  step_nzv(all_predictors(),-all_outcomes())%>%                         
  step_normalize(all_numeric(),-all_outcomes())%>%                      
  step_dummy(all_nominal(),-all_outcomes()) %>%                         
  step_corr(all_predictors(),-all_outcomes(), threshold = 0.9)         


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
#cv grid search
cv_grid <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 3,  
  search = "grid", 
  verboseIter= TRUE,
  classProbs = TRUE,
  returnResamp = "final",
  savePredictions = "final", 
  summaryFunction = MySummary,
  selectionFunction = "tolerance",
  allowParallel=TRUE
)

#cv random search
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





### XGBoost ####

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



#### (6) Resampling performance testing ####


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



#### calibration ####
## calibration plots 


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



## calibration scores
#define Spiegelhalter's Z

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


### ROC ####

roc_glm_validation = roc(as.vector(db_validation$outcome),as.matrix(predict(cv_glm , db_validation, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_glm_validation = pROC::auc(roc_glm_validation) #Calculate the area under the ROC curve
auc_CI_glm_validation = pROC::ci.auc(roc_glm_validation, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


roc_xgboost_validation = roc(as.vector(db_validation$outcome),as.matrix(predict(cv_xgboost , db_validation, type = "prob")$"Malignant")) #Conduct the ROC analyses
auc_xgboost_validation = pROC::auc(roc_xgboost_validation) #Calculate the area under the ROC curve
auc_CI_xgboost_validation = pROC::ci.auc(roc_xgboost_validation, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


#### confusion matrices ####

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

###  XGBoost Tree ####

##agnostic model explanation 


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



# partial dependencies for categorial variables 
xai_glm_cat  <- ingredients::accumulated_dependency(xai_glm_explainer, variable_type = "categorical")
plot(xai_glm_cat)



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
expert1_birads <- db_validation$birads_b_num

#for str dataset
#expert1_birads <- db_validation$birads_STR_num

roc_expert1 = roc(as.vector(db_validation$outcome),as.matrix(expert1_birads)) #Conduct the ROC analyses
auc_expert1 = pROC::auc(roc_expert1) #Calculate the area under the ROC curve
auc_CI_expert1 = pROC::ci.auc(roc_expert1, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve



binary_expert1 <- as.factor(
  ifelse(expert1_birads  ==1, "Benign", "Malignant"))

binary_expert1 <- factor(binary_expert1, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(binary_expert1), factor(db_validation$outcome), positive="Malignant") #confusion matrix



##

expert2_birads <- db_validation$birads_g_num

roc_expert2 = roc(as.vector(db_validation$outcome),as.matrix(expert2_birads)) #Conduct the ROC analyses
auc_expert2 = pROC::auc(roc_expert2) #Calculate the area under the ROC curve
auc_CI_expert2 = pROC::ci.auc(roc_expert2, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


binary_expert2 <- as.factor(
  ifelse(expert2_birads  ==1, "Benign", "Malignant"))

binary_expert2 <- factor(binary_expert2, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(binary_expert2), factor(db_validation$outcome), positive="Malignant") #confusion matrix


expert3_birads <- db_validation$birads_d_num

roc_expert3 = roc(as.vector(db_validation$outcome),as.matrix(expert3_birads)) #Conduct the ROC analyses
auc_expert3 = pROC::auc(roc_expert3) #Calculate the area under the ROC curve
auc_CI_expert3 = pROC::ci.auc(roc_expert3, method="bootstrap", boot.stratified=TRUE) #Calculate the area under the ROC curve


binary_expert3 <- as.factor(
  ifelse(expert3_birads  ==1, "Benign", "Malignant"))

binary_expert3 <- factor(binary_expert3, levels = c("Malignant", "Benign"))

confusionMatrix(as.factor(binary_expert3), factor(db_validation$outcome), positive="Malignant") #confusion matrix




plot.roc(roc_expert2, ylim=c(0,1), xlim=c(1,0), cex.lab=1.8, cex.axis=1.5, cex.main=1.8, cex.sub=1.8, 
         legacy.axes=TRUE) #Plot the ROC curves
lines(roc_expert2, col="blue")
lines(roc_expert1, col="red")
lines(roc_expert3, col="orange")
lines(roc_xgboost_validation , col="black")

lines(roc_human_usexperts, col="grey60")
legend("bottomright", legend=c("expert 1", "expert 2", "expert 3", "xgboost", "expert agreement"), col=c("blue", "red","orange", "black", "grey60"), lwd=2, cex=1.3)


ggroc(roc_expert3)
install.packages("ggroc")

library(pROC)

# Create a list of ROC objects (roc_expert2, roc_expert1, roc_expert3, etc.)
roc_list <- list(
  expert2 = roc_expert2,
  expert1 = roc_expert1,
  expert3 = roc_expert3,
  xgboost = roc_xgboost_validation,
  human_usexperts = roc_human_usexperts
)

roc_list <- list(
  expert_1 = roc_expert2,
  expert_2 = roc_expert1,
  expert_3 = roc_expert3,
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








#### (12) compare  Performance  ####

###AUC ####

# ML vs. human
roc.test(roc_glm_validation, roc_human, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_xgboost_validation, roc_human, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_glm_validation, roc_human_usexperts_full_cohort, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_xgboost_validation, roc_human_usexperts_full_cohort, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)

roc.test(roc_glm_validation, roc_human_usexperts, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)
roc.test(roc_xgboost_validation, roc_human_usexperts, method="bootstrap", alternative = "two.sided", boot.n=2000, boot.stratified=TRUE)




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

CrossTable(db$outcome, db$site=="1", chisq = TRUE, prop.chisq = TRUE)

