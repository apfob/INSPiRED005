# INSPiRED005
B-mode and Strain Breast Radiomics 

This document provides guidence on how to follow and apply the code for the INSPiRED 005 project. 
(1) image segmentation 
(2) feature extraction 
(3) model development 

1) Image segmentation was performed using Slicer 5.0.3
2) Feature extraction was performed using pyradiomics, see files:
     - for B-mode images: feature_extraction_B_clahe_pseudonymized.ipynb
     - for SE images: feature_extraction_STR_clahe_pseudonymized.ipynb

3) model development:
    - for B-mode model: code_b_clahe_VTIQ_ML_code_HD_3-4c_ pseudonymized.R, final model: Extreme Gradient Boosting Tree (xgboost)
   - for SE model: code_str_clahe_VTIQ_ML_code_HD_3-4c_ pseudonymized.R, final model: Linear Regression with Elastic Net Penalty (glm)
