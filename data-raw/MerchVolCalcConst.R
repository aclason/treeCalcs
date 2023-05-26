
MerchVolCalcConst <- data.table::fread("C:/Users/farne/OneDrive/Documents/Borealis_Ecological_Services/BVRC_21-37_MultiValues/FireRehab_Multivalues/Inputs/MerchVolCalcConst.csv")

usethis::use_data(MerchVolCalcConst, overwrite = TRUE)
install.packages("usethis")
library(usethis)
