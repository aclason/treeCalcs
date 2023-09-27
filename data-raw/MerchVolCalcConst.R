
MerchVolCalcConst <- data.table::fread("./data-raw/MerchVolCalcConst.csv")

usethis::use_data(MerchVolCalcConst, overwrite = TRUE)
