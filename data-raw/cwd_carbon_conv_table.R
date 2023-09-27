
cwdC_conv_table <- data.table::fread("./data-raw/cwd_carbon_conversion_factors_forTreeCalcs.csv")

usethis::use_data(cwdC_conv_table, overwrite = TRUE)
