#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__CONSTANTS_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__CONSTANTS_R <- new.env()

	dir_projectRoot <- rprojroot::find_rstudio_root_file()
	dir_data <- file.path(dir_projectRoot, "data")
	dir_export <- file.path(dir_projectRoot, "export")

	timeZone <- "Asia/Shanghai"
	# timeZone <- Sys.timezone()
}
