#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__CONSTANTS_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__CONSTANTS_R <- new.env()

	projectRoot <- rprojroot::find_rstudio_root_file()

	timeZone <- "Asia/Shanghai"
	# timeZone <- Sys.timezone()
}
