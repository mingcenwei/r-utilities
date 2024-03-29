#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__INDEX_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__INDEX_R <- new.env()

	LOCAL_ENVIRONMENT__INDEX_R$dir_utilities <- "src/utilities"

	list.files(
		file.path(rprojroot::find_rstudio_root_file(), LOCAL_ENVIRONMENT__INDEX_R$dir_utilities),
		pattern = r"(.+\.R$)",
		full.names = TRUE
	) %>%
		walk(~ source(.))
}
