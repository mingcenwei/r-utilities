#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__PROJECT_HELPERS_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__PROJECT_HELPERS_R <- new.env()
}
