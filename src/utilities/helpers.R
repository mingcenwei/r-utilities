#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__HELPERS_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__HELPERS_R <- new.env()

	getLatestFile <- function(dir) {
		fileList <-
			list.files(path = dir,
					   pattern = r"(\d{4}\-\d{2}\-\d{2}_\d{2}\-\d{2}\-\d{2})",
					   full.names = TRUE) %>%
			sort()
		return(last(fileList))
	}
	linkLatestFile <- function(dir) {
		file <- getLatestFile(dir)
		stopifnot(!is.null(file))
		latest <- file.path(dir, "latest")
		system2(
			command = "ln",
			args = c(
				"--symbolic",
				"--relative",
				"--force",
				"--no-target-directory",
				file,
				latest
			)
		)
		return(invisible(latest))
	}

	# manuallyCorrectedData <-
	# 	tribble(
	# 		~ id, ~ column, ~ original, ~ new, ~ equal,
	# 		112L, "birthYear", list(196910L), list(1969L), list(NULL),
	# 	)
	manuallyCorrectDataFrame <-
		function(dataFrame,
				 manuallyCorrectedData,
				 idColumn = "id",
				 defaultEqual = identical) {
			manuallyCorrectedData %>%
				array_branch(margin = 1L) %>%
				reduce(
					function(dataFrame, correction) {
						c(id, column, original, new, equal) %<-% correction
						original <- original[[1L]]
						new <- new[[1L]]
						equal <- equal[[1L]]
						rowIndex <-
							which(dataFrame[[idColumn]] == id)
						stopifnot(length(rowIndex) <= 1L)
						if (identical(length(rowIndex), 1L)) {
							value <- dataFrame[[column]][rowIndex]
							equalCheck <-
								if (is.null(equal)) defaultEqual else equal
							if (equalCheck(value, original)) {
								dataFrame[[column]][rowIndex] <- new
								cat(str_interp(
									"✅ Corrected: id ${id}, column ${column}\n"
								))
							} else {
								cat(str_interp(
									"❌ Skipped (value not match): id ${id}, column ${column}\n"
								))
							}
						} else {
							cat(str_interp(
								"❌ Skipped (id not exist): id ${id}, column ${column}\n"
							))
						}
						return(dataFrame)
					},
					.init = dataFrame
				)
		}
}
