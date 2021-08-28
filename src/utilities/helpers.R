#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__HELPERS_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__HELPERS_R <- new.env()

	overridableNamedList <- function(...) {
		rlang::dots_list(
			...,
			.named = TRUE,
			.ignore_empty = "trailing",
			.preserve_empty = FALSE,
			.homonyms = "last",
			.check_assign = TRUE
		)
	}

	makeSupportDynamicDots <- function(func) {
		return(function(...) {
			exec(.fn = func, ...)
		})
	}

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

	LOCAL_ENVIRONMENT__HELPERS_R$timeClustering_impl <-
		function(timestamps,
				 adjacentTolerance,
				 adjacentWarningTolerance,
				 totalTolerance,
				 totalWarningTolerance,
				 clusterClosenessWarningTolerance) {
			if (length(timestamps) == 0L) {
				return(list(clusterIndices = integer(), warnings = character()))
			}

			currentClusterIndex <- 1L
			clusterIndices <- c(currentClusterIndex)
			warnings <- c(list(character()))
			startTimestamp <- timestamps[1L]
			previousTimestamp <- timestamps[1L]
			for (timestamp in timestamps[-1L]) {
				currentWarnings <- character()
				if (previousTimestamp + adjacentTolerance >= timestamp &&
					startTimestamp + totalTolerance >= timestamp) {
					if (previousTimestamp + adjacentWarningTolerance < timestamp) {
						currentWarnings <- c(currentWarnings, "Adjacent difference no greater than tolerance, but greater than warning tolerance")
					}
					if (startTimestamp + totalWarningTolerance < timestamp) {
						currentWarnings <- c(currentWarnings, "Total difference no greater than tolerance, but greater than warning tolerance")
					}
				} else {
					if (previousTimestamp + adjacentTolerance >= timestamp) {
						currentWarnings <- c(currentWarnings, "Adjacent difference no greater than tolerance, but total difference greater than tolerance")
						if (previousTimestamp + adjacentWarningTolerance < timestamp) {
							currentWarnings <- c(currentWarnings, "Adjacent difference no greater than tolerance, but greater than warning tolerance")
						}
					}
					if (previousTimestamp + clusterClosenessWarningTolerance >= timestamp) {
						currentWarnings <- c(currentWarnings, "Clusters closer than warning tolerance")
					}
					currentClusterIndex <- currentClusterIndex + 1L
					startTimestamp <- timestamp
				}
				clusterIndices <- c(clusterIndices, currentClusterIndex)
				warnings <- c(warnings, list(currentWarnings))
				previousTimestamp <- timestamp
			}
			return(list(
				clusterIndices = clusterIndices,
				warnings = warnings
			))
		}
	timeClustering <-
		function(timestamps,
				 adjacentTolerance = lubridate::minutes(5L),
				 adjacentWarningTolerance = lubridate::minutes(2L),
				 totalTolerance = lubridate::minutes(30L),
				 totalWarningTolerance = lubridate::minutes(5L),
				 clusterClosenessWarningTolerance = lubridate::minutes(30L)) {
			timestamps <- as.POSIXct(timestamps)
			timestampOrder <- order(timestamps, na.last = TRUE)
			timestampRank <-
				rank(timestamps, na.last = TRUE, ties.method = "first")
			sortedNonNaTimestamps <-
				timestamps[timestampOrder] %>% discard(anyNA)

			c(clusterIndices, warnings) %<-%
				LOCAL_ENVIRONMENT__HELPERS_R$timeClustering_impl(
					timestamps = sortedNonNaTimestamps,
					adjacentTolerance = adjacentTolerance,
					adjacentWarningTolerance = adjacentWarningTolerance,
					totalTolerance = totalTolerance,
					totalWarningTolerance = totalWarningTolerance,
					clusterClosenessWarningTolerance = clusterClosenessWarningTolerance
				)
			clusterIndices <-
				c(clusterIndices,
				  rep_len(
				  	NA_integer_,
				  	length(timestamps) - length(sortedNonNaTimestamps)
				  ))
			warnings <-
				c(warnings,
				  rep_len(
				  	list(character()),
				  	length(timestamps) - length(sortedNonNaTimestamps)
				  ))
			return(list(
				clusterIndices = clusterIndices[timestampRank],
				warnings = warnings[timestampRank]
			))
		}
}
