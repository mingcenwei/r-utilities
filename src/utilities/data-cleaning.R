#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__DATA_CLEANING_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__DATA_CLEANING_R <- new.env()

	summarySavData <- function(savData) {
		convertAndGetType <- function(vector) {
			if (all(is.na(vector))) {
				return(list(vector = vector, type = "unknown"))
			} else if (isTRUE(all(is.na(vector) | vector == suppressWarnings(as.integer(vector))))) {
				return(list(vector = as.integer(vector), type = "integer"))
			} else if (isTRUE(all(is.na(vector) | vector == suppressWarnings(as.double(vector))))) {
				return(list(vector = as.double(vector), type = "double"))
			} else {
				return(list(vector = vector, type = typeof(vector)))
			}
		}

		savData %>%
			imap(function(column, columnName) {
				originalType <- typeof(column)
				label <- column %>% attr("label", exact = TRUE)
				choices <- column %>% attr("labels", exact = TRUE)
				c(column, type) %<-% convertAndGetType(column)
				originalType <- if_else(originalType == type, NA_character_, originalType)
				if (is.null(choices)) {
					c(choiceLabels, choiceType) %<-% list(NULL, NA_character_)
				} else {
					c(choiceLabels, choiceType) %<-% convertAndGetType(names(choices))
				}
				tibble_row(
					columnName,
					label,
					type,
					originalType,
					uniqueAnswerCount = column %>% unique() %>% length(),
					naCount = column %>% is.na() %>% sum(),
					min = (if (is.numeric(column)) {min(column, na.rm = TRUE)} else {NA_integer_}) %>% list(),
					max = (if (is.numeric(column)) {max(column, na.rm = TRUE)} else {NA_integer_}) %>% list(),
					choiceType,
					choiceCount = if (is.null(choices)) {NA_integer_} else {length(choices)},
					minChoice = (if (is.numeric(choiceLabels)) {min(choiceLabels, na.rm = TRUE)} else {NA_integer_}) %>% list(),
					maxChoice = (if (is.numeric(choiceLabels)) {max(choiceLabels, na.rm = TRUE)} else {NA_integer_}) %>% list(),
					choices = list(choices)
				)
			}) %>%
			bind_rows() %>%
			relocate(
				columnName,
				label,
				type,
				originalType,
				uniqueAnswerCount,
				naCount,
				min,
				max,
				choiceType,
				choiceCount,
				minChoice,
				maxChoice,
				choices,
			)
	}

	replaceChoicesWithLabels <- function(labelledVector, levels = c("default", "labels", "values", "both")) {
		if (!haven::is.labelled(labelledVector)) {
			stop("Not a labelled vector")
		}
		labelledVector %>% haven::as_factor(levels = levels) %>% as.character()
	}

	changeMinMax <- function(integerVector, reversed = FALSE, min = NA_integer_, max = NA_integer_, oldMin = base::min(integerVector, na.rm = TRUE), oldMax = base::max(integerVector, na.rm = TRUE)) {
		if (!is.integer(integerVector)) {
			stop("Not an integer vector")
		}
		if (
			!(reversed %in% c(TRUE, FALSE)) ||
			!all(list(min, max, oldMin, oldMax) %>% map_lgl(function(number) {
				is.integer(number) && length(number) == 1L
			}))
		) {
			stop("Invalid arguments")
		}
		vectorMin <- base::min(integerVector, na.rm = TRUE)
		vectorMax <- base::max(integerVector, na.rm = TRUE)
		if (anyNA(oldMin)) {
			oldMin <- vectorMin
		} else if (oldMin > vectorMin) {
			stop(glue::glue("`oldMin > vectorMin`: `oldMin` {oldMin}, `vectorMin` {vectorMin}"))
		}
		if (anyNA(oldMax)) {
			oldMax <- vectorMax
		} else if (oldMax < vectorMax) {
			stop(glue::glue("`oldMax < vectorMax`: `oldMax` {oldMax}, `vectorMax` {vectorMax}"))
		}
		if (anyNA(min)) {
			if (anyNA(max)) {
				min <- oldMin
				max <- oldMax
			} else {
				min <- oldMin + (max - oldMax)
			}
		} else if (anyNA(max)) {
			max <- oldMax + (min - oldMin)
		}
		if (oldMin > oldMax) {
			stop(glue::glue("`oldMin > oldMax`: `oldMin` {oldMin}, `oldMax` {oldMax}"))
		}
		if (min > max) {
			stop(glue::glue("`min > max`: `min` {min}, `max` {max}"))
		}
		if (oldMax - oldMin != max - min) {
			stop(glue::glue("`oldMax - oldMin != max - min`: `oldMin` {oldMin}, `oldMax` {oldMax}, `min` {min}, `max` {max}"))
		}
		if (reversed) {
			max + (oldMin - integerVector)
		} else {
			integerVector + (min - oldMin)
		}
	}
}
