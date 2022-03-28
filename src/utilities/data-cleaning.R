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
}
