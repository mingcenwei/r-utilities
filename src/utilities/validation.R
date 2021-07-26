#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__VALIDATION_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__VALIDATION_R <- new.env()

	LOCAL_ENVIRONMENT__VALIDATION_R$unreachable <- function() {
		stop("Unreachable code")
	}

	notNa <- function(vector) {
		if (anyNA(vector)) {
			vector %>%
				iwalk(function(value, index) {
					if (anyNA(value)) {
						stop(paste0("NA value: index ", index))
					}
				})
			localScope$unreachable()
		} else {
			return(invisible(vector))
		}
	}
	allIntegersOrNa <- function(numbers) {
		if (any(numbers != as.integer(numbers), na.rm = TRUE)) {
			numbers %>%
				iwalk(function(number, index) {
					if (!anyNA(number) && number != as.integer(number)) {
						stop(paste0("Not an integer or NA: index ", index))
					}
				})
			localScope$unreachable()
		} else {
			return(invisible(as.integer(numbers)))
		}
	}
	inClosedRangeOrNa <- function(numbers,
								  lower = -Inf,
								  upper = +Inf) {
		if (any(numbers < lower | numbers > upper, na.rm = TRUE)) {
			numbers %>%
				iwalk(function(number, index) {
					if (!anyNA(number) && (number < lower || number > upper)) {
						stop(paste0(
							"Not in [",
							lower,
							", ",
							upper,
							"] or NA: index ",
							index
						))
					}
				})
			localScope$unreachable()
		} else {
			return(invisible(numbers))
		}
	}
	inOpenRangeOrNa <- function(numbers,
								lower = -Inf,
								upper = +Inf) {
		if (any(numbers <= lower | numbers >= upper, na.rm = TRUE)) {
			numbers %>%
				iwalk(function(number, index) {
					if (!anyNA(number) && (number <= lower || number >= upper)) {
						stop(paste0(
							"Not in (",
							lower,
							", ",
							upper,
							") or NA: index ",
							index
						))
					}
				})
			localScope$unreachable()
		} else {
			return(invisible(numbers))
		}
	}
	inEnumOrNa <- function(vector, enum) {
		if (!all((vector %>% discard(is.na)) %in% enum)) {
			vector %>%
				iwalk(function(value, index) {
					if (!anyNA(value) && !(value %in% enum)) {
						stop(paste0("Not in enum or NA: index ", index))
					}
				})
			localScope$unreachable()
		} else {
			return(invisible(vector))
		}
	}
	allPatternOrNa <- function(texts, pattern) {
		if (!all(str_detect(texts, pattern), na.rm = TRUE)) {
			texts %>%
				iwalk(function(text, index) {
					if (!anyNA(text) && !(str_detect(text, pattern))) {
						stop(paste0("Wrong pattern: index ", index))
					}
				})
			localScope$unreachable()
		} else {
			return(invisible(texts))
		}
	}
	allDistinct <- function(vector) {
		duplicatedIndex <- anyDuplicated(vector)
		if (duplicatedIndex != 0L) {
			previousIndex <- match(vector[duplicatedIndex], vector)
			stop(paste0(
				"Duplicates: index ",
				previousIndex,
				", ",
				duplicatedIndex
			))
		} else {
			return(invisible(vector))
		}
	}
	identicalToGivenVector <- function(vector, givenVector) {
		if (!identical(vector, givenVector)) {
			if (!identical(typeof(vector), typeof(givenVector))) {
				stop(paste0(
					"Type mismatch: ",
					paste(typeof(vector), collapse = " "),
					", ",
					paste(typeof(givenVector), collapse = " ")
				))
			} else if (!identical(class(vector), class(givenVector))) {
				stop(paste0(
					"Class mismatch: ",
					paste(class(vector), collapse = " "),
					", ",
					paste(class(givenVector), collapse = " ")
				))
			} else if (length(vector) != length(givenVector)) {
				stop(paste0(
					"Length mismatch",
					length(vector),
					", ",
					length(givenVector)
				))
			} else {
				vector %>%
					iwalk(function(value, index) {
						if (!identical(value, givenVector[index])) {
							stop(paste0("Not identical: index ", index))
						}
					})
				localScope$unreachable()
			}
		} else {
			return(invisible(vector))
		}
	}
	equalToSequence <- function(numbers,
								start = 1L,
								step = 1L) {
		startInt <- as.integer(start)
		stepInt <- as.integer(step)
		intSequence <- ((startInt == start) && (stepInt == step)) ||
			((startInt == start) && length(numbers) == 1L) ||
			(length(numbers) == 0L)
		sequence <- seq(from = start,
						by = step,
						along.with = numbers)
		if (!all(numbers == sequence)) {
			numbers %>%
				iwalk(function(number, index) {
					if (number != sequence[index]) {
						stop(paste0("Not equal to sequence: index ", index))
					}
				})
			localScope$unreachable()
		} else {
			if (intSequence) {
				return(invisible(as.integer(numbers)))
			} else {
				return(invisible(numbers))
			}
		}
	}
	allInOrderOrNa <-
		function(vector,
				 decreasing = FALSE,
				 nonStrict = FALSE) {
			vectorWithoutNa <- vector %>% discard(anyNA)
			numOfValues <- length(vectorWithoutNa)
			if (numOfValues >= 2L) {
				if (isFALSE(decreasing) &&
					isFALSE(nonStrict) &&
					!all(vectorWithoutNa[1L:(numOfValues - 1L)] <
						 vectorWithoutNa[2L:numOfValues])) {
					nonNaIndices <- which(!is.na(vector))
					for (index in 1L:(length(nonNaIndices) - 1L)) {
						if (!(vector[nonNaIndices[index]] <
							  vector[nonNaIndices[index + 1L]])) {
							stop(paste0(
								"Not in order or NA: index ",
								nonNaIndices[index]
							))
						}
					}
					localScope$unreachable()
				} else if (isTRUE(decreasing) &&
						   isFALSE(nonStrict) &&
						   !all(vectorWithoutNa[1L:(numOfValues - 1L)] >
						   	 vectorWithoutNa[2L:numOfValues])) {
					nonNaIndices <- which(!is.na(vector))
					for (index in 1L:(length(nonNaIndices) - 1L)) {
						if (!(vector[nonNaIndices[index]] >
							  vector[nonNaIndices[index + 1L]])) {
							stop(paste0(
								"Not in order or NA: index ",
								nonNaIndices[index]
							))
						}
					}
					localScope$unreachable()
				} else if (isFALSE(decreasing) &&
						   isTRUE(nonStrict) &&
						   !all(vectorWithoutNa[1L:(numOfValues - 1L)] <=
						   	 vectorWithoutNa[2L:numOfValues])) {
					nonNaIndices <- which(!is.na(vector))
					for (index in 1L:(length(nonNaIndices) - 1L)) {
						if (!(vector[nonNaIndices[index]] <=
							  vector[nonNaIndices[index + 1L]])) {
							stop(paste0(
								"Not in order or NA: index ",
								nonNaIndices[index]
							))
						}
					}
					localScope$unreachable()
				} else if (isTRUE(decreasing) &&
						   isTRUE(nonStrict) &&
						   !all(vectorWithoutNa[1L:(numOfValues - 1L)] >=
						   	 vectorWithoutNa[2L:numOfValues])) {
					nonNaIndices <- which(!is.na(vector))
					for (index in 1L:(length(nonNaIndices) - 1L)) {
						if (!(vector[nonNaIndices[index]] >=
							  vector[nonNaIndices[index + 1L]])) {
							stop(paste0(
								"Not in order or NA: index ",
								nonNaIndices[index]
							))
						}
					}
					localScope$unreachable()
				}
			}
			return(invisible(vector))
		}
}
