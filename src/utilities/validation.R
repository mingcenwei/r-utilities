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
			LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
			LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
			LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
			LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
			LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
			LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
					"Length mismatch: ",
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
				LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
			LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
					LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
					LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
					LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
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
					LOCAL_ENVIRONMENT__VALIDATION_R$unreachable()
				}
			}
			return(invisible(vector))
		}
	simultaneouslyNa <- function(dataFrame, includingNull = TRUE) {
		checkNa <- if (isTRUE(includingNull)) {
			(function(column) {
				if (is.list(column)) {
					column <- flatten(column)
				}
				map_lgl(column, is.null) | base::is.na(column)
			})
		} else if (isFALSE(includingNull)) {
			(function(column) {
				if (is.list(column)) {
					column <- flatten(column)
				}
				base::is.na(column)
			})
		} else {
			stop("Invalid arguments: simultaneousNa")
		}

		naDataFrame <- dataFrame %>% mutate(across(.cols = everything(), .fns = checkNa))
		firstColumn <- naDataFrame[[1L]]
		successful <- TRUE
		for (column in naDataFrame %>% select(-1L)) {
			if (!isTRUE(all(firstColumn == column))) {
				successful <- FALSE
				break
			}
		}
		if (!isTRUE(successful)) {
			errorMessages <-
				dataFrame %>%
				mutate(across(.cols = everything(), .fns = checkNa)) %>%
				rename_with(~ str_c("prefix_", .)) %>%
				rowwise() %>%
				transmute(
					na = c_across(starts_with("prefix_")) %>% which() %>% str_c(collapse = ", "),
					notNa = c_across(starts_with("prefix_")) %>% not() %>% which() %>% str_c(collapse = ", "),
				) %>%
				ungroup() %>%
				mutate(rowIndex = row_number()) %>%
				transmute(
					message = if_else(
						na == "" | notNa == "",
						NA_character_,
						glue::glue_safe("Row index {rowIndex}: NA column indices {na}; Non-NA column indices {notNa}") %>% as.character()
					)
				) %>%
				filter(!is.na(message)) %>%
				pull(message) %>%
				str_c(collapse = "\n")
			stop(errorMessages)
		}
		return(invisible(dataFrame))
	}
}
