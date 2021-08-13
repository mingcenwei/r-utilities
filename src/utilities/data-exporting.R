#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__DATA_EXPORTING_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__DATA_EXPORTING_R <- new.env()

	LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNameAndValue <-
		function(nameOrValue,
				 mode = "any",
				 env = rlang::caller_env()) {
			quo <- rlang::enquo(nameOrValue)
			expr <- rlang::quo_get_expr(quo)
			if (is.character(expr)) {
				name <- expr
				value <- get(name, envir = env, mode = mode)
			} else {
				name <- if (is.symbol(expr))
					as.character(expr)
				else
					NA_character_
				value <- rlang::eval_tidy(quo)
			}
			return(list(name = name, value = value))
		}

	LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamesAndValues <-
		function(...,
				 list = character(),
				 mode = "any",
				 env = rlang::caller_env()) {
			exprList <- rlang::enexprs(...)
			if (length(exprList) != 0L && length(list) != 0L) {
				stop(r"(Either "..." or "list" should be empty)")
			}
			getNameAndValue <-
				LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNameAndValue
			if (length(exprList) != 0L) {
				namesAndValues <-
					exprList %>%
					map(~ getNameAndValue(!!., mode = mode, env = env))
				names <-
					namesAndValues %>%
					map_chr("name")
				if (all(is.na(names))) {
					names <- NULL
				} else {
					names <- replace_na(names, "")
				}

			} else if (length(list) != 0L) {
				namesAndValues <-
					list %>%
					map(~ getNameAndValue(!!., mode = mode, env = env))
				names <-
					namesAndValues %>%
					map_chr("name")
				givenNames <- names(namesAndValues)
				if (is.null(givenNames)) {
					if (all(is.na(names))) {
						names <- NULL
					} else {
						names <- replace_na(names, "")
					}
				} else {
					names <- case_when(
						givenNames != "" ~ givenNames,
						!is.na(names) ~ names,
						TRUE ~ ""
					)
				}
			} else {
				namesAndValues <- list()
			}
			namesAndValues <-
				namesAndValues %>%
				map("value")
			names(namesAndValues) <- names
			return(namesAndValues)
		}

	LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamedList <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 env = rlang::caller_env()) {
			namedList <- LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamesAndValues(
				...,
				list = list,
				mode = "list",
				env = env
			)
			if (is.null(newNames)) {
				newNames <- rlang::names2(namedList)
			}
			stopifnot(is.character(newNames))
			stopifnot(length(newNames) == length(namedList))
			stopifnot(!anyNA(newNames))
			stopifnot(!any(duplicated(newNames)))
			stopifnot(!any(newNames == ""))
			names(namedList) <- newNames
			return(namedList)
		}

	LOCAL_ENVIRONMENT__DATA_EXPORTING_R$makeKeepLabels <-
		function(transformer) {
			transformer <- as_mapper(transformer)
			isValidLabel <- function(label) {
				!is.null(label) &&
					is.character(label) &&
					length(label) != 0L &&
					!anyNA(label[1L]) &&
					label[1L] != ""
			}
			return(function(column) {
				label <- attr(column, "label", exact = TRUE)
				newColumn <- transformer(column)
				newLabel <- attr(newColumn, "label", exact = TRUE)
				if (isValidLabel(label) && !isValidLabel(newLabel)) {
					attr(newColumn, "label") <- label
				}
				return(newColumn)
			})
		}

	LOCAL_ENVIRONMENT__DATA_EXPORTING_R$listColumnSerializer <-
		function(list) {
			if (is.list(list)) {
				list %>%
					map_chr(~ paste(capture.output(dput(.)), collapse = ""))
			} else {
				list
			}
		}

	exportR <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 filenameWithoutExtension = stop(r"("filenameWithoutExtension" must be specified)")) {
			namedList <- LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamedList(
				...,
				list = list,
				newNames = newNames,
				env = rlang::caller_env()
			)
			save(
				list = rlang::names2(namedList),
				file = paste0(filenameWithoutExtension, ".RData"),
				envir = as.environment(namedList),
				compress = "xz",
				compression_level = 9L
			)
		}

	exportExcel <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 filenameWithoutExtension = stop(r"("filenameWithoutExtension" must be specified)"),
				 transformer = LOCAL_ENVIRONMENT__DATA_EXPORTING_R$transformer) {
			namedList <- LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamedList(
				...,
				list = list,
				newNames = newNames,
				env = rlang::caller_env()
			) %>%
				map(function(dataFrame) {
					dataFrame %>%
						mutate(across(.fns = LOCAL_ENVIRONMENT__DATA_EXPORTING_R$makeKeepLabels(transformer))) %>%
						mutate(
							across(
								.cols = where(is.list),
								.fns = LOCAL_ENVIRONMENT__DATA_EXPORTING_R$makeKeepLabels(LOCAL_ENVIRONMENT__DATA_EXPORTING_R$listColumnSerializer)
							)
						)
				})
			WriteXLS::WriteXLS(
				x = namedList,
				ExcelFileName = paste0(filenameWithoutExtension, ".xlsx"),
				SheetNames = names(namedList),
				Encoding = "UTF-8",
				AllText = FALSE,
				row.names = FALSE,
				col.names = TRUE,
				AdjWidth = TRUE,
				AutoFilter = TRUE,
				BoldHeaderRow = TRUE,
				FreezeRow = 1L
			)
		}

	exportSpss <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 filenameWithoutExtension = stop(r"("filenameWithoutExtension" must be specified)"),
				 transformer = identity) {
			namedList <- LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamedList(
				...,
				list = list,
				newNames = newNames,
				env = rlang::caller_env()
			) %>%
				map(function(dataFrame) {
					dataFrame %>%
						mutate(across(.fns = LOCAL_ENVIRONMENT__DATA_EXPORTING_R$makeKeepLabels(transformer))) %>%
						mutate(
							across(
								.cols = where(is.list),
								.fns = LOCAL_ENVIRONMENT__DATA_EXPORTING_R$makeKeepLabels(LOCAL_ENVIRONMENT__DATA_EXPORTING_R$listColumnSerializer)
							)
						) %>%
						mutate(
							across(
								.cols = where(~ is(., "POSIXt")),
								.fns = LOCAL_ENVIRONMENT__DATA_EXPORTING_R$makeKeepLabels(~ lubridate::with_tz(., tzone = "UTC"))
							)
						)
				})
			namedList %>%
				iwalk(function(dataFrame, name) {
					haven::write_sav(
						data = dataFrame,
						path = paste0(filenameWithoutExtension, "__", name, ".zsav"),
						compress = TRUE
					)
				})
		}

	exportData <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 filenameWithoutExtension = stop(r"("filenameWithoutExtension" must be specified)"),
				 excelTransformer = identity,
				 spssTransformer = identity) {
			exportR(
				...,
				list = list,
				newNames = newNames,
				filenameWithoutExtension = filenameWithoutExtension
			)
			exportExcel(
				...,
				list = list,
				newNames = newNames,
				filenameWithoutExtension = filenameWithoutExtension,
				transformer = excelTransformer
			)
			exportSpss(
				...,
				list = list,
				newNames = newNames,
				filenameWithoutExtension = filenameWithoutExtension,
				transformer = spssTransformer
			)
		}

	addLabels <- function(dataFrame, labels) {
		labeledDataFrame <- dataFrame
		labels %>%
			pwalk(function(columnName, label, ...) {
				stopifnot(!anyNA(columnName))
				attr(pluck(labeledDataFrame, columnName), "label") <<- label
			})
		stopifnot(
			labeledDataFrame %>%
				map(~ attr(., "label", exact = TRUE)) %>%
				imap_lgl(function(label, columnName) {
					if (!is.null(label)) {
						return(TRUE)
					} else {
						return(FALSE)
					}
				}) %>%
				all()
		)
		return(labeledDataFrame)
	}
}
