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
				names <- NULL
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

	label <- function(object, normalize = FALSE) {
		label <- base::attr(object, "label", exact = TRUE)
		if (isTRUE(normalize)) {
			if (!is.character(label) || length(label) == 0L || anyNA(label[1L]) || label[1L] == "") {
				label <- NULL
			} else {
				label <- label[1L]
			}
		}
		return(label)
	}
	`label<-` <- function(object, value, strict = FALSE) {
		label <- value
		stopifnot(
			is.null(label) || (
				is.character(label) && length(label) != 0L && (
					!isTRUE(strict) || (
						length(label) == 1L && !anyNA(label) && label != ""
					)
				)
			)
		)
		if (is.null(label) || anyNA(label[1L]) || label[1L] == "") {
			base::attr(object, "label") <- NULL
		} else {
			base::attr(object, "label") <- label
		}
		return(object)
	}
	addLabel <- function(object, value) {
		label(object) <- value
		return(object)
	}

	isValidNonEmptyLabel <- function(label) {
		is.character(label) &&
			length(label) != 0L &&
			!anyNA(label[1L]) &&
			label[1L] != ""
	}
	makeKeepLabels <-
		function(transformer) {
			transformer <- as_mapper(transformer)
			return(function(column) {
				label <- label(column)
				newColumn <- transformer(column)
				newLabel <- label(newColumn)
				if (isValidNonEmptyLabel(label) &&
					!isValidNonEmptyLabel(newLabel)) {
					label(newColumn) <- label
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

	exportSpss <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 filenameWithoutExtension = stop(r"("filenameWithoutExtension" must be specified)"),
				 transformer = identity,
				 columnNames = c("{.columnName}", "{.label}", "{.columnName}__{.label}")[1L]) {
			namedList <- LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamedList(
				...,
				list = list,
				newNames = newNames,
				env = rlang::caller_env()
			) %>%
				map(function(dataFrame) {
					dataFrame <-
						dataFrame %>%
						mutate(across(.fns = makeKeepLabels(transformer))) %>%
						mutate(
							across(
								.cols = where(is.list),
								.fns = makeKeepLabels(LOCAL_ENVIRONMENT__DATA_EXPORTING_R$listColumnSerializer)
							)
						) %>%
						mutate(
							across(
								.cols = where(~ is(., "POSIXt")),
								.fns = makeKeepLabels(~ lubridate::with_tz(., tzone = "UTC"))
							)
						)
					glueEnv <- new.env(parent = globalenv())
					glueEnv[[".columnName"]] <- names(dataFrame)
					glueEnv[[".label"]] <- map(dataFrame, label)
					names(dataFrame) <- glue::glue(columnNames, .envir = glueEnv)
					return(dataFrame)
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

	exportExcel <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 filenameWithoutExtension = stop(r"("filenameWithoutExtension" must be specified)"),
				 transformer = identity,
				 columnNames = c("{.columnName}", "{.label}", "{.columnName}__{.label}")[1L]) {
			errors <- tibble(sheet = character(), error = character(), .rows = 0L)
			namedList <- LOCAL_ENVIRONMENT__DATA_EXPORTING_R$getNamedList(
				...,
				list = list,
				newNames = newNames,
				env = rlang::caller_env()
			) %>%
				imap(function(dataFrame, name) {
					stopifnot(name != "..__errors__..")
					dataFrame <-
						dataFrame %>%
						mutate(across(.fns = makeKeepLabels(transformer))) %>%
						mutate(
							across(
								.cols = where(is.list),
								.fns = makeKeepLabels(LOCAL_ENVIRONMENT__DATA_EXPORTING_R$listColumnSerializer)
							)
						)
					if (nrow(dataFrame) > 1048576L) {
						dataFrame <- dataFrame %>% slice_head(n = 1048576L)
						errorMessage <- paste0(
							"⚠ Error: \"",
							name,
							"\" exceeds 1,048,576 rows. Only the first 1,048,576 rows will be exported"
						)
						cat(paste0(errorMessage, "\n"))
						errors <<-
							errors %>%
							add_row(sheet = name, error = errorMessage)
					}
					if (ncol(dataFrame) > 16384L) {
						dataFrame <- dataFrame %>% select(1L:16384L)
						errorMessage <- paste0(
							"⚠ Error: \"",
							name,
							"\" exceeds 16,384 columns. Only the first 16,384 columns will be exported"
						)
						cat(paste0(errorMessage, "\n"))
						errors <<-
							errors %>%
							add_row(sheet = name, error = errorMessage)
					}
					glueEnv <- new.env(parent = globalenv())
					glueEnv[[".columnName"]] <- names(dataFrame)
					glueEnv[[".label"]] <- map(dataFrame, label)
					names(dataFrame) <- glue::glue(columnNames, .envir = glueEnv)
					return(dataFrame)
				})
			if (nrow(errors) > 0L) {
				namedList <- c(list(..__errors__.. = errors), namedList)
			}
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

	exportData <-
		function(...,
				 list = character(),
				 newNames = NULL,
				 filenameWithoutExtension = stop(r"("filenameWithoutExtension" must be specified)"),
				 spssTransformer = identity,
				 excelTransformer = identity,
				 spssColumnNames = c("{.columnName}", "{.label}", "{.columnName}__{.label}")[1L],
				 excelColumnNames = c("{.columnName}", "{.label}", "{.columnName}__{.label}")[1L]) {
			exportR(
				...,
				list = list,
				newNames = newNames,
				filenameWithoutExtension = filenameWithoutExtension
			)
			exportSpss(
				...,
				list = list,
				newNames = newNames,
				filenameWithoutExtension = filenameWithoutExtension,
				transformer = spssTransformer,
				columnNames = spssColumnNames
			)
			exportExcel(
				...,
				list = list,
				newNames = newNames,
				filenameWithoutExtension = filenameWithoutExtension,
				transformer = excelTransformer,
				columnNames = excelColumnNames
			)
		}

	addLabels <- function(dataFrame, labels) {
		labeledDataFrame <- dataFrame
		labels %>%
			pwalk(function(columnName, label, ...) {
				stopifnot(
					isValidNonEmptyLabel(columnName) && length(columnName) == 1L
				)
				label(pluck(labeledDataFrame, columnName)) <<- label
			})

		unlabeledColumns <-
			labeledDataFrame %>%
			discard(~ isValidNonEmptyLabel(label(.))) %>%
			colnames()
		if (length(unlabeledColumns) == 1L) {
			stop(paste(
				"Column",
				capture.output(dput(unlabeledColumns)),
				"has no label."
			))
		} else if (length(unlabeledColumns) >= 2L) {
			stop(paste(
				"Columns",
				capture.output(dput(unlabeledColumns)),
				"have no label."
			))
		}

		return(labeledDataFrame)
	}
}
