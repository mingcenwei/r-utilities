#!/usr/bin/Rscript --vanilla

library(zeallot)
library(magrittr)
library(tidyverse)

if (!exists("LOCAL_ENVIRONMENT__STATISTICAL_TESTS_R", mode = "environment")) {
	LOCAL_ENVIRONMENT__STATISTICAL_TESTS_R <- new.env()

	LOCAL_ENVIRONMENT__STATISTICAL_TESTS_R$isSingleNonNaString <-
		function(maybeSingleNonNaString) {
			is.character(maybeSingleNonNaString) &&
				length(maybeSingleNonNaString) == 1L &&
				!anyNA(maybeSingleNonNaString)
		}

	summaryDataFrame <- function(dataFrame, transformer = identity) {
		results <- list()

		results[["summarytools__dfSummar"]] <-
			dataFrame %>%
			mutate(across(.fns = makeKeepLabels(transformer))) %>%
			mutate(across(
				.cols = where( ~ is(., "Period")),
				.fns = makeKeepLabels(lubridate::as.duration)
			)) %>%
			summarytools::dfSummary(
				round.digits = 4L,
				varnumbers = FALSE
			)

		results[["Hmisc__describe"]] <-
			dataFrame %>%
			mutate(across(.fns = makeKeepLabels(transformer))) %>%
			select(-where(is.list)) %>%
			Hmisc::describe()

		results[["rstatix__get_summary_stats"]] <- with(list(), {
			args <- rlang::list2(
				type = "full"
			)
			subDataFrame <-
				dataFrame %>%
				mutate(across(.fns = makeKeepLabels(transformer))) %>%
				select(where(is.numeric))
			if (identical(ncol(subDataFrame), 0L)) {
				rlang::exec(rstatix::get_summary_stats, tibble(x = 0L), !!!args) %>%
					suppressWarnings() %>%
					head(0L)
			} else {
				dataFrame %>%
					mutate(across(.fns = makeKeepLabels(transformer))) %>%
					rlang::exec(rstatix::get_summary_stats, ., !!!args) %>%
					suppressWarnings()
			}
		})

		results[["psych__describe"]] <- with(list(), {
			args <- rlang::list2(
				IQR = TRUE
			)
			dataFrame <-
				dataFrame %>%
				mutate(across(.fns = makeKeepLabels(transformer))) %>%
				select(where(is.numeric))
			if (identical(ncol(dataFrame), 0L)) {
				rlang::exec(psych::describe, tibble(x = 0L, y = 0L), !!!args) %>%
					as_tibble(rownames = "variable") %>%
					head(0L)
			} else {
				summary <-
					(if (identical(ncol(dataFrame), 1L)) {
						dummyColumn <- str_interp("temporary__${dataFrame %>% colnames()}")
						dataFrame <-
							dataFrame %>%
							rowid_to_column(var = dummyColumn)
						rlang::exec(psych::describe, dataFrame, !!!args) %>%
							as_tibble(rownames = "variable") %>%
							filter(variable != !!dummyColumn)
					} else {
						rlang::exec(psych::describe, dataFrame, !!!args) %>%
							as_tibble(rownames = "variable")
					})
				summary %>%
					select(-vars)
			}
		})

		return(results)
	}

	independentSamplesTest <- function(
		data,
		groupColumn = stop(r"("groupColumn" must be specified)"),
		valueColumn = stop(r"("valueColumn" must be specified)"),
		groupNameTransformer = identity,
		drawGraph = TRUE,
		paramsForBarPlot = list(),
		paramsForBoxPlot = list(),
		paramsForQqPlot = list(),
		paramsForHistogram = list(),
		paramsForTTest = list(),
		paramsForCohensD = list(),
		paramsForWilcoxonTest = list(),
		paramsForWilcoxonEffectSize = list()
	) {
		stopifnot(
			groupColumn %>%
				is_in(c(
					"variable", "n", "min", "max", "median", "q1", "q3", "iqr", "mad", "mean", "sd", "se", "ci",
					"is.outlier", "is.extreme", "statistic", "p"
				)) %>%
				not()
		)

		data_longer <-
			data %>%
			mutate(
				!!sym(groupColumn) :=
					as_factor(as_mapper(groupNameTransformer)(!!sym(groupColumn)))
			) %>%
			select(all_of(c(groupColumn, valueColumn)))

		stopifnot(
			data_longer[[groupColumn]] %>% unique() %>% length() %>% is_in(1L:2L)
		)

		formula <- as.formula(paste(valueColumn, groupColumn, sep = " ~ "))

		summaryStatistics <-
			data_longer %>%
			group_by(!!sym(groupColumn)) %>%
			rstatix::get_summary_stats(!!sym(valueColumn), type = "full")

		barPlot <-
			rlang::exec(
				.fn = ggpubr::ggbarplot,
				!!!overridableNamedList(
					data = data_longer,
					x = groupColumn,
					y = valueColumn,
					add = c("mean_ci", "jitter"),
					!!!paramsForBarPlot
				)
			)

		boxPlot <-
			rlang::exec(
				.fn = ggpubr::ggboxplot,
				!!!overridableNamedList(
					data = data_longer,
					x = groupColumn,
					y = valueColumn,
					add = c("mean", "jitter"),
					notch = TRUE,
					!!!paramsForBoxPlot
				)
			)

		qqplot <-
			rlang::exec(
				.fn = ggpubr::ggqqplot,
				!!!overridableNamedList(
					data = data_longer,
					x = valueColumn,
					facet.by = groupColumn,
					!!!paramsForQqPlot
				)
			)

		histogram <-
			rlang::exec(
				.fn = ggpubr::gghistogram,
				!!!overridableNamedList(
					data = data_longer,
					x = valueColumn,
					y = "..count..",
					binwidth = 1L,
					add_density = TRUE,
					!!!paramsForHistogram,
				)
			)

		outliers <-
			data_longer %>%
			group_by(!!sym(groupColumn)) %>%
			rstatix::identify_outliers(!!sym(valueColumn))

		shapiroWilkNormalityTest <- with(list(), {
			groupSize <- data_longer %>% rename(group = groupColumn, value = valueColumn) %>% count(group)
			group1 <- groupSize %>% pluck("group", 1L) %>% as.character()
			group2 <- groupSize %>% pluck("group", 2L) %>% as.character()
			if ((groupSize %>% pluck("n", 1L)) < 3L) {
				if ((groupSize %>% pluck("n", 2L)) < 3L) {
					rlang::list2(
						!!group1 := "Sample size is less than 3. Shapiro-Wilk normality test cannot be used.",
						!!group2 := "Sample size is less than 3. Shapiro-Wilk normality test cannot be used."
					)
				} else if ((groupSize %>% pluck("n", 2L)) > 5000L) {
					rlang::list2(
						!!group1 := "Sample size is less than 3. Shapiro-Wilk normality test cannot be used.",
						!!group2 := "Sample size is greater than 5000. Shapiro-Wilk normality test cannot be used."
					)
				} else {
					rlang::list2(
						!!group1 := "Sample size is less than 3. Shapiro-Wilk normality test cannot be used.",
						!!group2 := data_longer %>% filter(!!sym(groupColumn) == !!group2) %>% rstatix::shapiro_test(!!sym(valueColumn))
					)
				}
			} else if ((groupSize %>% pluck("n", 1L)) > 5000L) {
				if ((groupSize %>% pluck("n", 2L)) < 3L) {
					rlang::list2(
						!!group1 := "Sample size is greater than 5000. Shapiro-Wilk normality test cannot be used.",
						!!group2 := "Sample size is less than 3. Shapiro-Wilk normality test cannot be used."
					)
				} else if ((groupSize %>% pluck("n", 2L)) > 5000L) {
					rlang::list2(
						!!group1 := "Sample size is greater than 5000. Shapiro-Wilk normality test cannot be used.",
						!!group2 := "Sample size is greater than 5000. Shapiro-Wilk normality test cannot be used."
					)
				} else {
					rlang::list2(
						!!group1 := "Sample size is greater than 5000. Shapiro-Wilk normality test cannot be used.",
						!!group2 := data_longer %>% filter(!!sym(groupColumn) == !!group2) %>% rstatix::shapiro_test(!!sym(valueColumn))
					)
				}
			} else {
				if ((groupSize %>% pluck("n", 2L)) < 3L) {
					rlang::list2(
						!!group1 := data_longer %>% filter(!!sym(groupColumn) == !!group1) %>% rstatix::shapiro_test(!!sym(valueColumn)),
						!!group2 := "Sample size is less than 3. Shapiro-Wilk normality test cannot be used."
					)
				} else if ((groupSize %>% pluck("n", 2L)) > 5000L) {
					rlang::list2(
						!!group1 := data_longer %>% filter(!!sym(groupColumn) == !!group1) %>% rstatix::shapiro_test(!!sym(valueColumn)),
						!!group2 := "Sample size is greater than 5000. Shapiro-Wilk normality test cannot be used."
					)
				} else {
					data_longer %>% group_by(!!sym(groupColumn)) %>% rstatix::shapiro_test(!!sym(valueColumn))
				}
			}
		})

		leveneEqualityOfVariancesTest <-
			data_longer %>%
			rstatix::levene_test(data = ., formula = formula)

		tTest <-
			rlang::exec(
				.fn = rstatix::t_test,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					detailed = TRUE,
					!!!paramsForTTest
				)
			) %>%
			rstatix::add_significance(.)

		cohensD <-
			rlang::exec(
				.fn = rstatix::cohens_d,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					ci = FALSE,
					!!!paramsForCohensD
				)
			)

		tTestEqualVariances <-
			rlang::exec(
				.fn = rstatix::t_test,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					var.equal = TRUE,
					detailed = TRUE,
					!!!paramsForTTest
				)
			) %>%
			rstatix::add_significance(.)

		cohensDEqualVariances <-
			rlang::exec(
				.fn = rstatix::cohens_d,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					var.equal = TRUE,
					ci = FALSE,
					!!!paramsForCohensD
				)
			)

		wilcoxonTest <-
			rlang::exec(
				.fn = rstatix::wilcox_test,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					detailed = TRUE,
					!!!paramsForWilcoxonTest
				)
			) %>%
			rstatix::add_significance(.)

		wilcoxonEffectSize <-
			rlang::exec(
				.fn = rstatix::wilcox_effsize,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					ci = FALSE,
					!!!paramsForWilcoxonEffectSize
				)
			)

		return(
			list(
				barPlot = if (drawGraph) barPlot else NULL,
				boxPlot = if (drawGraph) boxPlot else NULL,
				qqplot = if (drawGraph) qqplot else NULL,
				histogram = if (drawGraph) histogram else NULL,
				summaryStatistics = summaryStatistics,
				outliers = outliers,
				shapiroWilkNormalityTest = shapiroWilkNormalityTest,
				leveneEqualityOfVariancesTest = leveneEqualityOfVariancesTest,
				tTest = tTest,
				cohensD = cohensD,
				tTestEqualVariances = tTestEqualVariances,
				cohensDEqualVariances = cohensDEqualVariances,
				wilcoxonTest = wilcoxonTest,
				wilcoxonEffectSize = wilcoxonEffectSize
			)
		)
	}

	pairedSamplesTest <- function(
		data,
		valueColumn1 = stop(r"("valueColumn1" must be specified)"),
		valueColumn2 = stop(r"("valueColumn2" must be specified)"),
		newGroupColumn = stop(r"("newGroupColumn" must be specified)"),
		newValueColumn = stop(r"("newValueColumn" must be specified)"),
		groupNameTransformer = identity,
		drawGraph = TRUE,
		# paramsForPairedDataPlot = list(),
		paramsForBarPlot = list(),
		paramsForBoxPlot = list(),
		paramsForQqPlot = list(),
		paramsForHistogram = list(),
		paramsForTTest = list(),
		paramsForCohensD = list(),
		paramsForWilcoxonTest = list(),
		paramsForWilcoxonEffectSize = list(),
		paramsForSignTest = list()
	) {
		stopifnot(
			newGroupColumn %>%
				is_in(c(
					"variable", "n", "min", "max", "median", "q1", "q3", "iqr", "mad", "mean", "sd", "se", "ci"
				)) %>%
				not()
		)
		stopifnot(
			c(valueColumn1, valueColumn2) %>%
				map_lgl(function(column) {
					column %>%
						is_in(c(
							"difference", "is.outlier", "is.extreme"
						)) %>%
						not()
				}) %>%
				all()
		)

		data_wider <-
			data %>%
			mutate(difference = !!sym(valueColumn1) - !!sym(valueColumn2)) %>%
			select(!!sym(valueColumn1), !!sym(valueColumn2), difference)
		# for (column in c(valueColumn1, valueColumn2)) {
		# 	label(data_wider[[column]]) <- NULL
		# }

		data_longer <-
			data %>%
			pivot_longer(
				cols = c(!!sym(valueColumn1), !!sym(valueColumn2)),
				names_to = newGroupColumn,
				values_to = newValueColumn,
			) %>%
			mutate(!!sym(newGroupColumn) := as_mapper(groupNameTransformer)(!!sym(newGroupColumn))) %>%
			select(!!sym(newGroupColumn), !!sym(newValueColumn))

		formula <- as.formula(paste(newValueColumn, newGroupColumn, sep = " ~ "))

		summaryStatistics <-
			data_longer %>%
			group_by(!!sym(newGroupColumn)) %>%
			rstatix::get_summary_stats(!!sym(newValueColumn), type = "full")

		# pairedDataPlot <-
		# 	rlang::exec(
		# 		.fn = ggpubr::ggpaired,
		# 		!!!overridableNamedList(
		# 			data = data_wider,
		# 			cond1 = valueColumn1,
		# 			cond2 = valueColumn2,
		# 			!!!paramsForPairedDataPlot
		# 		)
		# 	)

		barPlot <-
			rlang::exec(
				.fn = ggpubr::ggbarplot,
				!!!overridableNamedList(
					data = data_longer,
					x = newGroupColumn,
					y = newValueColumn,
					add = c("mean_ci", "jitter"),
					!!!paramsForBarPlot
				)
			)

		boxPlot <-
			rlang::exec(
				.fn = ggpubr::ggboxplot,
				!!!overridableNamedList(
					data = data_longer,
					x = newGroupColumn,
					y = newValueColumn,
					add = c("mean", "jitter"),
					notch = TRUE,
					!!!paramsForBoxPlot
				)
			)

		qqplot <-
			rlang::exec(
				.fn = ggpubr::ggqqplot,
				!!!overridableNamedList(
					data = data_wider,
					x = "difference",
					!!!paramsForQqPlot
				)
			)

		histogram <-
			rlang::exec(
				.fn = ggpubr::gghistogram,
				!!!overridableNamedList(
					data = data_wider,
					x = "difference",
					y = "..count..",
					binwidth = 1L,
					add_density = TRUE,
					!!!paramsForHistogram,
				)
			)

		outliers <-
			data_wider %>%
			rstatix::identify_outliers(difference)

		shapiroWilkNormalityTest <-
			if (nrow(data_wider) < 3L) {
				"Sample size is less than 3. Shapiro-Wilk normality test cannot be used."
			} else if (nrow(data_wider) > 5000L) {
				"Sample size is greater than 5000. Shapiro-Wilk normality test cannot be used."
			} else {
				data_wider %>%
					rstatix::shapiro_test(difference)
			}

		tTest <-
			rlang::exec(
				.fn = rstatix::t_test,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					detailed = TRUE,
					!!!paramsForTTest
				)
			) %>%
			rstatix::add_significance(.)

		cohensD <-
			rlang::exec(
				.fn = rstatix::cohens_d,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					ci = FALSE,
					!!!paramsForCohensD
				)
			)

		wilcoxonTest <-
			rlang::exec(
				.fn = rstatix::wilcox_test,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					detailed = TRUE,
					!!!paramsForWilcoxonTest
				)
			) %>%
			rstatix::add_significance(.)

		wilcoxonEffectSize <-
			rlang::exec(
				.fn = rstatix::wilcox_effsize,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					ci = FALSE,
					!!!paramsForWilcoxonEffectSize
				)
			)

		signTest <-
			rlang::exec(
				.fn = rstatix::sign_test,
				!!!overridableNamedList(
					data = data_longer,
					formula = formula,
					detailed = TRUE,
					!!!paramsForSignTest
				)
			) %>%
			rstatix::add_significance(.)

		return(
			list(
				# pairedDataPlot = if (drawGraph) pairedDataPlot else NULL,
				barPlot = if (drawGraph) barPlot else NULL,
				boxPlot = if (drawGraph) boxPlot else NULL,
				qqplot = if (drawGraph) qqplot else NULL,
				histogram = if (drawGraph) histogram else NULL,
				summaryStatistics = summaryStatistics,
				outliers = outliers,
				shapiroWilkNormalityTest = shapiroWilkNormalityTest,
				tTest = tTest,
				cohensD = cohensD,
				wilcoxonTest = wilcoxonTest,
				wilcoxonEffectSize = wilcoxonEffectSize,
				signTest = signTest
			)
		)
	}
}
