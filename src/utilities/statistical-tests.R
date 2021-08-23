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

	LOCAL_ENVIRONMENT__STATISTICAL_TESTS_R$list2 <-
		function(...) {
			rlang::dots_list(
				...,
				.named = TRUE,
				.ignore_empty = "all",
				.homonyms = "last",
				.check_assign = TRUE
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
		list2 <- LOCAL_ENVIRONMENT__STATISTICAL_TESTS_R$list2

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
		# 	attr(data_wider[[column]], "label") <- NULL
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
		# 	exec(
		# 		.fn = ggpubr::ggpaired,
		# 		!!!list2(
		# 			data = data_wider,
		# 			cond1 = valueColumn1,
		# 			cond2 = valueColumn2,
		# 			!!!paramsForPairedDataPlot
		# 		)
		# 	)

		barPlot <- NULL

		boxPlot <- NULL

		qqplot <-
			exec(
				.fn = ggpubr::ggqqplot,
				!!!list2(
					data = data_wider,
					x = "difference",
					!!!paramsForQqPlot
				)
			)

		histogram <-
			exec(
				.fn = ggpubr::gghistogram,
				!!!list2(
					data = data_wider,
					x = "difference",
					y = "..count..",
					binwidth = 1L,
					add_density = TRUE,
					!!!paramsForQqPlot,
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
			exec(
				.fn = rstatix::t_test,
				!!!list2(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					detailed = TRUE,
					!!!paramsForTTest
				)
			) %>%
			rstatix::add_significance(.)

		cohensD <-
			exec(
				.fn = rstatix::cohens_d,
				!!!list2(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					ci = FALSE,
					!!!paramsForCohensD
				)
			)

		wilcoxonTest <-
			exec(
				.fn = rstatix::wilcox_test,
				!!!list2(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					detailed = TRUE,
					!!!paramsForWilcoxonTest
				)
			) %>%
			rstatix::add_significance(.)

		wilcoxonEffectSize <-
			exec(
				.fn = rstatix::wilcox_effsize,
				!!!list2(
					data = data_longer,
					formula = formula,
					paired = TRUE,
					ci = FALSE,
					!!!paramsForWilcoxonEffectSize
				)
			)

		signTest <-
			exec(
				.fn = rstatix::sign_test,
				!!!list2(
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