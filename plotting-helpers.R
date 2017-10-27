# These are plotting settings and helpers.
library(ggplot2)
library(rlang)
library(dplyr, warn.conflicts = FALSE)

plot_text <- list(
  x_time = "Time (ms) after noun onset",
  y_target = "Proportion of looks to familiar object",
  y_fits = "Predicted prop. looks to familiar",
  caption_mean_se = "Mean ± SE",
  condition = "Child hears",
  group = "Group",
  group_intercept_smooth = "Line and ribbon: Group mean ± SE",
  x_match_pairs = "Pairs matched on age, sex and maternal ed."
)





#' Resequence a set of integer indices
#'
#' This function is useful for plotting by indices when there are gaps between
#' some indices' values. For example, if some subject IDs are numbered in the
#' 100's for group x and the 300's for group y, then when subject ID is used as
#' the x-axis in a plot, the plot axis would include all the empty indices
#' separating the two groups (which is ugly). Resequencing the IDs would remove
#' that gap, while preserving the relative ordering of the indices.
#'
#' @examples
#' resequence(c(10, 1, 3, 8, 10, 10))
#' #> [1] 4 1 2 3 4 4
resequence <- function(xs) {
  keys <- sort(unique(xs))
  values <- seq_along(keys)
  unname(setNames(values, keys)[as.character(xs)])
}


#' Wrap a function so that some arguments have default values
#'
#' @param func a function to wrap
#' @param hard_defaults a list of "hard" default argument values that cannot be
#'   overwritten by the user
#' @param soft_defaults a list of default argument values that can be
#'   overwritten by the user
#' @return an updated version of `func()` that uses the given default values
#' @examples
#' seq2 <- wrap_with_defaults(seq, hard_defaults = list(by = 2))
#' seq2(from = 1, to = 10)
#' #> [1] 1 3 5 7 9
#'
#' # Can't change the `by` value
#' seq2(from = 1, to = 10, by = 3)
#' #> [1] 1 3 5 7 9
wrap_with_defaults <- function(func, hard_defaults = list(), soft_defaults = list()) {
  force(func)
  force(hard_defaults)
  force(soft_defaults)
  function(...) {
    dots <- list(...)
    # overwrite soft defaults with user options
    # then overwrite with hard defaults
    args <- modifyList(
      modifyList(soft_defaults, dots, keep.null = TRUE),
      hard_defaults, keep.null = TRUE)
    do.call(func, args)
  }
}


# Count from 0 because ggplot2 looks better when 0 is in range x values
seq_by_one <- function(range) {
  seq(from = 0, max(range), by = 1)
}

# Useful for drawing a gridline at each step in a sequence
scale_x_continuous_index <- wrap_with_defaults(
  scale_x_continuous,
  hard_defaults = list(breaks = seq_by_one, minor_breaks = NULL),
  soft_defaults = list(expand = c(0.025, 0), labels = NULL)
)

legend_bottom <- wrap_with_defaults(
  theme,
  hard_defaults = list(legend.position = "bottom"),
  soft_defaults = list(legend.justification = "left")
)

legend_top <- wrap_with_defaults(
  theme,
  hard_defaults = list(legend.position = "top"),
  soft_defaults = list(legend.justification = "left")
)

stat_mean <- wrap_with_defaults(
  stat_summary,
  hard_defaults = list(fun.y = mean),
  soft_defaults = list(geom = "line")
)

stat_mean_se <- wrap_with_defaults(
  stat_summary,
  hard_defaults = list(fun.data = mean_se),
  soft_defaults = list(geom = "pointrange")
)

hline_chance <- wrap_with_defaults(
  geom_hline,
  soft_defaults = list(yintercept = .5, size = 2, color = "white")
)

vline_onset <- wrap_with_defaults(
  geom_vline,
  soft_defaults = list(xintercept = 0, size = 2, color = "white")
)

align_axis_right <- wrap_with_defaults(
  theme,
  soft_defaults = list(
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1))
)


tidy_quantile <- function(df, var, prob) {
  UseMethod("tidy_quantile")
}

tidy_quantile.default <- function(df, var, probs = seq(.1, .9, .2)) {
  q <- enquo(var)
  eval_tidy(q, data = df) %>%
    quantile(probs, na.rm = TRUE) %>%
    tibble::enframe("quantile", value = quo_name(q))
}

tidy_quantile.grouped_df <- function(df, var, probs = seq(.1, .9, .2)) {
  q <- enquo(var)

  groups <- split(df, group_indices(df)) %>%
    lapply(select, !!! group_vars(df)) %>%
    lapply(distinct) %>%
    lapply(ungroup) %>%
    bind_rows(.id = "....id")

  quantiles <- split(df, group_indices(df)) %>%
    lapply(ungroup) %>%
    lapply(tidy_quantile, !! q, probs) %>%
    bind_rows(.id = "....id")

  groups %>%
    left_join(quantiles, by = "....id") %>%
    select(-`....id`)
}



## Prediction helpers
predict_fixef <- function(...) {
  predict(..., re.form = ~ 0)
}

predict_fixef_resp <- function(...) {
  predict_fixef(..., type = "response")
}

predict_fixef_link <- function(...) {
  predict_fixef(..., type = "link")
}

# Create a set of predictions for a list of models
map_predict <- function(data, models, f_predict, col_to_add = "fitted") {
  # if user didn't give a list of models, make one
  what_user_entered <- enexpr(models)
  if (!is_list(models)) {
    model_name <- expr_name(what_user_entered)
    models <- set_names(list(models), model_name)
  }

  fits <- lapply(models, function(model) {
    fitted <- list(f_predict(model, newdata = data))
    column <- set_names(fitted, col_to_add)
    tibble::add_column(data, !!! column)
  })
  bind_rows(fits, .id = "model")
}

