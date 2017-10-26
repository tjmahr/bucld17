# These are plotting settings and helpers.
library(ggplot2)

plot_text <- list(
  x_time = "Time (ms) after noun onset",
  y_target = "Proportion of looks to familiar object",
  caption_mean_se = "Mean ± SE",
  condition = "Child hears",
  group = "Group"
)

wrap_with_defaults <- function(func, hard_defaults, soft_defaults) {
  force(func)
  force(hard_defaults)
  force(soft_defaults)
  function(...) {
    dots <- list(...)
    # overwrite soft defaults with user options
    # then overwrite with hard defaults
    args <- modifyList(modifyList(soft_defaults, dots), hard_defaults)
    do.call(func, args)
  }
}


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
  hard_defaults = list(),
  soft_defaults = list(yintercept = .5, size = 2, color = "white")
)

vline_onset <- wrap_with_defaults(
  geom_vline,
  hard_defaults = list(),
  soft_defaults = list(xintercept = 0, size = 2, color = "white")
)
