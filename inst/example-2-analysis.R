
op = rlang::env_get(basic_writer$env, 'output_path', default= NULL, inherit = TRUE)
outcome_files = fs::dir_ls(op, regexp  = 'outcomes--')
cohort_birth = purrr::map(outcome_files, readRDS) %>%
  purrr::lift_dl(dplyr::bind_rows)() %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(cohort_birth = min(time))
summary_data = purrr::map(outcome_files, readRDS) %>% 
  purrr::lift_dl(dplyr::bind_rows)() %>%
  dplyr::left_join(y = cohort_birth, by = 'group') %>%
  dplyr::mutate(cohort_age = time - cohort_birth)

timepoint_files = fs::dir_ls(op, regexp = 'timepoints--')
complete_files = fs::dir_ls(op, regexp = 'complete--')

pl_ts = function(
  data, x, y, 
  xlab = "cohort age (days)", 
  ylab,
  vline = 14,
  xlim = c(0, vline + 6),
  ylim = c(0, 100),
  grouping,
  ... 
) {
  x = rlang::enquo(x)
  y = rlang::enquo(y)
  data = tidyr::pivot_longer(data, !!y, names_to = "metrics")
  grouping = rlang::enquo(grouping)
  groups = dplyr::select(data, !!grouping) %>% 
    purrr::pmap(paste, sep = '--') %>%
    purrr::flatten_chr()
  pl = split(data, f = groups) %>% purrr::map( ~ 
    ggplot() +
      geom_line(data = .x, aes(x = !!x, y = value, colour = metrics)) + 
      geom_vline(xintercept = vline, colour = 'grey', linetype = 2) +
      scale_x_continuous(xlab) +
      scale_y_continuous(ylab) + 
      coord_cartesian(xlim = xlim, ylim = ylim) +
      theme_minimal() +
      theme(legend.position = c(0.3, 0.9)))
  extras = list(...)
  for (item in extras) {
    pl = purrr::map(pl, ~ .x + item)
  }
  return(pl)
}


pl_sim_summary_A = summary_data %>% 
  dplyr::mutate(`infected, undetected` = n_infectious_escaped + n_exposed_escaped,
                `infected, total` = n_infectious + n_exposed,
                `total - isolated` = n_total - n_isolated,
                `new` = n_infections) %>%
  pl_ts(x = cohort_age, y = c(tidyr::matches("infected, .*"), tidyr::matches("total - isolated"), new),
    xlab = "cohort age (days)", 
    ylab = "detainee count",
    vline = 14, xlim = c(0,20), ylim = c(0, 100),
    grouping = group)

pdf(file = '/tmp/infected-not-isolated-plots.pdf'); 
gridExtra::marrangeGrob(pl_sim_summary_A, nrow = 2, ncol = 2)
dev.off()


summary_intake_start = summary_data %>%
  dplyr::filter(cohort_age == 0) %>%
  dplyr::group_by(group) %>%
  dplyr::summarize(n_total = n_total,
                   n_isolated_start = n_isolated,
                   n_cohort_start = n_total - n_isolated,
                   n_escaped_cases_start = (n_infectious_escaped + n_exposed_escaped),
                   p_escaped_case_start = n_escaped_cases_start / n_cohort_start
  )

summary_intake_end = summary_data %>%
  dplyr::filter(cohort_age == 14) %>% 
  dplyr::group_by(group) %>%
  dplyr::summarize(n_isolated_end = n_isolated,
                   n_cohort_end = n_total - n_isolated_end,
                   n_escaped_cases_end = (n_infectious_escaped + n_exposed_escaped),
                   p_escaped_case_end = n_escaped_cases_end / n_cohort_end,
                   pr_escaped_case_end = n_escaped_cases_end > 0)

summary_of_cohorts = dplyr::left_join(summary_intake_start, summary_intake_end, by = 'group')

pl_summary_of_cohorts = ggplot(
  data = summary_of_cohorts, aes(x = p_escaped_case_start, y = p_escaped_case_end)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous("individual Pr[infected, undetected | day 0]", 
    breaks = c(0.01, 0.025, 0.05, 0.075, 0.09, 0.1)) +
  scale_y_continuous("individual Pr[infected, undetected | day 14]",
    breaks = c(0.1, 0.25, 0.5, 0.75, 0.9, 1.0)) +
  coord_cartesian(xlim = c(0, .1), ylim = c(0, 1.1)) +
  theme_minimal()
pdf(file = '/tmp/cohort-infection-risk-plot.pdf');
print(pl_summary_of_cohorts); dev.off()

pl_sim_summary_B = ggplot() +
  geom_line(data = summary_data %>% dplyr::filter((n_infectious + n_exposed + n_recovered) < n_total), 
    aes(x = cohort_age, y = 100 * p_infection, group = group), alpha = 0.1) +
  geom_vline(xintercept = 14, colour = 'grey', linetype = 2) +
  scale_x_continuous("cohort age (days)") +
  scale_y_continuous("individual infection risk (%)") +
  coord_cartesian(xlim = c(0,21), ylim = c(0,105)) +
  theme_minimal() +
  theme(legend.position = c(0.3, 0.9))

pdf(file = '/tmp/individual-infection-risk-plot.pdf');
print(pl_sim_summary_B); dev.off()


