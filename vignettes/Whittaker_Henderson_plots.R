source("preamble.R", encoding = "UTF8")
source("functions.R", encoding = "UTF8")

portfolio_1d <- map(portfolios_mort, process_portfolio_1d)
portfolio_2d <- map(portfolios_LTC, process_portfolio_2d)

# Illustration du lissage pour différentes valeurs de lambda
(plot_illu_1 <- map(portfolio_1d, build_plot_illu_1, surv = FALSE)[[2]])

# Vecteurs propres
(plot_eigen <- plot_eigen_vec(length(portfolio_1d[[2]]$y), 2))

# valeurs propres par paramètres
(plot_edf_1 <- build_plot_edf_1(portfolio_1d[[2]]))

# Critères de sélection
(plot_criterion_1 <- map(portfolio_1d, build_plot_criteria_1, surv = FALSE)[[2]])
(plot_criterion_2 <- map(portfolio_1d, build_plot_criteria_2, surv = FALSE)[[2]])

# Comparaison régression / ML
df_reg <- map(portfolio_1d, \(ptf) {
  WH_1d_reg_optim(ptf$y, ptf$wt) |> 
    output_to_df() |> 
    tibble()}) |> 
  imap(\(x,y) add_column(x, size = y, .before = 1)) |> 
  reduce(bind_rows) |> 
  add_column(fit = "Régression", .before = 1)
df_surv <- map(portfolio_1d, \(ptf) {
  WH_1d_ml_optim(ptf$d, ptf$ec) |> 
    output_to_df() |> 
    tibble()}) |> 
  imap(\(x,y) add_column(x, size = y, .before = 1)) |> 
  reduce(bind_rows) |> 
  add_column(fit = "Maximum de vraisemblance", .before = 1)
df_reg_surv <- bind_rows(df_reg, df_surv) |> 
  mutate(size = factor(size, levels = c("20k", "100k", "500k")),
         fit = factor(fit, levels = c("Régression", "Maximum de vraisemblance")),
         y = if_else(y == - 20, NA_real_, exp(y)),
         y_inf = exp(y_hat - 1.96 * std_y_hat),
         y_sup = exp(y_hat + 1.96 * std_y_hat),
         y_hat = exp(y_hat))

(plot_illu_2 <- ggplot(data = df_reg_surv, aes(x = x)) +
  facet_wrap(~ size) +
  geom_line(aes(y = y_hat, color = fit), size = 1) +
  geom_point(aes(y = y), shape = 1) +
  geom_ribbon(aes(ymin = y_inf, ymax = y_sup, fill = fit), alpha = 0.2) +
  scale_y_log10(breaks = auto_select_logbreaks(df_reg_surv),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme(legend.position = "bottom") +
  labs(x = "Age",
       y = "Force de mortalité (échelle logarithmique)",
       color = "Cadre d'estimation",
       fill = "Cadre d'estimation",
       title = "Mortalité estimée pour différentes tailles de portefeuille",
       subtitle = "Lissage de Whittaker-Henderson appliqué à des données de mortalité fictives"))



save(plot_illu_1, plot_illu_2, plot_eigen, plot_edf_1, plot_criterion_1, file = "Whittaker_Henderson.RData")

# Coût d'inversion d'une matrice

build_mat_sym <- function(n) {
  
  D <- rnorm(n * n)
  dim(D) <- rep(n , 2)
  P <- crossprod(D)
}

(df_t_inv <- tibble(n = c(1, 2, 5, 10, 20) * 1e2) |> 
  mutate(P = map(n, build_mat_sym),
         P_chol = map(P, chol),
         t_solve = map_dbl(P, \(x) bench::mark(solve(x))$median[[1]]),
         t_chol = map_dbl(P, \(x) bench::mark(chol(x))$median[[1]]),
         t_chol2inv = map_dbl(P_chol, \(x) bench::mark(chol2inv(x))$median[[1]]),
         t_inv_chol = t_chol + t_chol2inv))

# Lissage et vecteurs propres

n <- c(20, 15)
q <- c(2,2)
D_mat <- map2(n, q, build_D_mat)
P <- map(D_mat, crossprod)

P_mat_1 <- diag(n[[2]]) %x% P[[1]] + P[[2]] %x% diag(n[[1]])
SVD_1 <- eigen(P_mat_1)

P_mat_2 <- 1e2 * diag(n[[2]]) %x% P[[1]] + 1e3 * P[[2]] %x% diag(n[[1]])
SVD_2 <- eigen(P_mat_2)

SVD_1$values - SVD_2$values

sum(abs(SVD_1$vectors - SVD_2$vectors))

