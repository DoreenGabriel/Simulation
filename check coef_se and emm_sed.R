library(agridat)
library(desplot)
library(emmeans)
library(insight)
library(lme4)
library(tidyverse)
library(tidyr)

# data --------------------------------------------------------------------
dat <- as_tibble(agridat::john.alpha)

desplot(
  data = dat,
  form = gen ~ col + row | rep,
  text = gen, shorten = "none", cex = 0.7,
  out1 = rep,
  out2 = block, out2.gpar = list(col = "black", lwd = 1, lty = 2),
  show.key = F,
  layout = c(3, 1)
)

# functions ---------------------------------------------------------------
get_coef_SE <- function(mod) {
  summary(mod)$coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "Var") %>% 
    as_tibble() %>% 
    filter(str_detect(Var, "gen")) %>% 
    select(Var, Estimate, SE = `Std. Error`) %>% 
    mutate(via = "coef_SE")
}

get_emm_SED <- function(mod) {
  mod %>% 
    emmeans(specs = ~ gen) %>% 
    pairs() %>% 
    as_tibble() %>% 
    select(Var = contrast, Estimate = estimate, SE) %>% 
    mutate(via = "emm_SED")
}

get_both <- function(mod) {
  bind_rows(
    get_coef_SE(mod),
    get_emm_SED(mod)
  )
}


# Models ------------------------------------------------------------------
models <- expand_grid(
  data = list(
    "balanced" = dat,
    "unbalanced" = dat %>% slice(1:(n()-5)) # delete last five rows
  ),
  formula = list(
    "gen" = as.formula(yield ~ gen),
    "gen + rep" = as.formula(yield ~ gen + rep),
    "gen + rep + (1 | block)" = as.formula(yield ~ gen + rep + (1 | block))
  )
)

models <- models %>%
  mutate(
    data_lab = names(models$data),
    formula_lab = names(models$formula),
    is_mixed = str_detect(formula_lab, "\\|")
  )

models <- models %>%
  mutate(model = pmap(list(formula, data, is_mixed), function(form, data, is_mixed) {
    if (is_mixed) {
      lme4::lmer(form, data = data)
    } else {
      lm(form, data = data)
    }
  }))

models <- models %>% mutate(result = map(model, get_both))

models


# Plot --------------------------------------------------------------------
pdat <- models %>% unnest(result)

ggplot(data = pdat) +
  aes(y = SE, x = via) +
  geom_jitter(width = 0.1, alpha = 0.1) +
  facet_grid(formula_lab ~ data_lab) +
  stat_summary(
    color = "#00923f",
    fun = mean,
    geom = "point",
    shape = 23,
    position = position_nudge(x = -0.15)
  ) +
  stat_summary(
    color = "#00923f",
    fun = mean,
    geom = "text",
    aes(label = round(..y.., 4)),
    hjust = 1,
    position = position_nudge(x = -0.2)
  ) +
  scale_y_continuous(name = "Standard Error (of a Difference)",
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(
    name = "Obtained via",
    labels = c("coef_SE" = "summary(mod)$coefficients", "emm_SED" = "pairs(emmeans(mod))")
  ) +
  theme_bw()

