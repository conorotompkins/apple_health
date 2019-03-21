library(forcats)
library(broom)

#source("https://raw.githubusercontent.com/conorotompkins/apple_health/master/scripts/load_data.R")

df_distance_day <- df_distance_day %>% 
  mutate(quincy = as.factor(quincy))
df_distance_day


model_lm = lm(distance_imputed ~ month * wday * quincy * location, data = df_distance_day)

glance(model_lm)
tidy(model_lm) %>% 
  arrange(desc(estimate)) %>% 
  mutate(term = fct_inorder(term)) %>% 
  filter(abs(estimate) > 10) %>% 
  ggplot(aes(term, estimate)) +
  geom_point() +
  coord_flip()

augment(model_lm) %>% 
  ggplot(aes(distance_imputed, .fitted)) +
  geom_point() +
  geom_abline() +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 12))


model_glm <- glm(distance_imputed ~ month + wday, data = df_distance_day, family = Gamma)

glance(model)
tidy(model, .exponentiate = TRUE)
augment(model) %>% 
  ggplot(aes(distance_imputed, .fitted)) +
  geom_point() +
  geom_abline() +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 12))

warnings()
?glm
