library(forcats)
library(broom)
library(modelr)
set.seed(123)

#source("https://raw.githubusercontent.com/conorotompkins/apple_health/master/scripts/load_data.R")

df_distance_day <- df_distance_day %>% 
  mutate(quincy = as.factor(quincy))
df_distance_day


#model_lm = lm(distance_imputed ~ month * wday * quincy * location, data = df_distance_day)

#glance(model_lm)
#tidy(model_lm) %>% 
#  arrange(desc(estimate)) %>% 
#  mutate(term = fct_inorder(term)) %>% 
#  filter(abs(estimate) > 10) %>% 
#  ggplot(aes(term, estimate)) +
#  geom_point() +
#  coord_flip()

#augment(model_lm) %>% 
#  ggplot(aes(distance_imputed, .fitted)) +
#  geom_point() +
#  geom_abline() +
#  coord_cartesian(xlim = c(0, 12), ylim = c(0, 12))


model_glm <- glm(distance_imputed ~ month + wday + location + quincy, data = df_distance_day, family = Gamma(link = "identity"), start = c(rep(0, 20)))

glance(model_glm)
tidy(model_glm, .exponentiate = TRUE) %>% 
  arrange(desc(estimate)) %>% 
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(term, estimate)) +
  geom_point() +
  coord_flip()

df_augment <- augment(model_glm, type.predict = "response") %>% 
  mutate(ymd = row_number())


df_augment %>% 
  ggplot(aes(distance_imputed, .fitted)) +
  geom_point() +
  geom_abline() +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9))

df_augment %>%
  ggplot(aes(distance_imputed, .resid)) +
  geom_point()

df_augment %>%
  ggplot(aes(ymd, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2, size = 3)
  
df_augment %>% 
  select(ymd, distance_imputed, .fitted) %>% 
  gather(metric, measure, -ymd) %>% 
  ggplot(aes(ymd, measure, color = metric)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 463) +
  annotate("text", x = 500, y = 5.5, label = "Quincy", size = 8)

df_augment_cumulative <- df_augment %>% 
  select(ymd, distance_imputed, .fitted) %>% 
  gather(metric, measure, -ymd) %>% 
  group_by(metric) %>% 
  mutate(distance_cumulative = cumsum(measure))

df_augment_cumulative %>% 
  ggplot(aes(ymd, distance_cumulative, color = metric)) +
  geom_line()

