############
## 第一組 ##
############

library(rstatix)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(showtext)
library(gridExtra)
library(emmeans)
library(multcomp)
showtext_auto()

# 讀取資料
data <- read.csv("第一組.csv")
head(data)

##################
## Mixed Model  ##
##################

# 進行抽樣 與 資料編號整理 (整理成 mixed_data)
general_cities <- data %>% 
  filter(地區 == "一般縣市") %>% 
  distinct(縣市) %>% 
  pull(縣市)

set.seed(50)
random_cities <- sample(general_cities, 6)

mixed_data <- data %>% 
  filter(地區 == "直轄市" | 縣市 %in% random_cities)

mixed_data <-  mixed_data[order(mixed_data$季度,mixed_data$地區,mixed_data$品項定位,mixed_data$款式),]

# 編號部分是依照library(rstatix)的要求
mixed_data <- mixed_data %>%
  group_by(款式) %>%
  mutate(ID = row_number()) %>%
  ungroup()

# mixed model
mixed_model = anova_test(data = mixed_data, dv = 銷售總金額, wid = ID,
                   between = c(季度,品項定位,地區), within = 款式)

# Residual 殘差分析
fitted_values <- mixed_data %>%
  group_by(季度, 品項定位, 地區, 款式) %>%
  summarise(fitted = mean(銷售總金額), .groups = "drop") %>%
  right_join(mixed_data, by = c("季度", "品項定位", "地區", "款式"))

fitted_values <- fitted_values %>%
  mutate(residuals = 銷售總金額 - fitted)

qqnorm(scale(fitted_values$residuals))
qqline(scale(fitted_values$residuals), col = "red")

plot(fitted_values$fitted, scale(fitted_values$residuals),
     xlab = "Fitted Values", 
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Plot",
     pch = 19,
     abline(h = 0, col = "red", lwd = 2) )

get_anova_table(mixed_model)

##################
## Fixed Model  ##
##################

# 四因子：季度、地區、品項定位、款式
fixed_model <- lm(銷售總金額 ~ 季度 * 地區 * 品項定位 * 款式, data = data)

# Residual 殘差分析
qqnorm(scale(residuals(fixed_model)))
qqline(scale(residuals(fixed_model)), col = "red")

plot(fitted(fixed_model), scale(residuals(fixed_model)),
     xlab = "Fitted Values", 
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Plot",
     pch = 19,
     abline(h = 0, col = "red", lwd = 2) )

anova(fixed_model)



# 三因子：款式、地區、品項定位
fixed_model_3factor_1 <- lm(銷售總金額 ~ 品項定位 * 地區 * 款式, data = data)

# Residual 殘差分析
qqnorm(scale(residuals(fixed_model_3factor_1)))
qqline(scale(residuals(fixed_model_3factor_1)), col = "red")

plot(fitted(fixed_model_3factor_1), scale(residuals(fixed_model_3factor_1)),
     xlab = "Fitted Values", 
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Plot",
     pch = 19,
     abline(h = 0, col = "red", lwd = 2) )

anova(fixed_model_3factor_1)



# 三因子：季度、地區、品項定位
fixed_model_3factor_2 <- lm(銷售總金額 ~ 品項定位* 地區 * 季度, data = data)

# Residual 殘差分析
qqnorm(scale(residuals(fixed_model_3factor_2)))
qqline(scale(residuals(fixed_model_3factor_2)), col = "red")

plot(fitted(fixed_model_3factor_2), scale(residuals(fixed_model_3factor_2)),
     xlab = "Fitted Values", 
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Plot",
     pch = 19,
     abline(h = 0, col = "red", lwd = 2) )

anova(fixed_model_3factor_2)


# Contrast 款式 (-0.5 * 自動扣 - 0.5 * 一般款 + 1 * 針棒)
data$款式 = as.factor(data$款式)
contrast_model = aov(銷售總金額~款式, data=data)
contrast = rbind("一般款+自動扣vs針棒" = c(-1,-1,2))

contrast_test = glht(contrast_model, linfct = mcp(款式 = contrast))
summary(contrast_test)




# Polynomial 季度
data$季度數值 <- as.numeric(factor(data$季度, levels = c("第一季", "第二季", "第三季", "第四季")))

data$linear <- poly(data$季度數值, degree = 2)[, 1]
data$quadratic <- poly(data$季度數值, degree = 2)[, 2]

poly_model <- lm(銷售總金額 ~ (地區 * 品項定位 * 款式 ) * (linear + quadratic), data = data)

# Residual 殘差分析
qqnorm(scale(residuals(poly_model)))
qqline(scale(residuals(poly_model)), col = "red")

plot(fitted(poly_model), scale(residuals(poly_model)),
     xlab = "Fitted Values", 
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Plot",
     pch = 19,
     abline(h = 0, col = "red", lwd = 2) )

anova(poly_model)


# Nested model 地區：縣市
nested_model <- aov(銷售總金額 ~ (季度 * 品項定位 * 款式) + 地區/縣市, data = data)

qqnorm(scale(residuals(nested_model)))
qqline(scale(residuals(nested_model)), col = "red")

plot(fitted(nested_model), scale(residuals(nested_model)),
     xlab = "Fitted Values", 
     ylab = "Standardized Residuals",
     main = "Residuals vs Fitted Plot",
     pch = 19,
     abline(h = 0, col = "red", lwd = 2) )

anova(nested_model)



# 交互作用（款式、地區、品項定位）
ggplot(data, aes(x = 款式, y = 銷售總金額 , color = 品項定位, group = 品項定位)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  facet_wrap(~地區) +
  labs(title = "款式、品項定位與地區的三重交互作用", x = "款式", y = "銷售總金額") +
  theme_minimal()

ggplot(data, aes(x = 品項定位, y = 銷售總金額, color = 款式, group = 款式)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(title = "品項定位與款式的交互作用", x = "品項定位", y = "進貨總金額") +
  theme_minimal()

ggplot(data, aes(x = 款式, y = 銷售總金額, color = 地區, group = 地區)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(title = "地區與款式的交互作用", x = "款式", y = "進貨總金額") +
  theme_minimal()

ggplot(data, aes(x = 品項定位, y = 銷售總金額, color = 地區, group = 地區)) +
  stat_summary(fun = mean, geom = "line", size = 1) +  
  stat_summary(fun = mean, geom = "point", size = 3) + 
  labs(title = "品項定位與地區的交互作用", x = "品項定位", y = "銷售總金額") +
  theme_minimal()

# 事後檢定
emmeans(fixed_model, pairwise ~ 品項定位:地區)

# 品項定位:地區
pairwise_results_style <- emmeans(fixed_model, pairwise ~ 品項定位:地區)

直轄市_results <- pairwise_results_style$emmeans %>%
  subset(地區 == "直轄市")
p1 <- plot(直轄市_results)

一般縣市_results <- pairwise_results_style$emmeans %>%
  subset(地區 == "一般縣市")
p2 <- plot(一般縣市_results)

# 款式:地區
pairwise_results_items <- emmeans(fixed_model, pairwise ~ 款式:地區)

直轄市_results <- pairwise_results_items$emmeans %>%
  subset(地區 == "直轄市")
p3 <- plot(直轄市_results)

一般縣市_results <- pairwise_results_items$emmeans %>%
  subset(地區 == "一般縣市")
p4 <- plot(一般縣市_results)

grid.arrange(p1,p2, p3, p4, nrow = 2, ncol = 2)

