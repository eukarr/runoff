#----Предварительные операции.-------------------------------------------------------

library(tidyverse)                # набор пакетов для анализа и визуализации данных
library(cowplot)                  # несколько графиков на одном поле
library(alr4)                     # содержит датасет water
library(DAAG)                     # cross-validation
library(qqplotr)                  # qqplot в стиле ggplot2
library(GGally)                   # визуализация парного сравнения переменных
library(knitr)                    # формирование таблиц для отчета

# установка общей темы для рисунков
old <- theme_get()
my_theme <-  theme_bw() +
    theme(axis.text = element_text(size = 12)) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(axis.title = element_text(size = 16), legend.position = "none")
theme_set(my_theme)

#----Загрузка данных.-----------------------------------------------------------------

# исходные данные для анализа находятся в датасете water
data(water)                       




# Анализ распределения предикторов по диаграммам "ящик с усами" и определение отскоков.-----------------------------------
# Для облегчения этих (и некоторых дальнейших) операций создан дополнительный датафрейм----------------------------------- 
# с исходными данными в длинном формате (water_long),--------------------------------------------------------------------- 
# в который добавлены значения стандартизованных величин предикторов и отклика.-------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------


# переводим данные в длинный формат и добавляем колонку стандартизованных значений
water_long <- water %>% gather("APMAM", "APSAB", "APSLAKE", "OPBPC", "OPRC", "OPSLAKE", "BSAAM", key = "key", value = "value") %>% group_by(key) %>% mutate (scale_value = scale(value))

# диаграммы для исходных данных
gg_1a <- ggplot(water_long %>% filter(key != "BSAAM"), aes(x = key, y = value)) +
    geom_boxplot() +
    labs(x = "Location", y = "Precipitation") + 
    annotate("text", label = "a) Raw predictors", x = 0.5, y = 42, size = 5, colour = "red", hjust = "left")

# критическое значение t-статистики для числа наблюдений из датасета
t_crit <- qt(.975, df = nrow(water) - 1)

# диаграммы для автошкалированных данных
gg_1b <- ggplot(water_long %>% filter(key != "BSAAM"), aes(x = key, y = scale_value)) +
    geom_boxplot() +
    labs(x = 'Location', y = 'Std. precipitation') +
    scale_y_continuous(limits = c(-2.5, 4.4)) +
    geom_hline(yintercept = t_crit, linetype = 2) +
    geom_hline(yintercept = -t_crit, linetype = 2) +
    annotate("text", label = "b) Autoscaled predictors", x = 0.5, y = 4.2, size = 5, colour = "red", hjust = "left")

# общий заголовок
gg_title_1 <- ggdraw() + draw_label("Fig. 1 Distribution of values of predictors.", size = 18)

# формируем общий рис. 1
# здесь и далее - объекты, которые объединяются с помощью plot_grid, называются gg_XY,
# заголовок - gg_title_X, где x - номер рисунка, Y - идентификатор графика на рисунке
plot_grid(gg_title_1, gg_1a, gg_1b, nrow = 3, rel_heights = c(0.15, 1, 1))

# создаем график для парного сравнения предикторов
ggpairs(water[ , colnames(water) %in% c("APMAM", "APSAB", "APSLAKE", "OPBPC", "OPRC", "OPSLAKE", "BSAAM")], lower = list(continuous = "smooth_loess"), title = "Fig. 2. Pairwise comparison of parameters.", axisLabels = "none")

# создаем вектор с годом, когда наблюдались выбросы (по одному значению на КАЖДЫЙ отскок!)
outliers_y <- water_long$Year[water_long$scale_value > t_crit & water_long$key != "BSAAM"]
table(outliers_y)

# создаем датафрейм, в котором будут ВСЕ значения параметров в годы, когда был хоть один отскок
data.frame(water_long[water_long$Year %in% outliers_y, ] %>% select(-value) %>% spread(key = key, value = scale_value)) %>% select("Year", "APMAM", "APSAB", "APSLAKE", "OPBPC", "OPRC", "OPSLAKE", "BSAAM") %>% round(digits = 2)



# Построение аременных рядов для стандартизованных предикторов.-----------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

ggplot(water_long %>% filter(key != "BSAAM"), aes(x = Year, y = scale_value), color = "black") +
    geom_line() + geom_point() + geom_hline(yintercept = t_crit, linetype = 2) +
    facet_wrap( ~ key, ncol = 3, scales = "free") +
    labs(title = "Fig. 3 Time series for autoscaled predictors", y = "Std. precipitation")



# Графики для разведочного анализа переменной отклика.--------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

# заголовок рисунка
gg_title_4 <- ggdraw() + draw_label("Fig. 4 Distributions for response variable.", size = 18)

# диаграмма Кливленда = временная серия для отклика
gg_4a <- ggplot(water_long %>% filter(key == "BSAAM"), aes(x = Year, y = scale_value)) +
    geom_point() + geom_line() +
    labs(x = 'Year', y = 'Std. runoff') +
    annotate("text", label = "a) time series", x = 1947, y = 2.6, size = 6, colour = "red", hjust = "left")

# "ящик" для отклика
gg_4b <- ggplot(water_long %>% filter(key == "BSAAM"), aes(x = key, y = scale_value)) +
    geom_boxplot() +
    geom_hline(yintercept = t_crit, linetype = 2) +
    labs(x = '', y = 'Std. runoff') +
    annotate("text", label = "b) boxplot", x = 0.5, y = 2.6, size = 6, colour = "red", hjust = "left")

# проверка на нормальность для отклика
gg_4c <- ggplot(water_long %>% filter(key == "BSAAM"), aes(sample = scale_value), color = "black") +
    stat_qq_point() +
    stat_qq_line() +
    stat_qq_band() +
    annotate("text", label = "c) quantile-quantile plot", x = -2.5, y = 2.5, size = 6, colour = "red", hjust = "left")

# создаем окончательный рис. 4
plot_grid(gg_title_4, plot_grid(gg_4a, gg_4b, ncol = 2, rel_widths = c(1, 1)), gg_4c, nrow = 3, rel_heights = c(0.15, 1, 1))



# Анализ корреляции переменной отклика с каждым из предикторов.----------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------

# формируем данные для таблицы статистики корреляций
cor_stat <- data.frame(param = character(0), corr = numeric(0), t = numeric(0), p = integer(0))
for (i in 2:7) {
    temp <- cor.test(water[, 8], water[, i], method = 'pearson')
    cor_stat <- rbind.data.frame(cor_stat, cbind.data.frame(param = colnames(water)[i], corr = temp$estimate, t = temp$statistic, p = temp$p.value))
}
rownames(cor_stat) <- cor_stat$param
cor_stat <- signif(cor_stat[, 2:4], digits = 4)
kable(cor_stat, caption = "Table 1. Correlations of response with predictors")



# Функция для вывода диагностик модели типа lm. -------------------------------------------------------------------------
# По умолчанию выводятся только диагностические графики, ----------------------------------------------------------------
# опционально можно также вывести таблицу отскакивающих расстояний Кука и таблицу коэффициентов модели.------------------
# -----------------------------------------------------------------------------------------------------------------------

model.diag <- function(x, fig_n = 999, printing = FALSE) # x - объект lm, fig_n - номер рисунка (в подписи),
    # printing - печатать ли таблицы
{
    # диагностический датафрейм для модели x
    x_diag <- fortify(x)
    x_diag_long <- x_diag %>% mutate(Year = water$Year) %>% gather(colnames(x_diag)[colnames(x_diag) != "Year"], key = "key", value = "value")
    
    # критическое значение расстояния Кука
    cooksd_crit <- 4 / (nrow(x_diag) - length(coef(x_diag)))
    
    # таблица отскакивающих расстояний Кука
    outliers_cooks <- x_diag_long %>% filter(key == ".cooksd") %>% filter (value > cooksd_crit) %>% select(-key) %>% transmute(Year = Year, .cooksd = value)
    
    
    # график расстояния Кука
    gg_1 <- ggplot(x_diag, aes(x = water$Year, y = .cooksd)) +
        geom_bar(stat = 'identity') + coord_cartesian(ylim = c(0, max(x_diag$.cooksd))) +
        geom_hline(yintercept = cooksd_crit, linetype = 2) +
        labs(x = "Year")
    
    # график остатков от предсказанных значений  
    gg_2 <- ggplot(data = x_diag, aes(x = .fitted, y = .stdresid)) +
        geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = loess)
    
    # графики стандартизированных остатков от значений предикторов
    gg_3 <- ggplot(data = x_diag, aes(x = water$APMAM, y = .stdresid)) +
        geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = loess)  + labs(x = 'APMAM')
    gg_4 <- ggplot(data = x_diag, aes(x = water$APSAB, y = .stdresid)) +
        geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = loess)  + labs(x = 'APSAB')
    gg_5 <- ggplot(data = x_diag, aes(x = water$APSLAKE, y = .stdresid)) +
        geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = loess) + labs(x = 'APSLAKE')
    gg_6 <- ggplot(data = x_diag, aes(x = water$OPBPC, y = .stdresid)) +
        geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = loess) + labs(x = 'OPBPC') 
    gg_7 <- ggplot(data = x_diag, aes(x = water$OPRC, y = .stdresid)) +
        geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = loess)  + labs(x = 'OPRC')
    gg_8 <- ggplot(data = x_diag, aes(x = water$OPSLAKE, y = .stdresid)) +
        geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = loess) + labs(x = 'OPSLAKE')
    
    # квантильный график стандартизированных отклонений
    gg_9 <- ggplot(data.frame(x_diag), aes(sample = .stdresid), color = "black") +
        stat_qq_point(distribution = "norm") +
        stat_qq_line(distribution = "norm") +
        stat_qq_band(distribution = "norm")
    
    # вызов модели
    call <- paste(formula(x)[2], formula(x)[3], sep=' ~ ')
    # скорректированный коэффициент детерминации
    adj_r2 <- summary(x)$adj.r.squared
    # степени свободы остатков
    df <- summary(x)$df[2]
    # стандартное отклонение
    sigma <- summary(x)$sigma
    
    # формируем рисунок и заголовки
    composite <- plot_grid(gg_1, gg_2, gg_9, gg_3, gg_4, gg_5, gg_6, gg_7, gg_8, ncol = 3)
    title_1 <- ggdraw() + draw_label(paste("Fig. ", fig_n, " Model: ", call, sep = ""), size = 15)
    title_2 <- ggdraw() + draw_label(paste("adj. r-sq. = ", round(adj_r2, digits = 3), "; s = ", round(sigma, digits = 0), " (df = ", df, ")",   sep = ""), size = 10, fontface = "bold", color = "red")
    res <- plot_grid(title_1, title_2, composite, nrow = 3, rel_heights = c(0.15, 0.12, 3))
    
    if (printing == TRUE) {
        # отскакивающие расстояния Кука
        print("Table of Cook's distance outliers")
        print.data.frame(outliers_cooks, row.names = FALSE)
        # коэффициенты модели
        print("Table of model coefficients")
        print(round(summary(x)$coef, digits = 3))
    }
    
    return(res)
}




# Построение полной и нулевой моделей и их диагностических графиков.------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------


model_full <- lm(BSAAM ~ APMAM + APSAB + APSLAKE + OPBPC + OPRC + OPSLAKE, data = water)
model.diag(model_full, fig_n = 5, printing = TRUE)
model_null <- lm(BSAAM ~ 1, data = water)
model.diag(model_null, fig_n = 6, printing = FALSE)



# Сокращенная модель из полной по фактору инфляции дисперсии и построение ее диагностики.---------------------------------
# ------------------------------------------------------------------------------------------------------------------------

vif(model_full)
model_A <- update(model_full, . ~ . -OPSLAKE)

vif(model_A)
model_A <- update(model_A, . ~ . -APSAB)

vif(model_A)
model_A <- update(model_A, . ~ . -OPRC)

vif(model_A)
model_A <- update(model_A, . ~ . -APMAM)

vif(model_A)
model.diag(model_A, 7)



# Построение и диагностика моделей, включающих один из ранее исключенных предикторов.--------------------------------------
# -------------------------------------------------------------------------------------------------------------------------

model_B <- update(model_A, . ~ . + OPRC)
model.diag(model_B, 8, FALSE)

model_C <- update(model_A, . ~ . + OPSLAKE)
model.diag(model_C, 9, FALSE)



# Построение оптимальной линейной модели по критерию Акаике.--------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

model_AIC <- step(object = model_null, scope = model_full$call, direction = "both", trace = 100, k = 2)
model.diag(model_AIC, 10, FALSE)



# Кросс-валидация моделей и график результатов.---------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

model_null_cv <- cv.lm(data = water, form.lm = formula(BSAAM ~ 1), m = 5, dots = 0, seed = 123, plotit = FALSE, printit = TRUE)
gg_91 <- ggplot(data = model_null_cv, aes(x = BSAAM, y = cvpred)) +
    geom_point() + geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(limits = c(20000, 180000)) + 
    scale_x_continuous(limits = c(20000, 180000)) +
    annotate("text", label = "model_null", x = 50000, y = 160000, size = 6, colour = "red", hjust = "left")

model_A_cv <- cv.lm(data = water, form.lm = model_A$call, m = 5, dots = 0, seed = 123, plotit = FALSE, printit = TRUE)
gg_92 <- ggplot(data = model_A_cv, aes(x = BSAAM, y = cvpred)) +
    geom_point() + geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(limits = c(20000, 180000)) + 
    scale_x_continuous(limits = c(20000, 180000)) +
    annotate("text", label = "model_A", x = 50000, y = 160000, size = 6, colour = "red", hjust = "left")

model_B_cv <- cv.lm(data = water, form.lm = model_B$call, m = 5, dots = 0, seed = 123, plotit = FALSE, printit = TRUE)
gg_93 <- ggplot(data = model_B_cv, aes(x = BSAAM, y = cvpred)) +
    geom_point() + geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(limits = c(20000, 180000)) + 
    scale_x_continuous(limits = c(20000, 180000)) +
    annotate("text", label = "model_B", x = 50000, y = 160000, size = 6, colour = "red", hjust = "left")

model_C_cv <- cv.lm(data = water, form.lm = model_C$call, m = 5, dots = 0, seed = 123, plotit = FALSE, printit = TRUE)
gg_94 <- ggplot(data = model_C_cv, aes(x = BSAAM, y = cvpred)) +
    geom_point() + geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(limits = c(20000, 180000)) + 
    scale_x_continuous(limits = c(20000, 180000)) +
    annotate("text", label = "model_C", x = 50000, y = 160000, size = 6, colour = "red", hjust = "left")

model_AIC_cv <- cv.lm(data = water, form.lm = formula(model_AIC$call), m = 5, dots = 0, seed = 123, plotit = FALSE, printit = TRUE)
gg_95 <- ggplot(data = model_AIC_cv, aes(x = BSAAM, y = cvpred)) +
    geom_point() + geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(limits = c(20000, 180000)) + 
    scale_x_continuous(limits = c(20000, 180000)) +
    annotate("text", label = "model_AIC", x = 50000, y = 160000, size = 6, colour = "red", hjust = "left")

model_full_cv <- cv.lm(data = water, form.lm = formula(model_full$call), m = 5, dots = 0, seed = 123, plotit = FALSE, printit = TRUE)
gg_96 <- ggplot(data = model_full_cv, aes(x = BSAAM, y = cvpred)) +
    geom_point() + geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(limits = c(20000, 180000)) + 
    scale_x_continuous(limits = c(20000, 180000)) +
    annotate("text", label = "model_full", x = 50000, y = 160000, size = 6, colour = "red", hjust = "left")

gg_title_9 <- ggdraw() + draw_label("Fig. 11 Cross-validation of the models.", size = 18)

plot_grid(gg_title_9, plot_grid(gg_91, gg_92, gg_93, gg_94, gg_95, gg_96, ncol = 2), ncol = 1, rel_heights = c(0.15, 1))



# Создание сводной таблицы характеристик моделей.--------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------

full_sum <- cbind(adj.r.sq = summary(model_full)$adj.r.squared, model.s = summary(model_full)$sigma, model.df = summary(model_full)$df[2], cv.s = sqrt(attributes(model_full_cv)$ms))
A_sum <- cbind(adj.r.sq = summary(model_A)$adj.r.squared, model.s = summary(model_A)$sigma, model.df = summary(model_A)$df[2], cv.s = sqrt(attributes(model_A_cv)$ms))
B_sum <- cbind(adj.r.sq = summary(model_B)$adj.r.squared, model.s = summary(model_B)$sigma, model.df = summary(model_B)$df[2], cv.s = sqrt(attributes(model_B_cv)$ms))
C_sum <- cbind(adj.r.sq = summary(model_C)$adj.r.squared, model.s = summary(model_C)$sigma, model.df = summary(model_C)$df[2], cv.s = sqrt(attributes(model_C_cv)$ms))
null_sum <- cbind(adj.r.sq = summary(model_null)$adj.r.squared, model.s = summary(model_null)$sigma, model.df = summary(model_null)$df[2], cv.s = sqrt(attributes(model_null_cv)$ms))
AIC_sum <- cbind(adj.r.sq = summary(model_AIC)$adj.r.squared, model.s = summary(model_AIC)$sigma, model.df = summary(model_AIC)$df[2], cv.s = sqrt(attributes(model_AIC_cv)$ms))

overall <- (rbind(null_sum, A_sum, B_sum, C_sum, AIC_sum, full_sum))
rownames(overall) <- c("Null", "A", "B", "C", "AIC", "Full")
colnames(overall) <- c("Adj. r-squared", "Model MSe", "Model df", "Prediction MSe")

kable(overall, caption = "Table 2. Parameters of the built models.")
