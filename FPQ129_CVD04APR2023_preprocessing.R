library(readxl)
library(stringr)
library(dplyr)
library(kableExtra)
library(agricolae)
options(knitr.kable.NA = '')

# customize working directory
setwd('/n/holyscratch01/stephenson_lab/Users/taylerli/sLCA')
# customize data directory
dt <- read_excel('Test Data/FPQ129_CVD04APR2023.xlsx', sheet = 1)
# keep certain columns
dt1 <- dt[, 1:25]
colnames(dt1) <- c('ID', 'STRAT', 'PSU_ID', 'hypertension', 'age_group',
                   'agg_phys', 'agg_ment', 'gender', 'waist_hip', 'STAI10', 'marital_status',
                   'employed', 'alcohol_use', 'cigarette_use', 'site_bkgrd', 'education',
                   'income', 'CESD10', 'immig', 'obese', 'diabetes', 'highchol', 'csmok',
                   'pre_CHD', 'pre_stroke')
dt2 <- dt[, 26:154]               # keep the diet columns
dt2_colnames <- data.frame(name = as.numeric(str_replace(colnames(dt2), 'fp', '')),
                           num = 1:ncol(dt2))
dt2_colnames <- dt2_colnames[order(dt2_colnames$name), ]
dt2 <- dt2[, dt2_colnames$num]         # reorder the columns
df <- cbind(dt1, dt2)                  # join the dataframes together side by side
df <- df[!is.na(df$hypertension), ]    # drop the only 1 observation with NA hypertension
# change the category of certain columns to 'factor'
cnames_fac <- c('age_group', 'gender', 'marital_status', 'employed', 'alcohol_use',
                'cigarette_use', 'site_bkgrd', 'education', 'income', 'immig', 'obese',
                'diabetes', 'highchol', 'csmok', 'pre_CHD', 'pre_stroke')
df[cnames_fac] <- lapply(df[cnames_fac], factor)
# reset the reference level for each factor-type column
df <- df %>% mutate(age_group = relevel(age_group, ref = 1),
                    gender = relevel(gender, ref = 'M'),
                    marital_status = relevel(marital_status, ref = 1),
                    employed = relevel(employed, ref = 1),
                    alcohol_use = relevel(alcohol_use, ref = 1),
                    cigarette_use = relevel(cigarette_use, ref = 1),
                    site_bkgrd = relevel(site_bkgrd, ref = 8),
                    education = relevel(education, ref = 1),
                    income = relevel(income, ref = 1),
                    immig = factor(immig, levels = c(0, 1)),
                    obese = factor(obese, levels = c(0, 1)),
                    diabetes = relevel(diabetes, ref = 1),
                    highchol = factor(highchol, levels = c(0, 1)),
                    csmok = factor(csmok, levels = c(0, 1)),
                    pre_CHD = factor(pre_CHD, levels = c(0, 1)),
                    pre_stroke = factor(pre_stroke, levels = c(0, 1)))



# create dataset with complete profile in all demographic variables except immig
dfd <- df[complete.cases(df[,c(5:18,20:25)]), ]
# split dataset into ones with and without hypertension
df0 <- dfd[dfd$hypertension == 0, ]
df1 <- dfd[dfd$hypertension == 1, ]
lis <- c("df0", "df1", "dfd")    # a list that helps with the function below
# define a function that generates a summary table for one variable
summ_table <- function(val, varname, li = lis, d = dfd) {
  if (class(d[, val]) == 'numeric') {
    if (sum(is.na(d[, val])) != 0) {
      o_df <- data.frame(matrix(nrow = 2, ncol = 6))
      o_df[1, 1] <- varname
      o_df[, 2] <- c("Mean (SD)", "N_Miss (%)")
      for (i in 3:5) {
        dta <- get(li[i-2])
        o_df[1, i] = paste0(sprintf(mean(dta[, val], na.rm = T), fmt = '%#.2f'),
                            ' (',
                            sprintf(sd(dta[, val], na.rm = T), fmt = '%#.2f'),
                            ')')
        o_df[2, i] = paste0(sum(is.na(dta[, val])),
                            ' (',
                            as.numeric(sprintf(sum(is.na(dta[, val]))/nrow(dta), fmt = '%#.4f'))*100,
                            '%)')
      }
    } else {
      o_df <- data.frame(matrix(nrow = 1, ncol = 6))
      o_df[1, 1] <- varname
      o_df[1, 2] <- "Mean (SD)"
      for (i in 3:5) {
        dta <- get(li[i-2])
        o_df[1, i] = paste0(sprintf(mean(dta[, val], na.rm = T), fmt = '%#.2f'),
                            ' (',
                            sprintf(sd(dta[, val], na.rm = T), fmt = '%#.2f'),
                            ')')
      }
    }
  } else {
    if (sum(is.na(d[, val])) != 0) {
      l <- length(unique(d[, val]))
      o_df <- data.frame(matrix(nrow = l, ncol = 6))
      o_df[1, 1] <- varname
      o_df[, 2] <- c(levels(d[, val]), "N_Miss")
      for (i in 3:5) {
        dta <- get(li[i-2])
        for (j in 1:(l-1)) {
          o_df[j, i] = paste0(sum(dta[, val] == levels(d[, val])[j], na.rm = T),
                              ' (',
                              as.numeric(sprintf(sum(dta[, val] == levels(d[, val])[j], na.rm = T)/nrow(dta), fmt = '%#.4f'))*100,
                              '%)')
        }
        o_df[l, i] = paste0(sum(is.na(dta[, val])),
                            ' (',
                            as.numeric(sprintf(sum(is.na(dta[, val]))/nrow(dta), fmt = '%#.4f'))*100,
                            '%)')
      }
    } else {
      l <- length(levels(d[, val]))
      o_df <- data.frame(matrix(nrow = l, ncol = 6))
      o_df[1, 1] <- varname
      o_df[, 2] <- levels(d[, val])
      for (i in 3:5) {
        dta <- get(li[i-2])
        for (j in 1:l) {
          o_df[j, i] = paste0(sum(dta[, val] == levels(d[, val])[j]),
                              ' (',
                              as.numeric(sprintf(sum(dta[, val] == levels(d[, val])[j])/nrow(dta), fmt = '%#.4f'))*100,
                              '%)')
        }
      }
    }
  }
  one.way <- aov(as.formula(paste0('hypertension ~ ',
                                   colnames(d)[val])),
                 data = d)
  pval <- signif(summary(one.way)[[1]][['Pr(>F)']][1], 3)
  o_df[1, 6] <- ifelse(pval == 0,
                       '< 1e-1000',
                       pval)
  return(o_df)
}
# create a combined summary table
s_t11 <- data.frame(matrix(nrow = 0, ncol = 6))
v_l <- c(5, 8, 6, 7, 9:14, 16:18, 20:25)    # list of column index
n_l <- c('Age Group', 'Gender', 'Aggregate Physical Score',
         'Aggregate Mental Score', 'Waist-hip Raio', 'STAI10', 'Marital Status',
         'Employment Status', 'Alcohol Use', 'Cigarette Use', 'Education Status', 'Income',
         'CESD10', 'Obesity', 'Diabetes', 'High Cholestrol',
         'Current Smoker', 'CHD History', 'Stroke History')    # list of variable names
for (i in 1:length(v_l)) {
  s_t11 <- rbind(s_t11, summ_table(v_l[i], n_l[i]))
}
# change column names
colnames(s_t11) <- c('', '', 'Y=0 (N=9212)', 'Y=1 (N=4171)', 'Total (N=13383)', 'ANOVA P-value')
# show the summary table
s_t11 %>%
  kbl(caption = "Summary Statistics of Hypertension Dataset") %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  column_spec(1, bold = T)
# export as latex codes for supplemental table 4
knitr::kable(s_t11, "latex")
# create a sitebkgd summary table
s_t12 <- summ_table(15, "Site Background")
colnames(s_t12) <- c('', 'Site Background', 'Y=0 (N=9515)', 'Y=1 (N=4350)', 'Total (N=13865)', 'ANOVA P-value')
s_t12[,-c(1,6)] %>%
    kbl(caption = "Summary Statistics of Hypertension across Site Backgrounds") %>%
    kable_classic(full_width = T, html_font = "Cambria") %>%
    kable_styling(latex_options = "striped")
# export as latex codes for supplemental table 5
knitr::kable(s_t12[c(2:8,1,9:17), -c(1,6)], "latex")



# fitting logistic regression model, using obesity; immig not used due to too many missing values
mod11 <- glm(as.formula(paste0('hypertension ~ ',
                               paste(colnames(dfd)[c(5:8, 10:18, 20:23)],
                                     collapse = " + "))),
             data = dfd,
             family = "binomial")
summary(mod11)
# fitting logistic regression model, using waist-hip ratio; immig not used due to too many missing values
mod12 <- glm(as.formula(paste0('hypertension ~ ',
                               paste(colnames(dfd)[c(5:18, 21:23)],
                                     collapse = " + "))),
             data = dfd,
             family = "binomial")
summary(mod12)



##### drop the individuals with CVD history and do summary table & model fitting again #####
df_d <- dfd[dfd$pre_CHD==0 & dfd$pre_stroke==0, ]    # those who have a history of CHD/stroke are dropped
mod21 <- glm(as.formula(paste0('hypertension ~ ',
                               paste(colnames(df_d)[c(5:8, 10:18, 20:23)],
                                     collapse = " + "))),
             data = df_d,
             family = "binomial")    # obesity ratio is used
summary(mod21)
mod22 <- glm(as.formula(paste0('hypertension ~ ',
                               paste(colnames(df_d)[c(5:18, 21:23)],
                                     collapse = " + "))),
             data = df_d,
             family = "binomial")    # obese indicator is used
summary(mod22)



##### further exploring into variables #####
# marital status, relevel to ref = 2
dt_mod12_m <- dfd %>% mutate(marital_status = relevel(marital_status, ref = 2))
summary(glm(as.formula(paste0('hypertension ~ ',
                              paste(colnames(dt_mod12_m)[c(5:8, 10:18, 20:23)],
                                    collapse = " + "))),
            data = dt_mod12_m,
            family = "binomial"))$coefficients[7:8,]
# employment status, relevel to ref = 2
dt_mod12_em2 <- dfd %>% mutate(employed = relevel(employed, ref = 2))
summary(glm(as.formula(paste0('hypertension ~ ',
                              paste(colnames(dt_mod12_em2)[c(5:8, 10:18, 20:23)],
                                    collapse = " + "))),
            data = dt_mod12_em2,
            family = "binomial"))$coefficients[9:11,]
# employment status, relevel to ref = 3
dt_mod12_em3 <- dfd %>% mutate(employed = relevel(employed, ref = 3))
summary(glm(as.formula(paste0('hypertension ~ ',
                              paste(colnames(dt_mod12_em3)[c(5:8, 10:18, 20:23)],
                                    collapse = " + "))),
            data = dt_mod12_em3,
            family = "binomial"))$coefficients[9:11,]
# alcohol use, relevel to ref = 2
dt_mod12_a <- dfd %>% mutate(alcohol_use = relevel(alcohol_use, ref = 2))
summary(glm(as.formula(paste0('hypertension ~ ',
                              paste(colnames(dt_mod12_a)[c(5:8, 10:18, 20:23)],
                                    collapse = " + "))),
            data = dt_mod12_a,
            family = "binomial"))$coefficients[12:13,]
# cigarette use, relevel to ref = 2
dt_mod12_c <- dfd %>% mutate(cigarette_use = relevel(cigarette_use, ref = 2))
summary(glm(as.formula(paste0('hypertension ~ ',
                              paste(colnames(dt_mod12_c)[c(5:8, 10:18, 20:23)],
                                    collapse = " + "))),
            data = dt_mod12_c,
            family = "binomial"))$coefficients[14:15,]
# education, relevel to ref = 2
dt_mod12_e <- dfd %>% mutate(education = relevel(education, ref = 2))
summary(glm(as.formula(paste0('hypertension ~ ',
                              paste(colnames(dt_mod12_e)[c(5:8, 10:18, 20:23)],
                                    collapse = " + "))),
            data = dt_mod12_e,
            family = "binomial"))$coefficients[32:33,]
# diabetes, relevel to ref = 2
dt_mod12_d <- dfd %>% mutate(diabetes = relevel(diabetes, ref = 2))
summary(glm(as.formula(paste0('hypertension ~ ',
                              paste(colnames(dt_mod12_d)[c(5:8, 10:18, 20:23)],
                                    collapse = " + "))),
            data = dt_mod12_d,
            family = "binomial"))$coefficients[38:39,]



# due to subject knowledge, gender is included in the covariate set
dt_final_1 <- df %>%
  select(-c("STRAT", "PSU_ID", "agg_ment", "waist_hip",
            "STAI10", "income", "CESD10", "immig", "csmok", "pre_CHD",
            "pre_stroke"))
dt_final_1 <- dt_final_1[complete.cases(dt_final_1), ]
dt_0 <- dt_final_1[dt_final_1$hypertension == 0, ]
dt_1 <- dt_final_1[dt_final_1$hypertension == 1, ]
lis <- c("dt_0", "dt_1", "dt_final_1")
s_t21 <- data.frame(matrix(nrow = 0, ncol = 6))
v_l <- c(3,5,4,6:9,11:14)    # list of column index
n_l <- c('Age Group', 'Gender', 'Aggregate Physical Score',
         'Marital Status',
         'Employment Status', 'Alcohol Use', 'Cigarette Use', 'Education Status',
         'Obesity', 'Diabetes', 'High Cholestrol')    # list of variable names
for (i in 1:length(v_l)) {
  s_t21 <- rbind(s_t21, summ_table(v_l[i], n_l[i], d = dt_final_1))
}
colnames(s_t21) <- c('', '', 'Y=0 (N=8526)', 'Y=1 (N=3884)', 'Total (N=12410)', 'ANOVA P-value')
# export as latex code for table 1
knitr::kable(s_t21, "latex")
s_t22 <- summ_table(10, "Site Background", d = dt_final_1)
colnames(s_t22) <- c('', 'Site Background', 'Y=0 (N=8526)', 'Y=1 (N=3884)', 'Total (N=12410)', 'ANOVA P-value')
# export as latex code for table 2
knitr::kable(s_t22[c(2:8,1,9:17), -c(1,6)], "latex")
dt_final_1 <- dt_final_1 %>%
       mutate(gender = as.numeric(gender),
       marital_status = ifelse(marital_status == 3, 1, 0),    # binarize marital status 3
       employed = ifelse(employed == 1, 1, 0),                # binarize employment 1
       cigarette_use = ifelse(cigarette_use == 3, 1, 0),      # binarize cigarette use 3
       education = ifelse(education == 3, 1, 0)               # binarize education 3
       )
# corresponding ref is [1, NA, 1, 0, 0, 1, 0, 8, 0, 0, 1, 0]
write.csv(dt_final_1, file = "Test Data/FPQ129_cleaned_1.csv")