library(ggplot2)
library(glue)
library(dplyr)

df <- read.csv("diabetic_data.csv")
head(df)

# df <- sample_n(df, 1000)

# check nulls
colSums(is.na(df))

# dropping nas
df <- na.omit(df)

tgt = "readmitted"

drop_cols = c("examide", "citoglipton")

cat_cols = c(
  "race", "gender", "age", "weight", "payer_code", "medical_specialty",
  "diag_1", "diag_2", "diag_3", "max_glu_serum", "A1Cresult", "metformin",
  "repaglinide", "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide",
  "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone",
  "acarbose", "miglitol", "troglitazone", "tolazamide", 
  "insulin", "glyburide.metformin", "glipizide.metformin", 
  "glimepiride.pioglitazone", "metformin.rosiglitazone", 
  "metformin.pioglitazone", "change", "diabetesMed"
)

df <- df[, !(names(df) %in% drop_cols)]

num_cols = c(
  "admission_type_id", "discharge_disposition_id", "admission_source_id",
  "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications",
  "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses"
)

# cast all chrs to factors
df[, c(cat_cols, tgt)] = lapply(df[, c(cat_cols, tgt)], FUN=as.factor)

# cast all nums to numeric
df[, num_cols] = lapply(df[, num_cols], FUN=as.numeric)

str(df)

# cat vs tgt
for (col in cat_cols) {
  if (length(levels(df[, col])) < 2) {
    next
  }
  
  ct <- chisq.test(table(df[, tgt], df[, col]))
  
  if (ct$p.value < 0.05) {
    plot(table(df[, col], df[, tgt]),
         main=col,
         ylab="Readmitted")
    print(glue("{col}, p-value: {ct$p.value}"))
  }
}

# num vs tgt
for (col in num_cols) {
  reg <- aov(df[, col] ~ df[, tgt])
  
  coefs <- summary.lm(reg)$coefficients
  pvals <- coefs[rownames(coefs) != "(Intercept)", "Pr(>|t|)"]
  
  if (min(pvals) < 0.05) {
    boxplot(df[, col] ~ df[, tgt], ylab=col, xlab="Readmitted")
    print(glue("{col}, p-value: {min(pvals)}"))
  } else {
    print(glue("NOT RELEVANT: {col}, p-value: {min(pvals)}"))
  }
}
