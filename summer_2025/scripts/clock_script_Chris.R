#clock 9 script (R2= 0.84 with 56 CpG sites)
#this is Max's code with age transformation and sex related data hashtagged out
#this has alpha 0.1 and 30% predictive frequency
library(readr)
library(glmnet)
library(dplyr)


args <- commandArgs(trailingOnly = TRUE)
input_file      <- "/home/local/ADS/mweise/Yi_Lab/Lab_WorkDir/maddy/mussel_projects/epigenetic_clock/methylation_out/clean_tsv/normalized_methylation_matrix2.tsv"  # normalized methylation data
meta_file       <- "/home/local/ADS/mweise/Yi_Lab/Lab_WorkDir/maddy/mussel_projects/epigenetic_clock/scripts/age/age_112525_clean.tsv"  # sample metadata
bs_output       <- "/home/local/ADS/mweise/Yi_Lab/Lab_WorkDir/maddy/mussel_projects/epigenetic_clock/clock_output/clock9/bs_output.tsv"  # bootstrapped coefficient storage
ols_coefs       <- "/home/local/ADS/mweise/Yi_Lab/Lab_WorkDir/maddy/mussel_projects/epigenetic_clock/clock_output/clock9/ols_coefs.tsv"  # OLS coefficient output
predicted_tsv   <- "/home/local/ADS/mweise/Yi_Lab/Lab_WorkDir/maddy/mussel_projects/epigenetic_clock/clock_output/clock9/predicted.tsv"  # OLS age predictions
pdf_output      <- "/home/local/ADS/mweise/Yi_Lab/Lab_WorkDir/maddy/mussel_projects/epigenetic_clock/clock_output/clock9/pdf_output.pdf"  # plot of predictions vs actual age
#sex             <- args[7]
ALPH            <- as.numeric(0.1)


# Read in metainfo with known chronological ages
info <- read.table(meta_file, sep = "\t", header = TRUE)
print(paste("Number of rows after subsetting: ", nrow(info)))
info$Age <- as.numeric(info$Age)
info$GSM <- as.character(info$GSM)
#info$Sex <- as.character(info$Sex)
info$GSM <- trimws(info$GSM)   # trim whitespace
cat("info/md:\n")
print(head(info, 5))


# Read in epi data
epi <- read.table(input_file, sep = "\t", header = TRUE)
print(head(epi, 5))

#### FIX COLUMN NAMES TO MATCH METADATA #########
# 1. Replace dots with dashes
colnames(epi) <- gsub("\\.", "-", colnames(epi))
print(head(epi, 5))

# 2. Trim any leading/trailing whitespace
colnames(epi) <- trimws(colnames(epi))
info$GSM <- trimws(info$GSM)

# 3. Save first column and remove it
first_column <- epi[, 1, drop=FALSE]
epi <- epi[, -c(1)]

# 4. Align epi columns to metadata
matching <- match(info$GSM, colnames(epi))
if (any(is.na(matching))) {
  missing <- info$GSM[is.na(matching)]
  cat("These GSM IDs are in metadata but NOT in epi matrix:\n")
  print(missing)
  stop("ERROR: Some GSM IDs in metadata do not match column names in epi matrix.")
}
epi <- epi[, matching]
cat("epi reordered to match metadata.\n")
########
print(head(epi, 5))
print(paste("ncol epi for DNAm only: ", ncol(epi)))



############################################

# Initialize storage for coefficients, intercepts, and frequency counts
boot_iterations <- 500  # Define the number of bootstrap iterations

weights_df <- data.frame(matrix(NA, nrow = nrow(epi), ncol = boot_iterations))
colnames(weights_df) <- paste0("Boot_", seq_len(boot_iterations))
print(paste("Info$Age no transformation: ", info$Age))

#transform_age <- function(age) {
#	adult_age <- 20
#	if (age <= adult_age) {
#		return(log(age + 1) - log(adult_age + 1))
#		} 
#	else {
#		return((age - adult_age)/ (adult_age + 1))
#		}
#	}
#info$Age <- sapply(info$Age, transform_age)
#print(paste("Info$Age POST transformation: ", info$Age))

# Fit the elastic-net model
cvfit <- cv.glmnet(t(epi), info$Age, nfolds = 10, alpha = ALPH, standardize = FALSE)
min_lambda <- cvfit$lambda.min
min_lambda <- as.character(min_lambda)
print(paste("Optimal lambda for ALPH", ALPH, ": ", min_lambda)) 

# Perform bootstrapping
for (boot in seq_len(boot_iterations)) {
  cat("Running bootstrap iteration:", boot, "\n")
  
  # Generate bootstrap sample indices with replacement
  boot_indices <- sample(seq_len(ncol(epi)), replace = TRUE)
  
  # Create training and testing datasets based on bootstrap sample
  norm_train <- epi[, boot_indices]
  trainage <- info$Age[boot_indices]
  
  # Fit the model with glmnet
  fit <- glmnet(t(norm_train), trainage, alpha = ALPH, lambda = cvfit$lambda.min)
  
  # Get the coefficients
  coef.fit <- coef(fit)
  weights_df[, boot] <- as.vector(coef.fit[-1, 1])  # Exclude intercept and match rows
  int <- coef.fit@x[1]
  print(paste("Intercept for Alph", ALPH, "in iteration", boot, ": ", int))
  
}
weights_df <- cbind(first_column, weights_df)
print(colnames(weights_df))
### Candidate Site filtering:

weights_df$NZ_count <- apply(weights_df[, 2:ncol(weights_df)], 1, function(row) sum(row != 0))
weights_df$NZ_frq <- weights_df$NZ_count / boot_iterations
weights_30 <- as.data.frame(weights_df[weights_df$NZ_frq >= 0.3, ])
cat("Number of candidate sites with 30% predictive frequency:\n")
print(nrow(weights_30))
write.table(weights_30, file = bs_output, sep = "\t", row.names = FALSE)

######################################################

## OLS Prediction
epi <- read.table(input_file, sep = "\t", header = TRUE)
info <- read.table(meta_file, sep = "\t", header = TRUE)
info$Age <- as.numeric(info$Age)
raw_ages <- info$Age

print(head(info$Age, 5))
cat("raw_ages:\n")
print(head(raw_ages, 5))
#rownames(raw_ages) <- info$GSM #issue with vector vs df

cat("shape epi before subsetting to weights30:\n")
print(dim(epi))

print(colnames(epi))
colnames(epi)[1] <- "site" ######originally in max's code site is ID_ref

epi <- epi[epi[, "site"] %in% weights_30$site, ]
cat("epi after subsetting to weights50:")
print(head(epi, 5))
print(nrow(epi))
first_column <- epi$site
rownames(epi) <- epi$site
epi <- epi[, -c(1)]

#NO TRANSFORMATION
# Horvath transformation 
#trafo <- function(x, adult.age = 20) {
# x <- (x + 1) / (1 + adult.age)
# y <- ifelse(x <= 1, log(x), x - 1)
# return(y)
#}
#info$Age <- sapply(info$Age, trafo)

# Transpose and prepare for OLS
epi_t <- as.data.frame(t(epi))
cat("row names of epi_t before gsub:\n")
print(head(rownames(epi_t), 5))
rownames(epi_t) <- gsub("^X", "", rownames(epi_t))
rownames(epi_t) <- gsub("_beta", "", rownames(epi_t))  # Ensure GSM names match metadata
cat("row names of epi_t after gsub:\n")
print(head(rownames(epi_t), 5))

print(nrow(epi_t))


cat("epi_t before epi_t_aged:\n")
print(head(epi_t, 5))



#######################################################

# Match order to metadata

#epi_t <- epi_t[info$GSM, ]
epi_t_aged <- cbind(Age = info$Age, epi_t)

cat("epi_t_aged:\n")
print(head(epi_t_aged, 5))

# Fit OLS model
cat("Fitting OLS model...\n")
model <- lm(Age ~ ., data = epi_t_aged)
coefficients <- coef(model)
write.table(coefficients, file = ols_coefs, sep = "\t", row.names = FALSE, quote = FALSE)
# Predict
cat("Predicting ages..\n")

predicted_age <- predict(model, newdata = epi_t)

#NO INVERSE TRANSFORMATION
# Inverse transform
#anti.trafo <- function(x, adult.age = 20) {
#  ifelse(x < 0, (1 + adult.age) * exp(x) - 1, (1 + adult.age) * x + adult.age)
#}
#predicted_age <- sapply(predicted_age, anti.trafo)

# Output predictions
#Predicted_df <- data.frame(Sample = rownames(epi_t), Age = raw_ages, Predicted = predicted_age)


Predicted_df <- data.frame(
  Sample = info$GSM,   # use metadata GSM IDs here
  Age = raw_ages,
  Predicted = predicted_age
)

write.table(Predicted_df, file = predicted_tsv, sep = "\t", row.names = FALSE, quote = FALSE)


##############################
#Plots

## Plot predictions vs actual ages
cat("plotting to true age...\n")

library(ggplot2)

# Combine actual and predicted for plotting
#Predicted_df$Sex <- info$Sex[match(Predicted_df$Sample, info$GSM)]
cat("Predicted_df$Age:\n")  
print(head(Predicted_df$Age))
# Fit a linear model for stats
fit <- lm(Predicted ~ Age, data = Predicted_df)
summary_fit <- summary(fit)

# Extract stats
slope <- coef(fit)[["Age"]]
intercept <- coef(fit)[["(Intercept)"]]
r_squared <- summary_fit$r.squared

# Annotations
equation <- sprintf("y = %.2fx + %.2f", slope, intercept)
r2_label <- sprintf("R² = %.2f", r_squared)

# Save as PDF
pdf(pdf_output, width = 6, height = 6)
num_cpgs <- as.character(nrow(weights_30))

ggplot(Predicted_df, aes(x = Age, y = Predicted)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", size = 1) +
  annotate("text", x = 2.5, y = 8, label = r2_label, size = 5, fontface = "bold") +
  annotate("text", x = 2.5, y = 9, label = equation, size = 5, fontface = "bold") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(title = paste(num_cpgs,"CpG Model Predicted Age vs True Age"),
       x = 'Chronological Age (Years)',
       y = 'Predicted Age (Years)') +
  theme_bw() +
  theme(
    plot.title = element_text(size = 17, face = "bold", margin = margin(t = 10, b = 5)),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13, color = "black"),
    legend.title = element_text(size = 12),
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    aspect.ratio = 1,
    legend.position = c(0.95, 0.05)
  ) +
  guides(shape = "none")
dev.off()

