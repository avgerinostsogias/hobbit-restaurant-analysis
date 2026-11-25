# Hobbit's Choice restaurant survey
# Open in RStudio and click Source, or pass the workbook path to Rscript.
# Packages, if needed: install.packages(c("readxl", "ggplot2", "dplyr"))
library(readxl)
library(ggplot2)
library(dplyr)

# Use the workbook in the working directory or supply its full path.
args <- if (interactive()) character() else commandArgs(trailingOnly = TRUE)
input_file <- if (length(args)) args[1] else "Hobbit_values.xls"

if (!file.exists(input_file) && interactive()) {
  message("Select the original Hobbit_values.xls workbook if you have it.")
  input_file <- tryCatch(file.choose(), error = function(e) "")
}
if (!file.exists(input_file)) {
  stop("The original Hobbit_values.xls workbook is required to run this analysis. ",
       "See the README for the data source and the PDF report for the results.",
       call. = FALSE)
}
Hobbit_values <- as.data.frame(read_excel(input_file))

print(table(Hobbit_values$eatrest, useNA = "ifany"))
print(lapply(Hobbit_values, table, useNA = "ifany"))
# Reset the plot layout left by earlier commands in RStudio.
par(mfrow = c(1, 1), new = FALSE, mar = c(5.1, 4.1, 4.1, 2.1),
    oma = c(0, 0, 0, 0), cex = 1, mex = 1, pty = "m", bg = "white")
barplot(table(Hobbit_values$radprogr), main = "Radio Program Responses",
        xlab = "Program code", ylab = "Count")
print(summary(Hobbit_values$totspent))
plot(factor(Hobbit_values$eatrest), main = "Eats at This Type of Restaurant",
     xlab = "Response code", ylab = "Count")

boxplot(Hobbit_values$totspent, 
        main = "Boxplot of Total Spent (totspent)",
        ylab = "Total Spent",
        col = "lightblue",
        border = "darkblue")

# Δημιουργία του Boxplot
boxplot(totspent ~ factor(likely), data = na.omit(Hobbit_values),
        main = "Boxplot of Total Spent (totspent) by Likelihood of Visit (likely)",
        xlab = "Likelihood of Visit",
        ylab = "Total Spent",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
        border = "darkblue")

# Pearson heatmap. Keep numeric codes as in the original exploratory chart.
Hobbit_values$age <- 2006 - Hobbit_values$yrborn

Hobbit_values$hcpatron_bin <- as.integer(Hobbit_values$hcpatron == 1)
decor_vars <- c("elegant", "simple", "string", "jazz")
marketing_vars <- c("lisradio", "radprogr", "vietvnew", "timenew", "reapaper", "citymag")
Hobbit_values$decor_mean <- rowMeans(Hobbit_values[decor_vars])
# Original mean of media codes. Its meaning depends on the coding.
Hobbit_values$marketing_mean <- rowMeans(Hobbit_values[marketing_vars])

# Keep the same complete-case sample used for the original screenshots.
Hobbit_all <- Hobbit_values
Hobbit_values <- na.omit(Hobbit_values)
print(c(all_rows = nrow(Hobbit_all), complete_rows = nrow(Hobbit_values)))

# Constant columns stay grey because their correlation is undefined.
numeric_columns <- subset(Hobbit_values, select = -c(drive, zipcode))
cor_columns <- sapply(numeric_columns, sd) > 0
correlation_matrix <- matrix(NA_real_, ncol(numeric_columns), ncol(numeric_columns),
                             dimnames = list(names(numeric_columns), names(numeric_columns)))
correlation_matrix[cor_columns, cor_columns] <-
  cor(numeric_columns[cor_columns], method = "pearson")
cor_data <- as.data.frame(as.table(correlation_matrix))
names(cor_data) <- c("Var1", "Var2", "value")

heatmap_plot <- ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       limits = c(-1, 1), na.value = "grey60") +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7)) +
  labs(title = "Pearson Correlation Heatmap", x = NULL, y = NULL)
# Include cell values only when there is enough room to read them.
if (dev.size("in")[1] >= 9 && dev.size("in")[2] >= 5) {
  heatmap_plot <- heatmap_plot +
    geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))), size = 1.55)
}
print(heatmap_plot)
par(mfrow = c(1, 1), new = FALSE)

print(mean(Hobbit_values$totspent)
)
print(sd(Hobbit_values$likely)
)
print(table(Hobbit_values$avprice)
)

hist(Hobbit_values$totspent, main = "Monthly Restaurant Spending", xlab = "Total Spent (USD)")
hist(Hobbit_values$likely, breaks = seq(0.5, 5.5, 1),
     main = "Likelihood of Visit", xlab = "Visit Rating (1-5)")

print(summary(aov(waitstaf ~ factor(drive), data = Hobbit_values))
)

# TV-news status is constant in the complete cases. Radio program is nominal.
# Keep the original spending and visit outcomes and use a separate radio table.
manova_model <- manova(cbind(totspent, likely) ~ factor(educa), data = Hobbit_values)
print(summary(manova_model)
)
print(summary.aov(manova_model)
)
print(table(Hobbit_values$educa, Hobbit_values$radprogr)
)

t_test_result <- t.test(Hobbit_values$totspent ~ Hobbit_values$gender)
print(t_test_result)
print(mean(Hobbit_values$income)
)
t_test_result <- t.test(Hobbit_values$reincome ~ Hobbit_values$gender)
print(t_test_result)
hist(Hobbit_values$age, main = "Age in 2006", xlab = "Age")
t_test_result <- t.test(Hobbit_values$totspent~Hobbit_values$VLPatron)
print(t_test_result)

# Δημιουργία του Boxplot
boxplot( age ~ factor(likely), data = Hobbit_values ,
        main = "Age by Likelihood of Visit",
        xlab = "Likelihood of Visit",
        ylab = "Age",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
        border = "darkblue")
# Δημιουργία του Boxplot
boxplot( age ~ factor(gender), data = Hobbit_values ,
         main = "Age by Gender",
         xlab = "Gender (1 = Male, 2 = Female)",
         ylab = "Age",
         col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
         border = "darkblue")

# Δημιουργία του Boxplot
boxplot( age ~ factor(VLPatron), data = Hobbit_values ,
         main = "Age by VLPatron",
         xlab = "Very Likely Patron (0 = No, 1 = Yes)",
         ylab = "Age",
         col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
         border = "darkblue")

# Δημιουργία του Boxplot
boxplot(Hobbit_values$totspent ~ Hobbit_values$VLPatron,
        main = "Total Spent by VLPatron",
        xlab = "Very Likely Patron (0 = No, 1 = Yes)",
        ylab = "Total Spent",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
        border = "darkblue")
# Υπολογισμός της ηλικίας με βάση το έτος γέννησης (2006 είναι το έτος αναφοράς)

# Δημιουργία των ηλικιακών ομάδων
Hobbit_values$age_group <- cut(Hobbit_values$age,
                               breaks = c(29, 40, 50, 60, 70, Inf),
                               labels = c("30-40", "41-50", "51-60", "61-70", "71+"),
                               right = TRUE)

# Έλεγχος αν οι ηλικιακές ομάδες δημιουργήθηκαν σωστά
print(table(Hobbit_values$age_group)
)
# Φόρτωση της βιβλιοθήκης ggplot2 (αν δεν είναι ήδη φορτωμένη)

# Δημιουργία Scatter Plot με τις ηλικιακές ομάδες
print(ggplot(Hobbit_values, aes(x = age, y = totspent, color = age_group)) +
  geom_point() +
  labs(title = "Scatter Plot of Total Spent by Age Groups",
       x = "Age",
       y = "Total Spent") +
  theme_minimal()
)

# Υπολογισμός ηλικίας

# Φιλτράρισμα για άτομα ηλικίας 70 και άνω
income_70plus <- subset(Hobbit_values, age >= 70)$income

# Προβολή των τιμών εισοδήματος για την ομάδα 70+
print(income_70plus)

Hobbit_values$drive <- as.factor(Hobbit_values$drive)
Hobbit_values$zipcode <- as.factor(Hobbit_values$zipcode)

# Δημιουργία του γραφήματος χωρίς τη χρήση $

print(ggplot(Hobbit_values, aes(x = drive, fill = zipcode)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Bar Plot of Drive Preference by Zipcode",
    x = "Drive Preference (1: Very Low - 5: Very High)",
    y = "Count"
  ) +
  theme_minimal()
)

# Μετατροπή των μεταβλητών σε factors (χωρίς να αλλάξουμε το drive)
Hobbit_values$VLPatron <- as.factor(Hobbit_values$VLPatron)
Hobbit_values$zipcode <- as.factor(Hobbit_values$zipcode)

# Δημιουργία του γραφήματος χωρίς το λάθος στη μεταβλητή drive

print(ggplot(Hobbit_values, aes(x = zipcode, fill = VLPatron, group = VLPatron)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Very Likely Patron by Zipcode",
    x = "Zipcode",
    y = "Count"
  ) +
  theme_minimal()
)

# Υπολογισμός του ποσοστού για κάθε συνδυασμό drive και zipcode

print(Hobbit_values %>%
  count(zipcode, drive) %>%
  group_by(zipcode) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = drive, y = percentage, fill = zipcode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Drive Preference by Zipcode (Percentage)",
    x = "Drive Preference (1: Very Low - 5: Very High)",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5)
)

# Υπολογισμός του ποσοστού για κάθε συνδυασμό VLPatron και zipcode
print(Hobbit_values %>%
  count(zipcode, VLPatron) %>%
  group_by(zipcode) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = zipcode, y = percentage, fill = VLPatron)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Likelihood of Patron (VLPatron) by Zipcode (Percentage)",
    x = "Zipcode",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
  position = position_dodge(width = 0.9), vjust = -0.5)
)

# --------------------------------------------------
  Hobbit_values$hcpatron <- factor(Hobbit_values$hcpatron,
                                levels = c(1, 2), labels = c("Yes", "No"))

  # Υπολογισμός του ποσοστού για κάθε συνδυασμό hcpatron και zipcode
  print(Hobbit_values %>%
  count(zipcode, hcpatron) %>%
  group_by(zipcode) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = zipcode, y = percentage, fill = hcpatron)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Patron Likelihood (hcpatron) by Zipcode (Percentage)",
    x = "Zipcode",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5)
  )

# --------------------------------------------------

# Φιλτράρισμα για πιθανούς πελάτες (hcpatron = 1)
Hobbit_filtered <- Hobbit_values %>%
  filter(hcpatron == "Yes")

# Προετοιμασία των μεταβλητών
Hobbit_filtered$marital <- ifelse(Hobbit_filtered$marital == 3, 1, Hobbit_filtered$marital)
Hobbit_filtered$marital <- factor(Hobbit_filtered$marital, 
                                  levels = c(1, 2),
                                  labels = c("Single/Other", "Married"))
Hobbit_filtered$gender <- factor(Hobbit_filtered$gender, c(1, 2), c("Male", "Female"))

# Δημιουργία dataset με ποσοστά και συνολικά n ανά marital
plot_data <- Hobbit_filtered %>%
  count(marital, gender) %>%
  group_by(marital) %>%
  mutate(percentage = n / sum(n) * 100,
         total = sum(n))  # συνολικός αριθμός ατόμων ανά marital

# Δημιουργία του γραφήματος
print(ggplot(plot_data, aes(x = marital, y = percentage, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  geom_text(data = distinct(plot_data, marital, total),
            aes(x = marital, y = 105, label = paste0("N = ", total)),
            inherit.aes = FALSE, fontface = "bold", size = 4.5) +
  labs(
    title = "Marital Status by Gender (Probable Patrons)",
    subtitle = "Percent and sample size in each marital group",
    x = "Marital Status",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "pink")) +
  ylim(0, 110)
)

# --------------------------------------------------

# Δημιουργία πίνακα με counts ανά zipcode
zipcode_data <- Hobbit_values %>%
  count(zipcode) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(zipcode, " (", round(percentage, 1), "%)"))

# Δημιουργία Pie Chart με ggplot2
print(ggplot(zipcode_data, aes(x = "", y = percentage, fill = zipcode)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Percentage by Zipcode") +
  theme_void()
)

# Βεβαιώσου ότι hcpatron είναι factor (1 = Yes, 2 = No)

# Επιλογή μεταβλητών διακόσμησης (πρόσθεσε ή αφαίρεσε ανάλογα με το dataset σου)
decor_vars <- c("elegant", "simple", "string", "jazz")

# Δημιουργία long format με loop
decor_long <- data.frame()

for (var in decor_vars) {
  temp <- data.frame(
    hcpatron = Hobbit_values$hcpatron,
    score = Hobbit_values[[var]],
    decor_type = var
  )
  decor_long <- rbind(decor_long, temp)
}

# Υπολογισμός ποσοστών

plot_data <- decor_long %>%
  group_by(decor_type, hcpatron, score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(decor_type, hcpatron) %>%
  mutate(percentage = n / sum(n) * 100)

# Δημιουργία ιστογράμματος με ποσοστά
print(ggplot(plot_data, aes(x = factor(score), y = percentage, fill = hcpatron)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~decor_type, scales = "free_y") +
  labs(title = "Decor Preferences by Probable Patron Status",
       x = "Preference Rating (1-5)",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "salmon"))
)

# Αν το hcpatron είναι factor με Yes/No

print(names(Hobbit_values)
)

# Λογιστική Παλινδρόμηση με μεταβλητές ντεκόρ
log_model <- glm(Hobbit_values$hcpatron_bin ~
                   Hobbit_values$elegant +
                   Hobbit_values$simple +
                   Hobbit_values$string +
                   Hobbit_values$jazz,
                 family = binomial)
print(summary(log_model)
)
# Προβλεπόμενες πιθανότητες
Hobbit_values$predicted_prob <- predict(log_model, type = "response")

# Κατηγοριοποίηση: Πιθανότητα > 0.5 → Yes
Hobbit_values$predicted_class <- ifelse(Hobbit_values$predicted_prob > 0.5, "Yes", "No")

# Πίνακας σύγκρισης πραγματικής vs προβλεπόμενης κατηγορίας
print(table(Predicted = Hobbit_values$predicted_class, Actual = Hobbit_values$hcpatron)
)
# --------------------------------------------------
# Πίνακας περιγραφικών στατιστικών για τις marketing μεταβλητές
marketing_vars <- c("lisradio", "radprogr", "vietvnew", "timenew", "reapaper", "citymag")

print(summary(Hobbit_values[marketing_vars])
)
print(sapply(Hobbit_values[marketing_vars], sd, na.rm = TRUE)  # Τυπική απόκλιση
)

# Βεβαιώσου ότι το hcpatron είναι factor

# Long format για χρήση σε ggplot

marketing_long <- data.frame()
for (var in marketing_vars) {
  temp <- data.frame(
    hcpatron = Hobbit_values$hcpatron,
    score = Hobbit_values[[var]],
    marketing_type = var
  )
  marketing_long <- rbind(marketing_long, temp)
}

# Boxplot για κάθε μεταβλητή marketing
print(ggplot(marketing_long, aes(x = hcpatron, y = score, fill = hcpatron)) +
  geom_boxplot() +
  facet_wrap(~marketing_type, scales = "free_y") +
  labs(title = "Media Response Codes by Probable Patron Status",
       x = "Probable Patron",
       y = "Media Response Code") +
  theme_minimal()
)

# --------------------------------------------------

# Μεταβλητές marketing
marketing_vars <- c("lisradio", "radprogr", "vietvnew", "timenew", "reapaper", "citymag")

# Δημιουργία long format manually
marketing_long <- data.frame()
for (var in marketing_vars) {
  temp <- data.frame(
    hcpatron = Hobbit_values$hcpatron,
    score = Hobbit_values[[var]],
    marketing_type = var
  )
  marketing_long <- rbind(marketing_long, temp)
}

# Υπολογισμός ποσοστών για κάθε βαθμολογία ανά marketing μεταβλητή και hcpatron
plot_data <- marketing_long %>%
  group_by(marketing_type, hcpatron, score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(marketing_type, hcpatron) %>%
  mutate(percentage = n / sum(n) * 100)

# Ιστόγραμμα ποσοστών με Facets
print(ggplot(plot_data, aes(x = factor(score), y = percentage, fill = hcpatron)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~marketing_type, scales = "free_y") +
  labs(title = "Media Responses by Probable Patron Status",
       x = "Response Code",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "orange"))
)

print(table(Hobbit_values$hcpatron)
)
print(prop.table(table(Hobbit_values$hcpatron))
)

print(var(Hobbit_values$totspent, na.rm=TRUE)
)
print(sd(Hobbit_values$totspent , na.rm=TRUE)
)
print(range(Hobbit_values$totspent , na.rm=TRUE)
)
print(var(Hobbit_values$reincome, na.rm=TRUE)
)
print(range(Hobbit_values$reincome, na.rm=TRUE)
)
print(sd(Hobbit_values$reincome , na.rm=TRUE)
)

qqnorm(log(Hobbit_values$avprice), main="Normal Q-Q plot for price")
qqline(log(Hobbit_values$avprice))

qqnorm(Hobbit_values$avprice, main="Normal Q-Q plot for price")
qqline(Hobbit_values$avprice)

# ΠΡΟΕΤΟΙΜΑΣΙΑ
set.seed(42)  # Για αναπαραγωγιμότητα

# Λίστα με τις μεταβλητές
variables <- c("totspent", "reincome", "avprice")

# Αρχικοποίηση λίστας για αποτελέσματα
bootstrap_results <- list()

# Επανάληψη για κάθε μεταβλητή
for (var in variables) {
  data <- na.omit(Hobbit_values[[var]])  # Αφαίρεση NA
  n <- length(data)
  
  # Bootstrap δείγματα: 1000 μέσοι όροι
  boot_means <- replicate(1000, mean(sample(data, size = n, replace = TRUE)))
  
  # Αποθήκευση αποτελεσμάτων
  bootstrap_results[[var]] <- list(
    mean = mean(data),
    ci_lower = quantile(boot_means, 0.025),
    ci_upper = quantile(boot_means, 0.975),
    boot_means = boot_means
  )
  
  # Γράφημα
  hist(boot_means, 
       breaks = 30, 
       col = "skyblue", 
       main = paste("Bootstrap Mean Distribution for", var),
       xlab = paste("Bootstrap Means of", var))
  abline(v = quantile(boot_means, c(0.025, 0.975)), col = "red", lty = 2)
  abline(v = mean(data), col = "black", lwd = 2)
  legend("topright", legend = c("2.5% CI", "97.5% CI", "Mean"),
         col = c("red", "red", "black"), lty = c(2, 2, 1), lwd = c(1, 1, 2))
}

anova_result <- aov(totspent ~ as.factor(VLPatron), data = Hobbit_values)
print(summary(anova_result)
)
print(TukeyHSD(anova_result)
)

# --------------------------------------------------
plot(Hobbit_values$avprice, Hobbit_values$totspent,
     xlab = "Expected Entree Price (USD)", ylab = "Monthly Spending (USD)")
model1<-lm(Hobbit_values$totspent~Hobbit_values$avprice)
abline(model1)
print(summary(model1)
)

# Δημιουργία του γραμμικού μοντέλου
model1 <- lm(totspent ~ avprice, data = Hobbit_values)

# Δημιουργία όλων των διαγνωστικών γραφημάτων
par(mfrow = if (interactive() || dev.size("in")[2] < 4) c(1, 1) else c(2, 2))  # 2x2 διάταξη παραθύρων
plot(model1, id.n = 0)

# Επαναφορά γραφικής διάταξης
par(mfrow = c(1, 1))
# --------------------------------------------------
plot(log(Hobbit_values$avprice), log(Hobbit_values$totspent),
     xlab = "Log Expected Price", ylab = "Log Monthly Spending")
model2<-lm(log(Hobbit_values$totspent)~log(Hobbit_values$avprice))
abline(model2)
print(summary(model2)
)

# Δημιουργία νέων λογαριθμισμένων μεταβλητών (φροντίζουμε να μην έχει μηδενικά)
Hobbit_values$logtotspent <- log(Hobbit_values$totspent)
Hobbit_values$logavprice <- log(Hobbit_values$avprice)

# Δημιουργία του γραμμικού μοντέλου με τις λογαριθμισμένες μεταβλητές
model_log <- lm(logtotspent ~ logavprice, data = Hobbit_values)

# 4 Διαγνωστικά γραφήματα
par(mfrow = if (interactive() || dev.size("in")[2] < 4) c(1, 1) else c(2, 2))  # Διάταξη 2x2
plot(model_log, id.n = 0)

# Επαναφορά της διάταξης γραφημάτων
par(mfrow = c(1, 1))

# --------------------------------------------------

anova_result <- aov(reincome ~ as.factor(likely), data = Hobbit_values)
print(summary(anova_result)
)
print(TukeyHSD(anova_result)
)

# --------------------------------------------------
  # 1. Δημιουργούμε μια δυαδική μεταβλητή για το hcpatron (1 = Yes, 0 = No)

# 2. Υπολογισμός μέσων όρων για ντεκόρ και διαφήμιση
Hobbit_values$decor_mean <- rowMeans(Hobbit_values[, c("elegant", "simple", "string", "jazz")], na.rm = TRUE)

Hobbit_values$marketing_mean <- rowMeans(Hobbit_values[, c("lisradio", "radprogr", "vietvnew", "timenew", "reapaper", "citymag")], na.rm = TRUE)

# 3. Δημιουργία πολλαπλού γραμμικού μοντέλου
model_multi_log_profile <- lm(log(totspent) ~ reincome + decor_mean + marketing_mean, data = Hobbit_values)

# 4. Εμφάνιση αποτελεσμάτων
print(summary(model_multi_log_profile)
)
plot(model_multi_log_profile, id.n = 0)
model_multi2 <- lm(totspent ~ reincome + hcpatron_bin +decor_mean+marketing_mean + avprice + zipcode + educa + drive, data = Hobbit_values)
print(summary(model_multi2)
)

model_multi3 <- lm(totspent ~ reincome + hcpatron_bin +decor_mean+marketing_mean + avprice + zipcode + educa, data = Hobbit_values)
print(summary(model_multi3)
)

model_multi4 <- lm(totspent ~ reincome + hcpatron_bin + decor_mean +marketing_mean + avprice + zipcode, data = Hobbit_values)
print(summary(model_multi4)
)

model_multi5 <- lm(totspent ~ reincome + hcpatron_bin +decor_mean+avprice + zipcode , data = Hobbit_values)
print(summary(model_multi5)
)
plot(model_multi5, id.n = 0)
Hobbit_values$zipcode <- as.factor(Hobbit_values$zipcode)
model_multi6 <- lm(log(totspent) ~ reincome + hcpatron_bin + avprice, data = Hobbit_values)
print(summary(model_multi6)
)
plot(model_multi6, id.n = 0)

# Original linear probability model for comparison. Fitted values can fall outside 0-1.
model_multi7 <- lm(hcpatron_bin ~ reincome + totspent + avprice + zipcode, data = Hobbit_values)
print(summary(model_multi7)
)
plot(model_multi7, id.n = 0)
# Δημιουργία λογιστικού μοντέλου (logistic regression)
log_model <- glm(hcpatron_bin ~ avprice ,
                 data = Hobbit_values,
                 family = binomial)

# Περίληψη αποτελεσμάτων
print(summary(log_model)
)
plot(log_model, id.n = 0)








