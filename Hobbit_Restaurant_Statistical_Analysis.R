library(summarytools)
freq(Hobbit_values$eatrest)
freq(Hobbit_values)
barplot(Hobbit_values$radprogr)
summary(Hobbit_values$totspent)
plot(Hobbit_values$eatrest)

boxplot(Hobbit_values$totspent, 
        main = "Boxplot of Total Spent (totspent)",
        ylab = "Total Spent",
        col = "lightblue",
        border = "darkblue")

library(ggplot2)
Hobbit_values$totspent <- as.numeric(Hobbit_values$totspent)
Hobbit_values$likely <- as.factor(Hobbit_values$likely)  # Μετατροπή της "likely" σε factor (κατηγορία)

# Δημιουργία του Boxplot
boxplot(Hobbit_values$totspent ~ Hobbit_values$likely,
        main = "Boxplot of Total Spent (totspent) by Likelihood of Visit (likely)",
        xlab = "Likelihood of Visit",
        ylab = "Total Spent",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
        border = "darkblue")



library(ggplot2)
library(reshape2)



# Μετατροπή όλων των αριθμητικών μεταβλητών σε numeric
Hobbit_values$reincome <- as.numeric(gsub(",", ".", Hobbit_values$reincome))
Hobbit_values$totspent <- as.numeric(Hobbit_values$totspent)
Hobbit_values$likely <- as.numeric(Hobbit_values$likely)
Hobbit_values$avprice <- as.numeric(Hobbit_values$avprice)
Hobbit_values$yrborn <- as.numeric(Hobbit_values$yrborn)
Hobbit_values$income <- as.numeric(Hobbit_values$income)
Hobbit_values$hcpatron <- as.numeric(Hobbit_values$hcpatron)
Hobbit_values$VLPatron <- as.numeric(Hobbit_values$VLPatron)

# Επιλογή της μεθόδου για τον χειρισμό των NA
method_na <- "complete.obs" # Μπορείς να αλλάξεις σε "pairwise.complete.obs" ή "na.or.complete"

# Υπολογισμός του πίνακα συσχέτισης με τη συγκεκριμένη μέθοδο
numeric_columns <- Hobbit_values[sapply(Hobbit_values, is.numeric)]
correlation_matrix <- cor(numeric_columns, use = method_na, method = "pearson")

# Αναδιάταξη του πίνακα για ggplot2
cor_data <- melt(correlation_matrix)
colnames(cor_data) <- c("Var1", "Var2", "value")

# Δημιουργία του heatmap με ggplot2 και εμφάνιση των τιμών συσχέτισης
heatmap_plot <- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab") +
  geom_text(aes(label = round(value, 2)), size =2.3, color = "black") +  # Προσθήκη αριθμών
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)) +
  ggtitle(paste("Heatmap Συσχέτισης Μεταβλητών (", method_na, ")", sep = ""))

# Προβολή του heatmap
print(heatmap_plot)

# Αποθήκευση του heatmap ως εικόνα (PNG)
ggsave(paste0("heatmap_correlation_", method_na, ".png"), 
plot = heatmap_plot, width = 12, height = 8, dpi = 150)
cor(Hobbit_values)

Hobbit_values<-na.omit(Hobbit_values)

mean(Hobbit_values$totspent)
sd(Hobbit_values$likely)
table(Hobbit_values$avprice)

hist(Hobbit_values$totspent)
hist(Hobbit_values$likely)

summary(aov(Hobbit_values$waitstaf~Hobbit_values$drive))

manova_model <- manova(cbind(totspent, likely, radprogr, vietvnew) ~ educa, data = Hobbit_values)
summary(manova_model)
summary.aov(manova_model)





t_test_result <- t.test(Hobbit_values$totspent ~ Hobbit_values$gender)
print(t_test_result)
mean(Hobbit_values$income)
t_test_result <- t.test(Hobbit_values$reincome ~ Hobbit_values$gender)
print(t_test_result)
hist(2006-Hobbit_values$yrborn)
t_test_result <- t.test(Hobbit_values$totspent~Hobbit_values$VLPatron)
print(t_test_result)

Hobbit_values$age <- 2006 - Hobbit_values$yrborn

# Δημιουργία του Boxplot
boxplot( 2006-Hobbit_values$yrborn~Hobbit_values$likely ,
        main = "Boxplot of Total Spent (totspent) by Likelihood of Visit (likely)",
        xlab = "Likelihood of Visit",
        ylab = "age",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
        border = "darkblue")
# Δημιουργία του Boxplot
boxplot( 2006-Hobbit_values$yrborn~Hobbit_values$gender ,
         main = "",
         xlab = "",
         ylab = "",
         col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
         border = "darkblue")


# Δημιουργία του Boxplot
boxplot( 2006-Hobbit_values$yrborn~Hobbit_values$VLPatron ,
         main = "Boxplot of Total Spent (totspent) by VLPatron",
         xlab = "Likelihood of Visit",
         ylab = "Total Spent",
         col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
         border = "darkblue")


# Δημιουργία του Boxplot
boxplot(Hobbit_values$totspent ~ Hobbit_values$VLPatron,
        main = "Boxplot of Total Spent (totspent) by VLPatron",
        xlab = "Likelihood of Visit",
        ylab = "Total Spent",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray"),
        border = "darkblue")
# Υπολογισμός της ηλικίας με βάση το έτος γέννησης (2006 είναι το έτος αναφοράς)
Hobbit_values$age <- 2006 - Hobbit_values$yrborn

# Δημιουργία των ηλικιακών ομάδων
Hobbit_values$age_group <- cut(Hobbit_values$age,
                               breaks = c(30, 40, 50, 60, 70, Inf),
                               labels = c("30-40", "41-50", "51-60", "61-70", "71+"),
                               right = FALSE)

# Έλεγχος αν οι ηλικιακές ομάδες δημιουργήθηκαν σωστά
table(Hobbit_values$age_group)
# Φόρτωση της βιβλιοθήκης ggplot2 (αν δεν είναι ήδη φορτωμένη)
library(ggplot2)

# Δημιουργία Scatter Plot με τις ηλικιακές ομάδες
ggplot(Hobbit_values, aes(x = age, y = totspent, color = age_group)) +
  geom_point() +
  labs(title = "Scatter Plot of Total Spent by Age Groups",
       x = "Age",
       y = "Total Spent") +
  theme_minimal()


ggplot(Hobbit_values ,aes (x=2006-Hobbit_values$yrborn, y=Hobbit_values$totspent, color=age_group))+geom_point()



# Υπολογισμός ηλικίας
Hobbit_values$age <- 2006 - Hobbit_values$yrborn

# Φιλτράρισμα για άτομα ηλικίας 70 και άνω
income_70plus <- subset(Hobbit_values, age >= 70)$income

# Προβολή των τιμών εισοδήματος για την ομάδα 70+
print(income_70plus)





Hobbit_values$drive <- as.factor(Hobbit_values$drive)
Hobbit_values$zipcode <- as.factor(Hobbit_values$zipcode)

# Δημιουργία του γραφήματος χωρίς τη χρήση $
library(ggplot2)
ggplot(Hobbit_values, aes(x = drive, fill = zipcode)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Bar Plot of Drive Preference by Zipcode",
    x = "Drive Preference (1: Very Low - 5: Very High)",
    y = "Count"
  ) +
  theme_minimal()



# Μετατροπή των μεταβλητών σε factors (χωρίς να αλλάξουμε το drive)
Hobbit_values$VLPatron <- as.factor(Hobbit_values$VLPatron)
Hobbit_values$zipcode <- as.factor(Hobbit_values$zipcode)

# Δημιουργία του γραφήματος χωρίς το λάθος στη μεταβλητή drive
library(ggplot2)
ggplot(Hobbit_values, aes(x = zipcode, fill = VLPatron, group = VLPatron)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Bar Plot of Drive Preference by Zipcode",
    x = "Zipcode",
    y = "Count"
  ) +
  theme_minimal()

# Υπολογισμός του ποσοστού για κάθε συνδυασμό drive και zipcode
library(dplyr)
library(ggplot2)

Hobbit_values %>%
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

# Υπολογισμός του ποσοστού για κάθε συνδυασμό VLPatron και zipcode
Hobbit_values %>%
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



-----------------------------------------------------------------------------------------------------------------
  # Αν δεν το έχεις κάνει ήδη:
  Hobbit_values$hcpatron <- factor(Hobbit_values$hcpatron,
                                   levels = c(1, 2),
                                   labels = c("Yes", "No"))

  # Υπολογισμός του ποσοστού για κάθε συνδυασμό hcpatron και zipcode
  Hobbit_values %>%
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




-------------------------------------------------
  library(ggplot2)
library(dplyr)

# Φιλτράρισμα για hcpatron = 2
Hobbit_filtered <- Hobbit_values %>%
  filter(hcpatron == 1)

# Προετοιμασία των μεταβλητών
Hobbit_filtered$marital <- ifelse(Hobbit_filtered$marital == 3, 1, Hobbit_filtered$marital)
Hobbit_filtered$marital <- factor(Hobbit_filtered$marital, 
                                  levels = c(1, 2),
                                  labels = c("Single/Other", "Married"))
Hobbit_filtered$gender <- as.factor(Hobbit_filtered$gender)

# Δημιουργία dataset με ποσοστά και συνολικά n ανά marital
plot_data <- Hobbit_filtered %>%
  count(marital, gender) %>%
  group_by(marital) %>%
  mutate(percentage = n / sum(n) * 100,
         total = sum(n))  # συνολικός αριθμός ατόμων ανά marital

# Δημιουργία του γραφήματος
ggplot(plot_data, aes(x = marital, y = percentage, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  geom_text(data = distinct(plot_data, marital, total),
            aes(x = marital, y = 105, label = paste0("N = ", total)),
            inherit.aes = FALSE, fontface = "bold", size = 4.5) +
  labs(
    title = "Marital Status by Gender (Only hcpatron = 2)",
    subtitle = "Ποσοστά + Απόλυτος Αριθμός ανά Κατηγορία Marital",
    x = "Marital Status",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "pink")) +
  ylim(0, 110)

---------------------------------------------------------------------
  
  library(ggplot2)
library(dplyr)

# Δημιουργία πίνακα με counts ανά zipcode
zipcode_data <- Hobbit_values %>%
  count(zipcode) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(zipcode, " (", round(percentage, 1), "%)"))

# Δημιουργία Pie Chart με ggplot2
ggplot(zipcode_data, aes(x = "", y = percentage, fill = zipcode)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Ποσοστά ανά Zipcode") +
  theme_void()




library(ggplot2)

# Βεβαιώσου ότι hcpatron είναι factor (1 = Yes, 2 = No)
Hobbit_values$hcpatron <- factor(Hobbit_values$hcpatron,
                                 levels = c(1, 2),
                                 labels = c("Yes", "No"))

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
library(dplyr)
plot_data <- decor_long %>%
  group_by(decor_type, hcpatron, score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(decor_type, hcpatron) %>%
  mutate(percentage = n / sum(n) * 100)

# Δημιουργία ιστογράμματος με ποσοστά
ggplot(plot_data, aes(x = factor(score), y = percentage, fill = hcpatron)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~decor_type, scales = "free_y") +
  labs(title = "Ιστόγραμμα Προτίμησης Διακόσμησης ανά Πιθανότητα Να Είναι Πελάτης",
       x = "Βαθμολογία Προτίμησης (1–5)",
       y = "Ποσοστό (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "salmon"))


# Αν το hcpatron είναι factor με Yes/No
Hobbit_values$hcpatron_bin <- ifelse(Hobbit_values$hcpatron == "Yes", 1, 0)

names(Hobbit_values)


# Λογιστική Παλινδρόμηση με μεταβλητές ντεκόρ
log_model <- glm(Hobbit_values$hcpatron_bin ~
                   Hobbit_values$elegant +
                   Hobbit_values$simple +
                   Hobbit_values$string +
                   Hobbit_values$jazz,
                 family = binomial)
summary(log_model)
# Προβλεπόμενες πιθανότητες
Hobbit_values$predicted_prob <- predict(log_model, type = "response")

# Κατηγοριοποίηση: Πιθανότητα > 0.5 → Yes
Hobbit_values$predicted_class <- ifelse(Hobbit_values$predicted_prob > 0.5, "Yes", "No")

# Πίνακας σύγκρισης πραγματικής vs προβλεπόμενης κατηγορίας
table(Predicted = Hobbit_values$predicted_class, Actual = Hobbit_values$hcpatron)
--------------------------------------------------------------------------------------------------------
# Πίνακας περιγραφικών στατιστικών για τις marketing μεταβλητές
marketing_vars <- c("lisradio", "radprogr", "vietvnew", "timenew", "reapaper", "citymag")

summary(Hobbit_values[marketing_vars])
sapply(Hobbit_values[marketing_vars], sd, na.rm = TRUE)  # Τυπική απόκλιση

# Βεβαιώσου ότι το hcpatron είναι factor
Hobbit_values$hcpatron <- factor(Hobbit_values$hcpatron, levels = c(1, 2), labels = c("Yes", "No"))

# Long format για χρήση σε ggplot
library(ggplot2)
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
ggplot(marketing_long, aes(x = hcpatron, y = score, fill = hcpatron)) +
  geom_boxplot() +
  facet_wrap(~marketing_type, scales = "free_y") +
  labs(title = "Αντίληψη Marketing ανά Πιθανότητα Να Είναι Πελάτης",
       x = "Πιθανός Πελάτης (hcpatron)",
       y = "Βαθμολογία Marketing") +
  theme_minimal()


-----------------------------------------------------------
  library(ggplot2)
library(dplyr)


Hobbit_values$hcpatron <- factor(Hobbit_values$hcpatron, levels = c(1, 2), labels = c("Yes", "No"))

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
ggplot(plot_data, aes(x = factor(score), y = percentage, fill = hcpatron)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~marketing_type, scales = "free_y") +
  labs(title = "Προτιμήσεις Marketing Μέσων ανά Πιθανότητα Να Είναι Πελάτης",
       x = "Βαθμολογία (1–5)",
       y = "Ποσοστό (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "orange"))

table(Hobbit_values$hcpatron)
prop.table(table(Hobbit_values$hcpatron))

var(Hobbit_values$totspent, na.rm=TRUE)
sd(Hobbit_values$totspent , na.rm=TRUE)
range(Hobbit_values$totspent , na.rm=TRUE)
var(Hobbit_values$reincome, na.rm=TRUE)
range(Hobbit_values$reincome, na.rm=TRUE)
sd(Hobbit_values$reincome , na.rm=TRUE)

qqnorm(log(Hobbit_values$avprice), main="Normal Q-Q plot for price")
qqline(log(Hobbit_values$avprice))

qqnorm(Hobbit_values$avprice, main="Normal Q-Q plot for price")
qqline(Hobbit_values$avprice)


# ΠΡΟΕΤΟΙΜΑΣΙΑ
#set.seed(42)  # Για αναπαραγωγιμότητα

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
    mean = mean(boot_means),
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
  abline(v = mean(boot_means), col = "black", lwd = 2)
  legend("topright", legend = c("2.5% CI", "97.5% CI", "Mean"),
         col = c("red", "red", "black"), lty = c(2, 2, 1), lwd = c(1, 1, 2))
}

anova_result <- aov(totspent ~ as.factor(VLPatron), data = Hobbit_values)
summary(anova_result)
TukeyHSD(anova_result)

-----------------------------------------------------------------------------------------
plot(Hobbit_values$avprice,Hobbit_values$totspent)
model1<-lm(Hobbit_values$totspent~Hobbit_values$avprice)
abline(model1)
summary(model1)


# Δημιουργία του γραμμικού μοντέλου
model1 <- lm(totspent ~ avprice, data = Hobbit_values)

# Δημιουργία όλων των διαγνωστικών γραφημάτων
par(mfrow = c(2, 2))  # 2x2 διάταξη παραθύρων
plot(model1)

# Επαναφορά γραφικής διάταξης
par(mfrow = c(1, 1))
----------------------------------------------------------------------------------------------------------
  plot(log(Hobbit_values$avprice),log(Hobbit_values$totspent))
model2<-lm(log(Hobbit_values$totspent)~log(Hobbit_values$avprice))
abline(model2)
summary(model2)

# Δημιουργία νέων λογαριθμισμένων μεταβλητών (φροντίζουμε να μην έχει μηδενικά)
Hobbit_values$logtotspent <- log(Hobbit_values$totspent + 1)
Hobbit_values$logavprice <- log(Hobbit_values$avprice + 1)

# Δημιουργία του γραμμικού μοντέλου με τις λογαριθμισμένες μεταβλητές
model_log <- lm(logtotspent ~ logavprice, data = Hobbit_values)

# 4 Διαγνωστικά γραφήματα
par(mfrow = c(2, 2))  # Διάταξη 2x2
plot(model_log)

# Επαναφορά της διάταξης γραφημάτων
par(mfrow = c(1, 1))

----------------------------------------------------------------------------

library(DescTools)
anova_result <- aov(reincome ~ as.factor(likely), data = Hobbit_values)
summary(anova_result)
PostHocTest(anova_result,method="hsd")

--------------------------------------------------------------------------------
  # 1. Δημιουργούμε μια δυαδική μεταβλητή για το hcpatron (1 = Yes, 0 = No)
  Hobbit_values$hcpatron_bin <- ifelse(Hobbit_values$hcpatron == 2 , 1, 0)

# 2. Υπολογισμός μέσων όρων για ντεκόρ και διαφήμιση
Hobbit_values$decor_mean <- rowMeans(Hobbit_values[, c("elegant", "simple", "string", "jazz")], na.rm = TRUE)

Hobbit_values$marketing_mean <- rowMeans(Hobbit_values[, c("lisradio", "radprogr", "vietvnew", "timenew", "reapaper", "citymag")], na.rm = TRUE)

# 3. Δημιουργία πολλαπλού γραμμικού μοντέλου
model_multi2 <- lm(log(totspent) ~ reincome + decor_mean + marketing_mean, data = Hobbit_values)

# 4. Εμφάνιση αποτελεσμάτων
summary(model_multi2)
plot(model_multi2)
model_multi2 <- lm(totspent ~ reincome + hcpatron_bin +decor_mean+marketing_mean + avprice + zipcode + educa + drive, data = Hobbit_values)
summary(model_multi2)

model_multi3 <- lm(totspent ~ reincome + hcpatron_bin +decor_mean+marketing_mean + avprice + zipcode + educa, data = Hobbit_values)
summary(model_multi3)

model_multi4 <- lm(totspent ~ reincome + hcpatron_bin + decor_mean +marketing_mean + avprice + zipcode, data = Hobbit_values)
summary(model_multi4)

model_multi5 <- lm(totspent ~ reincome + hcpatron_bin +decor_mean+avprice + zipcode , data = Hobbit_values)
summary(model_multi5)
plot(model_multi5)
Hobbit_values$zipcode <- as.factor(Hobbit_values$zipcode)
model_multi6 <- lm(log(totspent) ~ reincome + hcpatron_bin + avprice, data = Hobbit_values)
summary(model_multi6)
plot(model_multi6)

model_multi7 <- lm(hcpatron_bin ~ reincome + totspent + avprice + zipcode , data = Hobbit_values)
summary(model_multi7)
plot(model_multi7)
# Δημιουργία λογιστικού μοντέλου (logistic regression)
log_model <- glm(hcpatron_bin ~ avprice ,
                 data = Hobbit_values,
                 family = binomial)

# Περίληψη αποτελεσμάτων
summary(log_model)
plot(log_model)
