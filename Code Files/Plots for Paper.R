library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(infotheo)

# Course Dropout and Male

ggplot(percent_male, aes(x = course_percent_male, y = course_percent_dropout)) +
  geom_point(color = "#FD8D3C", size = 2, shape = 17) +
  geom_smooth(method = "lm") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(x = "% Males", y = "% Dropout") +
  theme_minimal() +
  theme(plot.margin = margin(10,10,10,10),
        panel.border = element_rect(fill = NA)) +
  annotate("text", x = 9, y = 64, label = "Basic Education", color = "#FD8D3C", size = 3)

# Dropout Rates for Different Variables

# Binary

p_scholar <- ggplot(data) +
  geom_bar(mapping = aes(x = scholarship_holder, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("Scholarship") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_debtor <- ggplot(data) +
  geom_bar(mapping = aes(x = debtor, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("Debtor") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_gender <- ggplot(data) +
  geom_bar(mapping = aes(x = gender, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("Gender") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_x_discrete(labels = c("Female", "Male")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_transfer <- ggplot(data) +
  geom_bar(mapping = aes(x = transfer, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("Transfer") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_int <- ggplot(data) +
  geom_bar(mapping = aes(x = international, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("International") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_single <- ggplot(data) +
  geom_bar(mapping = aes(x = single, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("Single") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_displaced <- ggplot(data) +
  geom_bar(mapping = aes(x = displaced, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("Displaced") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_needs <- ggplot(data) +
  geom_bar(mapping = aes(x = educational_special_needs, fill = dropout),
           position = "fill") +
  theme(legend.position = "none", axis.title.x = element_text(size = 10)) +
  ylab("") +
  xlab("Special Needs") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

# Other

p_age <- ggplot(data) +
  geom_histogram(mapping = aes(x= age_at_enrollment, fill = dropout),
                 position = "fill") +
  ylab("") +
  xlab("Age at Enrolment") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))


p_prev <- ggplot(data) +
  geom_bar(mapping = aes(x = previous_education, fill = dropout),
           position = "fill") +
  ylab("") +
  xlab("Previous Education") +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_x_discrete(labels = c("Basic", "Secondary", "Degree")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))
  
p_ss_ed <- ggplot(data) +
  geom_bar(mapping = aes(x = same_sex_parent_education, fill = dropout),
           position = "fill") +
  theme(legend.position = "none") +
  ylab("") +
  xlab("Same-Sex Parent Education") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%")) +
  scale_x_discrete(labels = c("Basic", "Secondary", "Degree"))

p_os_prof <- ggplot(data) +
  geom_bar(mapping = aes(x = opposite_sex_parent_profession, fill = dropout),
           position = "fill") +
  theme(legend.position = "none") +
  ylab("") +
  xlab("Opposite-Sex Parent Profession") +
  scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%")) 

# Arrange Plots

grid.arrange(p_debtor, p_scholar, p_gender, p_single, p_displaced, p_transfer, p_needs, p_int, 
             ncol = 4,
             left = "% of Students Who Dropped Out",
             top = "Binary Variables")


grid.arrange(p_age, p_prev, p_ss_ed, p_os_prof,
             left = "% of Students Who Dropped Out",
             top = "Non-Binary Variables")

# Course and Gender

ggplot(data) +
  geom_bar(mapping = aes(x = course, fill = gender),
           position = "fill") +
  xlab("") +
  ylab("") +
  coord_flip() +
  scale_fill_manual(values = c("pink", "#4E84C4"), name = "", labels = c("Female", "Male")) +
  scale_x_discrete(labels = c("Biofuel Production Technologies", "Animation & Multimedia Design", "Social Service (eve)", "Agronomy", "Communication Design", "Veterinary Nursing", "Informatics Engineering", "Equiniculture", "Management", "Social Service", "Tourism", "Nursing", "Oral Hygiene", "Advertising & Marketing Management", "Journalism & Communication", "Basic Education", "Management (eve)")) +
  scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%")) +
  theme_minimal() +
  theme(legend.position = "top")

# MI Scores

rest <- data %>%
  select(c(dropout, course, age_at_enrollment, scholarship_holder, same_sex_parent_education, opposite_sex_parent_profession, debtor, gender, previous_education, displaced, single, international, educational_special_needs, transfer)) %>%
  impute_mode(c("same_sex_parent_education", "opposite_sex_parent_profession"))

mut_info_rest <- mutinformation(rest)

mi_scores_rest <- mut_info_rest[-1,"dropout"] %>%
  sort()

par(mar = c(3,13,0,1))
barplot(mi_scores_rest,
        horiz = T, 
        las = 1,
        col = "#FD8D3C",
        cex.names = 0.9)

# Confusion Matrix

as.data.frame(cm_rad$table)

ggplot(as.data.frame(cm_rad$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "#FFF5EB", high = "#FD8D3C") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "black", size = 5, nudge_y = 0.1) +
  geom_text(aes(label = c("True Negatives", "False Positives", "False Negatives", "True Positives")), size = 5, nudge_y = -0.1) +
  labs(x = "Predicted", y = "Actual") +
  theme(legend.position="none", plot.title = element_text(hjust=0.5, size = 14), axis.text = element_text(size = 12), axis.title = element_text(size = 12))
