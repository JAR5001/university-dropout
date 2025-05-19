# PRESENTATION

# Libraries
library(tidyverse)
library(gridExtra)
library(RColorBrewer)

# Dropout Rates for Different Variables

p_prev <- ggplot(data) +
          geom_bar(mapping = aes(x = previous_education, fill = dropout),
                  position = "fill") +
          ylab("") +
          xlab("Previous Education") +
          theme(legend.position = "none") + 
          scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
          scale_x_discrete(labels = c("Basic", "Secondary", "Degree")) +
          scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_scholar <- ggplot(data) +
            geom_bar(mapping = aes(x = scholarship_holder, fill = dropout),
                    position = "fill") +
            theme(legend.position = "none", axis.text.y = element_blank()) +
            ylab("") +
            xlab("Scholarship") +
            scale_fill_manual(values = c("#FFF5EB", "#FD8D3C"))

p_debtor <- ggplot(data) +
            geom_bar(mapping = aes(x = debtor, fill = dropout),
                     position = "fill") +
            theme(legend.position = "none", axis.text.y = element_blank()) +
            ylab("") +
            xlab("Debtor") +
            scale_fill_manual(values = c("#FFF5EB", "#FD8D3C"))

p_age <- ggplot(data) +
          geom_histogram(mapping = aes(x= age_at_enrollment, fill = dropout),
                 position = "fill") +
          ylab("") +
          xlab("Age at Enrolment") +
          theme(legend.position = "bottom") +
          scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
          scale_y_continuous(labels = c("0", "25%", "50%", "75%", "100%"))

p_gender <- ggplot(data) +
            geom_bar(mapping = aes(x = gender, fill = dropout),
                    position = "fill") +
            theme(legend.position = "none", axis.text.y = element_blank()) +
            ylab("") +
            xlab("Gender") +
            scale_fill_manual(values = c("#FFF5EB", "#FD8D3C")) +
            scale_x_discrete(labels = c("Female", "Male"))

p_transfer <- ggplot(data) +
                geom_bar(mapping = aes(x = transfer, fill = dropout),
                         position = "fill") +
                theme(legend.position = "none", axis.text.y = element_blank()) +
                ylab("") +
                xlab("Transfer") +
                scale_fill_manual(values = c("#FFF5EB", "#FD8D3C"))



# Arrange Plots

grid.arrange(p_prev, p_scholar, p_debtor, p_age, p_gender, p_transfer, 
             nrow = 2,
             layout_matrix = rbind(c(1,1,2,3), c(4,4,5,6)),
             top = "Dropout Rate for Different Variables")

# MI Heatmap

# Install libraries
library("praznik")
library("infotheo")

variables_pres <- data %>% select(c("marital_status", "application_mode", "application_order", "course", 
                                    "attendance_mode", "previous_qualification", "nationality", "mother_qualification", 
                                    "father_qualification", "mother_occupation", "father_occupation", "displaced", 
                                    "educational_special_needs", "debtor", "gender", "scholarship_holder", 
                                    "age_at_enrollment", "international","dropout"))

mut_info <- mutinformation(variables_pres)

heatmap(mut_info, 
        scale = "column", 
        Colv = NA, 
        Rowv = NA, 
        margins = c(11,11), 
        col = colorRampPalette(brewer.pal(9, "Oranges"))(25),
        main = "Heatmap of MI Scores")


