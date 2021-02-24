# function to compare models

library(ggsci)

plot_model_compare <- function(result_table, 
                               legend_title, 
                               scales = "fixed",
                               legend_position = "right") { 
  
  result_table$outcome <- factor(result_table$outcome, 
                                 levels = c("prem_20_to_31", "prem_32_to_36"),
                                 labels = c("20 to 31 weeks", "32 to 36 weeks"))
  
  result_table$exposure_cat <- factor(result_table$exposure_cat,
                                      levels = c("Medium", "High"),
                                      labels = c("Medium exposure (5-10 mg/L)", 
                                                 paste("High exposure (\u2265 10 mg/L)", sep = "")))
    
  ggplot(result_table, aes(x = outcome, y = OR, col = reorder(model, desc(model)))) +
    facet_wrap(~exposure_cat, labeller = label_value, scales = scales) +
    geom_hline(yintercept = 1, col = "darkgrey") +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(min = lower, ymax = upper), 
                  size = 0.5, 
                  width = 0, 
                  position = position_dodge(width = 0.5)) +
  #  ylim(0,2)+
    labs(y = "OR and 95% CI", x = "Gestational \nLength", 
         col = legend_title) +   
    coord_flip() +
    guides(col = guide_legend(reverse=T)) +
    theme_classic() +
    scale_color_lancet() +
    theme(text = element_text(size =10),
          legend.position = legend_position,
          strip.text = element_text(hjust = 0, face = "bold"),
          strip.background = element_rect(color = NA)) %>% 
    return()
}

