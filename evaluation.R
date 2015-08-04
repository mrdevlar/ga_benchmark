## Make sure that you have output from raw_data_input.R and iterloop.R
library(dplyr)
library(ggplot2)

output = tbl_df(output)

maxiters = output %>% 
  group_by(Type, Rep) %>%
  summarise(maxIter = max(Iter)) %>%
  ungroup() 
maxiters = maxiters %>%
  group_by(Type) %>%
  summarise(mean(maxIter), sd(maxIter), median(maxIter), mad(maxIter))


bestsol = output %>%
  group_by(Type, Rep) %>%
  summarise(bestSol = max(Best)) %>%
  ungroup()
bestsol = bestsol %>%
  group_by(Type) %>%
  summarise(mean(bestSol), sd(bestSol), median(bestSol), mad(bestSol))



output %>%
  ggplot(., aes(x=Iter, y=Best, color=as.factor(Type))) + geom_line(alpha=0.4) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("Number of Iterations") + ylab("Best Fitness") + labs(color='Type')



output = output %>%
  mutate(cro =  ifelse(grepl("cx", Type), "cx",
                ifelse(grepl("ox", Type), "ox",  
                ifelse(grepl("pmx", Type),"pmx",   
                ifelse(grepl("pbx", Type),"pbx", NA))))) %>%
  mutate(mut =  ifelse(grepl("dm", Type), "dm",
                ifelse(grepl("ism", Type), "ism",
                ifelse(grepl("scr", Type), "scr",
                ifelse(grepl("sim", Type), "sim",
                ifelse(grepl("sw", Type), "sw", NA ))))))

output %>%
  ggplot(., aes(x=Iter, y=Best)) + geom_line(alpha=0.7, show_guide=FALSE) +
  # guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("Number of Iterations") + ylab("Best Fitness") + labs(color='Type') +
  facet_grid(mut~cro)


write.table(maxiters, "clipboard", sep="\t", row.names=FALSE)
write.table(bestsol, "clipboard", sep="\t", row.names=FALSE)
