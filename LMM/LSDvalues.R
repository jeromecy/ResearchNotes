
#### ASReml-R code, post-hoc model functions #######

#### LSDlabels function is used for calcualting the LSD values for the pairwise comparisons from ASReml-R model ####
LSDlabels<- function(P,alpha) {
  a <- P
  diag(a) <- 1
  b <- which(a>alpha,arr.ind = TRUE)
  top <- data.frame(N1=b[,1],N2=b[,2])
  g3 <- igraph::simplify(igraph::graph.data.frame(top[order(top[[1]]),],directed=FALSE))
  cliq <- igraph::maximal.cliques(g3)
  nn <- length(cliq )
  temp1 <- rep("",nrow(a))
  assignment <- c(letters, LETTERS,paste(letters,'.',sep=''),paste(LETTERS,'.',sep=''))
  cliq2<-list()
  for (j in 1:nn){
    cliq2[[j]] <- colnames(a)[cliq[[j]]]
  }
  for (ind in 1:nrow(a)){
    ## check which list number contains this col name
    ## this one is problematic, since "1" is in "10" so list number containing "10" but no "1" will still be returned.
    ## tt <- grep(colnames(a)[ind],cliq2)
    ## solve the issue above
    tt=which(sapply(1:length(cliq2), function(x) colnames(a)[ind] %in% cliq2[[x]])==TRUE)
    temp1[ind]=paste0(assignment[tt],collapse = "")
  }
  return(temp1)
}

require(igraph)

library(asreml)
library(asremlPlus)
library(ggplot2)

data(oats)
oats.asr <- asreml(yield ~ Variety*Nitrogen, random = ~ Blocks/Wplots, data = oats)

mod.asrt <- as.asrtests(oats.asr,NULL,NULL)

diffs <- predictPlus(classify = "Variety*Nitrogen",
                     asreml.obj = oats.asr,
                     wald.tab = mod.asrt$wald.tab,
                     pairwise = TRUE)

### at p = 0.05 levels ###
diffs$predictions$LSD <- LSDlabels(diffs$p.differences,0.05)


ggplot(data=diffs$predictions, aes(Variety,predicted.value,fill=Nitrogen)) +
  geom_bar(stat="identity",
           position = position_dodge()) +
  geom_text(aes(Variety,predicted.value,label= round(predicted.value,2)), 
            vjust= -0.5, size=5,position = position_dodge(0.9))+ 
  geom_text(aes(label=LSD), vjust= 1.6, size=5,
            position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Paired") +
  labs(y="Predicted values with LSD labels") +
  theme_bw() +
  theme(text= element_text(size=15),
        legend.position = "bottom")


###### the above method is equivalent to the following method #######
## function “orderPvalue” from agricolae to get the lettering
### notes: agricolae may not be compatible with gam random effects.

library(agricolae)
library(tidyverse)

pred <- predictPlus.asreml(oats.asr, 
                           classify = "Variety*Nitrogen", 
                           wald.tab = as.data.frame(wald(oats.asr)), 
                           pairwise = TRUE)

std.error <- pred$predictions$standard.error
prob.matrix <- ifelse(is.na(pred$p.differences), 1, pred$p.differences)
treatments <- colnames(prob.matrix)
means <- pred$predictions$predicted.value
alpha <- 0.05
lsdmeantab <- orderPvalue(treatments, means, alpha, prob.matrix, 
                          console = TRUE)
lsdmeantab$Treatment <- rownames(lsdmeantab)


lsdmeantab <- lsdmeantab %>% 
  separate(Treatment, into = c("Variety", "Nitrogen"), sep = ",")


ggplot(data=lsdmeantab, aes(Variety,means,fill=Nitrogen)) +
  geom_bar(stat="identity",
           position = position_dodge()) +
  geom_text(aes(Variety,means,label= round(means,2)), 
            vjust= -0.5, size=5,position = position_dodge(0.9))+ 
  geom_text(aes(label=groups), vjust= 1.6, size=5,
            position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Paired") +
  labs(y="Predicted values with LSD labels") +
  theme_bw() +
  theme(text= element_text(size=15),
        legend.position = "bottom")



















