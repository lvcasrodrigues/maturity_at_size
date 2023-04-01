## Size at maturity ##
## by Lucas Rodrigues 31/03/23 ##

# libraries
library(readxl)
library(car)
library(ggplot2)
library(emmeans)
library(dplyr)

# insert data
data <- read_excel("example_maturity_at_size.xlsx", sheet = "Plan1")

# frequentist Binomial model
mod <- glm(cbind(mature, total-mature) ~ class, data = data, family = binomial(link="logit"))
Anova(mod)

# generating data from model
yhat.df <- emmeans(mod, ~ class, at=list(class=seq(min(data$class),max(data$class),by=ifelse(max(data$class)>1000,1,.1))), type='response') %>%
  as.data.frame()

# choose the cutoff! ex. .5, .9, etc
prob_mat <- .5

# size at maturity
cut <- filter(yhat.df, prob >= prob_mat)

SzAtMat <- round(min(cut$class),3)
SzAtMat

# plot
ggplot() +
  geom_point(data = data, aes(x = class, y = mature/total), colour = "black", shape = 16, size=3)+
  geom_line(data = yhat.df, aes(y = prob, x = class), col="black", lty=1, lwd=1) +
  geom_line(data = yhat.df, aes(y = asymp.LCL, x = class), col="grey", lty=2, lwd=.8) +
  geom_line(data = yhat.df, aes(y = asymp.UCL, x = class), col="grey", lty=2, lwd=.8) +
  geom_segment(aes(x = -15, xend = SzAtMat, y = prob_mat, yend = prob_mat), color ="red", size =.8, linetype = "dashed")+
  geom_segment(aes(x = SzAtMat, xend = SzAtMat, y = -.5, yend = prob_mat), color ="red", size =.8, linetype = "dashed")+
  annotate(geom = "text",x = quantile(seq(min(data$class),max(data$class)),.1), y = .95, label = bquote(L[50] == .(SzAtMat) ~ "mm"), color = "black") +
  ylab("Maturity") +
  xlab("Size class (mm)") +
  scale_x_continuous(breaks = as.numeric(data$class)) +
  coord_cartesian(xlim = c(min(data$class),max(data$class)), ylim = c(0,1))+
   theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color = "grey90", size = 0.1, linetype = 2), legend.position = 'none')

# saving plot
ggsave("image.jpeg", width = 120, height = 100, units = c("mm"), dpi = 600)
