## Size at maturity - Bayesian inference ##
## by Lucas Rodrigues 10/11/23 ##

# libraries
library(readxl)
library(jagsUI)
library(car)
library(ggplot2)

# insert data
data <- read_excel("example_maturity_at_size.xlsx", sheet = "Plan1")

mature <- c(0,0,2,2,7,31,39,32,32,26,28,16,20,23,12,10,2,1)
total <- c(1,3,6,6,16,39,41,35,33,27,28,16,20,23,12,10,2,1)
class <- seq(170,340,10)
data <- data.frame(cbind(class, mature, total))

# frequentist Binomial model
mod <- glm(cbind(mature, total-mature) ~ class, data = data, family = binomial(link="logit"))
Anova(mod)

# parameters extracted from frequestist GLM
alfa <- as.numeric(coef(mod))

# ploting curve
plot(data$class,data$mature/data$total,xlab="Total length (mm)", ylab="Probability of maturation", ylim=c(0,1), las = 1, pch = 16)
curve((exp(alfa[1]+alfa[2]*x))/(1+(exp(alfa[1]+alfa[2]*x))),add=T,col="blue",lwd=2)

# inform data to jags
datalist = list(y=data$mature,n=data$total,x=data$class,k=length(data$mature))

# targeting paremeters
params = c('beta0','beta1')

# init values
inits = function() {
  beta0 = as.numeric(mod$coefficients[1])
  beta1 = as.numeric(mod$coefficients[2])
  list(beta0=beta0,beta1=beta1)
}

# writing the model
sink('MtAtSz.txt')
cat('
    model {
    beta0 ~ dnorm(0,0.001)
    beta1 ~ dnorm(0,0.001)
    for (i in 1:k) {
    logit(p[i]) <- beta0 + beta1*x[i]
    y[i] ~ dbin(p[i], n[i])
    }
    }
    ', fill=TRUE)
sink()

# running the model
mod.jags <- jags(datalist, inits = inits, parameters.to.save = params, model.file='MtAtSz.txt',
                           n.chains=3, n.iter=30000, n.burnin=2000, n.thin=3, DIC=T)

# choose the cutoff! ex. .5, .9, etc
prob_mat <- .5

# size at maturity
beta <- cbind(mod.jags$sims.list$beta0, mod.jags$sims.list$beta1)
lrPerc <- function(cf,p) (log(prob_mat/(1-prob_mat))-cf[,1])/cf[,2]
SzAtMat <- round(lrPerc(beta, prob_mat), 1)
(mean(SzAtMat))

# parameters for median
p50_est <- apply(beta,2,median)

# parameters for 2.5 quantile
p2.5 <- function(x) quantile(x,.025)
p2.5_est <- apply(beta, 2, p2.5)

# parameters for 97.5 quantile
p97.5 <- function(x) quantile(x,.975)
p97.5_est <- apply(beta, 2, p97.5)

# plot
plot1 <- ggplot() +
  geom_histogram(aes(SzAtMat), fill = "black", bins = 20) +
  ylab("Density") +
  xlab("Size class (mm)") +
  theme(panel.background = element_blank(),
        axis.title = element_text(size=8),
        axis.text = element_text(size=5),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(color = "red", linewidth = 1.5),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.1, linetype = 2), legend.position = 'none')
plot1

ggplot() +
  geom_segment(aes(x = mean(SzAtMat), xend = .85*max(data$class), y = prob_mat, yend = .5), color ="red", size =.3, linetype = "dashed")+
  geom_segment(aes(x = mean(SzAtMat), xend = .85*max(data$class), y = prob_mat, yend = 0), color ="red", size =.3, linetype = "dashed")+
  annotation_custom(ggplotGrob(plot1), xmin = .85*max(data$class), xmax = max(data$class), 
                    ymin = 0, ymax = .5) +
  geom_function(fun = function(x) (exp(p50_est[1]+p50_est[2]*x))/(1+(exp(p50_est[1]+p50_est[2]*x))), col="black", lty=1, lwd=1) +
#  geom_function(fun = function(x) (exp(p50_est[1]+p2.5_est[2]*x))/(1+(exp(p50_est[1]+p2.5_est[2]*x))), col="grey", lty=2, lwd=.8) +
#  geom_function(fun = function(x) (exp(p50_est[1]+p97.5_est[2]*x))/(1+(exp(p50_est[1]+p97.5_est[2]*x))), col="grey", lty=2, lwd=.8) +
  geom_point(data = data, aes(x = class, y = mature/total), colour = "black", shape = 16, size=3) +
  geom_segment(aes(x = -15, xend = mean(SzAtMat), y = prob_mat, yend = prob_mat), color ="red", size =.8, linetype = "dashed")+
#  geom_segment(aes(x = mean(SzAtMat), xend = mean(SzAtMat), y = -.5, yend = prob_mat), color ="red", size =.8, linetype = "dashed")+
  annotate(geom = "text",x = quantile(seq(min(data$class),max(data$class)),.1), y = .95, label = bquote(L[50] == .(SzAtMat) ~ "mm"), color = "black") +
  annotate(geom = "point",x = mean(SzAtMat), y = prob_mat, color = "red", shape = 0, size = 5, stroke = 1.5) +
  ylab("Maturity") +
  xlab("Size class (mm)") +
  scale_x_continuous(breaks = as.numeric(data$class)) +
  coord_cartesian(xlim = c(min(data$class),max(data$class)), ylim = c(0,1))+
  theme(panel.background = element_blank(),
        axis.text = element_text(size=7),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.1, linetype = 2), legend.position = 'none')

# saving plot
ggsave("image2.jpeg", width = 130, height = 100, units = c("mm"), dpi = 600)
