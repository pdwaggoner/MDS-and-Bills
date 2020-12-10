# Replication code for MDS and Congressional Bills paper
# Philip Waggoner (pdwaggoner@uchicago.edu)

# Load libs
library(proxy)
library(smacof)
library(ggplot2)
library(arm)
library(foreign)

## Load Sponsorship Data

## Correlation Matrix
round(cor(all.dta[,-1]), digits = 4)
bills.cor <- (cor(all.dta[,-1])) # store the correlation matrix without congress

# EVs
ev <- eigen(bills.cor)

# Scree
qplot(y=ev$values, 
      main='Bill Sponsorship SCREE Plot,\n1947-2016', 
      xlab='Dimensions', 
      ylab='Eigenvalue') +
  geom_line() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Subsets for MDS; update accordingly
total <- subset(all.dta, Cong=="Total") # pooled sponsorships
#x114 <- subset(all.dta, Cong==114) # individual congs

#total$Cong <- NULL # Drop "Cong" variable for MDS if desired

dta <- as.data.frame(t(total)) # take transpose

## Compute Dissimilarities Matrix
xdta <- dist(dta, y = NULL, 
             method = "Euclidean", 
             diag = TRUE, upper = TRUE,
             pairwise = FALSE, 
             by_rows = TRUE, 
             convert_similarities = TRUE,
             auto_convert_data_frames = TRUE)

## MDS
fitclas <- smacofSym(xdta, 
                     ndim = 1)
metric.conf <- fitclas$conf # store

## Pooled Plot
plot(metric.conf[,1], axes=FALSE, pch=20,
     main="Metric Multidimensional Scaling Solution Cross-Section, All Congresses (1947-2016)",
     ylab="Federal Focal Priority", xlab="Issue Index")
Axis(side=1, labels=FALSE)
Axis(side=2, labels=FALSE)
points(metric.conf[,1],  pch=20, col="darkgray", font=2)
text(metric.conf[,1], rownames(dta),
     pos=c(1,4,3,1,3,4,2,1,1,2,3,1,3,3,4,2,3,1,2,2), offset=0.35, col="black", font=1, cex=.7)
grid()

## Paths
# Load Full MDS Solutions Data for each Congress
total.mds <- read.csv("FILE_PATH") # "2 - MDS.Solutions_80-114.csv"

# Drop issues indiv if desired
total.mds1 <- total.mds[!total.mds$issue=="Education",]
total.mds1 <- total.mds1[!total.mds1$issue=="Environment",]
total.mds1 <- total.mds1[!total.mds1$issue=="Public.Lands",]
total.mds1 <- total.mds1[!total.mds1$issue=="Immigration",]
total.mds1 <- total.mds1[!total.mds1$issue=="Transportation",]
total.mds1 <- total.mds1[!total.mds1$issue=="Law.Crime",]
total.mds1 <- total.mds1[!total.mds1$issue=="Commerce",]
total.mds1 <- total.mds1[!total.mds1$issue=="Housing",]
total.mds1 <- total.mds1[!total.mds1$issue=="Economics",]
total.mds1 <- total.mds1[!total.mds1$issue=="Civil.Rights",]
#total.mds1 <- total.mds1[!total.mds1$issue=="Science.Technology",]
total.mds1 <- total.mds1[!total.mds1$issue=="Trade",]
total.mds1 <- total.mds1[!total.mds1$issue=="International.Affairs",]
total.mds1 <- total.mds1[!total.mds1$issue=="Labor",]
total.mds1 <- total.mds1[!total.mds1$issue=="Agriculture",]
total.mds1 <- total.mds1[!total.mds1$issue=="Welfare",]
total.mds1 <- total.mds1[!total.mds1$issue=="Energy",]
total.mds1 <- total.mds1[!total.mds1$issue=="Defense",]
total.mds1 <- total.mds1[!total.mds1$issue=="Health",]

# plot
qplot(total.mds1$congress, total.mds1$mds, geom = "path", linetype = total.mds1$issue,
           main = "Prioritization of Science/Technology Bills, Compared to Government Operations Bills",
           xlab = "Congress",
           ylab = "Federal Focal Prioritization (MDS Solutions)") +
  scale_linetype_manual(values=c("solid", "dotdash")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# appendix

# Shepard (plots distances as a function of dissimilarities)
plot(fitclas, 
     plot.type = "Shepard")

# Follow MDS procedure above for Appendix plots on (Civil Rights, Healthcare, and Public Law) Comparisons (see datasets starting with "A...")
