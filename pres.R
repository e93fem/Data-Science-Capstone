library(ggplot2)
test_data <- readRDS(file="test_data.rds")
lm_res <- readRDS(file="lm_res.rds")   
pred <- predict(lm_res, test_data)
round_pred <- round(pred)
diff <- round_pred-test_data$r_stars
plotter <- data.frame(diff)
ggplot(plotter, aes(diff)) + geom_histogram(aes(y=..count../sum(..count..))) + xlab("Differens") + ylab(label="Distribution") + scale_x_continuous(breaks = round(seq(-3, 3, by = 1),1)) + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22), axis.text= element_text(angle=0, vjust=0.5, size=16)) + ggtitle("Histogram")

