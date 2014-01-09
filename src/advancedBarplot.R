GenerateDummyData <- function (type = "I"){
	Conditions <- rep(paste("Cond", 1:6, sep=""), each = 3)
	Replicates <- rep(paste("R", 1:3, sep=""), 6)
	Number1 <- round(rnorm(n = 18, mean = 10) * 100)
	Number2 <- round(rnorm(n = 18, mean = 5, sd = 10) * 100)
	Number2[1] <- NA
	Number1[Number1 < 0 ] <- NA
	Number2[Number2 < 0 ] <- NA
	Type <- rep(type, 18)
	data <- data.frame(Conditions, Replicates, Number1, Number2 , Type)
	return (data)
}
data1 <- GenerateDummyData(type = "I")
data2 <- GenerateDummyData(type = "II")
data3 <- GenerateDummyData(type = "III")
data4 <- GenerateDummyData(type = "IV")

library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(ggplot2)
library(plyr)
myColor <- rev(brewer.pal(3, "Pastel1"))
myPairColor <- brewer.pal(12, "Paired")[c(1,2,9,10)]
myPairColor1 <- myPairColor[1:2]
myPairColor2 <- myPairColor[3:4]
options(scipen=8)

PlotReadsStackTwoLayersAmount <- function (amountData, mainTitle = "Data", myPalette = myPairColor){
    meltAmountData <- melt(data = amountData, 
                        id.vars = c("Conditions", "Replicates", "Type"))
    meltAmountData$value <- as.numeric(meltAmountData$value)
    stackAmountData <- ddply(amountData, c("Conditions", "Replicates"), summarise, 
                             Number1 = sum(Number1, na.rm = TRUE), 
                             Number2 = sum(Number2, na.rm = TRUE))
    meltStackAmountData <- melt(data = stackAmountData, 
                                id.vars = c("Conditions", "Replicates"))

    p_amount <- ggplot(meltStackAmountData, na.rm = FALSE, 
                    aes(x = variable, 
                        y = ifelse(value %in% NA, 0, value ), 
                        ymax = max(as.numeric(na.omit(value))) * 1.2
                    )
                ) + 
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + 
        facet_grid(Conditions ~ Replicates) + 
        geom_text(aes(x = variable, 
                label = ifelse(value %in% NA , 
                                "N/A", 
                                format(value, big.mark = ",", scientific = F)
                                ), 
                    cex = 1 ), 
                position = position_dodge(width = 0.9), 
                show_guide = FALSE, 
                vjust = -0.25) + 
        theme_bw() + 
        geom_bar(data = meltAmountData, 
                stat = "identity", 
                position = position_stack(width = 0.9), 
                aes(x = variable, 
                    y = ifelse(value %in% NA, 0, value), 
                    #ymax = max(as.numeric(na.omit(value)) / (10 ^ 6)) * 1.2, 
                    fill = factor(Type)
                    )
                ) +
        scale_fill_manual(values = myPalette, guide = guide_legend(title = NULL)) +
        theme(panel.grid.major.x = element_blank()) +  
        labs(title = mainTitle, y = "Amount", x = "") 
        
    return (p_amount)
}

PlotReadsAmount <- function (amountData, mainTitle = "Data", myPalette = myColor){
    meltAmountData <- melt(data = amountData[,-5], 
                        id.vars = c("Conditions", "Replicates"))
    meltAmountData$value <- as.numeric(meltAmountData$value)
    p_amount <- ggplot(meltAmountData, na.rm = FALSE, 
                    aes(x = variable, 
                        y = ifelse(value %in% NA, 0, value ), 
                        ymax = max(as.numeric(na.omit(value))) * 1.2, 
                        fill = factor(variable)
                    )
                ) + 
        geom_bar(stat = "identity", position = position_dodge(width = 0.9))

    p_amount <- p_amount + facet_grid(Conditions ~ Replicates) + 
        geom_text(aes(x = variable, 
                label = ifelse(value %in% NA , 
                            "N/A", 
                            format(value, big.mark = ",", scientific = F)
                            ), 
                cex = 1 ), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25) + 
        theme_bw() + scale_fill_manual(values = myPalette) +
        theme(legend.position = "none", panel.grid.major.x = element_blank()) +  
        labs(title = mainTitle, y = "Amount", x = "")
     
    return (p_amount)
}


