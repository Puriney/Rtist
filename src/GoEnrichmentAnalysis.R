#-----------
# GO Enrichment Analysis
library("org.At.tair.db")
library("GO.db")
library("GOstats")
library("ggplot2")
# Full universal gene list as background
universe <- mappedkeys(org.At.tairENTREZID)
exonOfInterest <- sample(universe, size = 300, replace = F)
params <- new('GOHyperGParams', 
                geneIds = exonOfInterest, universeGeneIds = universe, 
                ontology = 'BP', pvalueCutoff = 0.01, 
                conditional = F, 
                testDirection = 'over', 
                annotation = "org.At.tair.db")
exonOfInterest_hgobj <- hyperGTest(params)

plotExonPostTs <- plotGO(exonOfInterest_hgobj, 20)
  
#-----------
# Customed visualization function of GO Enrichemnt results
plotGO <- function(GOobj, GoTermAmount = 10, sortMethod = "pvalue"){
  GOresult <- summary(GOobj)
  n <- length(unique(GOobj@geneIds))
  if (sortMethod == "count"){
    GOresult <- GOresult[order(GOresult$Count, -GOresult$Pvalue, decreasing = TRUE), ]
    titleOpt <- ggtitle(paste0("Top ", GoTermAmount, " GO clusters with genes covered"))
  } else {
    GOresult <- GOresult[order(GOresult$Pvalue, -GOresult$Count), ]
    titleOpt <- ggtitle(paste0("Top ", GoTermAmount, " significant GO clusters"))
  }
  GOresult <- GOresult[1:GoTermAmount, ]
  p <- ggplot(data = GOresult, 
         aes(x = Term, y = Count, fill = Pvalue)) + 
      geom_bar(stat = "identity") + 
      scale_fill_continuous(low = "red", high="blue") +
      scale_x_discrete(limits = rev(GOresult$Term)) + 
      theme_bw() +  theme(text = element_text(size = 20)) + 
      coord_flip() + 
      xlab("") + ylab(paste0("Count (", n, " genes of interest)")) + titleOpt
      
  return(p)
}