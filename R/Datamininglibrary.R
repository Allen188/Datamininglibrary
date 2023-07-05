# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#as.data.frame(lapply(datexpr,as.numeric))
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ge.convert.geneprot <- function(vector,type="uniprot"){
  library(org.Hs.eg.db)
  library(clusterProfiler)
  keytypes(org.Hs.eg.db)
  protID = bitr(vector, fromType="UNIPROT", toType=c("SYMBOL", "UNIPROT"),drop = F, OrgDb="org.Hs.eg.db")
  return(paste0(vector,"_",protID$SYMBOL[match(vector,protID$UNIPROT)]))
}

ge.convert.number <- function(data){
  data2 <- as.data.frame(lapply(data, as.numeric))
  row.names(data2) <- row.names(data)
  return(data2)
}

ge.summary.data <- function(data){
  a <- matrix(summary(data)) %>%as.data.frame()
  row.names(a) <- c("Min.","1st Qu.",  "Median",    "Mean", "3rd Qu.",    "Max.")
  names(a) <- "Summary"
  return(a)
}

ge.color <- function(n){
  if(n<10){
    set.seed(21)
    m <- sample(9,n)
    color <- brewer.pal(9,"Set1")[m]
  }else{
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    set.seed(10)
    color=sample(col_vector, n)
  }
  return(color)
}

