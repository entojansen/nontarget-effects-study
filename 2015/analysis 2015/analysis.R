# Andrew Jansen
# created iv.15.2018

# Miscellaneous Functions ------------------------------------------------------
#Import the data
GetData = function(file.name) {
  test.data = data.frame(read.csv(file.name, sep = ';'))
  return(test.data)
}

#For ANOVA
SqDev = function(x) {var(x) * (length(x) - 1)}

# Analysis 1 -------------------------------------------------------------------
#   correspondence b/w sample weight and diversity, by sample method and order
Analysis1 = function(raw.data) {
  #Orders that don't break the script... droplevels not removing unused orders.
  orders = c("Coleoptera",
            "Diptera",
            "Hemiptera",
            "Hymenoptera",
            "Lepidoptera",
            "Orthoptera")
  for (method.type in levels(raw.data$method)) {
    for (name.order in orders) {
      # Regression via linear model
      temp.factor.x = raw.data[with(raw.data, year == 2015
                                    & method == method.type
                                    & order == name.order
                                    & mass > 0
                                    & family_count > 0), ]$mass
      temp.factor.y = raw.data[with(raw.data, year == 2015
                                    & method == method.type
                                    & order == name.order
                                    & mass > 0
                                    & family_count > 0), ]$family_count
      temp.frame = data.frame(temp.factor.y, temp.factor.x)
      lin.mod = lm(temp.frame)
      print(summary(lin.mod))
      # Make scatter plots for each order
      plot(family_count ~ mass,
           data = raw.data[with(raw.data, year == 2015
                                & method == method.type
                                & order == name.order
                                & mass > 0
                                & family_count > 0), ],
           xlab = "sample mass (g)",
           ylab = "number of families",
           main = paste(name.order, " (", method.type, ")", sep = "")
           )
      abline(lin.mod, col = "blue")
      dev.copy(pdf,
               paste("plots/2015",
                     method.type,
                     name.order,
                     "mass-vs-count.pdf",
                     sep = "_"),
               width = 3,
               height = 3
               )
      dev.off()
    }
  }
}

# Analysis 2 -------------------------------------------------------------------
#   correspondence b/w sample weight and diversity per order, by sample method
Analysis2 = function(raw.data) {
  
}

# Analysis 3 -------------------------------------------------------------------
#   comparisons between blocks for bulk diversity and sample weight
#   all comparisons (here and below) by sample method and in bulk
Analysis3 = function(raw.data) {
  
}

# Analysis 4 -------------------------------------------------------------------
#   comparisons between blocks for diversity and weight of individual orders
Analysis4 = function(raw.data) {
  
}

# Analysis 5 -------------------------------------------------------------------
#   comparisons between blocks for bulk diversity of pollinator families
Analysis5 = function(raw.data) {
  
}

# Analysis 6 -------------------------------------------------------------------
#   comparisons between blocks for diversity of pollinator families, per order
Analysis6 = function(raw.data) {
  
}

# Main Function ----------------------------------------------------------------
main = function() {
  #Clear workspace
  rm(list = ls())
  
  #Load gplots for plotCI
  library(gplots)
  library(car)
  
  #Create folder for plots
  dir.create("plots")
  
  #Minimal margins on graphs
  par(mar = c(4, 4, 1.1, 0.6) + 0.1)
  
  #Run analysis functions
  readline(prompt = "reading raw_data.txt: press enter to continue.")
  raw.data = GetData("raw_data.txt")
  
  readline(prompt = "first analysis: press enter to continue.")
  Analysis1(raw.data)
  # readline(prompt = "second analysis: press enter to continue.")
  # Analysis2(raw.data)
  # 
  # readline(prompt = "third analysis: press enter to continue.")
  # Analysis3(raw.data)
  # 
  # readline(prompt = "fourth analysis: press enter to continue.")
  # Analysis4(raw.data)
  # 
  # readline(prompt = "fifth analysis: press enter to continue.")
  # Analysis5(raw.data)
  # 
  # readline(prompt = "sixth analysis: press enter to continue.")
  # Analysis6(raw.data)
}

# Execute Code -----------------------------------------------------------------
main()