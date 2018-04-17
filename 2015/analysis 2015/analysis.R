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
  #Create folder for plots
  dir.create("plots/mass-vs-count/scatter")
  dir.create("plots/mass-vs-count/residual")
  dir.create("plots/mass-vs-count/qqplot")
  
  name = "mass-vs-count.txt"
  capture.output(cat("Linear Regression by sample method and order\n\n"), file = name)
  
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
      temp.mass = raw.data[with(raw.data, year == 2015
                                    & method == method.type
                                    & order == name.order
                                    & mass > 0
                                    & family_count > 0), ]$mass
      temp.family = raw.data[with(raw.data, year == 2015
                                    & method == method.type
                                    & order == name.order
                                    & mass > 0
                                    & family_count > 0), ]$family_count
      temp.frame = data.frame(temp.family, temp.mass)
      lin.mod = lm(temp.frame)
      
      # Write linear model output to console and file
      cat(method.type, name.order)
      data.summary = summary(lin.mod)
      print(data.summary)

      capture.output(cat(method.type, name.order), file = name, append = TRUE)
      capture.output(data.summary, file = name, append = TRUE)
      
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
               paste("plots/mass-vs-count/scatter/2015",
                     method.type,
                     name.order,
                     "mass-vs-count.pdf",
                     sep = "_"),
               width = 3,
               height = 3
               )
      dev.off()
      
      # Plot the residuals vs. the fitted values
      plot(residuals(lin.mod) ~ fitted(lin.mod),
           data = temp.frame,
           xlab = "fitted family count",
           ylab = "residual family count",
           main = NULL
      )
      dev.copy(pdf,
               paste("plots/mass-vs-count/residual/2015",
                     method.type,
                     name.order,
                     "residual.pdf",
                     sep = "_"),
               width = 3,
               height = 3
      )
      dev.off()
      
      # Make a normal probability plot of the residuals
      qqnorm(lin.mod$residuals,
             ylab = "Residual Quantiles",
             main = NULL
      )
      qqline(lin.mod$residuals,
             probs = c(0.25, 0.75),
             col = 'blue'
      )
      
      dev.copy(pdf,
               paste("plots/mass-vs-count/qqplot/2015",
                     method.type,
                     name.order,
                     "qqplot.pdf",
                     sep = "_"),
               width = 3,
               height = 3
      )
      dev.off()
    }
  }
}

# Analysis 2 -------------------------------------------------------------------
#   linear mixed model: sample weight and diversity per order and sample method
Analysis2 = function(raw.data) {
  # Mixed linear model with mass as fixed effect and order and method as random
  mixed.lmer = lmer(family_count ~ mass + (1 | order) + (1 | method),
              data = raw.data[with(raw.data, mass > 0 & family_count > 0), ], family = poisson)
  
  print(summary(mixed.lmer))
  
  plot(fitted(mixed.lmer),
       residuals(mixed.lmer),
       xlab = "fitted family count",
       ylab = "residual family count"
  )
  
  qqnorm(residuals(mixed.lmer),
         ylab = "Residual Quantiles",
         main = NULL
  )
  qqline(residuals(mixed.lmer),
         probs = c(0.25, 0.75),
         col = 'blue'
  )
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
  library(lme4)
  
  #Create folder for plots
  dir.create("plots")
  
  #Minimal margins on graphs
  par(mar = c(4, 4, 1.1, 0.6) + 0.1)
  
  #Run analysis functions
  readline(prompt = "reading raw_data.txt: press enter to continue.")
  raw.data = GetData("raw_data.txt")
  
  readline(prompt = "first analysis: press enter to continue.")
  Analysis1(raw.data)
  
  readline(prompt = "second analysis: press enter to continue.")
  Analysis2(raw.data)
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