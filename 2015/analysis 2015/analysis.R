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
  capture.output(cat("Linear Regression by sample method and order\n\n"),
                 file = name)
  
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
  dir.create("plots/glmm")
  name = "general_linear_mixed_model.txt"
  capture.output(cat("General Linear Mixed Model, negative binomial\n\n"),
                 file = name)
  
  # Mixed linear model with mass as fixed effect and order and method as random
  mixed.lmer.poisson = glmer(family_count ~ mass + (1 | order) + (1 | method),
                        data = raw.data[with(raw.data, mass > 0 
                                             & family_count > 0), ],
                        family = poisson)
  
  print(summary(mixed.lmer.poisson))
  
  mixed.lmer.nb = glmer.nb(family_count ~ mass + (1 | order) + (1 | method),
                        data = raw.data[with(raw.data, mass > 0 
                                             & family_count > 0), ],
                        family = negative.binomial)
  
  print(summary(mixed.lmer.nb))
  
  print(lrtest(mixed.lmer.poisson, mixed.lmer.nb))
  
  mixed.lmer.nomass = glmer.nb(family_count ~ (1 | order) + (1 | method),
       data = raw.data[with(raw.data, mass > 0 
                            & family_count > 0), ],
       family = negative.binomial)
  
  mixed.lmer.noorder = glmer.nb(family_count ~ mass + (1 | method),
       data = raw.data[with(raw.data, mass > 0 
                            & family_count > 0), ],
       family = negative.binomial)
  
  mixed.lmer.nomethod = glmer.nb(family_count ~ mass + (1 | order),
                             data = raw.data[with(raw.data, mass > 0 
                                                  & family_count > 0), ],
                             family = negative.binomial)
  
  print(lrtest(mixed.lmer.nb,
               mixed.lmer.nomass,
               mixed.lmer.noorder,
               mixed.lmer.nomethod))
  
  capture.output(summary(mixed.lmer.poisson), file = name, append = TRUE)
  capture.output(summary(mixed.lmer.nb), file = name, append = TRUE)
  capture.output(lrtest(mixed.lmer.poisson, mixed.lmer.nb),
                 file = name, append = TRUE)
  capture.output(lrtest(mixed.lmer.nb,
                        mixed.lmer.nomass,
                        mixed.lmer.noorder,
                        mixed.lmer.nomethod), 
                 file = name, append = TRUE)
  
  plot(fitted(mixed.lmer.nb),
       residuals(mixed.lmer.nb),
       xlab = "fitted family count",
       ylab = "residual family count"
  )
  dev.copy(pdf,
           paste("plots/glmm/2015_glmm_residual.pdf",
                 sep = "_"),
           width = 3,
           height = 3
  )
  dev.off()
  
  qqnorm(residuals(mixed.lmer.nb),
         ylab = "Residual Quantiles",
         main = NULL
  )
  qqline(residuals(mixed.lmer.nb),
         probs = c(0.25, 0.75),
         col = 'blue'
  )
  dev.copy(pdf,
           paste("plots/glmm/2015_glmm_qqplot.pdf",
                 sep = "_"),
           width = 3,
           height = 3
  )
  dev.off()
}

# Analysis 3 -------------------------------------------------------------------
#   comparisons between blocks for diversity and weight of individual orders
Analysis3 = function(raw.data) {
  dir.create("plots/blocks")
  dir.create("plots/blocks/tukeyHSD")
  dir.create("plots/blocks/tukeyHSD/mass")
  dir.create("plots/blocks/tukeyHSD/family")
  dir.create("plots/blocks/interaction")
  dir.create("plots/blocks/interaction/mass")
  dir.create("plots/blocks/interaction/family")
  
  name = "blocks.txt"
  capture.output(cat("Analyses of Variance (order, method) \n\n"), file = name)
  
  #Orders that don't break the script... droplevels not removing unused orders.
  orders = c("Coleoptera",
             "Diptera",
             "Hemiptera",
             "Hymenoptera",
             "Lepidoptera",
             "Orthoptera",
             "Neuroptera")
  
  raw.data.2015 = raw.data[raw.data$year == 2015, ]
  
  for (method.type in levels(raw.data.2015$method)) {
    for (name.order in orders) {
      capture.output(cat(name.order, method.type, "\n\n"),
                     file = name, append = TRUE)
      temp.data = raw.data.2015[with(raw.data.2015,
                                method == method.type 
                                & order == name.order), ]
      temp.data$event = factor(temp.data$event)
      mass.model = lm(mass ~ block * event, temp.data)
      family.model = lm(family_count ~ block * event, temp.data)
      mass.aov = aov(mass.model)
      family.aov = aov(family.model)
      
      capture.output(cat("sample mass", "\n\n"), file = name, append = TRUE)
      capture.output(summary(mass.aov), file = name, append = TRUE)
      capture.output(leveneTest(mass.model), file = name, append = TRUE)
      capture.output(shapiro.test(residuals(mass.model)),
                     file = name, append = TRUE)
      capture.output(TukeyHSD(mass.aov), file = name, append = TRUE)
      
      par(mar = c(4, 4, 2, 0.6) + 0.1)
      plot(TukeyHSD(mass.aov))
      dev.copy(pdf,
               paste("plots/blocks/tukeyHSD/mass/mass",
                     method.type,
                     name.order,
                     "anova.pdf",
                     sep = "_"),
               width = 7.5,
               height = 200
      )
      dev.off()
      par(mar = c(4, 4, 1.1, 0.6) + 0.1)
      
      mass.anova = anova(mass.model)
      MS.res = mass.anova$Mean[4]
      nu = mass.anova$Df[4]
      means = tapply(temp.data$mass,
                     list(temp.data$block, temp.data$event),
                     mean)
      alpha = 0.05
      n = tapply(temp.data$mass,
                 list(temp.data$block, temp.data$event),
                 length)
      se = sqrt(MS.res / n)
      t.critical = qt(1 - alpha / 2, nu)
      inter = t.critical * se
      
      plotCI(x = 1:5,
             y = means[1,],
             uiw = inter[1,],
             type = "b",
             xaxt = "n",
             col = "red",
             xlab = "Sample Week",
             ylab = "Mean Order Mass",
             ylim = c(-1, 8),
             xlim = c(1, 6),
             main = paste(name.order, " (", method.type, ")", sep = "")
      )
      plotCI(x = 1:5 + 0.05,
             y = means[2,],
             uiw = inter[2,],
             type = "b",
             xaxt = "n",
             col = "blue",
             add = TRUE
      )
      plotCI(x = 1:5 + 0.1,
             y = means[3,],
             uiw = inter[3,],
             type = "b",
             xaxt = "n",
             col = "green",
             add = TRUE
      )
      plotCI(x = 1:5 + 0.15,
             y = means[4,],
             uiw = inter[4,],
             type = "b",
             xaxt = "n",
             col = "black",
             add = TRUE
      )
      axis(1, at = 1:5, labels = c(1, 2, 3, 4, 5))
      legend(x = 5.25,
             y = 0,
             yjust = 0,
             lty = 1,
             col = c("red", "blue", "green", "black"),
             legend = c("1---", "2---", "3---", "UTC       "),
             bty = "o"
      )
      dev.copy(pdf,
               paste("plots/blocks/interaction/mass/mass",
                     method.type,
                     name.order,
                     "interplot.pdf",
                     sep = "_"),
               width = 6,
               height = 3
      )
      dev.off()
      
      capture.output(cat("sample families", "\n\n"), file = name, append = TRUE)
      capture.output(summary(family.aov), file = name, append = TRUE)
      capture.output(leveneTest(family.model), file = name, append = TRUE)
      capture.output(shapiro.test(residuals(family.model)),
                     file = name, append = TRUE)
      capture.output(TukeyHSD(family.aov), file = name, append = TRUE)
      
      par(mar = c(4, 4, 2, 0.6) + 0.1)
      plot(TukeyHSD(family.aov))
      dev.copy(pdf,
               paste("plots/blocks/tukeyHSD/family/family",
                     method.type,
                     name.order,
                     "anova.pdf",
                     sep = "_"),
               width = 7.5,
               height = 200
      )
      dev.off()
      par(mar = c(4, 4, 1.1, 0.6) + 0.1)
      
      family.anova = anova(family.model)
      MS.res = family.anova$Mean[4]
      nu = family.anova$Df[4]
      means = tapply(temp.data$family_count,
                     list(temp.data$block, temp.data$event),
                     mean)
      alpha = 0.05
      n = tapply(temp.data$family_count,
                 list(temp.data$block, temp.data$event),
                 length)
      se = sqrt(MS.res / n)
      t.critical = qt(1 - alpha / 2, nu)
      inter = t.critical * se
      
      plotCI(x = 1:5,
             y = means[1,],
             uiw = inter[1,],
             type = "b",
             xaxt = "n",
             col = "red",
             xlab = "Sample Week",
             ylab = "Mean Family Count",
             ylim = c(-1, 25),
             xlim = c(1, 6),
             main = paste(name.order, " (", method.type, ")", sep = "")
      )
      plotCI(x = 1:5 + 0.05,
             y = means[2,],
             uiw = inter[2,],
             type = "b",
             xaxt = "n",
             col = "blue",
             add = TRUE
      )
      plotCI(x = 1:5 + 0.1,
             y = means[3,],
             uiw = inter[3,],
             type = "b",
             xaxt = "n",
             col = "green",
             add = TRUE
      )
      plotCI(x = 1:5 + 0.15,
             y = means[4,],
             uiw = inter[4,],
             type = "b",
             xaxt = "n",
             col = "black",
             add = TRUE
      )
      axis(1, at = 1:5, labels = c(1, 2, 3, 4, 5))
      legend(x = 5.25,
             y = 0,
             yjust = 0,
             lty = 1,
             col = c("red", "blue", "green", "black"),
             legend = c("1---", "2---", "3---", "UTC       "),
             bty = "o"
      )
      dev.copy(pdf,
               paste("plots/blocks/interaction/family/family",
                     method.type,
                     name.order,
                     "interplot.pdf",
                     sep = "_"),
               width = 6,
               height = 3
      )
      dev.off()
    }
  }
}

# Analysis 4 -------------------------------------------------------------------
#   comparisons between blocks for bulk diversity and sample weight
#   all comparisons (here and below) by sample method and in bulk
Analysis4 = function(raw.data) {
  dir.create("plots/bulk")
  
  name = "bulk.txt"
  capture.output(cat("Analyses of Variance (bulk) \n\n"), file = name)
  
  raw.data.2015 = raw.data[raw.data$year == 2015, ]
  raw.data.2015$event = factor(raw.data.2015$event)
  
  bulk.data = aggregate(formula = (cbind(mass, family_count) 
                           ~ site + event + block + method),
                data = raw.data.2015,
                FUN = sum
  )
  
  bulk.lm = lm(family_count ~ mass,
               data = bulk.data[with(bulk.data, mass > 0 
                                     & family_count > 0), ])
  
  plot(family_count ~ mass,
       data = bulk.data[with(bulk.data, mass > 0 & family_count > 0), ],
       xlab = "sample mass (g)",
       ylab = "number of families",
       main = "Bulk (by site and event)"
  )
  abline(bulk.lm, col = "blue")
  dev.copy(pdf, "plots/bulk/2015_bulk_scatter.pdf", width = 3, height = 3)
  dev.off()
  
  # bulk by method
  for (method.type in levels(raw.data$method)) {
    # Regression via linear model
    lin.mod = lm(family_count ~ mass,
                 data = bulk.data[with(bulk.data, mass > 0 
                                       & family_count > 0
                                       & method == method.type), ])
    
    plot(family_count ~ mass,
         data = bulk.data[with(bulk.data, mass > 0 
                               & family_count > 0
                               & method == method.type), ],
         xlab = "sample mass (g)",
         ylab = "number of families",
         main = paste("Bulk (", method.type, ")", sep = "" )
    )
    
    capture.output(cat(method.type, "bulk"), file = name, append = TRUE)
    capture.output(summary(bulk.lm), file = name, append = TRUE)
    
    #scatterplot for each method
    abline(lin.mod, col = "blue")
    dev.copy(pdf,
             paste("plots/bulk/2015_bulk",
                   method.type,
                   "scatter.pdf",
                   sep = "_"),
             width = 3,
             height = 3
    )
    dev.off()
    
    # Plot the residuals vs. the fitted values
    plot(residuals(lin.mod) ~ fitted(lin.mod),
         data = bulk.data,
         xlab = "fitted family count",
         ylab = "residual family count",
         main = NULL
    )
    dev.copy(pdf,
             paste("plots/bulk/2015_bulk",
                   method.type,
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
             paste("plots/bulk/2015_bulk",
                   method.type,
                   "qqplot.pdf",
                   sep = "_"),
             width = 3,
             height = 3
    )
    dev.off()
  }
  
  mixed.lmer.poisson = glmer(family_count ~ mass + (1 | method),
                             data = bulk.data[with(bulk.data, mass > 0 
                                                  & family_count > 0), ],
                             family = poisson)
  
  mixed.lmer.nb = glmer.nb(family_count ~ mass + (1 | method),
                           data = bulk.data[with(bulk.data, mass > 0 
                                                 & family_count > 0), ],
                           family = negative.binomial)
  
  print(summary(bulk.lm))
  print(summary(mixed.lmer.poisson))
  print(summary(mixed.lmer.nb))
  print(lrtest(mixed.lmer.poisson, mixed.lmer.nb))
  
  capture.output(summary(bulk.lm), file = name, append = TRUE)
  capture.output(summary(mixed.lmer.poisson), file = name, append = TRUE)
  capture.output(summary(mixed.lmer.nb), file = name, append = TRUE)
  capture.output(lrtest(mixed.lmer.poisson, mixed.lmer.nb),
                 file = name, append = TRUE)
  
  plot(fitted(mixed.lmer.nb),
       residuals(mixed.lmer.nb),
       xlab = "fitted family count",
       ylab = "residual family count"
  )
  dev.copy(pdf,
           paste("plots/bulk/2015_bulk_residual.pdf",
                 sep = "_"),
           width = 3,
           height = 3
  )
  dev.off()
  
  qqnorm(residuals(mixed.lmer.nb),
         ylab = "Residual Quantiles",
         main = NULL
  )
  qqline(residuals(mixed.lmer.nb),
         probs = c(0.25, 0.75),
         col = 'blue'
  )
  dev.copy(pdf,
           paste("plots/bulk/2015_bulk_qqplot.pdf",
                 sep = "_"),
           width = 3,
           height = 3
  )
  dev.off()
  
  capture.output(summary(mixed.lmer.poisson), file = name, append = TRUE)
  capture.output(summary(mixed.lmer.nb), file = name, append = TRUE)
  capture.output(lrtest(mixed.lmer.poisson, mixed.lmer.nb),
                 file = name, append = TRUE)
  
  for (method.type in levels(raw.data$method)) {
    capture.output(cat(method.type, "\n\n"),
                   file = name, append = TRUE)
    temp.data = bulk.data[with(bulk.data, method == method.type), ]
    mass.model = lm(mass ~ block * event, temp.data)
    family.model = lm(family_count ~ block * event, temp.data)
    mass.aov = aov(mass.model)
    family.aov = aov(family.model)
    
    capture.output(cat("sample mass", "\n\n"), file = name, append = TRUE)
    capture.output(summary(mass.aov), file = name, append = TRUE)
    capture.output(leveneTest(mass.model), file = name, append = TRUE)
    capture.output(shapiro.test(residuals(mass.model)),
                   file = name, append = TRUE)
    capture.output(TukeyHSD(mass.aov), file = name, append = TRUE)
    
    par(mar = c(4, 4, 2, 0.6) + 0.1)
    plot(TukeyHSD(mass.aov))
    dev.copy(pdf,
             paste("plots/bulk/2015_mass",
                   method.type,
                   "anova.pdf",
                   sep = "_"),
             width = 7.5,
             height = 200
    )
    dev.off()
    par(mar = c(4, 4, 1.1, 0.6) + 0.1)
    
    mass.anova = anova(mass.model)
    MS.res = mass.anova$Mean[4]
    nu = mass.anova$Df[4]
    means = tapply(temp.data$mass,
                   list(temp.data$block, temp.data$event),
                   mean)
    alpha = 0.05
    n = tapply(temp.data$mass,
               list(temp.data$block, temp.data$event),
               length)
    se = sqrt(MS.res / n)
    t.critical = qt(1 - alpha / 2, nu)
    inter = t.critical * se
    
    plotCI(x = 1:5,
           y = means[1,],
           uiw = inter[1,],
           type = "b",
           xaxt = "n",
           col = "red",
           xlab = "Sample Week",
           ylab = "Mean Bulk Mass",
           ylim = c(-1, 8),
           xlim = c(1, 6),
           main = paste("Bulk (", method.type, ")", sep = "")
    )
    plotCI(x = 1:5 + 0.05,
           y = means[2,],
           uiw = inter[2,],
           type = "b",
           xaxt = "n",
           col = "blue",
           add = TRUE
    )
    plotCI(x = 1:5 + 0.1,
           y = means[3,],
           uiw = inter[3,],
           type = "b",
           xaxt = "n",
           col = "green",
           add = TRUE
    )
    plotCI(x = 1:5 + 0.15,
           y = means[4,],
           uiw = inter[4,],
           type = "b",
           xaxt = "n",
           col = "black",
           add = TRUE
    )
    axis(1, at = 1:5, labels = c(1, 2, 3, 4, 5))
    legend(x = 5.25,
           y = 0,
           yjust = 0,
           lty = 1,
           col = c("red", "blue", "green", "black"),
           legend = c("1---", "2---", "3---", "UTC       "),
           bty = "o"
    )
    dev.copy(pdf,
             paste("plots/bulk/2015_mass",
                   method.type,
                   "interplot.pdf",
                   sep = "_"),
             width = 6,
             height = 3
    )
    dev.off()
    
    capture.output(cat("sample families", "\n\n"), file = name, append = TRUE)
    capture.output(summary(family.aov), file = name, append = TRUE)
    capture.output(leveneTest(family.model), file = name, append = TRUE)
    capture.output(shapiro.test(residuals(family.model)),
                   file = name, append = TRUE)
    capture.output(TukeyHSD(family.aov), file = name, append = TRUE)
    
    par(mar = c(4, 4, 2, 0.6) + 0.1)
    plot(TukeyHSD(family.aov))
    dev.copy(pdf,
             paste("plots/bulk/2015_family",
                   method.type,
                   "anova.pdf",
                   sep = "_"),
             width = 7.5,
             height = 200
    )
    dev.off()
    par(mar = c(4, 4, 1.1, 0.6) + 0.1)
    
    family.anova = anova(family.model)
    MS.res = family.anova$Mean[4]
    nu = family.anova$Df[4]
    means = tapply(temp.data$family_count,
                   list(temp.data$block, temp.data$event),
                   mean)
    alpha = 0.05
    n = tapply(temp.data$family_count,
               list(temp.data$block, temp.data$event),
               length)
    se = sqrt(MS.res / n)
    t.critical = qt(1 - alpha / 2, nu)
    inter = t.critical * se
    
    plotCI(x = 1:5,
           y = means[1,],
           uiw = inter[1,],
           type = "b",
           xaxt = "n",
           col = "red",
           xlab = "Sample Week",
           ylab = "Mean Family Count",
           ylim = c(-1, 50),
           xlim = c(1, 6),
           main = paste("Bulk (", method.type, ")", sep = "")
    )
    plotCI(x = 1:5 + 0.05,
           y = means[2,],
           uiw = inter[2,],
           type = "b",
           xaxt = "n",
           col = "blue",
           add = TRUE
    )
    plotCI(x = 1:5 + 0.1,
           y = means[3,],
           uiw = inter[3,],
           type = "b",
           xaxt = "n",
           col = "green",
           add = TRUE
    )
    plotCI(x = 1:5 + 0.15,
           y = means[4,],
           uiw = inter[4,],
           type = "b",
           xaxt = "n",
           col = "black",
           add = TRUE
    )
    axis(1, at = 1:5, labels = c(1, 2, 3, 4, 5))
    legend(x = 5.25,
           y = 0,
           yjust = 0,
           lty = 1,
           col = c("red", "blue", "green", "black"),
           legend = c("1---", "2---", "3---", "UTC       "),
           bty = "o"
    )
    dev.copy(pdf,
             paste("plots/bulk/2015_family",
                   method.type,
                   "interplot.pdf",
                   sep = "_"),
             width = 6,
             height = 3
    )
    dev.off()
  }
  Y <- cbind(bulk.data$mass, bulk.data$family_count)
  fit <- manova(Y ~ bulk.data$block * bulk.data$event * bulk.data$method)
  print(summary(fit, test = "Pillai"))
  capture.output(summary(fit, test = "Pillai"), file = name, append = TRUE)
  print(summary.aov(fit, test = "Pillai"))
  
  # Mix methods and orders together for complete picture of plot
  super.bulk.data = aggregate(formula = (cbind(mass, family_count)
                                   ~ site + event + block),
                        data = raw.data.2015,
                        FUN = sum
  )
  Y <- cbind(super.bulk.data$mass, super.bulk.data$family_count)
  fit <- manova(Y ~ super.bulk.data$block * super.bulk.data$event)
  print(summary(fit, test = "Pillai"))
  capture.output(summary(fit, test = "Pillai"), file = name, append = TRUE)
  print(summary.aov(fit, test = "Pillai"))
  
  capture.output(cat(method.type, "\n\n"),
                 file = name, append = TRUE)
  temp.data = super.bulk.data
  mass.model = lm(mass ~ block * event, temp.data)
  family.model = lm(family_count ~ block * event, temp.data)
  mass.aov = aov(mass.model)
  family.aov = aov(family.model)
  
  capture.output(cat("bulk sample mass", "\n\n"), file = name, append = TRUE)
  capture.output(summary(mass.aov), file = name, append = TRUE)
  capture.output(leveneTest(mass.model), file = name, append = TRUE)
  capture.output(shapiro.test(residuals(mass.model)),
                 file = name, append = TRUE)
  capture.output(TukeyHSD(mass.aov), file = name, append = TRUE)
  
  par(mar = c(4, 4, 2, 0.6) + 0.1)
  plot(TukeyHSD(mass.aov))
  dev.copy(pdf,"plots/bulk/2015_bulk_mass_anova.pdf", width = 7.5, height = 200)
  dev.off()
  par(mar = c(4, 4, 1.1, 0.6) + 0.1)
  
  mass.anova = anova(mass.model)
  MS.res = mass.anova$Mean[4]
  nu = mass.anova$Df[4]
  means = tapply(temp.data$mass,
                 list(temp.data$block, temp.data$event),
                 mean)
  alpha = 0.05
  n = tapply(temp.data$mass,
             list(temp.data$block, temp.data$event),
             length)
  se = sqrt(MS.res / n)
  t.critical = qt(1 - alpha / 2, nu)
  inter = t.critical * se
  
  plotCI(x = 1:5,
         y = means[1,],
         uiw = inter[1,],
         type = "b",
         xaxt = "n",
         col = "red",
         xlab = "Sample Week",
         ylab = "Mean Bulk Mass",
         ylim = c(-1, 15),
         xlim = c(1, 6),
         main = "Bulk sample mass"
  )
  plotCI(x = 1:5 + 0.05,
         y = means[2,],
         uiw = inter[2,],
         type = "b",
         xaxt = "n",
         col = "blue",
         add = TRUE
  )
  plotCI(x = 1:5 + 0.1,
         y = means[3,],
         uiw = inter[3,],
         type = "b",
         xaxt = "n",
         col = "green",
         add = TRUE
  )
  plotCI(x = 1:5 + 0.15,
         y = means[4,],
         uiw = inter[4,],
         type = "b",
         xaxt = "n",
         col = "black",
         add = TRUE
  )
  axis(1, at = 1:5, labels = c(1, 2, 3, 4, 5))
  legend(x = 5.25,
         y = 0,
         yjust = 0,
         lty = 1,
         col = c("red", "blue", "green", "black"),
         legend = c("1---", "2---", "3---", "UTC       "),
         bty = "o"
  )
  dev.copy(pdf,
           "plots/bulk/2015_bulk_mass_interplot.pdf",
           width = 6,
           height = 3
  )
  dev.off()
  
  capture.output(cat("sample families", "\n\n"), file = name, append = TRUE)
  capture.output(summary(family.aov), file = name, append = TRUE)
  capture.output(leveneTest(family.model), file = name, append = TRUE)
  capture.output(shapiro.test(residuals(family.model)),
                 file = name, append = TRUE)
  capture.output(TukeyHSD(family.aov), file = name, append = TRUE)
  
  par(mar = c(4, 4, 2, 0.6) + 0.1)
  plot(TukeyHSD(family.aov))
  dev.copy(pdf,
           "plots/bulk/2015_bulk_family_anova.pdf",
           width = 7.5,
           height = 200
  )
  dev.off()
  par(mar = c(4, 4, 1.1, 0.6) + 0.1)
  
  family.anova = anova(family.model)
  MS.res = family.anova$Mean[4]
  nu = family.anova$Df[4]
  means = tapply(temp.data$family_count,
                 list(temp.data$block, temp.data$event),
                 mean)
  alpha = 0.05
  n = tapply(temp.data$family_count,
             list(temp.data$block, temp.data$event),
             length)
  se = sqrt(MS.res / n)
  t.critical = qt(1 - alpha / 2, nu)
  inter = t.critical * se
  
  plotCI(x = 1:5,
         y = means[1,],
         uiw = inter[1,],
         type = "b",
         xaxt = "n",
         col = "red",
         xlab = "Sample Week",
         ylab = "Mean Family Count",
         ylim = c(-1, 90),
         xlim = c(1, 6),
         main = "Bulk family count"
  )
  plotCI(x = 1:5 + 0.05,
         y = means[2,],
         uiw = inter[2,],
         type = "b",
         xaxt = "n",
         col = "blue",
         add = TRUE
  )
  plotCI(x = 1:5 + 0.1,
         y = means[3,],
         uiw = inter[3,],
         type = "b",
         xaxt = "n",
         col = "green",
         add = TRUE
  )
  plotCI(x = 1:5 + 0.15,
         y = means[4,],
         uiw = inter[4,],
         type = "b",
         xaxt = "n",
         col = "black",
         add = TRUE
  )
  axis(1, at = 1:5, labels = c(1, 2, 3, 4, 5))
  legend(x = 5.25,
         y = 0,
         yjust = 0,
         lty = 1,
         col = c("red", "blue", "green", "black"),
         legend = c("1---", "2---", "3---", "UTC       "),
         bty = "o"
  )
  dev.copy(pdf,
           "plots/bulk/2015_Bulk_family_interplot.pdf",
           width = 6,
           height = 3
  )
  dev.off()
}

# Main Function ----------------------------------------------------------------
main = function() {
  #Clear workspace
  rm(list = ls())
  
  #Load gplots for plotCI
  library(gplots)
  library(car)
  library(lme4)
  library(lmtest)
  
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
  
  readline(prompt = "third analysis: press enter to continue.")
  Analysis3(raw.data)
  
  readline(prompt = "fourth analysis: press enter to continue.")
  Analysis4(raw.data)
}

# Execute Code -----------------------------------------------------------------
main()