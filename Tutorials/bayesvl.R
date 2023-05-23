getwd()
setwd("/Users/youngbeomcho/Desktop/Young Beom Cho/UST/22-23 Spring/SOSC 4110")

packages <- c("coda","devtools","loo","ggplot2","vctrs", "bayesplot", "rlang")

# install any packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load required packages
invisible(lapply(packages, library, character.only = TRUE))
install.packages()

devtools::install_github("sshpa/bayesvl")
install.packages()

library(bayesvl)

data1 <- read.csv('Legends345.csv')
head(data1)


model <- bayesvl()
model <- bvl_addNode(model, "O", "binom")
model <- bvl_addNode(model, "Lie", "binom")
model <- bvl_addNode(model, "Viol", "binom")
model <- bvl_addNode(model, "VB", "binom")
model <- bvl_addNode(model, "VC", "binom")
model <- bvl_addNode(model, "VT", "binom")
model <- bvl_addNode(model, "Int1", "binom")
model <- bvl_addNode(model, "Int2", "binom")

model <- bvl_addNode(model, "B_and_Viol", "trans") 
model <- bvl_addNode(model, "C_and_Viol", "trans") 
model <- bvl_addNode(model, "T_and_Viol", "trans")

model <- bvl_addArc(model, "VB", "B_and_Viol", "*") 
model <- bvl_addArc(model, "Viol", "B_and_Viol", "*")
model <- bvl_addArc(model, "VC", "C_and_Viol", "*") 
model <- bvl_addArc(model, "Viol", "C_and_Viol", "*")
model <- bvl_addArc(model, "VT", "T_and_Viol", "*") 
model <- bvl_addArc(model, "Viol", "T_and_Viol", "*")

model <- bvl_addArc(model, "B_and_Viol", "O", "slope") 
model <- bvl_addArc(model, "C_and_Viol", "O", "slope") 
model <- bvl_addArc(model, "T_and_Viol", "O", "slope")

model <- bvl_addArc(model, "Viol", "O", "slope")

model <- bvl_addNode(model, "B_and_Lie", "trans") 
model <- bvl_addNode(model, "C_and_Lie", "trans") 
model <- bvl_addNode(model, "T_and_Lie", "trans")

model <- bvl_addArc(model, "VB", "B_and_Lie", "*") 
model <- bvl_addArc(model, "Viol", "B_and_Lie", "*")
model <- bvl_addArc(model, "VC", "C_and_Lie", "*") 
model <- bvl_addArc(model, "Viol", "C_and_Lie", "*")
model <- bvl_addArc(model, "VT", "T_and_Lie", "*") 
model <- bvl_addArc(model, "Viol", "T_and_Lie", "*")

model <- bvl_addArc(model, "B_and_Lie", "O", "slope") 
model <- bvl_addArc(model, "C_and_Lie", "O", "slope") 
model <- bvl_addArc(model, "T_and_Lie", "O", "slope")

model <- bvl_addArc(model, "Lie", "O", "slope")

model <- bvl_addNode(model, "Int1_or_Int2", "trans", fun = "({0} > 0 ? 1 : 0)", 
                     out_type = "int", lower = 0, test = c(0, 1))

model <- bvl_addArc(model, "Int1", "Int1_or_Int2", "+") 
model <- bvl_addArc(model, "Int2", "Int1_or_Int2", "+")
model <- bvl_addArc(model, "Int1_or_Int2", "O", "varint", 
                    priors = c("a0_ ~ normal(0,5)", "sigma_ ~ normal(0,5)"))



bvl_bnPlot(model)
bvl_formula(model, "B_and_Viol")
bvl_formula(model, "Int1_or_Int2")

summary(model)


model_string <- bvl_model2Stan(model) 
cat(model_string)

options(mc.cores = parallel::detectCores())

model <- bvl_modelFit(model, data1, warmup = 2000, iter = 10000, chains = 4)

summary(model)
bvl_plotTrace(model)

bvl_plotGelmans(model, NULL, 4, 3)
bvl_plotAcfs(model, NULL, 4, 3)

bvl_plotIntervals(model)
bvl_plotParams(model, 4, 3)
bvl_plotParams (model, row = 4, col = 3, credMass = 0.95, params = NULL)

bvl_plotIntervals(model, c("b_B_and_Lie_O", "b_C_and_Lie_O", "b_T_and_Lie_O", "b_Lie_O"))
bvl_plotDensity(model, c("b_B_and_Lie_O", "b_C_and_Lie_O", "b_T_and_Lie_O", "b_Lie_O"))


