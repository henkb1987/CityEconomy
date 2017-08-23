# source functions
source("MarketFunctions.R")
source("CompanyFunctions.R")
source("SimulationFunctions.R")
source("CompanyStrategyFunctions.R")
source("GovernmentFunctions.R")
source("PopulationFunctions.R")

n.iter <- 60
price.history <- matrix(NA, 26, n.iter)

# init world
source("InitWorld.R")
# run world
world <- CreateCompany(world, 26 * 5)
for(time in 1:n.iter){
  message("===== ",time," =====")
  world <- DeterminePrices(world)
  for(i in 1:nrow(world$companies)){world <- RunCompany(world, i)}
  world <- CollectInterest(world)
  world <- ConsumeAllResources(world)
  world <- MoveResourceQueues(world)
  world <- IncreaseLotAges(world)
  price.history[, time] <- world$resource.information$price
  cat("State Cash:",round(world$government$state.funds),"Pop size:",round(world$population$size)," lots on market:",nrow(world$market),"\n")
  par(mfcol=c(1,2))
  #image(price.history)
  PlotPriceHistory(world, price.history, time, n.iter)
  plot(world$companies$money - world$companies$loan.remaining.amount,col="white")
  text(x=1:nrow(world$companies), y=world$companies$money - world$companies$loan.remaining.amount, world$companies$type)
  par(mfcol=c(1,1))
}
