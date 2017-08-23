CollectInterest <- function(world){
  for(i in which(world$companies$loan.remaining.amount > 0)){
    # payment parts
    interest <- world$companies$loan.remaining.amount[i] * world$government$interest.rate
    loan.payment <- world$companies$loan.amount[i] / world$companies$loan.term[i]
    # pay or payment is added to loan
    if((interest + loan.payment) < world$companies$money[i]){
      world$companies$money[i] <- world$companies$money[i] - (interest + loan.payment)
      world$companies$loan.remaining.amount[i] <- world$companies$loan.remaining.amount[i] - loan.payment
      world$government$state.funds <- world$government$state.funds + interest + loan.payment
    } else {
      # world$companies$loan.remaining.amount[i] <- world$companies$loan.remaining.amount[i] + interest + loan.payment
      world$companies$money[i] <- world$companies$money[i] - (interest + loan.payment)
      world$companies$loan.remaining.amount[i] <- world$companies$loan.remaining.amount[i] - loan.payment
      world$government$state.funds <- world$government$state.funds + interest + loan.payment
    }
    world$companies$loan.remaining.time[i] <- max(world$companies$loan.remaining.time[i] - 1, 0)
    # end of loan term dealings
    if(world$companies$loan.remaining.time[i] <= 0 & world$companies$loan.remaining.amount[i] < 10){
      world$companies$loan.amount[i] <- 0
      world$companies$loan.remaining.amount[i] <- 0
      world$companies$loan.term[i] <- 0
      world$companies$loan.remaining.time[i] <- 0
    } else if(world$companies$loan.remaining.time[i] <= 0 & world$companies$loan.remaining.amount[i] > 0){
      world <- BankruptCompany(world, i)
    } else if(world$companies$money[i] < -1 * world$companies$loan.amount[i] & world$companies$loan.remaining.amount[i] > 0){
      world <- BankruptCompany(world, i)
    }
  }
  return(world)
}
BankruptCompany <- function(world, company.id){
  debt <- world$companies$loan.remaining.amount[company.id] - world$companies$money[company.id]
  cat("Company",company.id,"(type=",world$companies$type[company.id],") is bankrupt with a debt of",debt,",")
  # sell off all resources to consumer market
  queues <- which(grepl("queue", colnames(world$companies)))
  company.resources <- paste0(world$companies[company.id, c(min(queues) - 1, queues)])
  company.resources <- company.resources[which(company.resources != "")]
  for(r in company.resources){
    # put offer on market
    world <- SellResource(world, 0, r)
    # deduct expected gains from debt
    debt <- debt - sum(world$resource.information$price[which(LETTERS == r)])
  }
  cat(debt,"after selling of active assets,")
  # the government takes over all market lots of the company, counting them in part to debt reduction
  on.market <- match(world$market$resource[world$market$seller.id == company.id], LETTERS)
  debt <- debt - sum(world$resource.information$price[on.market])
  world$market$seller.id[world$market$seller.id == company.id] <- 0
  cat(debt,"after selling of assets on market,")
  if(debt >= 0){
    cat("and is removed.\n")
    world <- ReplaceCompany(world, company.id)
  } else {
    cat("and is given a restart with starting capital of",-debt,".\n")
    world$companies$money <- -debt
    world$companies$loan.amount[company.id] <- 0
    world$companies$loan.remaining.amount[company.id] <- 0
    world$companies$loan.term[company.id] <- 0
    world$companies$loan.remaining.time[company.id] <- 0
  }
  return(world)
}
IncreaseLoan <- function(world, company.id, amount){
  # decline loan if loaned too much or if government has no funds
  if(world$government$state.funds > 0 |world$companies$loan.remaining.amount > world$government$loan.size * world$government$max.relative.loan.size){
    return(world)
  }
  # company-side
  world$companies$money[company.id] <- world$companies$money[company.id] + amount * (1 - world$government$transaction.tax)
  world$companies$loan.amount[company.id] <- world$companies$loan.amount[company.id] + amount
  percentage.increased.amount <- amount / world$companies$loan.remaining.amount[company.id]
  world$companies$loan.remaining.time[company.id] <- world$companies$loan.remaining.time[company.id] + percentage.increased.amount
  # government side
  world$government$state.funds <- world$government$state.funds - amount * (1 - world$government$transaction.tax)
  # return
  return(world)
}