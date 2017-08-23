CreateCompany <- function(world, n.new.companies){
  for(i in 1:n.new.companies){
    new.company <- data.frame(
      id = max(world$companies$id, 0, na.rm = T) + 1,
      type = sample(LETTERS[which(degree(world$resource.graph) > 0)], 1, T),
      last.action = NA,
      money = world$government$loan.size,
      loan.amount = world$government$loan.size,
      loan.remaining.amount = world$government$loan.size,
      loan.term = world$government$loan.term,
      loan.remaining.time = world$government$loan.term,
      inventory = "",
      queue.1 = "",
      queue.2 = "",
      queue.3 = "",
      queue.4 = "",
      queue.5 = "",
      stringsAsFactors = F
    )
    message("A new ",new.company$type," company starts operation.")
    world$companies <- rbind(world$companies, new.company)
    world$government$state.funds <- world$government$state.funds - world$government$loan.size
  }
  return(world)
}

MoveResourceQueues <- function(world){
  queues <- which(grepl("queue", colnames(world$companies)))
  # queue append to inventory
  world$companies$inventory <- paste0(world$companies$inventory, world$companies[, queues[1]])
  # move over queues 2 through 5
  world$companies[, queues[1:(length(queues) - 1)]] <- world$companies[, queues[2:length(queues)]]
  # empty queue 5
  world$companies[, queues[length(queues)]] <- ""
  # return statement
  return(world)
}
ProduceResource <- function(world, company.id){
  # the resource that I want to gather/produce from scratch
  my.r <- world$companies$type[company.id]
  my.r.num <- which(LETTERS == my.r)
  
  # production difficulty and produce if we can
  difficulty <- degree(world$resource.graph)[my.r.num]^2
  if(world$companies$money[company.id] > difficulty & world$population$employed < (world$population$size + difficulty)){
    # employ the workers
    world$population$employed <- world$population$employed + difficulty
    # pay the workers
    world$companies$money[company.id] <- world$companies$money[company.id] - sqrt(difficulty)
    world$population$money <- world$population$money + sqrt(difficulty)
    # get the resource
    world$companies$queue.5[company.id] <- paste0(world$companies$queue.5[company.id], world$companies$type[company.id], collapse = "")
    world$companies$last.action[company.id] <- "produce"
    message("Company ",company.id," produces ",my.r)
  } else {
    message("Company", company.id, "fails to produce",my.r)
  }
  return(world)
}
ReplaceCompany <- function(world, company.id){
  # cannot replace if government has no money
  if(world$government$state.funds < 0){
    return(world)
  }
  possible.industry.types <- degree(world$resource.graph) > 0 & world$resource.information$price > 0
  new.company <- data.frame(
    id = company.id,
    type = sample(LETTERS[possible.industry.types], 1, T, world$resource.information$price[possible.industry.types]),
    last.action = NA,
    money = world$government$loan.size,
    loan.amount = world$government$loan.size,
    loan.remaining.amount = world$government$loan.size,
    loan.term = world$government$loan.term,
    loan.remaining.time = world$government$loan.term,
    inventory = "",
    queue.1 = "",
    queue.2 = "",
    queue.3 = "",
    queue.4 = "",
    queue.5 = "",
    stringsAsFactors = F
  )
  world$government$state.funds <- world$government$state.funds - world$government$loan.size
  world$companies[company.id, ] <- new.company
  message("A new ",new.company$type," company starts operation.")
  return(world)
}