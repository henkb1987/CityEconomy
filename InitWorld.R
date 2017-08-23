library("igraph")
world <- list(
  resource.graph = sample_smallworld(1, 26, 1.7, 0.25),
  resource.information = data.frame(
    name = LETTERS,
    type = 0,
    supply = 0,
    demand = 0,
    in.system = 0,
    price = round(runif(26), 2),
    stringsAsFactors = F
  ),
  companies = data.frame(
    id = NULL,
    type = NULL,
    last.action = NULL,
    money = NULL,
    loan.amount = NULL,
    loan.remaining.time = NULL,
    inventory = NULL,
    queue.1 = NULL,
    queue.2 = NULL,
    queue.3 = NULL,
    queue.4 = NULL,
    queue.5 = NULL,
    stringsAsFactors = F
  ),
  market = data.frame(
    lot.id = 1,
    seller.id = -1,
    resource = NA,
    price = 0,
    age = 0
  ),
  government = data.frame(
    state.funds = 1e6,
    loan.size = 250,
    max.relative.loan.size = 2,
    loan.term = 30 * 12,
    interest.rate = .08,
    transaction.tax = .01
  ),
  population = data.frame(
    size=1e3,
    employed=0,
    mean.employed=0,
    money=1e7
  )
)
world$resource.information$type <- degree(world$resource.graph)
ends <- which(world$resource.information$type == 1)
world$resource.information$type[ends] <- world$resource.information$type[ends] * rep(0:1, length(ends))[1:length(ends)]

plot(world$resource.graph, vertex.label = LETTERS,vertex.color=rainbow(max(world$resource.information$type)+1)[world$resource.information$type + 1])
