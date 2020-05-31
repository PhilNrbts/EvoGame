#' Simulate a two dimensional plane with agents and food over time
#'
#' The function \emph{base_evo_game} runs agents through circles of status changes and movements that resemble a simplified evolution process on a 2 dimensional plane. The agents have the possibility to eat food when being on a coordinate with food.
#'
#' @param rowsField Numeric value > 1 that determines the number of rows the game takes place in.
#' @param columnsField Numeric value > 1 that determines the number of columns the game takes place in.
#' @param nFood Numeric value that determines the number of food palets that are present at the start of the game.
#' @param nAgent Numeric value that determines the number of agents that are present at the start of the game.
#' @param tick Numeric value that determines the number of time points that the game runs at most.
#' @param foodGeneratingType String value "linear" or "cumulative", which determines if food is added per time point as constant or based on the amount of food that is already present.
#' @param foodGeneratingFactor Numeric value that determines how many food is added, depending on foodGeneratingType.
#'
#' @return A list of two data frames with information about individual agents ("Agent_Details") and food quantities ("Food_Details") at each time point. Each agent individual has a personal identification ("ID"), each field with food a quantity ("n"). Both data frames contain information about the row ("row"), column ("col"), and index ("ind") where the agent/food is located at that time.
#'
#' @details For information each data frame column and information about the underlying generating process, see the github \href{https://github.com/PhilNrbts/Evo_simulation/blob/master/README.md}{README.md file}.
#'
#' @seealso \code{\link{base_game_function}}
#'
#' @examples
#' game <- base_evo_game(rowsField = 4, columnsField = 4, nFood = 2, nAgent = 3, tick = 5, foodGeneratingType = "linear", foodGeneratingFactor = 1)
#' game$Agent_Details
#' game$Food_Details

#'@export
base_evo_game <- function(rowsField = 20, columnsField = 20, nFood = 50, nAgent = 20, tick = 20, foodGeneratingType = "linear", foodGeneratingFactor = 1) {

  # 1. Preparation

  ## 1.1 Create initial dummy matrice field, matrices for food and agents, and index for food and agents -----------------

  t <- 0

  field <- matrix(NA, rowsField, columnsField)
  indField <- matrix(1:(rowsField * columnsField), rowsField, columnsField)

  foodField <- generate_field_position(field, nFood, placeHolder = 1, replace = TRUE)
  foodIndex <- data.frame(n = foodField[!is.na(foodField)], data.frame(which(!is.na(foodField),
                                                                             arr.ind = TRUE)), ind = which(!is.na(foodField)))
  if (nAgent > rowsField * columnsField) stop ("nAgent exceeds game matrix")
  agentField <- generate_field_position(field, nAgent, placeHolder = TRUE, replace = FALSE)
  agentIndex <- data.frame(ID = paste0("A", 1:nAgent), data.frame(which(!is.na(agentField),
                                                                        arr.ind = TRUE)), ind = which(!is.na(agentField)))

  ## 1.2 Create structure of the output dataframes FoodDetail & AgentDetail
  wholeFoodDetail <- data.frame(t = t, foodIndex)

  agentDetail <- data.frame(t = t, agentIndex, row.d = 0, col.d = 0,
                            sense = 0, action = 0, energy = 0, fitness = as.factor("normal"))
  previousAgentDetail <- data.frame()
  wholeAgentDetail <- agentDetail
  wholeAgentDetail["t"] <- "Start"

  while (t != tick) {

    # 2.0 Status Change interval for t

    ## 2.1 Perception

    agentDetail <- perceive_sense_food(agentDetail, nAgent, nFood, foodIndex)

    ## 2.2 Action (eating)

    foodList <- act_eat_food(agentDetail, previousAgentDetail, t, foodField, foodIndex, nFood)
    agentDetail <- foodList$agentDetail
    foodField <- foodList$foodField
    foodIndex <- foodList$foodIndex
    nFood <- foodList$nFood
    rm(foodList)

    ## 2.3 Evaluating fitness

    agentDetail <- calculate_energy(agentDetail)
    agentDetail <- categorise_fitness(agentDetail)

    ## 2.4 Organising documentation in output script and removing died agents

    wholeAgentDetail <- rbind(wholeAgentDetail, agentDetail)
    nAgent <- nAgent - nrow(agentDetail[agentDetail["fitness"] == "vanishing", ])
    agentDetail <- agentDetail[agentDetail["fitness"] != "vanishing", ]
    if (nAgent < 1) {print(paste("All agents died already after",t,"ticks :/")); break}

    # 3. Adjusting the agent space for the following time point
    previousAgentDetail <- agentDetail
    t <- t + 1
    agentDetail["t"] <- t
    agentDetail[c("row.d","col.d")] <- 0

    ## 3.1 Generating new food and documenting food for t.
    growList <- grow_food(foodGeneratingType, foodGeneratingFactor, field, foodField, nFood)
    foodField <- growList$foodField
    foodIndex <- growList$foodIndex
    nFood <- growList$nFood
    rm(growList)
    if (nrow(foodIndex) > 0) {
      wholeFoodDetail <- rbind(wholeFoodDetail, data.frame(t = t, foodIndex))
    }

    ## 3.2 Generating new agents
    bornList <- born_agent(agentDetail, wholeAgentDetail, nAgent, t, field, indField)
    agentDetail <- bornList$agentDetail
    nAgent <- bornList$nAgent
    rm(bornList)

    if (nrow(field)*ncol(field) < nAgent) {print(paste("Too crowded after",t,"ticks :/")); break}

    ## 3.3 Moving agents with moving intent
    agentDetail <- moving_agent(agentDetail, previousAgentDetail, t, field, indField)

    ## 3.4 Preventing that two agents sit at the same spot (older agents remain on place)
    agentDetail <- distribute_overlaying_agent(agentDetail, previousAgentDetail, field, indField)
  }

  wholeAgentDetail <- rbind(wholeAgentDetail, agentDetail)
  row.names(wholeAgentDetail) <- NULL
  row.names(wholeFoodDetail) <- NULL
  if (t == tick) {print(paste("Simulation finished after ",t,"ticks :)"))}
  return(list(Agent_Details = wholeAgentDetail, Food_Details = wholeFoodDetail))
}
