#' Simulate a two dimensional plane with agents and food over time and also return meta data
#'
#' The function \emph{evo_game}, as \emph{base_evo_game} gives information about agents and food per time point and additionally returns meta data for each time point.
#'
#' @param rowsField Numeric value > 1 that determines the number of rows the game takes place in.
#' @param columnsField Numeric value > 1 that determines the number of columns the game takes place in.
#' @param nFood Numeric value that determines the number of food palets that are present at the start of the game.
#' @param nAgent Numeric value that determines the number of agents that are present at the start of the game.
#' @param tick Numeric value that determines the number of time points that the game runs at most.
#' @param foodGeneratingType String value "linear" or "cumulative", which determines if food is added per time point as constant or based on the amount of food that is already present.
#' @param foodGeneratingFactor Numeric value that determines how many food is added, depending on foodGeneratingType.
#'
#' @return A list of two data frames with information about individual agents ("Agent_Details") and food quantities ("Food_Details") at each time point, equal to \emph{base_evo_game}. Additionally provides a data frame about meta data ("Meta_Data").
#'
#' @details For information each data frame column and information about the underlying generating process, see the github \href{https://github.com/PhilNrbts/Evo_simulation/blob/master/README.md}{README.md file}.
#'
#' @seealso \code{\link{base_evo_game}}
#'
#' @examples
#' game <- base_evo_game(rowsField = 4, columnsField = 4, nFood = 2, nAgent = 3, tick = 5, foodGeneratingType = "linear", foodGeneratingFactor = 1)
#' game$Agent_Details
#' game$Food_Details
#' game$Meta_Data

#'@export
evo_game <- function(rowsField = 20, columnsField = 20, nFood = 50, nAgent = 20, tick = 20, foodGeneratingType = "linear", foodGeneratingFactor = 1) {
  if (rowsField == 1 | columnsField == 1){stop("rowsField and columnsField need to be at least > 2")}
  # 1. Preparation

  # 1.1 Create initial dummy matrice field, matrices for food and agents, and index for food and agents -----------------

  t <- 0
  bornTotal <- nAgent
  diedTotal <- 0

  field <- matrix(NA, rowsField, columnsField)
  indField <- matrix(1:(rowsField * columnsField), rowsField, columnsField)

  foodField <- generate_field_position(field, nFood, placeHolder = 1, replace = TRUE)
  foodIndex <- data.frame(n = foodField[!is.na(foodField)], data.frame(which(!is.na(foodField),
                                                                             arr.ind = TRUE)), ind = which(!is.na(foodField)))
  if (nAgent > rowsField * columnsField) stop ("nAgent exceeds game matrix")
  agentField <- generate_field_position(field, nAgent, placeHolder = TRUE, replace = FALSE)
  agentIndex <- data.frame(ID = paste0("A", 1:nAgent), data.frame(which(!is.na(agentField),
                                                                        arr.ind = TRUE)), ind = which(!is.na(agentField)))
  Population <- data.frame(ID = paste0("A", 1:nAgent), Birth_Ind = agentIndex["ind"], Birth_t = t, Death_Ind = NA, Death_t = NA, Live_Duration = NA,
                           Parent_ID = NA, Food_Consumed = 0)
  # 1.2 Create structure of the output dataframes FoodDetail & AgentDetail
  wholeFoodDetail <- data.frame(t = t, foodIndex)

  agentDetail <- data.frame(t = t, agentIndex, row.d = 0, col.d = 0,
                            sense = 0, action = 0, energy = 0, fitness = as.factor("normal"))
  previousAgentDetail <- data.frame()
  wholeAgentDetail <- agentDetail
  wholeAgentDetail["t"] <- "Start"
  # 1.3 Create structure of the output dataframe MetaData
  wholeMetaData <- data.frame(t = t, Food_Total = nFood, Agent_Total = nAgent, Ratio_Food_by_Agent = round(c(nFood/nAgent),2), Ratio_Food_by_Total = round(nFood/length(field),2), Born = bornTotal, Born_Total = bornTotal, Died = diedTotal, Died_Total = diedTotal)
  wholeMetaData["t"] <- "Start"
  while (t != tick) {

    # 2.0 Status changing interval for t

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
    previousAgentDetail <- agentDetail
    wholeAgentDetail <- rbind(wholeAgentDetail, agentDetail)
    nAgent <- nAgent - nrow(agentDetail[agentDetail["fitness"] == "vanishing", ])
    agentDetail <- agentDetail[agentDetail["fitness"] != "vanishing", ]

    ## 2.5 Creating meta data and meta data output

    metaList <- generate_meta_data(t, previousAgentDetail, nFood, nAgent, field, bornTotal, diedTotal)
    metaData <- metaList$metaData
    wholeMetaData <- rbind(wholeMetaData,metaData)
    bornTotal <- metaList$bornTotal
    diedTotal <- metaList$diedTotal

    ## 2.6 Eventually terminate loop due to death of all agents

    if (nAgent < 1) {print(paste("All agents died already after",t,"ticks :/")); break}

    # 3. Adjusting the agent space for the following time point

    ## 3.1 Creating new time point and reset details of agents


    t <- t + 1
    agentDetail["t"] <- t
    agentDetail[c("row.d","col.d","sense","action")] <- 0

    ## 3.2 Generating new food and documenting food for t.

    growList <- grow_food(foodGeneratingType, foodGeneratingFactor, field, foodField, nFood)
    foodField <- growList$foodField
    foodIndex <- growList$foodIndex
    nFood <- growList$nFood
    rm(growList)

    if (nrow(foodIndex) > 0) {
      wholeFoodDetail <- rbind(wholeFoodDetail, data.frame(t = t, foodIndex))
    }

    ## 3.3 Generating new agents

    bornList <- born_agent(agentDetail, wholeAgentDetail, nAgent, t, field, indField)
    agentDetail <- bornList$agentDetail
    nAgent <- bornList$nAgent
    rm(bornList)

    if (nrow(field)*ncol(field) < nAgent) {print(paste("Too crowded after",t,"ticks :/")); break}

    # 3.4 Moving agents with moving intent

    agentDetail <- moving_agent(agentDetail, previousAgentDetail, t, field, indField)

    # 3.5 Preventing that two agents sit at the same spot (older agents remain on place)

    agentDetail <- distribute_overlaying_agent(agentDetail, previousAgentDetail, field, indField)

  }

  # 4. Adding last agent details (missing status changes) to the data for the time point and cleaning up

  wholeAgentDetail <- rbind(wholeAgentDetail, agentDetail)
  row.names(wholeAgentDetail) <- NULL
  row.names(wholeFoodDetail) <- NULL
  if (t == tick) {print(paste("Simulation finished after ",t,"ticks :)"))}
  return(list(Agent_Details = wholeAgentDetail, Food_Details = wholeFoodDetail, Meta_Data = wholeMetaData))
}
