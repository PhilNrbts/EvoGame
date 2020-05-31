
generate_field_position <- function (field, n, placeHolder, replace) {
  fieldPosition <- sample(1:length(field), size = n, replace = replace)
  if (is.numeric(placeHolder)) {
    fieldPosition <- dplyr::count(data.frame(ind = matrix(c(fieldPosition),ncol = 1)),ind)
    field[fieldPosition[["ind"]]] <- fieldPosition[["n"]] * placeHolder
  }

  if (is.logical(placeHolder)) {
    field[fieldPosition] <- placeHolder
  }
  return(field)
}

perceive_sense_food <- function (agentDetail, nAgent, nFood, foodIndex) {
  if (nFood > 0) {
    agentDetail["sense"] <- 0
    if (length(intersect(foodIndex[["ind"]], agentDetail[["ind"]])) > 0) {
      senseIndex <- intersect(foodIndex[["ind"]], agentDetail[["ind"]])
      agentDetail[agentDetail[["ind"]] %in% senseIndex, "sense"] <- 1
    }
  }
  return (agentDetail)
}

act_eat_food <- function(agentDetail, previousAgentDetail, t, foodField, foodIndex, nFood) {
  if (t > 0 & nFood > 0) {
    if (nrow(previousAgentDetail[previousAgentDetail["sense"] == 1 & previousAgentDetail["fitness"] != "vanishing", ]) > 0) {
      actID <- previousAgentDetail[previousAgentDetail["sense"] == 1 & previousAgentDetail["fitness"] != "vanishing", ]
      agentDetail["action"] <- 0
      agentDetail[agentDetail[["ID"]] %in% actID[["ID"]], "action"] <- 1

      foodField[agentDetail[agentDetail[["action"]] == 1,"ind"]] <- foodField[agentDetail[agentDetail[["action"]] == 1,"ind"]] - 1
      foodField[foodField == 0] <- NA
      foodIndex <- data.frame(n = foodField[!is.na(foodField)], data.frame(which(!is.na(foodField),
                                                                                 arr.ind = TRUE)), ind = which(!is.na(foodField)))
      nFood <- sum(foodField[!is.na(foodField)])
    }
  }
  return(list(agentDetail = agentDetail, foodField = foodField, foodIndex = foodIndex, nFood = nFood))
}

calculate_energy <- function(agentDetail) {
  agentDetail[c(agentDetail["row.d"] != 0 | agentDetail["col.d"] != 0), "energy"] <- agentDetail[c(agentDetail["row.d"] !=
                                                                                                     0 | agentDetail["col.d"] != 0), "energy"] - 3
  agentDetail[c(agentDetail["row.d"] == 0 & agentDetail["col.d"] == 0), "energy"] <- agentDetail[c(agentDetail["row.d"] ==
                                                                                                     0 & agentDetail["col.d"] == 0), "energy"] - 1
  agentDetail[agentDetail["action"] == 1, "energy"] <- agentDetail[agentDetail["action"] ==
                                                                     1, "energy"] + 15

  return(agentDetail)
}

categorise_fitness <- function(agentDetail) {
  if (any(agentDetail["energy"] > 45 | agentDetail["energy"] < -15)) {
    agentDetail["fitness"] <- factor(agentDetail[["fitness"]], levels = c("vanishing",
                                                                          "normal", "reproducing"))
    agentDetail["fitness"][c(agentDetail["energy"] < -15), ] <- "vanishing"
    agentDetail["fitness"][c(agentDetail["energy"] >= -15 & agentDetail["energy"] <
                               45), ] <- "normal"
    agentDetail["fitness"][c(agentDetail["energy"] >= 45), ] <- "reproducing"
  }
  return(agentDetail)
}

grow_food <- function(foodGeneratingType, foodGeneratingFactor, field, foodField, nFood) {
  if (foodGeneratingType == "cumulative") {
    moreFood <- round(nFood * foodGeneratingFactor)
  } else if (foodGeneratingType == "linear") {
    moreFood <- round(foodGeneratingFactor)
  }
  foodPosition <- sample(1:length(field), size = moreFood, replace = TRUE)
  foodPosition <- dplyr::count(data.frame(ind = matrix(c(foodPosition),ncol = 1)),ind)
  field[] <- 0
  field[foodPosition[["ind"]]] <- foodPosition[["n"]]
  foodField[is.na(foodField)] <- 0
  foodField <- foodField + field
  foodField[foodField==0] <- NA
  foodIndex <- data.frame(n = foodField[!is.na(foodField)], data.frame(which(!is.na(foodField),arr.ind = TRUE)),
                          ind = c(which(!is.na(foodField))))
  nFood <- nFood + moreFood
  return(list(foodField = foodField, foodIndex = foodIndex, nFood = nFood))
}

born_agent <- function (agentDetail, wholeAgentDetail, nAgent, t, field, indField) {
  parentAgent <- agentDetail[agentDetail["fitness"] == "reproducing", ]
  if (dim(parentAgent)[1] > 0) {
    parentAgent[, "fitness"] <- "normal"
    parentAgent["energy"] <- 0
    childID <- paste0("A", nrow(unique(wholeAgentDetail["ID"])) + 1:nrow(parentAgent))
    childAgent <- data.frame(t = t, ID = childID, parentAgent[-1:-2])

    for (i in 1:nrow(childAgent)) {
      moveFieldChild <- matrix(NA, 3, 3)
      moveFieldChild[c(sample(c(1:4, 6:9), 1))] <- 1  #exclude 5 to avoid similar position as parent agent.
      childAgent[i, c("row.d", "col.d")] <- which(moveFieldChild == 1, arr.ind = TRUE)
      childAgent[i, c("row.d", "col.d")] <- childAgent[i, c("row.d", "col.d")] - c(2, 2)
    }
    childAgent[, c("row", "col")] <- childAgent[, c("row", "col")] + childAgent[, c("row.d", "col.d")]
    childAgent[, c("row", "col")] <- cbind((childAgent["row"] - 1)%%nrow(field) + 1, (childAgent["col"] - 1)%%ncol(field) + 1)

    childAgent["ind"] <- as.vector(apply(childAgent[c("row", "col")],1,
                                         function(x) indField[x[1], x[2]]))

    agentDetail[agentDetail[["ID"]] %in% parentAgent[["ID"]], ] <- parentAgent
    agentDetail <- rbind(agentDetail, childAgent)
    nAgent <- nAgent + nrow(childAgent)
  }
  return(list(agentDetail = agentDetail, nAgent = nAgent))
}

moving_agent <- function (agentDetail, previousAgentDetail, t, field, indField) {
  normalAgent <- previousAgentDetail[previousAgentDetail["fitness"] == "normal" & previousAgentDetail["sense"]==0,]  # to prevent adjusted parent agents to be selected
  if (dim(normalAgent)[1] > 0) {
    normalAgent["t"] <- t
    for (i in 1:nrow(normalAgent)) {
      moveField <- matrix(NA, 3, 3)
      moveField[c(sample(c(1:9), 1))] <- 1
      normalAgent[i, c("row.d", "col.d")] <- which(moveField == 1, arr.ind = TRUE)
      normalAgent[i, c("row.d", "col.d")] <- normalAgent[i, c("row.d",
                                                              "col.d")] - c(2, 2)
    }
    normalAgent[, c("row", "col")] <- normalAgent[, c("row", "col")] + normalAgent[, c("row.d", "col.d")]
    normalAgent[, c("row", "col")] <- cbind((normalAgent["row"] -
                                               1)%%nrow(field) + 1, (normalAgent["col"] - 1)%%ncol(field) + 1)
    normalAgent["ind"] <- as.vector(apply(normalAgent[c("row",
                                                        "col")], 1, function(x) indField[x[1], x[2]]))
    agentDetail[agentDetail[["ID"]] %in% normalAgent[["ID"]], ] <- normalAgent
  }
  return(agentDetail)
}

distribute_overlaying_agent <- function (agentDetail, previousAgentDetail, field, indField) {
  if (any(dplyr::count(agentDetail, ind)[2] > 1)) {
    while (any(dplyr::count(agentDetail, ind)[2] > 1)) {
      overlayed <- dplyr::count(agentDetail, ind)[dplyr::count(agentDetail, ind)[2] > 1, ]
      for (i in 1:nrow(overlayed)) {
        overlayRows <- row(agentDetail)[,1][agentDetail[["ind"]] == overlayed[[1, 1]]][-1]  #removing the agent higher in the list, as allowed to stay
        toMove <- agentDetail[overlayRows, ]
        for (j in 1:nrow(toMove)) {
          moveField <- matrix(NA, 3, 3)
          moveField[c(sample(c(1:4, 6:9), 1))] <- 1
          toMove[j, c("row.d", "col.d")] <- which(moveField == 1, arr.ind = TRUE)
          toMove[j, c("row.d", "col.d")] <- toMove[i, c("row.d", "col.d")] -
            c(2, 2)
          toMove[j, c("row", "col")] <- as.vector(toMove[j, c("row", "col")] +
                                                    toMove[j, c("row.d", "col.d")])
          toMove[j, c("row", "col")] <- cbind((toMove[j, "row"] - 1) %% nrow(field) +
                                                1, (toMove[j, "col"] - 1) %% ncol(field) + 1)
          toMove[j, "ind"] <- indField[toMove[j, "row"], toMove[j, "col"]]
          toMove[j, "energy"] <- toMove[j, "energy"] - 3
        }
        previousAgentDetail[previousAgentDetail[["ID"]] %in% toMove[["ID"]], "sense"] <- 0
        toMove["energy"] <- toMove["energy"] + 3
        agentDetail[agentDetail[["ID"]] %in% toMove[["ID"]], ] <- toMove
      }
    }
  }
  return(agentDetail)
}

# evo_game ---------------

generate_meta_data <- function(t, previousAgentDetail, nFood, nAgent, field, bornTotal, diedTotal) {
  if(nrow(previousAgentDetail)>0) {
    born <- nrow(previousAgentDetail[previousAgentDetail["fitness"]== "reproducing",])
    died <- nrow(previousAgentDetail[previousAgentDetail["fitness"]== "vanishing",])
    bornTotal <- bornTotal + born
    diedTotal <- diedTotal + died
    metaData <- data.frame(t = t, Food_Total = nFood, Agent_Total = nAgent, Ratio_Food_by_Agent = round(c(nFood/nAgent),2), Ratio_Food_by_Total = round(nFood/length(field),2),
                           Born = born, Born_Total = bornTotal, Died = died, Died_Total = diedTotal)
  } else {
    born <- 0
    died <- 0
    metaData <- data.frame(t = t, Food_Total = nFood, Agent_Total = nAgent, Ratio_Food_by_Agent = round(c(nFood/nAgent),2), Ratio_Food_by_Total = round(nFood/length(field),2),
                           Born = born, Born_Total = bornTotal, Died = died, Died_Total = diedTotal)
  }
  return(list(metaData = metaData, bornTotal = bornTotal, diedTotal = diedTotal))
}
