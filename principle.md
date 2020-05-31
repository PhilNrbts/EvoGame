# Evo_simulation

## Description

# Concept
In this project, I want to create my own evolutionary simulation (ES). On a 2d plane, both food patches and agents are generated. The user can print/save a 2D plane, single agent statistics and/or population statistics.
I will use R to do so.

# Summary of Mechanisms and user adjusteable features
Core functions
Simulation function parameter: 1) a function that specifies the 2d plane (ideally even as rectangle), and if the walls are solid or make the agent reappear on the other side of the matrix
2) amount of initial food and reappearing random food generation
3) the initial number of agents to put into the model.
4) More advanced features, such as threshold adjustments might also be interesting, but low on the agenda.

library(ggplot)
2D graph function parameter: 1) probe frequency of state transitions 
2) subpopulation tracking (would be amazing but might also takes too much effort) 
3) customiseable colours and agents internal state representation at that point, for example, level of fitness.

Subpopulation plot overlay functions for all personal dimensions (not here explained) as tool for future explorations (not so important) 

# The idea without need for the bottom explanation
By saving meta data estimates of position or map elements on a high levels, the simulation eventually allow for more efficient dependet value correction (two agents should not be on the same spot), while at the same time receiving the data for visualising it. The global time point transitions allow fine (basically infinite) temporal segmentation, which reserves the possibility to increase the simulation complexity at some point. I hope for a fast simulation that on the one hand can display the 2d space in realtime, while still generating a bunch of data to connect them later.

# Minimal Internal Model

*Simulation*
During a discrete time step (TS), transition on a place in the plane, the agent can experience changes in internal states, that are discretely updated and saved in a vector (or list) per agent: a) motor, b) sense, c) action, or d) fitness.

a) Motoric is to take a random step or stay (9 options, like the king in chess)

b) Sensation is wether occupying a non-food or food patch. 
      Non-food patch: a random step at the next (+1) TP
      Food patch: motoric option "stay" at +1 TP
    
c) Action is whether the agent eats a patch or not          #Could also include reproduction to already bundle non-movement action?
      If yes, one patch stack depletes from plane
    
d) Fitness (F) is on a discrete scale, causing the agent transitions in extremes, namely reproducing or vanishing
      Two consecutive step procedure:
      (1) The agent calculates new score 
          > Motoric ('Stay': F-1, else: F-3)
          > Action (Eating: 15)
          > Reproduction (x == 0)
      (2) Evaluation/Classification
          > When exceeding positive threshold, the agent reproduces at TP+1 (F > 45) 
          > when exceeding negative threshold, it vanishes at TP+1 (F < -30)
    
  
-Sensing costly for fitness, detecting close-by patches

# Minimal Global Model Part
A discription of the most basic model to achieve the described project functions. Only a single agent can be modeled for a single agent.
1. Food patches and agents are randomly scattered across the 2D plane. 

2. Important general state dimensions that affect the 2D print (discrete, globally dependent): 
**Position tuple** (PT) on the plane
**Time Step** (TS): single number
**Fitness Transition** (FT): 3 states (vanishing, normal, reproducing)
**Food map** (FM) Amount of food at each point (triple: position, like PT and length or amount)

2. Basically 2 lists are constantly updated: (A) A population list with individual agent lists that contains multiple internal, local states (too compicated here) and (B) a global list that will generate the printeable 2D map at some TS.

3. (A) Locally, each agent calculates the outcome of PT at TS and how it affects FT. FT changes and other factors then determine a preferred PT for TS+1. PT at TS+1 forms the preliminary 2D map. It is send as suggestion for the future to (B), the global list.

4. This preliminary 2D map is then checked for repetition and eventually corrected and accepted.

5. Superimposed on an updated food map (from local signalling of agents that caused changes of it), it gives us a prediction how the map looks like before the process reitterates from point 3.


