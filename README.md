# Evo_simulation

## Description

# Concept
In this project, I want to create my own evolutionary simulation (ES). On a 2D plane, both food patches and agents are generated. The user can print/save a 2D plane, single agent statistics and/or population statistics.
I will use R as programing language.

# Minimal Model
A discription of the most basic model to achieve the described project functions. If necessary for single agent

*Simulation*
During a discrete time step (TS), transition on a place in the plane, the agent can experience changes in internal states, that are discretely updated and saved in a vector (or list) per agent: a) motor, b) sense, c) action, or d) fitness.

a) Motoric is to take a random step or stay (9 options, like the king in chess)

b) Sensation is wether occupying a non-food or food patch. 
    -> Non-food patch: a random step at the next (+1) TP
    -> Food patch: motoric option "stay" at +1 TP
    
c) Action is whether the agent eats a patch or not          #Could also include reproduction to already bundle non-movement action?
    -> If yes, one patch stack depletes from plane
    
d) Fitness (F) is on a discrete scale, causing the agent transitions in the extremes, namely reproducing or vanishing
    -> Two consecutive step procedure:
        (1) The agent calculates new score 
            > Motoric ('Stay': F-1, else: F-3)
            > Action (Eating: 15)
            > Reproduction (x == 0)
            
        (2) Evaluation/Classification
            > When exceeding positive threshold, the agent reproduces at TP+1 (F > 45) 
            > when exceeding negative threshold, it vanishes at TP+1 (F < -30)
    
            
            
-Sensing costly for fitness, detecting close-by patches
