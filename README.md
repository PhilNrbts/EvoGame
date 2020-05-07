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


# Evo_simulation

## Description

# Concept
In this project, I want to create my own evolutionary simulation (ES). On a 2D plane, both food patches and agents are generated. The user can print/save a 2D plane, single agent statistics and/or population statistics.
I will use R to do so.

# Minimal Model
<p> A discription of the most basic model to achieve the described <br>
    project functions. If necessary only for single agent. <br>

1. Food patches and agents are randomly scattered across the 2D plane.

2. Important general state dimensions that affect the 2D print (discrete, globally dependent): 
**Position tuple** (PT) on the plane
**Time Step** (TS): single number
**Fitness Transition** (FT): 3 states (vanishing, normal, reproducing)

2. Basically 2 lists are constantly updated: A population list with individual agent lists that contains multiple internal, local states (too compicated here) and a global list that will generate the printeable 2D map at some TS.

3. Locally, each agent calculates the outcome of PT at TS and how it affects FT. FT changes and earlier factors or randomisation then determine a preferred PT for TS+1, forming the preliminary 2D map. Summarised, generative local updating happens in temporal order: TS(PT,FT), and intended TS+1(PT).
4. The PT for TS+1 are A Time-space meta log, the last validator before entering TS +1 checks for equal PT at TS+1 to  **global check** (GC) 
prevent equal space occupation (at least for now to ensure full 2D mapping of PT at TS without colours for printing),
before TS+1 calculated,  PT prevention <br>
    updates meta log for died and newborn agents
2. Agent as list of states in a greater list of the population states

