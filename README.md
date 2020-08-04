# hhco
Modeling the game "Hi Ho! Cherry-O"

## Overview
My kids like the the game "Hi Ho! Cherry-O." Each player has tree with 10 cherries and an empty basket. The goal is to move all the cherries from the tree to the basket. On your turn, you spin a spinner with 7 options:
* Move 1 cherry from the tree to your basket
* Move 2 cherries from the tree to your basket
* Move 3 cherries from the tree to your basket
* Move 4 cherries from the tree to your basket
* Move 2 cherries from your basket to the tree (two separate spaces)
* Move all cherries from your basket to the tree

Games can last a long time, and I was curious as to how long they might last. Originally I made a Monte Carlo simulation, but later changed it to an absorbing Markov chain simulation. I also created a network plot to visualize the flow of the game.

## Files
* markov_network.R : The R code for the Markov simulation and network plot.
* hhco_markov.xlsx : Spreadsheet with the data for the Markov simulation and network plot.
* monte_carlo.R : Original Monte Carlo simulation.
* hhco_final.png : The network and cumulative probability of ending plots. I had to cheat a bit to make the figure, since I couldn't get the network plot arrow weights to display as fractions, only decimals.
