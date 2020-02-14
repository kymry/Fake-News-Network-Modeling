# About

This implementation is Laura's approach on how to simulate the fake news spread in a given network, knowing how the final state of the network is.

# Definition

First of all, let us define the network.

## Basic model

For starters, what we want to model is the spread of fake news in a given network. To do so, we are going to create an SI epidemics model, which is the most basic model on an epidemics system. 

Each user can only have two states:

- *Susceptible*. A user that has not been reached  yet by the fake news and has not consumed nor shared it.
- *Infected*. A  user that has consumed the fake news and has shared it. So we will assume that infected means not only exposure to the news (by having a neighbor that has shared it), but also to be invested enough on what s/he has read that s/he shares it with their followers.

Thus, since the network only has 2 states, once infected, the user cannot recover (for now).