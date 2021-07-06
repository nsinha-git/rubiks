# Rubik Solver


Represetational Ideas:
1. Every cube is symmetric by making an abstraction that a cube has 6 faces,
some of which is hidden is a normal rubiks. But that hidden is not a matter
that affects the game analysis in any way. Hence we treat every cube as
6 faced without resuming to considerations of its hiddeness. This simplifies
the algorithmic logic immensely as now we have all cubes similar.
   

Algorithmic Ideas:
1. Place every cube w/o worrying much about other cubes to its rightful 
position and orientation.
2. Once done with that, are their cubes more complex than others.
3. Get the most complicated cube set. 
4. Is there a path that solves them and  has common prefix.
5. If yes take the step to that path.