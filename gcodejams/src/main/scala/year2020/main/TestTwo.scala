package year2020.main

/**
 * Two players, A and B, are playing a game. The game uses N tiles numbered 1 through N, and a board consisting of a single horizontal row of N empty cells.
Players alternate turns, with Player A going first. On a turn, a player picks an unused tile and an empty cell and places the tile in the cell.
At the end of the game, Player A wins if there are two tiles with consecutive numbers in adjacent cells (regardless of who put them there).
Otherwise, Player B wins. For example, final boards of 1 2 3 4 and 4 1 3 2 are examples of wins for Player A, and a final board of 3 1 4 2 is an
example of a win for Player B. (Notice that consecutive numbers may appear in either order.)
You just watched two players play a game, but you could not understand their strategy. They may not have played rationally! You decide to compare
their moves against an optimal strategy.
A winning state is a state of the game from which the player whose turn it is can guarantee a win if they play optimally, regardless of what the
opponent does. A mistake is a move made while in a winning state that results in the opponent having a winning state on their next turn. (Notice
that it is not possible to make a mistake on the last turn of the game, since if the last turn begins with a winning state for that player, it must be
because that player's only move results in a win.)
 */
case class TestTwo(n: Int, moves: Seq[Int]) {
  /***
   * startegy:
   * A has a well defined set of non-error moves.
   * By other definition B has also a complement of A as non-error moves.
   * 1Mainitaining the set of A leads us to correct direction.
   * 2.How to form setA. Its hould be hash searchable for performance
   * 3. At start setA is all moves. ie. (1,_) ,(2,-) or (k,-) where k is the label and - is hole
   * 4.After 1st move say (1,3) setA is  (3,-), (4,-), .. and has set of pos->label like 2-2, 4->2
   * 5. if 2 is placed the pos->labels are removed that have labels 2. similarly if  a space is filled the corresponding space is also removed.
   * 6. setA can be maintained just by pos->label and label->pos.bot are multi map as same labe can be placed at multiple pos and same pos can take mutiple
   * labesl for win.at the start both the sets are empty.
   * 7. A good move by A is to chose from the above sets. or choose an unused label.Dont choose unused label if there are one unused and one label left in set A.
   * 8. A wrong move by B is to choose a move strictly from set A. Using unused is ok till set A has exactly one label and unused is also 1.
   *
   * O(n) ops
   */

  def process(): Unit = {

  }



}

object TestTwo {

  var curT = tOne

  val tOne = {
    (6, Seq((2,2), (3,5), (4,3), (6,6), (1,4), (5,1)))
  }


}
