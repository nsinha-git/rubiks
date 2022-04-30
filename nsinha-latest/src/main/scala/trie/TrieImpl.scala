package org.nsinha.trie

import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn.readLine

/***
 * Trie should store words in least storage
 * It should also retrive easiliy all words starting from say ap?
 * or a? etc.
 *
 */

/*
An achitecture could be:
       node  ----sibEdge--->
    node  -SibEdge--> node

A node should have a char, a sib_edge, a word marker to denote that its a word if traversed from root


*/

case class TrieNode(c: String) {
  import TrieNode._
  var kids = new Array[TrieNode](26)
  var word = false

  def insertWord(w: String): Unit = {
    if (w == null) {
      word = true
    } else {
      val (s, e) = splitWordIntoFrontAndRemaining(w)
      if (s != null) {
        val s_indx = getIndexChar(s)
        if (kids(s_indx) != null) {
          kids(s_indx).insertWord(e)
        } else {
          //create a node
          kids(s_indx) = TrieNode(s)
          kids(s_indx).insertWord(e)
        }
      }
    }
  }

  def findWordsBeginWith(w: String) : List[String] = {
    if (w == null || w.isEmpty) {
      val res = mutable.ListBuffer[String]()
      if (word)
        res += c
      for (k <- kids) {
        if (k == null) {

        } else {
          k.findWordsBeginWith("").foreach(x => res += c + x)
        }
      }
      res.toList
    } else {
      val (s, e) = splitWordIntoFrontAndRemaining(w)
      assert(s != null)
      val s_indx = getIndexChar(s)
      if (kids(s_indx) == null) {
        List.empty
      } else {
        kids(s_indx).findWordsBeginWith(e).map(x => s + x)
      }

    }

  }

}

class TrieImpl {
  import TrieNode._
  var kids = new Array[TrieNode](26)

  def insertWord(w: String): Unit = {
    val (s, e) = splitWordIntoFrontAndRemaining(w)
    if (s != null) {
      val s_indx = getIndexChar(s)
      if (kids(s_indx) != null) {
        kids(s_indx).insertWord(e)
      } else {
        //create a node
        kids(s_indx) = TrieNode(s)
        kids(s_indx).insertWord(e)
      }
    } else {

    }

  }

  def findWordsBeginWith(w: String) : List[String] = {
    if (w == null) {
      List.empty
    } else {
      val (s, e) = splitWordIntoFrontAndRemaining(w)
      val s_indx = getIndexChar(s)
      if (kids(s_indx) == null) {
        List.empty
      } else {
        kids(s_indx).findWordsBeginWith(e).map(x => s + x)
      }

    }

  }






}

object TrieNode {

  def getIndexChar(str: String): Int = {
    assert(str.length == 1)
    str.charAt(0).toInt - 'a'.toInt
  }


  def splitWordIntoFrontAndRemaining(w: String): (String, String) = {
    if (w == null) {
      (null, null)
    } else {
      w.length match {
        case 0 => (null, null)
        case 1 => (w.charAt(0).toString, null)
        case _ =>
          val startChar = w.charAt(0)
          val leftWord = w.substring(1)
          (startChar.toString, leftWord)
      }
    }

  }
  val trie = new TrieImpl()


  def main(args: Array[String]): Unit = {
    Source.fromFile("/usr/share/dict/words").getLines().foreach(w => {
      println(w.toLowerCase)

      trie.insertWord(w.toLowerCase.replace("-", ""))
    })
    while (true) {
      val input = readLine()
      val words = trie.findWordsBeginWith(input)
      println(input + ":")
      words.foreach(x => println(x))
    }
  }


}





