package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    // use for-expressions
    val m = w.toLowerCase.groupBy(c => c)
    (m.keys map (x => (x, m.get(x).get.length))).toList.sortBy(y => y._1)

    /*def loop(w: Word, o: Occurrences): Occurrences = {
      if (w.isEmpty) o
      else loop(w.tail, inc(w.head, o))
    }
    def inc(c: Char, o: Occurrences): Occurrences = {
      if (o.isEmpty) List((c, 1))
      else if (c == o.head._1) (c, o.head._2 + 1) :: o.tail
      else if (c < o.head._1) (c, 1) :: o
      else o.head :: inc(c, o.tail)
    }

    loop(w.toLowerCase, Nil)*/
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    def loop(s: Sentence, o: Occurrences): Occurrences = {
      if (s.isEmpty) o
      else loop(s.tail, merge(wordOccurrences(s.head), o))
    }
    def merge(o: Occurrences, p: Occurrences): Occurrences = {
      if (o.isEmpty) p
      else if (p.isEmpty) o
      else if (o.head._1 < p.head._1) (o.head._1, o.head._2) :: merge(o.tail, p)
      else if (o.head._1 == p.head._1) (o.head._1, o.head._2 + p.head._2) :: merge(o.tail, p.tail)
      else p.head :: merge(o, p.tail)
    }
    loop(s, Nil)

    // use for-expressions
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy (p => wordOccurrences(p))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def loop(a: (Char, Int), os: List[Occurrences], acc: List[Occurrences]): List[Occurrences] = {
      if (a._2 == 0) os ::: acc
      else loop((a._1, a._2 - 1), os, (os map (o => a :: o)) ::: acc)
    }

    if (occurrences.isEmpty) List(Nil)
    else {
      loop(occurrences.head, combinations(occurrences.tail), List(Nil))

      /*val x = List(List(('b', 2)), List(('b', 1)), List())
      val y = (x map (x => ('a', 2) :: x)) ::: (x map (x => ('a', 1) :: x)) ::: (x map (x => x))  ::: List(List())
      println(y)
      y*/
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def loopy(c: (Char, Int), d: Occurrences): (Char, Int) = {
      if (d.isEmpty) c
      else if (c._1 == d.head._1) (c._1, c._2 - d.head._2)
      else loopy(c, d.tail)
    }
    def loopx(a: Occurrences, b: Occurrences, acc: Occurrences): Occurrences = {
      if (a.isEmpty) acc
      else {
        val g = loopy(a.head, b)
        if (g._2 > 0) loopx(a.tail, b, acc ::: List(g))
        else loopx(a.tail, b, acc)
      }
    }

    loopx(x, y, Nil)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def isSubset(xs: Occurrences, ys: Occurrences): Boolean = {
      ys forall (y => (xs exists (x => x._1 == y._1 && x._2 >= y._2)))
    }
    def loop5(cs: List[Occurrences], sos: Occurrences, acc: List[Occurrences], acc2: List[List[Occurrences]]): List[List[Occurrences]] = {
      if (sos.isEmpty) acc :: acc2
      else if (cs.isEmpty) acc2
      else {
        val g = cs filter (x => isSubset(sos, x))
        //g foreach (x => loop5(g, subtract(sos, x), x :: acc, acc2))
        (g map (x => loop5(g, subtract(sos, x), x :: acc, acc2))).flatten
      }
    }

    def loop6(os: List[List[Occurrences]], acc: List[Sentence]): List[Sentence] = {
      if (os.isEmpty) acc
      else {
        val h = os.head map (x => dictionaryByOccurrences.get(x)) filter (y => y != None) map (z => z.get)
        if (h.isEmpty) {
          loop6(os.tail, acc)
        }
        else {
          //println(h)
          loop6(os.tail, loop7(h, List(), List(Nil)) ::: acc)
        }
      }
    }
    def loop7(os: List[List[Word]], a: Sentence, acc: List[Sentence]): List[Sentence] = {
      if (os.isEmpty) a :: acc
      else {
        (os.head map (x => loop7(os.tail, a ::: List(x), acc))).flatten
      }
    }

    if (sentence.isEmpty) List(sentence)
    else {
      val os = sentenceOccurrences(sentence)
      //println("os->" + os)
      val co = combinations(os).filter(x => dictionaryByOccurrences.get(x) != None).distinct
      //co filter (x => isSubset(os, x)) foreach (y => println(y + "/" + subtract(os, y)))

      //co filter (x => isSubset(os, x)) foreach (y => println(dictionaryByOccurrences.get(y)))
      //println(loop3(co, os, List(List(Nil))))
      loop6(loop5(co, os, List(Nil), List(List(Nil))), List(Nil)) filterNot (x => x.isEmpty)
    }
  }
}
