(*  COMP 212 Homework 8:  Higher-order programming
*
*   N. Danner
*  Rafael Goldstein
*)

structure Hw8 =
struct

exception Unimplemented
exception Failure

(*  Naming conventions.  I found it useful to consistently use the same names
*   for the same types of values:
*   - c : category
*   - d : doc
*   - ccs : ctycounts
*   - wcs : wordcounts
*   - wps : wordprobs
*   - keyws : string list (list of keywords)
*)

(*  These type declarations just let us use meaningful names for the types
*  that we use here.
*)

type category = string
type doc = string list

(*  Think of a value wcs : wordcounts as a function from strings to ints.
*   If wcs = [(s_0, i_0),...,(s_n, i_n)], then the value wcs on string s
*   is computed as follows:
*   - if s = s_j, then the value is i_j
*   - if s <> s_j for any j, then the value is undefined.
*   In the documentation below, we will write wcs(s) for the value of wcs
*   on string s.
*
*   Think of each of the list types below in the same way.
*)
type wordcounts = (string*int) list

(*  The type of functions from categories to wordcounts.
*)
type ctycounts = (category*wordcounts) list

(*  A type of functions from strings to reals.
*)
type wordprobs = (string*real) list

(*  A type of functions from categories to wordprobs.
*)
type ctyprobs = (category*wordprobs) list

(*  remove(xs, x) = ys, where ys is a copy of xs with all occurrences of x
*  removed.
*)
fun remove (xs : ''a list, x : ''a) : ''a list =
  List.filter(fn y => y <> x) xs

(*  removeDups xs = ys, where ys is a copy of xs with all duplicates removed.
*)
fun removeDups (xs : ''a list) : ''a list =
    List.foldr(fn (x1, xs') => if List.find(fn (y) => y = x1) xs' = NONE
      then x1 :: xs' else xs') [] xs

(*  member([x_0,...,x_{n-1}], x) = true,  x = x_i for some i
*                                = false, x <> x_i for all i.
*)
fun member (xs : 'a list, p : 'a -> bool) : bool =
  List.foldr(fn (x, ys) => p(x) orelse ys) false xs

(*  getCtyCounts (ccs, c) = ccs(c).
*)
fun getCtyCounts (ccs : ctycounts, c : category) : wordcounts =
  let
    val x = List.find(fn (cat1, ct1) => c = cat1) ccs
  in
    case x of
    NONE => []
    | SOME(cat, cts) => cts
  end

(*  updCtyCounts (ccs, c, wcs) = ccs', where:
*   - ccs'(c) = wcs
*   - ccs'(c') = ccs(c') if c' <> c.
*   In other words, updCtyCounts(ccs, c, wcs) is just like ccs, except
*   the value on category c is wcs.
*)
fun updCtyCounts (
  ccs : ctycounts,
  c : category,
  wcs : wordcounts) : ctycounts =
   List.map(fn (cat1, ct1) => if c = cat1 then (cat1, wcs) else (cat1, ct1)) ccs

  (*  getWordCount(wcs, w) = wcs(w).
  *)
  fun getWordCount(wcs : wordcounts, w : string) : int =
    let
      val x =  List.find(fn (w1, wc) => w1 = w) wcs
    in
      case x of
      NONE => ~1
      | SOME(w, cts) => cts
    end

  (*  updWordCounts(wcs, w, n) = wcs', where
  *   - wcs'(w) = n
  *   - wcs'(w') = wcs(w') if w' <> w.
  *)
  fun updWordCounts(wcs : wordcounts, w : string, n : int) : wordcounts =
    List.map(fn (word, ct) => if word = w then (word, n) else (word, ct)) wcs

  (*  addWordCounts(wcs1, wcs2) = wcs, where wcs(w) = wcs1(w) + wcs2(w).
  *)
  fun addWordCounts(wcs1 : wordcounts, wcs2 : wordcounts) : wordcounts =
    List.map(fn (word, ct) => (word, ct + getWordCount(wcs1, word))) wcs2

  (*  incrCount (w, wcs) = wcs', where:
  *   - wcs'(w) = 1 + wcs(w)
  *   - wcs'(w') = if w' <> w.
  *   In other words, incrCount keyws (w, wcs) is just like wcs, except the
  *   count for w is incremented.
  *)
  fun incrCount (w : string, wcs : wordcounts) : wordcounts =
    List.map(fn (word, ct) =>
      if word = w then (word, 1 + ct) else (word, ct)) wcs

  (*  initCounts(keyws) = wcs, where wcs(w) = 0 for each w in keys.
  *)
  fun initCounts (keyws : string list) : wordcounts =
    List.map(fn (word) => (word, 0)) keyws

  (*  countDoc(keyws, d) = wcs, where for k in keyws, wcs(k) is the number of
  *  occurrences of k in d.
  *)
  fun countDoc(keyws : string list, d : doc) : wordcounts =
    let
      val wcs = initCounts keyws
    in
      List.foldr(fn (word, wcs') =>
        if member(keyws, fn (y) =>
          word = y) then incrCount(word, wcs') else wcs') wcs d
    end

  (*  count (keyws, cds) = ccs, where ccs(c) is the word count for all documents
  *   in cds with category c.
  *)
  fun count (keyws : string list, cds : (category*doc) list) : ctycounts =
    let
      val initKCounts = initCounts keyws

  (*  getCtys([(c_0, d_0),..., (c_{n-1}, d_{n-1})] = [c_0,...,c_{n-1}].
  *)
      fun getCtys(cds : (category*doc) list) : category list =
        List.map(fn (cat, doc) => cat) cds

  (*  makeInitCtyCounts(ctys) = ccs, where ccs(c) = initKCounts for each
  *   c in ctys.
  *)
      fun makeInitCtyCounts(ctys : category list) : ctycounts =
        List.map(fn (cat) => (cat, initKCounts)) ctys

        val initCtyCounts = makeInitCtyCounts(removeDups(getCtys(cds)))

  (*  countDocs([(c_0,d_0),...,(c_{n-1}, d_{n-1})]) = ccs, where domain(ccs)
  *  = the unique categories in cds (the argument to count), ccs(c) = wcs,
  *  where wcs(w) = the number of occurrences of w in all documents d_i such
  *  that c_i = c.
  *)
      fun countDocs(cds : (category*doc) list) : ctycounts =
        List.foldr(fn ((cat, d), (ctyCts')) =>
        updCtyCounts(ctyCts', cat, addWordCounts(getCtyCounts(ctyCts', cat),
        countDoc(keyws, d)))) initCtyCounts cds
    in
      countDocs(cds)
    end

    (*  getProb (wps, w) = wps(w).
    *)
  fun getProb (wps : wordprobs, w : string) : real =
    let
      val x = List.find(fn (word, prob) => w = word) wps
    in
      case x of
      NONE => ~1.0
      | SOME(w, pbs) => pbs
    end

    (*  makeWordProbs wcs = wps, where wps(s) = wcs(s)/n and n is the sum
    *   of the counts of all the words in wcs.
    *)
  fun makeWordProbs (wcs : wordcounts) : wordprobs =
    let
      (*  countWords([(w_0,n_0),...,(w_{k-1}, n_{k-1})]) = n_0 + ... + n_{k-1}.
      *)
      fun countWords(wcs : wordcounts) : int =
        List.foldr(fn (ct1, ct2) => ct1 + ct2) 0 (List.map(fn (w, c) => c) wcs)

      (*  nWords = n_0 + ... + n_{k-1}, where
      *     wcs = [(w_0,n_0),...,(w_{k-1}, n_{k-1})].
      *)
      val nWords = countWords(wcs)

      (*  countProbs([(w_0, n_0),...,(w_{k-1},n_{k-1})]) =
      *     [(w_0,p_0),...,(w_{n-1}, p_{n-1})], where
      *     p_i = max(1, n_i)/(n_0 + ... + n_{k-1}).
      *)
      fun countProbs(wcs : wordcounts) : wordprobs =
        List.map(fn (word, ct) => (word, real(Int.max(1, ct))/real(nWords))) wcs
    in
      countProbs wcs
    end

    (*  makeCtyProbs ccs = cps, where cps(c) = makeWordProbs(ccs(c))
    *)
  fun makeCtyProbs(ccs : ctycounts) : ctyprobs =
      List.map(fn (cat, ct) => (cat, makeWordProbs ct)) ccs

    (*  computeLL (keyws, d, wps) = the log-likelihood of the document d
    *   being produced by the model wps.  See assignment description for details.
    *)
  fun computeLL (keyws : string list, d : doc, wps : wordprobs) : real =
    let
    (*  dCounts(w) = the number of occurrences of w in d, for each w in keyws.
    *)
      val dCounts : wordcounts = countDoc(keyws, d)

    (*  getCountsWProbs([(w_0,n_0),...,(w_{k-1}, n_{k-1})]) =
    *     [(n_0, p_0),..., (n_{k-1}, p_{k-1})], where p_i = getProb(wps, w_i).
    *)
      fun getCountsWProbs(wcs : wordcounts) : (int*real) list =
      List.map(fn (word, cat) => (cat, getProb (wps, word))) wcs

    (*  countsWProbs = [(n_0, p_0),..., (n_{k-1}, p_{k-1})], where
    *     keyws = [w_0,..., w_{k-1}],
    *     n_i   = the number of occurrences of w_i in d
    *     p_i   = getProb(wps, w_i)
    *)
      val countsWProbs = getCountsWProbs(dCounts)

    (*  getLL([(n_0, p_0),..., (n_{k-1}, p_{k-1})]) =
    *     n_0*log(p_0) + ... + n_{k-1}*log(p_{k-1}), where log = log base 10.
    *)
      fun getLL(cps : (int*real) list) : real =
      List.foldr(
        fn (p1, p2) => p1 + p2) 0.0 (List.map(
          fn (n, p) => real(n) * Math.log10(p)) cps)
    in
      getLL countsWProbs
    end

    (*  makeClassifier (keyws, cds) = cl, where cl(d) = [...,(c, r),...],
    *   where the list ranges over pairs (c, r) such that c is a category
    *   in cds and r is the log-likelihood that the document d is produced
    *   by the model computed from c.
    *)
  fun makeClassifier
    (keyws : string list, cds : (category*doc) list)
    : doc -> (category*real) list =
      let
        val cps = makeCtyProbs(count(keyws, cds))
      in
        fn(d) =>
        List.map(fn (cat, probs) => (cat, computeLL(keyws, d, probs))) cps
      end

  end
