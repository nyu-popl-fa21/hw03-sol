package popl

import org.scalatest.flatspec.AnyFlatSpec
import popl.hw03._


class hw03Spec extends AnyFlatSpec {

  // Check that filter removes all values smaller than n from the list l

  "append" should "concatenate lists l1 and l2" in {
    val l1 = 3 :: 4 :: 2 :: Nil
    val l2 = 1 :: 8 :: Nil
    
    assert(append(l1, l2) === 3 :: 4 :: 2 :: 1 :: 8 :: Nil)
    assert(append(l2, l1) === 1 :: 8 :: 3 :: 4 :: 2 :: Nil)
    assert(append(Nil, l1) === 3 :: 4 :: 2 :: Nil)
    assert(append(l1, Nil) === 3 :: 4 :: 2 :: Nil)
  }
  
  "filter" should "remove all elements from the list l that are smaller than n" in {
     val l = 3 :: 4 :: -2 :: 0 :: Nil
    
     assert(filter(0, Nil) === Nil)
     assert(filter(-3, l) === l)
     assert(filter(0, l) === 3 :: 4 :: 0 :: Nil)
     assert(filter(2, l) === 3 :: 4 :: Nil)
     assert(filter(4, l) === 4 :: Nil)
     assert(filter(5, l) === Nil)
  }

}
