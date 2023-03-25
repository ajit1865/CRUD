import org.scalatest.flatspec.AnyFlatSpec

class ListOperationOnCRUDTest extends AnyFlatSpec {

  it should "create elements in list" in {
    val objOflistOperationOnCRUD = new ListOperationOnCRUD[Int]
    //adding elements in List
    objOflistOperationOnCRUD.create(5)
    objOflistOperationOnCRUD.create(6)
    objOflistOperationOnCRUD.create(7)
    assert(objOflistOperationOnCRUD.items(2) == 5)
    assert(objOflistOperationOnCRUD.items(1) == 6)
    assert(objOflistOperationOnCRUD.items(0) == 7)

    // reading all elements from list
    assert(objOflistOperationOnCRUD.read() == List(7,6,5))

    //Updaing a value with another value in list
    objOflistOperationOnCRUD.update(6,9)

    //checking the updated value is updated in list
    assert(objOflistOperationOnCRUD.read() == List(7,9,5))

    // Deleting an element from list
    objOflistOperationOnCRUD.delete(7)
    assert(objOflistOperationOnCRUD.read() == List(9,5))
  }

  // test cases for seq
  it should "create elements in Seq" in {
    val objSeqOperationOnCRUD = new SeqOperationOnCRUD[Int]

    // adding elements in list
    objSeqOperationOnCRUD.create(1)
    objSeqOperationOnCRUD.create(2)
    objSeqOperationOnCRUD.create(3)

    //checking elements are in right index
    assert(objSeqOperationOnCRUD.items(0) == 1)
    assert(objSeqOperationOnCRUD.items(1) == 2)
    assert(objSeqOperationOnCRUD.items(2) == 3)

    // Reading all elements from Seq
    assert(objSeqOperationOnCRUD.read() == Seq(1,2,3))

    // Updaing element in Seq with another number
    objSeqOperationOnCRUD.update(2,6)
    assert(objSeqOperationOnCRUD.read() == Seq(1,6,3))

    // Deleting an element from Seq
    objSeqOperationOnCRUD.delete(1)
    assert(objSeqOperationOnCRUD.read() == Seq(6,3))
  }

}
