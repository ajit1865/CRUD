import scala.collection.mutable.ListBuffer

abstract class CRUDOperation[T] {
  def create(element: T): CRUDOperation[T]
  def read() : Seq[T]
  def update(element: T , newElement: T): CRUDOperation[T]
  def delete(element: T): CRUDOperation[T]
}
// ListOperationOnCRUD class which contains methods for list operation
class ListOperationOnCRUD[T] extends CRUDOperation[T]{
  val emptyList : List[T]= List.empty[T]
  var items : List[T] = emptyList

  // create method creates elements in list
  override def create(element : T): CRUDOperation[T] = {
    items = List(element) ::: items
    this
  }

  //Read method read all elements from list
  override def read(): List[T] = {
    items
  }

  // update method used to update existing element with new element
  override def update(element: T, newElement: T): CRUDOperation[T] = {
    items = items.map(e => if (e == element) newElement else e)
    this
  }

  // Delete method delete an element from list
  override def delete(element: T): CRUDOperation[T] = {
    items = items.filter(_ != element)
    this
  }
}

// SeqOperationOnCRUD class which contain methods for Seq operation
class SeqOperationOnCRUD[T](var items: ListBuffer[T] = ListBuffer.empty[T]) extends CRUDOperation[T] {

  //create new element in seq
  override def create(element: T): CRUDOperation[T] = {
    items += element
    this
  }

  // read method is used to display all method in seq
  override def read(): Seq[T] = {
    items.toSeq
  }

  //update method used to update existing element with new element
  override def update(element: T, newElement: T): CRUDOperation[T] = {
    require(element != null && newElement != null, "element and newElement cannot be null")
    items.zipWithIndex.foreach {
      case (e, index) =>
        if (e == element) {
          items.update(index, newElement)
        }
    }
    this
  }

  // delete method is used to delete element from seq
  override def delete(element: T): CRUDOperation[T] = {
    items -= element
    this
  }
}