case class Allergies() {
  def isAllergicTo(allergen: Allergen.Allergen, n: Int): Boolean = allergies(n).contains(allergen)

  def allergies(n: Int): List[Allergen.Allergen] = {
    @scala.annotation.tailrec
    def getAllergies(m: Int, allergens: List[Allergen.Value], acc: List[Allergen.Allergen]): List[Allergen.Allergen] =
      if (allergens.isEmpty) acc.toSet.toList.sorted // to remove duplicates
      else {
        // here we keep removing the 'higher' allergen from the value until we need to move to the next allergen
        val allergen = allergens.head.id
        if (allergen <= m) getAllergies(m - allergen, allergens, allergens.head :: acc)
        else getAllergies(m, allergens.tail, acc)
      }

    getAllergies(n, Allergies.orderedEnumList, Nil)
  }

}

object Allergies {
  //ordered from higher value to lower
  private val orderedEnumList = Allergen.values.toList.reverse
}

object Allergen extends Enumeration {
  type Allergen = Value
  val Eggs = Value(1)
  val Peanuts = Value(2)
  val Shellfish = Value(4)
  val Strawberries = Value(8)
  val Tomatoes = Value(16)
  val Chocolate = Value(32)
  val Pollen = Value(64)
  val Cats = Value(128)
}

