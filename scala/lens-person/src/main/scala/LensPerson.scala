import monocle.Lens

import java.time.LocalDate

object LensPerson {
  case class Person(_name: Name, _born: Born, _address: Address)

  case class Name(_foreNames: String /*Space separated*/ , _surName: String)

  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)

  case class Address(_street: String, _houseNumber: Int, _place: String /*Village / city*/ , _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

  // Implement these.


  val birthData: Lens[Person, Born] = Lens[Person, Born](_._born)(born => _.copy(_born = born))
  val address: Lens[Person, Address] = Lens[Person, Address](_._address)(addr => _.copy(_address = addr))
  val birthStreet: Lens[Born, Address] = Lens[Born, Address](_._bornAt)(address => _.copy(_bornAt = address))
  val birthDate: Lens[Born, EpochDay] = Lens[Born, EpochDay](_._bornOn)(bdate => _.copy(_bornOn = bdate))
  val street: Lens[Address, String] = Lens[Address, String](_._street)(street => _.copy(_street = street))


  val bornStreet: Born => String = birthStreet.composeLens(street).get

  val setCurrentStreet: String => Person => Person =
    str => p => address.composeLens(street).set(str)(p)

  val setBirthMonth: Int => Person => Person =
    month => p => birthData.modify(b => b.copy(_bornOn = setMonth(month)(b._bornOn)))(p)

  val setMonth: Int => EpochDay => EpochDay =
    m => d => LocalDate.ofEpochDay(d).withMonth(m).toEpochDay

  // Transform both birth and current street names.
  val renameStreets: (String => String) => Person => Person =
    mod => p => address.composeLens(street).modify(mod)(
      birthData.composeLens(birthStreet).composeLens(street).modify(mod)(p))

}
