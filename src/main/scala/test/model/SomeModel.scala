package test.model

import net.liftweb.mapper.LongKeyedMapper
import net.liftweb.mapper.LongKeyedMetaMapper
import net.liftweb.mapper.IdPK
import test.lib.MappedKlangEnum
import test.lib.KlangEnum
import net.liftweb.mapper.CRUDify

class SomeModel extends LongKeyedMapper[SomeModel] with IdPK {
  def getSingleton = SomeModel

  object favFakeLatinPhrase extends MappedKlangEnum(this, Foo) {
    override def displayName = "This is a mapped enum"

    // TODO: This is the problem, would be nice to be able to specify this earlier, 
    //      like it's done in Lifts MappedEnum:
    def dbFieldClass = classOf[Foo.EnumVal]
  }

}

object SomeModel extends SomeModel with LongKeyedMetaMapper[SomeModel] with CRUDify[Long, SomeModel] {
}

object Foo extends KlangEnum {
  case class EnumVal private[Foo] (name: String, abbr: String) extends Value

  val LI = EnumVal("Lorem Ipsum", "LI")
  val DE = EnumVal("Dolores Sit Amet", "DSA")
}
