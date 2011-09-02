package test.lib

import net.liftweb._
import common._
import http._
import util._
import js._
import java.sql.Types
import json.JsonAST
import java.lang.reflect.Method
import java.util.Date
import scala.xml.NodeSeq
import net.liftweb.mapper.MappedField
import net.liftweb.mapper.Mapper
import net.liftweb.db.DriverType

abstract class MappedKlangEnum[T <: Mapper[T], ENUM <: KlangEnum](val fieldOwner: T, val enum: ENUM) extends MappedField[ENUM#EnumVal, T] {
  private var data: ENUM#EnumVal = defaultValue
  private var orgData: ENUM#EnumVal = defaultValue
  def defaultValue: ENUM#EnumVal = enum.values.iterator.next
  // TODO: Don't know if it is possible to do anything here?
  //  def dbFieldClass = classOf[ENUM#EnumVal]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  protected def i_was_! = orgData
  /**
   * Called after the field is saved to the database
   */
  override def doneWithSave() {
    orgData = data
  }

  protected def real_i_set_!(value: ENUM#EnumVal): ENUM#EnumVal = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true

  def real_convertToJDBCFriendly(value: ENUM#EnumVal): Object = new java.lang.Integer(value.ordinal)

  def toInt = is.ordinal
  def fromInt(in: Int): ENUM#EnumVal = enum(in)

  def jdbcFriendly(field: String) = new java.lang.Integer(toInt)
  override def jdbcFriendly = new java.lang.Integer(toInt)

  def asJsExp: JsExp = JE.Num(is.ordinal)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(is.ordinal))

  override def setFromAny(in: Any): ENUM#EnumVal = {
    in match {
      case JsonAST.JInt(bi) => this.set(fromInt(bi.intValue))
      case n: Int => this.set(fromInt(n))
      case n: Long => this.set(fromInt(n.toInt))
      case n: Number => this.set(fromInt(n.intValue))
      case (n: Number) :: _ => this.set(fromInt(n.intValue))
      case Some(n: Number) => this.set(fromInt(n.intValue))
      case Full(n: Number) => this.set(fromInt(n.intValue))
      case None | Empty | Failure(_, _, _) => this.set(defaultValue)
      case (s: String) :: _ => this.set(fromInt(Helpers.toInt(s)))
      case vs: ENUM#EnumVal => this.set(vs)
      case null => this.set(defaultValue)
      case s: String => this.set(fromInt(Helpers.toInt(s)))
      case o => this.set(fromInt(Helpers.toInt(o)))
    }
  }

  protected def i_obscure_!(in: ENUM#EnumVal) = defaultValue

  private def st(in: ENUM#EnumVal) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String): (T, AnyRef) => Unit =
    (inst, v) => doField(inst, accessor, { case f: MappedKlangEnum[T, ENUM] => f.st(if (v eq null) defaultValue else fromInt(Helpers.toInt(v.toString))) })

  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, { case f: MappedKlangEnum[T, ENUM] => f.st(if (isNull) defaultValue else fromInt(v.toInt)) })

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
    (inst, v) => doField(inst, accessor, { case f: MappedKlangEnum[T, ENUM] => f.st(if (v eq null) defaultValue else fromInt(Helpers.toInt(v))) })

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, { case f: MappedKlangEnum[T, ENUM] => f.st(if (v eq null) defaultValue else fromInt(Helpers.toInt(v))) })

  def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, { case f: MappedKlangEnum[T, ENUM] => f.st(defaultValue) })

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType + notNullAppender()

  /*
  Mapper dependency on Widgets is the wrong order.  There should be a trait in Widgets that's
  mixed into this class that provides autocomplete.  dpp 2009/12/01

  /**
   * Whether or not to use autocomplete in toForm
   */
  def autocomplete_? = false
*/

  /**
   * Build a list for the select.  Return a tuple of (String, String) where the first string
   * is the id.string of the Value and the second string is the Text name of the Value.
   */
  def buildDisplayList: List[(Int, String)] = enum.values.map(a => (a.ordinal, a.toString)).toList

  /**
   * Create an input field for the item
   */
  override def _toForm: Box[NodeSeq] =
    /*
    if (autocomplete_?)
      Full(AutoComplete.autocompleteObj[Int](buildDisplayList, Full(toInt),
                                      v => this.set(fromInt(v))))
    else
    */
    Full(SHtml.selectObj[Int](buildDisplayList, Full(toInt),
      v => this.set(fromInt(v))))
}