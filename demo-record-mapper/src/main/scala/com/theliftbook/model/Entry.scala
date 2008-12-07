package com.theliftbook.model

import java.util.Date

import _root_.java.lang.reflect.Method

import scala.xml.Text

import net.liftweb.http.SHtml.text
import net.liftweb.http.js.JE
import net.liftweb.util.{Can,Empty,Failure,Full}
import net.liftweb.record._
import net.liftweb.record.field._
import net.liftweb.mapper._

import java.math.{BigDecimal,RoundingMode}

import java.sql.Types

class DecimalField[OwnerType <: Record[OwnerType]](rec : OwnerType, scale : Int) extends Field[BigDecimal,OwnerType] {
  def this(rec : OwnerType, value : BigDecimal) = {
    this(rec, value.scale)
    set(value)
  }

  var rounding = RoundingMode.HALF_EVEN

  // Impls
  def defaultValue = (new BigDecimal("0")).setScale(scale)

  def owner = rec

  def asXHtml = Text(value.toString)

  def toForm = text(value.toString, this.setFromString)

  def setFromAny (in : Any) : Can[BigDecimal] = 
    in match {
      case n :: _ => setFromString(n.toString)
      case Some(n) => setFromString(n.toString)
      case Full(n) => setFromString(n.toString)
      case None | Empty | Failure(_, _, _) | null => setFromString("0")
      case n => setFromString(n.toString)
    }

  def setFromString (in : String) : Can[BigDecimal] = {
    try {
      Full(this.setAll(new BigDecimal(in)))
    } catch {
      case e : Exception => couldNotSetValue; Empty
    }
  }
  
  /** Set the value along with proper scale and rounding */
  protected def setAll (in : BigDecimal) = set(in.setScale(scale,rounding))

  // Convenience methods
  def += (other : BigDecimal) = setAll(value.add(other))
  def -= (other : BigDecimal) = setAll(value.subtract(other))
  def *= (other : BigDecimal) = setAll(value.multiply(other))
  def /= (other : BigDecimal) = setAll(value.divide(other))
}
      
  

class RecEntry extends KeyedRecord[RecEntry,Long] {
  def meta = RecEntryMeta
  def primaryKey = id

  object id extends LongField(this) with KeyField[Long,RecEntry]

  object date extends DateTimeField(this) {
    def noFutureDates (time : java.util.Calendar) =
      if (time.getTimeInMillis > System.currentTimeMillis) {
	Full(Text("You cannot make future entries"))
      } else {
	Empty
      }

    override def validators = noFutureDates _ :: Nil
  }

  object description extends StringField(this, 100)

  object amount extends DecimalField(this,2)

  def double = amount *= BigDecimal.valueOf(2)
}

object RecEntryMeta extends RecEntry with MetaRecord[RecEntry] {
  override def fieldOrder = List(date, description, amount)
}

class Entry extends KeyedMapper[Long,Entry] {
  def getSingleton = EntryMeta
  def primaryKeyField = id

  object id extends MappedLongIndex(this)

  object date extends MappedDateTime(this)

  object description extends MappedString(this,100)
  
  object amount extends MappedDecimal(this,2)

  object owner extends MappedLongForeignKey(this,DemoUserMeta)

  object categories extends HasManyThrough(this,
					   CategoryMeta,
					   CategoryEntryMeta,
					   CategoryEntryMeta.entry,
					   CategoryEntryMeta.category)

  def categoriesPull = CategoryEntryMeta.findAll(By(CategoryEntryMeta.entry, this.id)).map(_.category.obj)	 
}

object EntryMeta extends Entry with KeyedMetaMapper[Long,Entry] {
  override def fieldOrder = List(date, description, amount)

  def deleteBefore (date : Date) = 
    this.bulkDelete_!!(By_<(EntryMeta.date, date))
}

class DemoUser extends KeyedMapper[Long,DemoUser] {
  def getSingleton = DemoUserMeta
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
}

object DemoUserMeta extends DemoUser with KeyedMetaMapper[Long,DemoUser] {
}

class Category extends KeyedMapper[Long,Category] {
  def getSingleton = CategoryMeta
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object name extends MappedString(this,100)
}

object CategoryMeta extends Category with KeyedMetaMapper[Long,Category] {
  override def fieldOrder = List(name)
}

class CategoryEntry extends KeyedMapper[Long,CategoryEntry] {
  def getSingleton = CategoryEntryMeta
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object entry extends MappedLongForeignKey(this,EntryMeta)
  object category extends MappedLongForeignKey(this,CategoryMeta)
}

object CategoryEntryMeta extends CategoryEntry with KeyedMetaMapper[Long,CategoryEntry] {
  def join (category : Category, entry : Entry) = {
    val joiner = this.create
    joiner.category(category)
    joiner.entry(entry)
    joiner
  }
}

class Test {
  def findEntryById (id : Long) = EntryMeta.findAll(By(EntryMeta.id,id))

  def findByCategories(like : String) = {
    // TODO: Make this better
    val joins = CategoryEntryMeta.findAll(In(CategoryEntryMeta.category,CategoryMeta.id, Like(CategoryMeta.name,like)))
    joins.map(_.entry.obj)
  }

  def gifts (offset : Long, pageSize : Long) = {
    EntryMeta.findAll(Like(EntryMeta.description, "Gift for%"),
                  OrderBy(EntryMeta.date,Descending),
                  StartAt(offset),
                  MaxRows(pageSize))
  }
}

class MappedDecimal[T <: Mapper[T]] (val fieldOwner : T, val scale : Int) extends MappedField[BigDecimal,T] {
  def this(rec : T, newVal : BigDecimal) = {
    this(rec, newVal.scale)
    set(newVal)
  }
  var rounding = RoundingMode.HALF_EVEN

  def defaultValue = new BigDecimal("0").setScale(scale)
  def dbFieldClass = classOf[BigDecimal]
  
  private var data : BigDecimal = defaultValue
  private var orgData : BigDecimal = defaultValue

  private def st (in : BigDecimal) {
    data = in
    orgData = in
  }

  protected def i_is_! = data
  protected def i_was_! = orgData

  override def doneWithSave() {
    orgData = data
  }

  override def readPermission_? = true  
  override def writePermission_? = true  
   
  protected def i_obscure_!(in : BigDecimal) = defaultValue  
   
  protected def real_i_set_!(value : BigDecimal): BigDecimal = {  
    if (value != data) {  
      data = value  
      dirty_?(true)  
    }  
    data  
  }  
  
  def asJsExp = JE.Num(is)

  def setFromAny (in : Any) : BigDecimal = 
    in match {
      case n :: _ => setFromString(n.toString)
      case Some(n) => setFromString(n.toString)
      case Full(n) => setFromString(n.toString)
      case None | Empty | Failure(_, _, _) | null => setFromString("0")
      case n => setFromString(n.toString)
    }

  def setFromString (in : String) : BigDecimal = {
    this.setAll(new BigDecimal(in))
  }
  
  /** Set the value along with proper scale and rounding */
  protected def setAll (in : BigDecimal) = this.set(in.setScale(scale,rounding))

  def targetSQLType = Types.DECIMAL
  def jdbcFriendly(field : String) = i_is_!  
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null  
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =  
    (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.st(if (v == null) defaultValue else (new BigDecimal(v.getTime).setScale(scale)))})  
  
  def buildSetStringValue(accessor: Method, columnName: String): (T, String) =>  
    Unit = (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.st(new BigDecimal(v).setScale(scale))})  
  
  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) =>  
    Unit = (inst, v, isNull) => doField(inst, accessor, {case f: MappedDecimal[T] => f.st(if (isNull) defaultValue else (new BigDecimal(v).setScale(scale)))})  
  
  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) =>  
    Unit = (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.st(new BigDecimal(v.toString).setScale(scale))})  
  
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " DECIMAL(20," + scale + ")" 

  // Convenience methods
  def += (other : BigDecimal) = setAll(data.add(other))
  def -= (other : BigDecimal) = setAll(data.subtract(other))
  def *= (other : BigDecimal) = setAll(data.multiply(other))
  def /= (other : BigDecimal) = setAll(data.divide(other))

}
  
  
