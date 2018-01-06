
package $package$

import play.api.libs.json._

object PersonEvent {
  val writes: Writes[PersonEvent] = Writes.apply({
    case e: PersonCreated =>
      Json.obj("type" -> "person-created") ++ PersonCreated.format.writes(e)
    case e: NameChanged =>
      Json.obj("type" -> "name-changed") ++ NameChanged.format.writes(e)
    case e: NoOperation =>
      Json.obj("type" -> "no-operation") ++ NoOperation.format.writes(e)
  })

  val reads: Reads[PersonEvent] = new Reads[PersonEvent] {
    override def reads(json: JsValue): JsResult[PersonEvent] = {
      (json \ "type").as[String] match {
        case "person-created" => PersonCreated.format.reads(json)
        case "name-changed" => NameChanged.format.reads(json)
        case "no-operation" => NoOperation.format.reads(json)
      }
    }
  }

  implicit val format: Format[PersonEvent] = Format.apply(reads, writes)
}

sealed trait PersonEvent
object PersonCreated {
  val format: OFormat[PersonCreated] = Json.format
}
case class PersonCreated(id: String, name: String, timestamp: Long) extends PersonEvent
object NameChanged {
  val format: OFormat[NameChanged] = Json.format
}
case class NameChanged(id: String, name: String, timestamp: Long) extends PersonEvent

object NoOperation {
  val format: OFormat[NoOperation] = Json.format
}
case class NoOperation(id: String, timestamp: Long) extends PersonEvent