package $package$

import java.util.UUID

import com.github.dnvriend.lambda._
import com.github.dnvriend.lambda.annotation.HttpHandler
import com.github.dnvriend.service.Aggregate
import play.api.libs.json.{Json, _}
import JsonReads._

import scala.compat.Platform
import scalaz.Scalaz._
import scalaz._

object Person {
  implicit val format: Format[Person] = Json.format[Person]
}

final case class Person(name: String, id: Option[String] = None)

object PersonAggregate {
  def aggregate(id: String, ctx: SamContext): Aggregate[PersonEvent, PersonAggregate, Unit] = {
    Aggregate[PersonEvent, PersonAggregate, Unit](id, "person_events", ctx, 100) { event =>
      State {
        case None => event match {
          case PersonCreated(_, name, timestamp) => (Option(PersonAggregate(id, name, timestamp)), ())
        }
        case Some(person) => event match {
          case NameChanged(_, newName, timestamp) => (Option(person.copy(name = newName).copy(lastUpdated = timestamp)), ())
          case NoOperation(_, timestamp) => (Option(person.copy(lastUpdated = timestamp)), ())
        }
      }
    }
  }

  implicit val format: Format[PersonAggregate] = Json.format
}

case class PersonAggregate(id: String, name: String, lastUpdated: Long)

@HttpHandler(path = "/person", method = "put")
class CreatePerson extends JsonApiGatewayHandler[Person] {
  override def handle(person: Option[Person],
                      pathParams: Map[String, String],
                      requestParams: Map[String, String],
                      request: HttpRequest,
                      ctx: SamContext): HttpResponse = {

    person.fold(HttpResponse.validationError.withBody(Json.toJson("Could not deserialize person"))) { person =>
      val id: String = UUID.randomUUID().toString
      val now = Platform.currentTime
      PersonAggregate.aggregate(id, ctx).handle {
        case None => (PersonAggregate(id, person.name, now), PersonCreated(id, person.name, now))
        case Some(aggregate) => (aggregate, NoOperation(id, now))
      }.bimap(t => HttpResponse.serverError.withBody(Json.toJson(t.getMessage)), {
        case (state, event) =>
          HttpResponse.ok.withBody(Json.toJson(state))
      }).merge
    }
  }
}

@HttpHandler(path = "/person/{id}", method = "get")
class ReadPerson extends JsonApiGatewayHandler[Nothing] {
  override def handle(person: Option[Nothing],
                      pathParams: Map[String, String],
                      requestParams: Map[String, String],
                      request: HttpRequest,
                      ctx: SamContext): HttpResponse = {

    pathParams.get("id")
      .fold(HttpResponse.notFound.withBody(Json.toJson("No id found in path")))(id => {
        PersonAggregate.aggregate(id, ctx)
          .currentState
          .fold(HttpResponse.notFound.withBody(Json.toJson("No person found for id: " + id)))(person => {
            HttpResponse.ok.withBody(Json.toJson(person))
          })
      })
  }
}

@HttpHandler(path = "/person/{id}", method = "post")
class UpdatePerson extends JsonApiGatewayHandler[Person] {
  override def handle(person: Option[Person],
                      pathParams: Map[String, String],
                      requestParams: Map[String, String],
                      request: HttpRequest,
                      ctx: SamContext): HttpResponse = {

    (pathParams.get("id") |@| person) ((_, _))
      .fold(HttpResponse.notFound.withBody(Json.toJson("No id found in path"))) { case (id, person) =>
        val now = Platform.currentTime
        PersonAggregate.aggregate(id, ctx)
          .handle {
            case Some(aggregate) => (aggregate.copy(name = person.name, lastUpdated = now), NameChanged(id, person.name, now))
          }.bimap(t => HttpResponse.serverError.withBody(Json.toJson(t.getMessage)), {
          case (state, event) =>
            HttpResponse.ok.withBody(Json.toJson(state))
        }).merge
      }
  }
}
