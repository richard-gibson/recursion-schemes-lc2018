package lc2018
package solutions

import jto.validation.ValidationError
import jto.validation.jsonast.JValue
import matryoshka._, implicits._
import matryoshka.patterns.{ListF, NilF, ConsF}
import scalaz._, Scalaz._

sealed trait Step[S, D]
final case class InnerStep[S, D](position: Position, schema: S, data: D) extends Step[S, D]
final case class LastStep[S, D](data: D)                                 extends Step[S, D]

object Step {
  def inner[S, D](pos: Position, schema: S, data: D): Step[S, D] = InnerStep(pos, schema, data)
  def last[S, D](data: D): Step[S, D]                            = LastStep[S, D](data)
}

sealed trait EarlyResult                                                                extends Product with Serializable
final case class InvalidPath()                                                          extends EarlyResult
final case class InvalidPatch(errors: Seq[(jto.validation.Path, Seq[ValidationError])]) extends EarlyResult

trait PatchAlgebras {

  type Path = List[Position]

  type Traversal[Schema, Data] = (Path, Schema, Data)

  type ShortCircuitable[A] = EarlyResult \/ A

  def lookupS[S](position: Position, schema: S)(implicit S: Recursive.Aux[S, SchemaF]): Option[S] =
    (position, schema.project) match {
      case (Field(name), StructF(fields)) => fields.get(name)
      case (Index(idx), ArrayF(elem))     => Some(elem)
      case _                              => None
    }

  def lookupD[D](position: Position, data: D)(implicit D: Recursive.Aux[D, GData]): Option[D] =
    (position, data.project) match {
      case _ => None
    }

  def validatePatch[S, D](patchValue: JValue)(
      implicit S: Recursive.Aux[S, SchemaF],
      D: Birecursive.Aux[D, GData]): CoalgebraM[ShortCircuitable, ListF[Step[S, D], ?], Traversal[S, D]] = {
    case (Nil, _, _) => NilF().right
    case (path @ (last :: Nil), schema, data) =>
      lookupS(last, schema)
        .map { subSchema =>
          val validator = SchemaRules.fromSchemaToRules(schema)
          \/.fromEither(validator.validate(patchValue).toEither)
            .bimap(InvalidPatch.apply, { subData =>
              ConsF(Step.last[S, D](data), (List.empty[Position], subSchema, subData))
            })
        }
        .getOrElse(InvalidPath().left)
    case (path, schema, data) =>
      (lookupS(path.head, schema) |@| lookupD(path.head, data)) { (subSchema, subData) =>
        ConsF(Step.inner(path.head, schema, data), (path.tail, subSchema, subData)).right
      }.getOrElse(InvalidPath().left)
  }

  def updateValue[S, D](implicit S: Recursive.Aux[S, SchemaF],
                        D: Birecursive.Aux[D, GData]): Algebra[ListF[Step[S, D], ?], Option[D]] = {
    case NilF()                   => None
    case ConsF(LastStep(data), _) => Some(data)
    case ConsF(InnerStep(position, schema, current), Some(newData)) =>
      doUpdate(position, current, newData)
    case _ => None
  }

  def doUpdate[D](position: Position, current: D, newData: D)(implicit D: Birecursive.Aux[D, GData]): Option[D] =
    (position, current.project) match {
      case (Field(n), GStruct(fields)) =>
        GStruct(fields.map {
          case (name, field) =>
            if (name == n) name -> newData else name -> field
        }).embed.some
      case (Index(i), GArray(elements)) =>
        GArray(elements.take(i) ++ Seq(newData) ++ elements.drop(i + 1)).embed.some
      case _ => None
    }

  def applyPatch[S, D](schema: S, patch: JsonPatch, current: D)(
      implicit S: Recursive.Aux[S, SchemaF],
      D: Birecursive.Aux[D, GData]): EarlyResult \/ Option[D] =
    (patch.path, schema, current).hyloM[ShortCircuitable, ListF[Step[S, D], ?], Option[D]](
      new AlgebraOps[ListF[Step[S, D], ?], Option[D]](updateValue).generalizeM[ShortCircuitable],
      validatePatch(patch.value))
}