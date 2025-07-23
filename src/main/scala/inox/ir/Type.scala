package inox.ir

import inox.{Span, Spanned}

/** An origin identifier. */
type OriginId = Int

/** An IR type. */
case class Type(value: Spanned[TypeKind]):
  import TypeKind.*

  /** Checks if two types are equivalent. */
  def ===(that: Type): Boolean =
    (this.value.item, that.value.item) match
      case (Fn(lParams, lResult), Fn(rParams, rResult)) =>
        lParams.length == rParams.length &&
        lParams.zip(rParams).forall { case (lParam, rParam) =>
          lParam === rParam
        } &&
        lResult === rResult
      case (Ref(lOrigin, lMut, lType), Ref(rOrigin, rMut, rType)) =>
        lOrigin == rOrigin && lMut == rMut && lType === rType
      case (I32, I32) | (Bool, Bool) | (Unit, Unit) => true
      case _                                        => false

  /** Checks if a type is a subtype of another. */
  def :<(that: Type): Boolean =
    (this.value.item, that.value.item) match
      case (Fn(lParams, lResult), Fn(rParams, rResult)) =>
        lParams.length == rParams.length &&
        lParams.zip(rParams).forall { case (lParam, rParam) =>
          rParam :< lParam
        } &&
        lResult :< rResult
      case (Ref(lOrigin, lMut, lType), Ref(rOrigin, rMut, rType)) =>
        rOrigin.forall(o =>
          lOrigin.forall(o == _)
        ) && (lMut || !rMut) && lType :< rType
      case (_, _) => this === that

  /** Substitutes the origin ids in a type with the ids in some substitution
    * map.
    */
  def substitute(ids: IndexedSeq[Option[OriginId]]): Type =
    this.value.item match
      case Fn(params, result) =>
        Type.Fn(
          params.map(_.substitute(ids)),
          result.substitute(ids),
          this.value.span
        )
      case Ref(origin, mut, ty) =>
        val newOrigin: Option[OriginId] =
          origin.flatMap(id => if id < ids.length then ids(id) else origin)
        Type.Ref(newOrigin, mut, ty.substitute(ids), this.value.span)
      case _ => this

  override def toString: String = this.value.toString

object Type:
  def Fn(params: IndexedSeq[Type], result: Type, span: Span): Type =
    Type(Spanned(TypeKind.Fn(params, result), span))

  def Ref(
      origin: Option[OriginId],
      mutable: Boolean,
      ty: Type,
      span: Span
  ): Type =
    Type(Spanned(TypeKind.Ref(origin, mutable, ty), span))

  def I32(span: Span): Type = Type(Spanned(TypeKind.I32, span))

  def Bool(span: Span): Type = Type(Spanned(TypeKind.Bool, span))

  def Unit(span: Span): Type = Type(Spanned(TypeKind.Unit, span))

/** An IR type's kind. */
enum TypeKind:
  case Fn(params: IndexedSeq[Type], result: Type)
  case Ref(origin: Option[OriginId], mutable: Boolean, ty: Type)
  case I32
  case Bool
  case Unit

  override def toString: String =
    this match
      case Fn(params, result) => s"fn(${params.mkString(", ")}) -> ${result}"
      case Ref(origin, mutable, ty) =>
        val mut = if mutable then "mut" else ""
        origin match
          case Some(id) => s"&'$id $mut $ty"
          case None     => s"&$mut $ty"
      case I32  => "i32"
      case Bool => "bool"
      case Unit => "()"
