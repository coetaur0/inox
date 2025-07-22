package inox.parsing

import scala.util.control.Breaks

import inox.ast.{BinaryOp, Block, Expr, Name, Type, UnaryOp}
import inox.{Result, Span, Spanned}

/** A parser for Inox. */
object Parser {

  /** Parses an expression in a source and returns the corresponding AST or a
    * sequence of errors if parsing fails.
    */
  def parseExpr(source: String): Result[Expr, ParseError] =
    Parser(source).expr()

  /** Parses a type expression in a source and returns the corresponding AST or
    * a sequence of errors if parsing fails.
    */
  def parseType(source: String): Result[Type, ParseError] =
    Parser(source).ty()
}

/** A parser for Inox. */
private class Parser(source: String) {
  private val lexer = Lexer(source)
  private var token = lexer.next()

  /** Parses a block expression. */
  private def block(): Result[Spanned[Block], ParseError] = ???

  /** Parses an expression. */
  private def expr(): Result[Expr, ParseError] =
    for {
      lhs <- unaryExpr()
      expr <- binaryExpr(0, lhs)
    } yield expr

  /** Parses a binary expression. */
  private def binaryExpr(prec: Int, lhs: Expr): Result[Expr, ParseError] = {
    val (op, nextPrec) = token.item match {
      case Token.PipeX2      => (BinaryOp.Or, 1)
      case Token.AmpersandX2 => (BinaryOp.And, 2)
      case Token.EqualX2     => (BinaryOp.Eq, 3)
      case Token.BangEqual   => (BinaryOp.Neq, 3)
      case Token.LAngleEqual => (BinaryOp.Le, 4)
      case Token.RAngleEqual => (BinaryOp.Ge, 4)
      case Token.LAngle      => (BinaryOp.Lt, 4)
      case Token.RAngle      => (BinaryOp.Gt, 4)
      case Token.Plus        => (BinaryOp.Add, 5)
      case Token.Minus       => (BinaryOp.Sub, 5)
      case Token.Star        => (BinaryOp.Mul, 6)
      case Token.Slash       => (BinaryOp.Div, 6)
      case _                 => (BinaryOp.Or, 0)
    }
    if (prec < nextPrec) {
      advance()
      for {
        unary <- unaryExpr()
        rhs <- binaryExpr(nextPrec, unary)
        expr <- binaryExpr(
          prec,
          Expr.Binary(op, lhs, rhs, Span(lhs.span.start, rhs.span.end))
        )
      } yield expr
    } else
      Result.Success(lhs)
  }

  /** Parses a unary expression. */
  private def unaryExpr(): Result[Expr, ParseError] = {
    val op = token.item match {
      case Token.Bang  => Some(UnaryOp.Not)
      case Token.Minus => Some(UnaryOp.Neg)
      case _           => None
    }
    op match {
      case Some(op) =>
        val start = advance().span.start
        for operand <- unaryExpr()
        yield Expr.Unary(op, operand, Span(start, operand.span.end))
      case None =>
        for {
          callee <- primaryExpr()
          call <- callExpr(callee)
        } yield call
    }
  }

  /** Parses a sequence of call expressions. */
  private def callExpr(callee: Expr): Result[Expr, ParseError] =
    if (token.item == Token.LParen)
      for {
        args <- delimited(
          () => list(expr, Token.Comma, Token.RParen),
          Token.LParen,
          Token.RParen
        )
        call <- callExpr(
          Expr.Call(callee, args.item, Span(callee.span.start, args.span.end))
        )
      } yield call
    else
      Result.Success(callee)

  /** Parses a primary expression. */
  private def primaryExpr(): Result[Expr, ParseError] =
    token.item match {
      case Token.LParen    => parenExpr()
      case Token.IfKw      => ifExpr()
      case Token.Ampersand => borrowExpr()
      case Token.Name      => varExpr()
      case Token.LBrace    =>
        for {
          body <- block()
        } yield Expr.Block(body.item, body.span)
      case Token.IntLit =>
        val span = advance().span
        val value = source.substring(span.start.offset, span.end.offset).toInt
        Result.Success(Expr.IntLit(value, span))
      case Token.TrueLit  => Result.Success(Expr.BoolLit(true, advance().span))
      case Token.FalseLit => Result.Success(Expr.BoolLit(false, advance().span))
      case _              => Result.Failure(expected("an expression"))
    }

  /** Parses a parenthesised expression. */
  private def parenExpr(): Result[Expr, ParseError] = {
    val start = advance().span.start
    if (token.item == Token.RParen)
      Result.Success(Expr.Unit(Span(start, advance().span.end)))
    else
      for {
        expr <- expr()
        _ <- close("(", Token.RParen)
      } yield expr
  }

  /** Parses an if expression. */
  private def ifExpr(): Result[Expr, ParseError] = {
    def elseExpr(): Result[Expr, ParseError] =
      if (token.item == Token.ElseKw) {
        advance()
        if (token.item == Token.IfKw)
          ifExpr()
        else {
          for els <- block() yield Expr.Block(els.item, els.span)
        }
      } else
        Result.Success(Expr.Unit(token.span))

    val start = advance().span.start
    for {
      cond <- expr()
      thn <- block()
      els <- elseExpr()
    } yield Expr.If(cond, thn, els, Span(start, els.span.end))
  }

  /** Parses a borrow expression. */
  private def borrowExpr(): Result[Expr, ParseError] = {
    val start = advance().span.start
    for {
      mutable <- mut()
      expr <- expr()
    } yield Expr.Borrow(mutable, expr, Span(start, expr.span.end))
  }

  /** Parses a variable expression. */
  private def varExpr(): Result[Expr, ParseError] = {
    def originArg() =
      token.item match {
        case Token.Origin =>
          val origin = text
          advance()
          Result.Success(Some(origin))
        case Token.LocalOrigin =>
          advance()
          Result.Success(None)
        case _ => Result.Failure(expected("an origin"))
      }

    def originArgs(
        default: Span
    ): Result[Spanned[IndexedSeq[Option[Name]]], ParseError] =
      if (token.item == Token.ColonX2) {
        advance()
        for origins <- delimited(
            () => list(originArg, Token.Comma, Token.RAngle),
            Token.LAngle,
            Token.RAngle
          )
        yield origins
      } else
        Result.Success(Spanned(IndexedSeq[Option[Name]](), default))

    for {
      name <- consume(Token.Name)
      origins <- originArgs(name.span)
    } yield Expr.Var(
      name,
      origins.item,
      Span(name.span.start, origins.span.end)
    )
  }

  /** Parses a type expression. */
  private def ty(): Result[Type, ParseError] =
    token.item match {
      case Token.LParen    => parenType()
      case Token.FnKw      => fnType()
      case Token.Ampersand => refType()
      case Token.I32Kw     => Result.Success(Type.I32(advance().span))
      case Token.BoolKw    => Result.Success(Type.Bool(advance().span))
      case _               => Result.Failure(expected("a type expression"))
    }

  /** Parses a parenthesised type expression. */
  private def parenType(): Result[Type, ParseError] = {
    val start = advance().span.start
    if (token.item == Token.RParen)
      Result.Success(Type.Unit(Span(start, advance().span.end)))
    else
      for {
        ty <- ty()
        _ <- close("(", Token.RParen)
      } yield ty
  }

  /** Parses a function type expression. */
  private def fnType(): Result[Type, ParseError] = {
    val start = advance().span.start
    for {
      params <- delimited(
        () => list(ty, Token.Comma, Token.RParen),
        Token.LParen,
        Token.RParen
      )
      _ <- consume(Token.Arrow)
      result <- ty()
    } yield Type.Fn(params.item, result, Span(start, result.span.end))
  }

  /** Parses a reference type expression. */
  private def refType(): Result[Type, ParseError] = {
    val start = advance().span.start
    for {
      origin <- origin()
      mutable <- mut()
      ty <- ty()
    } yield Type.Ref(origin, mutable, ty, Span(start, ty.span.end))
  }

  /** Parses an optional origin annotation. */
  private def origin(): Result[Option[Name], ParseError] =
    optional(
      () => for name <- consume(Token.Origin) yield Some(name),
      Token.Origin,
      None
    )

  /** Parses an optional mutability marker. */
  private def mut(): Result[Boolean, ParseError] =
    optional(
      () => for _ <- consume(Token.MutKw) yield true,
      Token.MutKw,
      false
    )

  /** Parses an item delimited by some `start` and `end` tokens. */
  private def delimited[A](
      parse: () => Result[A, ParseError],
      start: Token,
      end: Token
  ): Result[Spanned[A], ParseError] = {
    for {
      open <- consume(start)
      item <- parse()
      close <- close(open.item, end)
    } yield Spanned(item, Span(open.span.start, close.end))
  }

  /** Parses a list of items separated by some `separator` token and ending with
    * some `end` token.
    */
  private def list[A](
      parse: () => Result[A, ParseError],
      separator: Token,
      end: Token
  ): Result[IndexedSeq[A], ParseError] = {
    val items = IndexedSeq.newBuilder[A]
    val errorBuilder = IndexedSeq.newBuilder[ParseError]

    Breaks.breakable {
      while (token.item != Token.Eof && token.item != end) {
        parse() match {
          case Result.Success(item)   => items += item
          case Result.Failure(errors) => errorBuilder ++= errors
        }

        if (token.item == separator)
          advance()
        else
          Breaks.break()
      }
    }

    val errors = errorBuilder.result()
    if (errors.nonEmpty)
      Result.Failure(errors)
    else
      Result.Success(items.result())
  }

  /** Parses an item starting with some `start` token and returns it or returns
    * some default value if the next token in the source doesn't match the
    * `start` kind.
    */
  private def optional[A](
      parse: () => Result[A, ParseError],
      start: Token,
      default: A
  ): Result[A, ParseError] =
    if (token.item == start)
      parse()
    else
      Result.Success(default)

  /** Advances the parser's position in the source to the next token and returns
    * the last token that was seen.
    */
  private def advance(): Spanned[Token] = {
    val last = token
    token = lexer.next()
    last
  }

  /** Consumes the next token in the source if it is of the expected kind or
    * returns an error.
    */
  private def consume(kind: Token): Result[Spanned[String], ParseError] =
    if (token.item == kind) {
      val next = text
      advance()
      Result.Success(next)
    } else
      Result.Failure(expected(kind.toString))

  /** Consumes the next token in the source if it is of the expected `end` kind
    * or returns an error.
    */
  private def close(start: String, end: Token): Result[Span, ParseError] =
    if (token.item == end)
      Result.Success(advance().span)
    else
      Result.Failure(
        IndexedSeq(
          ParseError.UnclosedDelimiter(start, end.toString, text)
        )
      )

  /** Returns a syntax error indicating that something else than the next token
    * in the source was expected.
    */
  private def expected(message: String): IndexedSeq[ParseError] =
    IndexedSeq(ParseError.UnexpectedSymbol(message, text))

  /** Resynchronises the parser at the next token that belongs to a list of
    * token kinds.
    */
  private def recover(kinds: Set[Token]): Unit =
    while (token.item != Token.Eof && !kinds.contains(token.item))
      advance()

  /** Returns the spanned string representation of the next token in the source.
    */
  private def text: Spanned[String] =
    Spanned(
      source.substring(token.span.start.offset, token.span.end.offset),
      token.span
    )
}
