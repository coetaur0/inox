package inox.parsing

import inox.ast.*
import inox.parsing.ParseError.*
import inox.util.{Name, Result, Span, Spanned}

import scala.collection.mutable
import scala.util.control.Breaks

/** A parser for Inox. */
object Parser {

  /** Parses a module declaration in a source and returns the corresponding AST or a sequence of
    * errors if parsing fails.
    */
  def parseModule(source: String): Result[ModuleDecl, ParseError] = Parser(source).parseModule()

  /** Parses a statement in a source and returns the corresponding AST or a sequence of errors if
    * parsing fails.
    */
  def parseStmt(source: String): Result[Stmt, ParseError] = Parser(source).parseStmt()

  /** Parses an expression in a source and returns the corresponding AST or a sequence of errors if
    * parsing fails.
    */
  def parseExpr(source: String): Result[Expr, ParseError] = Parser(source).parseExpr()

  /** Parses a type expression in a source and returns the corresponding AST or a sequence of errors
    * if parsing fails.
    */
  def parseTypeExpr(source: String): Result[TypeExpr, ParseError] = Parser(source).parseTypeExpr()
}

/** A parser for Inox. */
private class Parser(source: String) {
  private val lexer = Lexer(source)
  private var token = lexer.next()

  /** Parses a module declaration. */
  private def parseModule(): Result[ModuleDecl, ParseError] = Result.build { errors =>
    val fnDecls = Map.newBuilder[String, FnDecl]
    while (token.item != Token.Eof) {
      parseFunction() match {
        case Result.Success((name, function)) =>
          if (fnDecls.result().contains(name.item)) {
            errors += DuplicateFunction(name)
          } else {
            fnDecls += (name.item -> function)
          }
        case Result.Failure(errs) => {
          errors ++= errs
          recover(Set(Token.FnKw))
        }
      }
    }
    fnDecls.result()
  }

  /** Parses a function declaration. */
  private def parseFunction(): Result[(Name, FnDecl), ParseError] = {
    val parseOrigins = () =>
      parseOptional(
        () =>
          parseDelimited(
            () => parseList(() => consume(Token.Origin), Token.Comma, Token.RAngle),
            Token.LAngle,
            Token.RAngle
          ).map(_.item),
        Token.LAngle,
        IndexedSeq()
      )
    val parseParams = () =>
      parseDelimited(
        () => parseList(parseParameter, Token.Comma, Token.RParen),
        Token.LParen,
        Token.RParen
      ).map(_.item)
    val parseResult = () =>
      parseOptional(
        () => {
          advance()
          parseTypeExpr()
        },
        Token.Arrow,
        TypeExpr.Unit(token.span)
      )
    for {
      _       <- consume(Token.FnKw)
      name    <- consume(Token.Name)
      origins <- parseOrigins()
      params  <- parseParams()
      result  <- parseResult()
      body    <- parseBlock().map(_.item)
    } yield {
      (name, inox.ast.FnDecl(origins, params, result, body))
    }
  }

  /** Parses a function parameter. */
  private def parseParameter(): Result[ParamDecl, ParseError] = for {
    mutable <- parseMut()
    name    <- consume(Token.Name)
    _       <- consume(Token.Colon)
    ty      <- parseTypeExpr()
  } yield {
    ParamDecl(mutable, name, ty)
  }

  /** Parses a block expression. */
  private def parseBlock(): Result[Spanned[BlockExpr], ParseError] = {
    val parseStmts = () =>
      Result.build { (errors: mutable.Builder[ParseError, Seq[ParseError]]) =>
        val stmts                = IndexedSeq.newBuilder[Stmt]
        var result: Option[Expr] = None
        Breaks.breakable(
          while (token.item != Token.Eof && token.item != Token.RBrace) {
            parseStmt() match {
              case Result.Success(stmt) =>
                stmt.item match {
                  case StmtKind.ExprStmt(e) if token.item == Token.RBrace => {
                    result = Some(Spanned(e, stmt.span))
                    Breaks.break()
                  }
                  case _ => stmts += stmt
                }
              case Result.Failure(errs) => errors ++= errs
            }
            if (token.item == Token.Semicolon) {
              advance()
            } else {
              Breaks.break()
            }
          }
        )
        (
          stmts.result(),
          result match
            case Some(expr) => expr
            case None       => Expr.Unit(Span(token.span.start, token.span.start))
        )
      }
    for {
      open            <- consume(Token.LBrace).map(_.span.start)
      (stmts, result) <- parseStmts()
      close           <- close("{", Token.RBrace).map(_.end)
    } yield {
      Spanned(BlockExpr(stmts, result), Span(open, close))
    }
  }

  /** Parses a statement. */
  private def parseStmt(): Result[Stmt, ParseError] = token.item match {
    case Token.WhileKw  => parseWhile()
    case Token.LetKw    => parseLet()
    case Token.ReturnKw => parseReturn()
    case _              => parseAssignment()
  }

  /** Parses a while statement. */
  private def parseWhile(): Result[Stmt, ParseError] = {
    val start = advance().span.start
    for {
      cond <- parseExpr()
      body <- parseBlock()
    } yield {
      Stmt.While(cond, body, Span(start, body.span.end))
    }
  }

  /** Parses a let statement. */
  private def parseLet(): Result[Stmt, ParseError] = {
    val start = advance().span.start
    for {
      mutable <- parseMut()
      name    <- consume(Token.Name)
      ty      <- parseOptional(
        () =>
          advance()
          parseTypeExpr().map(t => Some(t))
        ,
        Token.Colon,
        None
      )
      value <- parseOptional(
        () =>
          advance()
          parseExpr().map(e => Some(e))
        ,
        Token.Equal,
        None
      )
    } yield {
      val end = value.map(_.span.end).getOrElse(ty.map(_.span.end).getOrElse(name.span.end))
      Stmt.Let(mutable, name, ty, value, Span(start, end))
    }
  }

  /** Parses a return statement. */
  private def parseReturn(): Result[Stmt, ParseError] = {
    val start = advance().span.start
    for (value <- parseExpr()) yield {
      Stmt.Return(value, Span(start, value.span.end))
    }
  }

  /** Parses an assignment or expression statement. */
  private def parseAssignment(): Result[Stmt, ParseError] = {
    def assign(): Result[Option[Expr], ParseError] = if (token.item == Token.Equal) {
      advance()
      for (value <- parseExpr()) yield {
        Some(value)
      }
    } else {
      Result.Success(None)
    }

    for {
      lhs <- parseExpr()
      rhs <- assign()
    } yield {
      rhs match {
        case Some(value) => Stmt.Assign(lhs, value, Span(lhs.span.start, value.span.end))
        case None        => Stmt.ExprStmt(lhs.item, lhs.span)
      }
    }
  }

  /** Parses an expression. */
  private def parseExpr(): Result[Expr, ParseError] = for {
    lhs  <- parseUnary()
    expr <- parseBinary(0, lhs)
  } yield {
    expr
  }

  /** Parses a binary expression. */
  private def parseBinary(prec: Int, lhs: Expr): Result[Expr, ParseError] = {
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
        unary <- parseUnary()
        rhs   <- parseBinary(nextPrec, unary)
        expr  <- parseBinary(prec, Expr.Binary(op, lhs, rhs, Span(lhs.span.start, rhs.span.end)))
      } yield {
        expr
      }
    } else {
      Result.Success(lhs)
    }
  }

  /** Parses a unary expression. */
  private def parseUnary(): Result[Expr, ParseError] = {
    val op = token.item match {
      case Token.Bang  => Some(UnaryOp.Not)
      case Token.Minus => Some(UnaryOp.Neg)
      case Token.Star  => Some(UnaryOp.Deref)
      case _           => None
    }
    op match {
      case Some(op) => {
        val start = advance().span.start
        for (operand <- parseUnary()) yield {
          Expr.Unary(op, operand, Span(start, operand.span.end))
        }
      }
      case None =>
        for {
          callee <- parsePrimary()
          call   <- parseCall(callee)
        } yield {
          call
        }
    }
  }

  /** Parses a sequence of call expressions. */
  private def parseCall(callee: Expr): Result[Expr, ParseError] = if (token.item == Token.LParen) {
    for {
      args <- parseDelimited(
        () => parseList(parseExpr, Token.Comma, Token.RParen),
        Token.LParen,
        Token.RParen
      )
      call <- parseCall(Expr.Call(callee, args.item, Span(callee.span.start, args.span.end)))
    } yield {
      call
    }
  } else {
    Result.Success(callee)
  }

  /** Parses a primary expression. */
  private def parsePrimary(): Result[Expr, ParseError] = token.item match {
    case Token.LParen    => parseParenExpr()
    case Token.IfKw      => parseIf()
    case Token.Ampersand => parseBorrow()
    case Token.Name      => parseVar()
    case Token.LBrace    =>
      for (body <- parseBlock()) yield {
        Expr.Block(body.item, body.span)
      }
    case Token.IntLit => {
      val span  = advance().span
      val value = source.substring(span.start.offset, span.end.offset).toInt
      Result.Success(Expr.IntLit(value, span))
    }
    case Token.TrueLit  => Result.Success(Expr.BoolLit(true, advance().span))
    case Token.FalseLit => Result.Success(Expr.BoolLit(false, advance().span))
    case _              => expected("an expression")
  }

  /** Parses a parenthesised expression. */
  private def parseParenExpr(): Result[Expr, ParseError] = {
    val start = advance().span.start
    if (token.item == Token.RParen) {
      Result.Success(Expr.Unit(Span(start, advance().span.end)))
    } else {
      for {
        expr <- parseExpr()
        _    <- close("(", Token.RParen)
      } yield {
        expr
      }
    }
  }

  /** Parses an if expression. */
  private def parseIf(): Result[Expr, ParseError] = {
    def elseExpr(): Result[Expr, ParseError] = if (token.item == Token.ElseKw) {
      advance()
      if (token.item == Token.IfKw) {
        parseIf()
      } else {
        for (els <- parseBlock()) yield {
          Expr.Block(els.item, els.span)
        }
      }
    } else {
      Result.Success(Expr.Unit(token.span))
    }

    val start = advance().span.start
    for {
      cond <- parseExpr()
      thn  <- parseBlock()
      els  <- elseExpr()
    } yield {
      Expr.If(cond, thn, els, Span(start, els.span.end))
    }
  }

  /** Parses a borrow expression. */
  private def parseBorrow(): Result[Expr, ParseError] = {
    val start = advance().span.start
    for {
      mutable <- parseMut()
      expr    <- parseExpr()
    } yield {
      Expr.Borrow(mutable, expr, Span(start, expr.span.end))
    }
  }

  /** Parses a variable expression. */
  private def parseVar(): Result[Expr, ParseError] = {
    def originArg() = token.item match {
      case Token.Origin => {
        val origin = text
        advance()
        Result.Success(Some(origin))
      }
      case Token.LocalOrigin => {
        advance()
        Result.Success(None)
      }
      case _ => expected("an origin")
    }

    def parseOriginArgs(default: Span): Result[Spanned[IndexedSeq[Option[Name]]], ParseError] =
      if (token.item == Token.ColonX2) {
        advance()
        for {
          origins <- parseDelimited(
            () => parseList(originArg, Token.Comma, Token.RAngle),
            Token.LAngle,
            Token.RAngle
          )
        } yield {
          origins
        }
      } else {
        Result.Success(Spanned(IndexedSeq[Option[Name]](), default))
      }

    for {
      name    <- consume(Token.Name)
      origins <- parseOriginArgs(name.span)
    } yield {
      Expr.Var(name, origins.item, Span(name.span.start, origins.span.end))
    }
  }

  /** Parses a type expression. */
  private def parseTypeExpr(): Result[TypeExpr, ParseError] = token.item match {
    case Token.LParen    => parseParenType()
    case Token.FnKw      => parseFnType()
    case Token.Ampersand => parseRefType()
    case Token.I32Kw     => Result.Success(TypeExpr.I32(advance().span))
    case Token.BoolKw    => Result.Success(TypeExpr.Bool(advance().span))
    case _               => expected("a type expression")
  }

  /** Parses a parenthesised type expression. */
  private def parseParenType(): Result[TypeExpr, ParseError] = {
    val start = advance().span.start
    if (token.item == Token.RParen) {
      Result.Success(TypeExpr.Unit(Span(start, advance().span.end)))
    } else {
      for {
        ty <- parseTypeExpr()
        _  <- close("(", Token.RParen)
      } yield {
        ty
      }
    }
  }

  /** Parses a function type expression. */
  private def parseFnType(): Result[TypeExpr, ParseError] = {
    val start = advance().span.start
    for {
      params <- parseDelimited(
        () => parseList(parseTypeExpr, Token.Comma, Token.RParen),
        Token.LParen,
        Token.RParen
      )
      _      <- consume(Token.Arrow)
      result <- parseTypeExpr()
    } yield {
      TypeExpr.Fn(params.item, result, Span(start, result.span.end))
    }
  }

  /** Parses a reference type expression. */
  private def parseRefType(): Result[TypeExpr, ParseError] = {
    val start = advance().span.start
    for {
      origin  <- parseOrigin()
      mutable <- parseMut()
      ty      <- parseTypeExpr()
    } yield {
      TypeExpr.Ref(origin, mutable, ty, Span(start, ty.span.end))
    }
  }

  /** Parses an optional origin annotation. */
  private def parseOrigin(): Result[Option[Name], ParseError] = parseOptional(
    () => consume(Token.Origin).map(name => Some(name)),
    Token.Origin,
    None
  )

  /** Parses an optional mutability marker. */
  private def parseMut(): Result[Boolean, ParseError] = parseOptional(
    () => consume(Token.MutKw).map(_ => true),
    Token.MutKw,
    false
  )

  /** Parses an item delimited by some `start` and `end` tokens. */
  private def parseDelimited[A](
      parse: () => Result[A, ParseError],
      start: Token,
      end: Token
  ): Result[Spanned[A], ParseError] = for {
    open  <- consume(start)
    item  <- parse()
    close <- close(open.item, end)
  } yield {
    Spanned(item, Span(open.span.start, close.end))
  }

  /** Parses a list of items separated by some `separator` token and ending with some `end` token.
    */
  private def parseList[A](
      parse: () => Result[A, ParseError],
      separator: Token,
      end: Token
  ): Result[IndexedSeq[A], ParseError] = Result.build { errors =>
    val items = IndexedSeq.newBuilder[A]
    Breaks.breakable {
      while (token.item != Token.Eof && token.item != end) {
        parse() match {
          case Result.Success(item) => items += item
          case Result.Failure(errs) => {
            errors ++= errs
            recover(Set(separator, end))
          }
        }
        if (token.item == separator) {
          advance()
        } else {
          Breaks.break()
        }
      }
    }
    items.result()
  }

  /** Parses an item starting with some `start` token and returns it or returns some default value
    * if the next token in the source doesn't match the `start` kind.
    */
  private def parseOptional[A](
      parse: () => Result[A, ParseError],
      start: Token,
      default: A
  ): Result[A, ParseError] = if (token.item == start) {
    parse()
  } else {
    Result.Success(default)
  }

  /** Advances the parser's position in the source to the next token and returns the last token that
    * was seen.
    */
  private def advance(): Spanned[Token] = {
    val last = token
    token = lexer.next()
    last
  }

  /** Consumes the next token in the source if it is of the expected kind or returns an error. */
  private def consume(kind: Token): Result[Spanned[String], ParseError] = if (token.item == kind) {
    val next = text
    advance()
    Result.Success(next)
  } else {
    expected(kind.toString)
  }

  /** Consumes the next token in the source if it is of the expected `end` kind or returns an error.
    */
  private def close(start: String, end: Token): Result[Span, ParseError] = if (token.item == end) {
    Result.Success(advance().span)
  } else {
    Result.fail(UnclosedDelimiter(start, end.toString, text))
  }

  /** Returns a syntax error indicating that something else than the next token in the source was
    * expected.
    */
  private def expected[A](message: String): Result[A, ParseError] =
    Result.fail(UnexpectedSymbol(message, text))

  /** Resynchronises the parser at the next token that belongs to a list of token kinds. */
  private def recover(kinds: Set[Token]): Unit =
    while (token.item != Token.Eof && !kinds.contains(token.item)) {
      advance()
    }

  /** Returns the spanned string representation of the next token in the source. */
  private def text: Spanned[String] =
    Spanned(source.substring(token.span.start.offset, token.span.end.offset), token.span)
}
