package inox.parsing

import inox.{Location, Span, Spanned}

/** A lexical analyser for Inox. */
class Lexer(source: String):
  private var line = 1
  private var column = 1
  private var offset = 0

  /** Returns the next token in the source. */
  def next(): Spanned[Token] =
    consume(_.isWhitespace)
    peek(0) match
      case Some('/') if peek(1).contains('/') =>
        advance(2)
        consume(_ != '\n')
        next()
      case Some('\'')                        => lexOrigin()
      case Some(c) if c.isLetter || c == '_' => lexWord()
      case Some(c) if c.isDigit              =>
        val start = location
        consume(_.isDigit)
        Spanned(Token.IntLit, Span(start, location))
      case Some(_) => lexSymbol()
      case None    => Spanned(Token.Eof, Span(location, location))

  /** Tokenises a keyword, a name or a boolean literal. */
  private def lexWord(): Spanned[Token] =
    val start = location
    val kind = consume(c => c.isLetterOrDigit || c == '_') match
      case "bool"   => Token.BoolKw
      case "else"   => Token.ElseKw
      case "false"  => Token.FalseLit
      case "fn"     => Token.FnKw
      case "i32"    => Token.I32Kw
      case "if"     => Token.IfKw
      case "let"    => Token.LetKw
      case "mut"    => Token.MutKw
      case "return" => Token.ReturnKw
      case "true"   => Token.TrueLit
      case "while"  => Token.WhileKw
      case _        => Token.Name
    Spanned(kind, Span(start, location))

  /** Tokenises an origin. */
  private def lexOrigin(): Spanned[Token] =
    val start = location
    advance(1)
    val kind = consume(c => c.isLetterOrDigit || c == '_') match
      case "_" => Token.LocalOrigin
      case _   => Token.Origin
    Spanned(kind, Span(start, location))

  /** Tokenises an operator or punctuation symbol. */
  private def lexSymbol(): Spanned[Token] =
    val (kind, length) = (peek(0), peek(1)) match
      case (Some('&'), Some('&')) => (Token.AmpersandX2, 2)
      case (Some('-'), Some('>')) => (Token.Arrow, 2)
      case (Some('!'), Some('=')) => (Token.BangEqual, 2)
      case (Some(':'), Some(':')) => (Token.ColonX2, 2)
      case (Some('='), Some('=')) => (Token.EqualX2, 2)
      case (Some('<'), Some('=')) => (Token.LAngleEqual, 2)
      case (Some('|'), Some('|')) => (Token.PipeX2, 2)
      case (Some('>'), Some('=')) => (Token.RAngleEqual, 2)
      case (Some('&'), _)         => (Token.Ampersand, 1)
      case (Some('!'), _)         => (Token.Bang, 1)
      case (Some(','), _)         => (Token.Comma, 1)
      case (Some(':'), _)         => (Token.Colon, 1)
      case (Some('='), _)         => (Token.Equal, 1)
      case (Some('<'), _)         => (Token.LAngle, 1)
      case (Some('{'), _)         => (Token.LBrace, 1)
      case (Some('('), _)         => (Token.LParen, 1)
      case (Some('-'), _)         => (Token.Minus, 1)
      case (Some('+'), _)         => (Token.Plus, 1)
      case (Some('>'), _)         => (Token.RAngle, 1)
      case (Some('}'), _)         => (Token.RBrace, 1)
      case (Some(')'), _)         => (Token.RParen, 1)
      case (Some(';'), _)         => (Token.Semicolon, 1)
      case (Some('/'), _)         => (Token.Slash, 1)
      case (Some('*'), _)         => (Token.Star, 1)
      case (_, _)                 => (Token.Unknown, 1)
    val start = location
    advance(length)
    Spanned(kind, Span(start, location))

  /** The lexer's current location in the source. */
  private def location: Location = Location(line, column, offset)

  /** Peeks at the character at some offset from the lexer's current location in
    * the source.
    */
  private def peek(n: Int): Option[Char] =
    if offset + n < source.length then Some(source(offset + n))
    else None

  /** Advances the lexer's location in the source to the next character. */
  private def advance(): Unit =
    if offset < source.length then
      if source(offset) == '\n' then
        line += 1
        column = 1
      else column += 1
      offset += 1

  /** Advances the lexer's position in the source by some offset. */
  private def advance(n: Int): Unit =
    for i <- 1 to n do advance()

  /** Consumes characters from the source as long as they satisfy some
    * `predicate` and returns them.
    */
  private def consume(predicate: Char => Boolean): String =
    val start = offset
    while offset < source.length && predicate(source(offset)) do advance()
    source.substring(start, offset)
