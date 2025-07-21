package inox

import org.scalatest.funsuite.AnyFunSuite

class LexerTests extends AnyFunSuite {
  test("Comments should be ignored") {
    check("// This is a comment\n // And another one.", List((Token.Eof, "")))
  }

  test("Keywords should be lexed properly") {
    check(
      "bool else fn i32 if let mut return while",
      List(
        (Token.BoolKw, "bool"),
        (Token.ElseKw, "else"),
        (Token.FnKw, "fn"),
        (Token.I32Kw, "i32"),
        (Token.IfKw, "if"),
        (Token.LetKw, "let"),
        (Token.MutKw, "mut"),
        (Token.ReturnKw, "return"),
        (Token.WhileKw, "while"),
        (Token.Eof, "")
      )
    )
  }

  test("Identifiers should be lexed properly") {
    check(
      "id x1 _name snake_case camelCase PascalCase _",
      List(
        (Token.Name, "id"),
        (Token.Name, "x1"),
        (Token.Name, "_name"),
        (Token.Name, "snake_case"),
        (Token.Name, "camelCase"),
        (Token.Name, "PascalCase"),
        (Token.Name, "_"),
        (Token.Eof, "")
      )
    )
  }

  test("Origins should be lexed properly") {
    check(
      "'_ 'a",
      List(
        (Token.LocalOrigin, "'_"),
        (Token.Origin, "'a"),
        (Token.Eof, "")
      )
    )
  }

  test("Literals should be lexed properly") {
    check(
      "false 0 42 true",
      List(
        (Token.FalseLit, "false"),
        (Token.IntLit, "0"),
        (Token.IntLit, "42"),
        (Token.TrueLit, "true"),
        (Token.Eof, "")
      )
    )
  }

  test("Operators should be lexed properly") {
    check(
      "! != == <= - + >= / *",
      List(
        (Token.Bang, "!"),
        (Token.BangEqual, "!="),
        (Token.EqualEqual, "=="),
        (Token.LAngleEqual, "<="),
        (Token.Minus, "-"),
        (Token.Plus, "+"),
        (Token.RAngleEqual, ">="),
        (Token.Slash, "/"),
        (Token.Star, "*"),
        (Token.Eof, "")
      )
    )
  }

  test("Punctuation should be lexed properly") {
    check(
      "& -> , : :: = < { ( > } ) ;",
      List(
        (Token.Ampersand, "&"),
        (Token.Arrow, "->"),
        (Token.Comma, ","),
        (Token.Colon, ":"),
        (Token.ColonColon, "::"),
        (Token.Equal, "="),
        (Token.LAngle, "<"),
        (Token.LBrace, "{"),
        (Token.LParen, "("),
        (Token.RAngle, ">"),
        (Token.RBrace, "}"),
        (Token.RParen, ")"),
        (Token.Semicolon, ";"),
        (Token.Eof, "")
      )
    )
  }

  /** Checks that the tokens produced by the lexer for some source string match
    * an expected list of tokens.
    */
  private def check(source: String, expected: Seq[(Token, String)]): Unit = {
    val lexer = new Lexer(source)
    for ((kind, text) <- expected) {
      val next = lexer.next()
      assert(kind == next.item)
      assert(
        text == source.substring(next.span.start.offset, next.span.end.offset)
      )
    }
  }
}
