package inox

/** A lexical token. */
enum Token {
  // Keywords:
  case BoolKw
  case ElseKw
  case FnKw
  case I32Kw
  case IfKw
  case LetKw
  case MutKw
  case ReturnKw
  case WhileKw

  // Literals:
  case FalseLit
  case IntLit
  case LocalOrigin
  case Name
  case Origin
  case TrueLit

  // Operators:
  case Bang
  case BangEqual
  case EqualEqual
  case LAngleEqual
  case Minus
  case Plus
  case RAngleEqual
  case Slash
  case Star

  // Punctuation:
  case Ampersand
  case Arrow
  case Comma
  case Colon
  case ColonColon
  case Equal
  case LAngle
  case LBrace
  case LParen
  case RAngle
  case RBrace
  case RParen
  case Semicolon
  case Eof

  // Errors:
  case Unknown

  override def toString: String =
    this match {
      case BoolKw      => "the 'bool' keyword"
      case ElseKw      => "the 'else' keyword"
      case FnKw        => "the 'fn' keyword"
      case I32Kw       => "the 'i32' keyword"
      case IfKw        => "the 'if' keyword"
      case LetKw       => "the 'let' keyword"
      case MutKw       => "the 'mut' keyword"
      case ReturnKw    => "the 'return' keyword"
      case WhileKw     => "the 'while' keyword"
      case FalseLit    => "the 'false' literal"
      case IntLit      => "an integer literal"
      case LocalOrigin => "a local origin"
      case Name        => "a name"
      case Origin      => "an origin"
      case TrueLit     => "the 'true' literal"
      case Bang        => "the '!' operator"
      case BangEqual   => "the '!=' operator"
      case EqualEqual  => "the '==' operator"
      case LAngleEqual => "the '<=' operator"
      case Minus       => "the '-' operator"
      case Plus        => "the '+' operator"
      case RAngleEqual => "the '>=' operator"
      case Slash       => "the '/' operator"
      case Star        => "the '*' operator"
      case Ampersand   => "a '&'"
      case Arrow       => "a '->'"
      case Comma       => "a ','"
      case Colon       => "a ':'"
      case ColonColon  => "a '::'"
      case Equal       => "a '='"
      case LAngle      => "a '<'"
      case LBrace      => "a '{'"
      case LParen      => "a '('"
      case RAngle      => "a '>'"
      case RBrace      => "a '}'"
      case RParen      => "a ')'"
      case Semicolon   => "a ';'"
      case Eof         => "the end of file"
      case Unknown     => "an unknown token"
    }
}
