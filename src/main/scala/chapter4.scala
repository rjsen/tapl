sealed trait Term

case object TmTrue extends Term
case object TmFalse extends Term
case object TmZero extends Term

case class TmSucc(tm: Term) extends Term
case class TmPred(tm: Term) extends Term
case class TmIsZero(tm: Term) extends Term
case class TmIf(tm1: Term, tm2: Term, tm3: Term) extends Term

class NoRuleApplies extends Exception

object chapter4 {
  def isNumerical(tm: Term): Boolean = tm match {
    case TmZero     ⇒ true
    case TmSucc(t1) ⇒ isNumerical(t1)
    case _          ⇒ false
  }

  def isVal(tm: Term): Boolean = tm match {
    case TmTrue              ⇒ true
    case TmFalse             ⇒ true
    case t if isNumerical(t) ⇒ true
    case _                   ⇒ false
  }

  def eval1(tm: Term): Term = tm match {
    case TmIf(TmTrue, tm2, tm3)                    ⇒ tm2
    case TmIf(TmFalse, tm2, tm3)                   ⇒ tm3
    case TmIf(tm1, tm2, tm3)                       ⇒ TmIf(eval1(tm1), tm2, tm3)
    case TmSucc(tm1)                               ⇒ TmSucc(eval1(tm1))
    case TmPred(TmZero)                            ⇒ TmZero
    case TmPred(TmSucc(tm1)) if isNumerical(tm1)   ⇒ tm1
    case TmPred(tm1)                               ⇒ TmPred(eval1(tm1))
    case TmIsZero(TmZero)                          ⇒ TmTrue
    case TmIsZero(TmSucc(tm1)) if isNumerical(tm1) ⇒ TmFalse
    case TmIsZero(tm1)                             ⇒ TmIsZero(eval1(tm1))
    case _                                         ⇒ throw new NoRuleApplies
  }

  def eval(tm: Term): Term = try {
    eval(eval1(tm))
  } catch {
    case nra: NoRuleApplies ⇒ tm
  }

  def evalOpt(tm: Term): Option[Term] = tm match {
    case TmIf(TmTrue, tm2, _)                      ⇒ Some(tm2)
    case TmIf(TmFalse, _, tm3)                     ⇒ Some(tm3)
    case TmIf(tm1, tm2, tm3)                       ⇒ evalOpt(tm1).map(t ⇒ TmIf(t, tm2, tm3))
    case TmSucc(tm1)                               ⇒ evalOpt(tm1).map(TmSucc(_))
    case TmPred(TmZero)                            ⇒ Some(TmZero)
    case TmPred(TmSucc(tm1)) if isNumerical(tm1)   ⇒ Some(tm1)
    case TmPred(tm1)                               ⇒ evalOpt(tm1).map(TmPred(_))
    case TmIsZero(TmZero)                          ⇒ Some(TmTrue)
    case TmIsZero(TmSucc(tm1)) if isNumerical(tm1) ⇒ Some(TmFalse)
    case TmIsZero(tm1)                             ⇒ evalOpt(tm1).map(TmIsZero(_))
    case _                                         ⇒ None
  }

  def bigEval(tm: Term): Term = tm match {
    case TmIf(tm1, tm2, _) if bigEval(tm1) == TmTrue ⇒ tm2
    case TmIf(tm1, _, tm3) if bigEval(tm1) == TmFalse ⇒ tm3
    case TmSucc(tm1) if isNumerical(bigEval(tm1)) ⇒ TmSucc(bigEval(tm1))
    case TmPred(tm1) ⇒ bigEval(tm1) match {
      case TmZero ⇒ TmZero
      case TmSucc(tm2) if isNumerical(tm2) ⇒ tm2
      case _ ⇒ tm
    }
    case TmIsZero(tm1) ⇒ bigEval(tm1) match {
      case TmZero ⇒ TmTrue
      case TmSucc(tm2) if isNumerical(tm2) ⇒ TmFalse
      case _ ⇒ tm
    }
    case _ ⇒ tm
  }

  def main(args: Array[String]): Unit = {
    assert(bigEval(TmTrue) == TmTrue)
    assert(bigEval(TmPred(TmSucc(TmZero))) == TmZero)
    assert(bigEval(TmIf(TmIsZero(TmPred(TmSucc(TmZero))), TmSucc(TmZero), TmZero)) == TmSucc(TmZero))
  }
}
