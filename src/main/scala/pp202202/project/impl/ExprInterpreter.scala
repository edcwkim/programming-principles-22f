package pp202202.project.impl

import pp202202.project.common._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ExprInterpreter {
  implicit def exprInterpreter[Env, V](
    implicit envOps: EnvOps[Env, V],
    lazyOps: LazyOps[Val, V],
  ): Interpreter[Expr, V] = new Interpreter[Expr, V] {
    private def eval(env: Env, expr: Expr): V = expr match {
      case EInt(n) => lazyOps.to(VInt(n))
      case EFloat(f) => lazyOps.to(VFloat(f))
      case EString(s) => lazyOps.to(VString(s))
      case EName(x) => {
        envOps.findItem(env, x) match {
          case Some(item) => item
          case _ => throw new Exception("Item not found for given name")
        }
      }
      case ENil => lazyOps.to(VNil)
      case ECons(head, tail) => {
        val hv = lazyOps.evaluate(eval(env, head))
        val tv = lazyOps.evaluate(eval(env, tail))
        lazyOps.to(VCons(hv, tv))
      }
      case EFst(e) => {
        val result = lazyOps.evaluate(eval(env, e)) match {
          case VCons(head, _) => head
          case _ => throw new Exception("EFst type error")
        }
        lazyOps.to(result)
      }
      case ESnd(e) => {
        val result = lazyOps.evaluate(eval(env, e)) match {
          case VCons(_, tail) => tail
          case _ => throw new Exception("ESnd type error")
        }
        lazyOps.to(result)
      }
      case ENilP(e) => {
        val resultInt = lazyOps.evaluate(eval(env, e)) match {
          case VNil => 1
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case EIntP(e) => {
        val resultInt = lazyOps.evaluate(eval(env, e)) match {
          case VInt(_) => 1
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case EFloatP(e) => {
        val resultInt = lazyOps.evaluate(eval(env, e)) match {
          case VFloat(_) => 1
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case EStringP(e) => {
        val resultInt = lazyOps.evaluate(eval(env, e)) match {
          case VString(_) => 1
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case EPairP(e) => {
        val resultInt = lazyOps.evaluate(eval(env, e)) match {
          case VCons(_, _) | VNil => 1
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case ESubstr(e, start, end) => {
        val resultString = lazyOps.evaluate(eval(env, e)) match {
          case VString(s) => {
            val sv = lazyOps.evaluate(eval(env, start))
            val ev = lazyOps.evaluate(eval(env, end))
            (sv, ev) match {
              case (VInt(sn), VInt(en)) => s.substring(sn, en)
              case _ => throw new Exception("ESubstr type error")
            }
          }
          case _ => throw new Exception("ESubstr type error")
        }
        lazyOps.to(VString(resultString))
      }
      case ELen(e) => {
        val resultInt = lazyOps.evaluate(eval(env, e)) match {
          case VString(s) => s.length
          case VCons(_, tail) => recLen(tail) + 1
          case VNil => 0
          case _ => throw new Exception("ELen type error")
        }
        lazyOps.to(VInt(resultInt))
      }
      case EIf(cond, ifTrue, ifFalse) => {
        val cv = lazyOps.evaluate(eval(env, cond))
        val selectedExpr = if (cv == VInt(0) || cv == VFloat(0.0f)) ifFalse else ifTrue
        eval(env, selectedExpr)
      }
      case ELet(bs, e) => {
        val envWithEmptyFrame = envOps.pushEmptyFrame(env)
        val envFurnished = bs.foldLeft(envWithEmptyFrame)((intermediateEnv, b) => {
          val (name, item) = b match {
            case BDef(f, params, body) => (f, lazyOps.to(VFunc(f, params, body, intermediateEnv)))
            case BVal(x, e) => (x, eval(intermediateEnv, e))
            case BLVal(x, e) => (x, lazyOps.pend(() => lazyOps.evaluate(eval(intermediateEnv, e))))
          }
          envOps.setItem(intermediateEnv, name, item)
        })
        eval(envFurnished, e)
      }
      case EApp(f, args) => {
        lazyOps.evaluate(eval(env, f)) match {
          case fv@VFunc(funcName, params, body, funcEnv: Env) => {
            val funcEnvWithEmptyFrame = envOps.pushEmptyFrame(funcEnv)
            val funcEnvBasic = envOps.setItem(funcEnvWithEmptyFrame, funcName, lazyOps.to(fv))
            val funcEnvFurnished = params.zip(args).foldLeft(funcEnvBasic)((intermediateEnv, paramArgPair) => {
              val (name, item) = paramArgPair._1 match {
                case AVName(x) => (x, eval(env, paramArgPair._2))
                case ANName(x) => (x, lazyOps.pend(() => lazyOps.evaluate(eval(env, paramArgPair._2))))
              }
              envOps.setItem(intermediateEnv, name, item)
            })
            eval(funcEnvFurnished, body)
          }
          case _ => throw new Exception("EApp type error")
        }
      }
      case EAdd(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val result = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => VInt(ln + rn)
          case (VInt(ln), VFloat(rf)) => VFloat(ln + rf)
          case (VFloat(lf), VInt(rn)) => VFloat(lf + rn)
          case (VFloat(lf), VFloat(rf)) => VFloat(lf + rf)
          case (VString(ls), VString(rs)) => VString(ls + rs)
          case _ => throw new Exception("EAdd type error")
        }
        lazyOps.to(result)
      }
      case ESub(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val result = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => VInt(ln - rn)
          case (VInt(ln), VFloat(rf)) => VFloat(ln - rf)
          case (VFloat(lf), VInt(rn)) => VFloat(lf - rn)
          case (VFloat(lf), VFloat(rf)) => VFloat(lf - rf)
          case _ => throw new Exception("ESub type error")
        }
        lazyOps.to(result)
      }
      case EMul(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val result = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => VInt(ln * rn)
          case (VInt(ln), VFloat(rf)) => VFloat(ln * rf)
          case (VFloat(lf), VInt(rn)) => VFloat(lf * rn)
          case (VFloat(lf), VFloat(rf)) => VFloat(lf * rf)
          case _ => throw new Exception("EMul type error")
        }
        lazyOps.to(result)
      }
      case EDiv(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val result = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => VInt(ln / rn)
          case (VInt(ln), VFloat(rf)) => VFloat(ln / rf)
          case (VFloat(lf), VInt(rn)) => VFloat(lf / rn)
          case (VFloat(lf), VFloat(rf)) => VFloat(lf / rf)
          case _ => throw new Exception("EDiv type error")
        }
        lazyOps.to(result)
      }
      case EMod(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val result = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => VInt(ln % rn)
          case (VInt(ln), VFloat(rf)) => VFloat(ln % rf)
          case (VFloat(lf), VInt(rn)) => VFloat(lf % rn)
          case (VFloat(lf), VFloat(rf)) => VFloat(lf % rf)
          case _ => throw new Exception("EMod type error")
        }
        lazyOps.to(result)
      }
      case EEq(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val resultInt = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => if (ln == rn) 1 else 0
          case (VInt(ln), VFloat(rf)) => if (ln == rf) 1 else 0
          case (VFloat(lf), VInt(rn)) => if (lf == rn) 1 else 0
          case (VFloat(lf), VFloat(rf)) => if (lf == rf) 1 else 0
          case (VString(ls), VString(rs)) => if (ls == rs) 1 else 0
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case ELt(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val resultInt = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => if (ln < rn) 1 else 0
          case (VInt(ln), VFloat(rf)) => if (ln < rf) 1 else 0
          case (VFloat(lf), VInt(rn)) => if (lf < rn) 1 else 0
          case (VFloat(lf), VFloat(rf)) => if (lf < rf) 1 else 0
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case EGt(left, right) => {
        val lv = lazyOps.evaluate(eval(env, left))
        val rv = lazyOps.evaluate(eval(env, right))
        val resultInt = (lv, rv) match {
          case (VInt(ln), VInt(rn)) => if (ln > rn) 1 else 0
          case (VInt(ln), VFloat(rf)) => if (ln > rf) 1 else 0
          case (VFloat(lf), VInt(rn)) => if (lf > rn) 1 else 0
          case (VFloat(lf), VFloat(rf)) => if (lf > rf) 1 else 0
          case _ => 0
        }
        lazyOps.to(VInt(resultInt))
      }
      case ETry(e, handlers) => {
        val envWithEmptyFrame = envOps.pushEmptyFrame(env)
        val envFurnished = handlers.foldLeft(envWithEmptyFrame)((intermediateEnv, handler) => {
          handler match {
            case CCase(handlerNameExpr, x, handlerExpr) => {
              val handlerFunc = VFunc("", List(AVName(x)), handlerExpr, env)
              val handlerName = lazyOps.evaluate(eval(env, handlerNameExpr))
              val handlerNameString = handlerName match {
                case VString(s) => s
                case _ => throw new Exception("EEffect type error")
              }
              envOps.setItem(envWithEmptyFrame, s"handler--$handlerNameString", lazyOps.to(handlerFunc))
            }
          }
        })
        eval(envFurnished, e)
      }
      case EEffect(handlerNameExpr, returnValueExpr) => {
        val returnValue = lazyOps.evaluate(eval(env, returnValueExpr))
        val handlerName = lazyOps.evaluate(eval(env, handlerNameExpr))
        val handlerNameString = handlerName match {
          case VString(s) => s
          case _ => throw new Exception("EEffect type error")
        }
        val handler = envOps.findItem(env, s"handler--$handlerNameString") match {
          case Some(item) => item
          case _ => throw new Exception("Item not found for given name")
        }
        lazyOps.evaluate(handler) match {
          case VFunc(funcName, params, body, funcEnv: Env) => {
            val param = params match {
              case List(param) => param
              case _ => throw new Exception("Unexpected error!")
            }
            val paramString = param match {
              case AVName(x) => x
              case _ => throw new Exception("Unexpected error!")
            }
            val funcEnvFurnished = envOps.setItem(funcEnv, paramString, lazyOps.to(returnValue))
            eval(funcEnvFurnished, body)
          }
          case _ => throw new Exception("Unexpected error!")
        }
      }
    }

    private def recLen(pair: Val): Int = {
      pair match {
        case VCons(_, tail) => recLen(tail) + 1
        case VNil => 0
        case _ => throw new Exception("Unexpected error!")
      }
    }

    def interpret(expr: Expr): Try[V] = {
      try {
        Success(eval(envOps.emptyEnv(), expr))
      } catch {
        case e: Exception => Failure(e)
      }
    }
  }
}
