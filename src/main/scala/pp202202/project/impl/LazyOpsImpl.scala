package pp202202.project.impl

import pp202202.project.common.{Val, LVLazy, LVVal, LazyOps, LazyVal}

object LazyOpsImpl {
  type LO = LazyOps[Val, LazyVal[Val]]

  implicit val lazyOpsImpl: LO = new LO {
    def to(value: Val): LVVal[Val] = {
      new LVVal(value)
    }

    def pend(pending: () => Val): LVLazy[Val] = {
      new LVLazy(pending)
    }

    def evaluate(value: LazyVal[Val]): Val = {
      value match {
        case LVVal(v) => v
        case l@LVLazy(_) => l.evaluated
      }
    }
  }
}
