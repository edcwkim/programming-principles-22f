package pp202202.project.impl

import pp202202.project.common.MapEnv
import pp202202.project.common._

import scala.annotation.tailrec

object MapEnvImpl {
  implicit def mapEnvImpl[V]: EnvOps[MapEnv[V], V] = new EnvOps[MapEnv[V], V] {
    def emptyEnv(): MapEnv[V] = {
      val frames = List()
      new MapEnv(frames)
    }

    def pushEmptyFrame(env: MapEnv[V]): MapEnv[V] = {
      val frames = Map[String, V]() :: env.frames
      new MapEnv(frames)
    }

    def popFrame(env: MapEnv[V]): MapEnv[V] = {
      val _ :: frames = env.frames
      new MapEnv(frames)
    }

    def setItem(env: MapEnv[V], name: String, item: V): MapEnv[V] = {
      val topFrame :: otherFrames = env.frames
      val topFrameNew = {
        if (topFrame.contains(name)) {
          topFrame
        }
        else {
          topFrame + (name -> item)
        }
      }
      new MapEnv(topFrameNew :: otherFrames)
    }

    def findItem(env: MapEnv[V], name: String): Option[V] = {
      val reducedFrame = env.frames.reduce((lm, rm) => rm ++ lm)
      reducedFrame.get(name)
    }
  }
}
