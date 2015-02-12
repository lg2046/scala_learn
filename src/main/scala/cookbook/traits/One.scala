package cookbook.traits


/**
 * Created by liguang on 15/2/11.
 * trait
 */

//class StarfleetComponent
//
//trait StarfleetWarpCore extends StarfleetComponent
//
//class Starship extends StarfleetComponent with StarfleetWarpCore


abstract class StarfleetComponent {
  def ejectWarpCore(password: String): Boolean = {
    true
  }
}

trait StarfleetWarpCore {
  //  this: StarfleetComponent =>
  //  this: Starship with WarpCoreEjector with FireExtinguisher =>
  //  限制有指定的方法在那个类里面
  this: {
    def ejectWarpCore(password: String): Boolean
//    def startWarpCore: Unit
  } =>
}

class RomulanStuff

// won't compile
class Warbird extends StarfleetComponent with StarfleetWarpCore

object One extends StarfleetComponent{
  def main(args: Array[String]) {
    println("fuck")
    println(ejectWarpCore("s"))
  }
}
