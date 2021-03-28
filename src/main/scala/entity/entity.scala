package entity

import graphics._
import messageHandler._
import animation._
import animation.Animation.Animation
import scalafx.Includes._
import scalafx.scene.image._
import scalafx.scene.canvas._
import position._
import map._
import game._
import item._

abstract class Entity(animation:Animation, pos:Point, dest:GraphicsContext) 
    extends GraphicEntity(animation, pos, dest)
{
    val name:String
    def move(dir:Point):Unit
}

abstract class SentientEntity(animation:Animation, pos:Point) 
    extends Entity(animation, pos, GameWindow.contextGame)
{
    val name:String
    var maxHP:Int           // health points
    var curHP:Int           // current hp

    var armorClass:Int      // AC
    
    var baseAP:Int          // Action Points
    var modifAP:Int
    var curAP:Int
    
    var baseStr:Int         // starting value
    var modifStr:Int        // relative modifications
    
    var baseDex:Int 
    var modifDex:Int

    var basePow:Int 
    var modifPow:Int

    var weapon:Weapon       // equipped weapon
    var inventory:Inventory = new Inventory(this)

    var poisonDuration:Int  = 0 // number of turn before end of poison effect
    var poisonDammage:Int   = 0

    var onFireDuration:Int  = 0
    var onFireDamage:Int    = 0

    // While frozen the entity can still move, but gets less AP and takes damage
    var frozenDuration:Int  = 0
    var frozenDamage:Int    = 0

    // While paralyzed the entity can only use item but takes no damage
    var paralyzedDuration:Int = 0
    var paralyzedDamage:Int   = 0

    // We setup the armor // TODO: define some default armor
    var headset:Armor
    var chestplate:Armor
    var leggings:Armor
    var boots:Armor

    // And the jewelry
    var ring1:Item
    var ring2:Item
    var Pendant:Item
    
    val dirArray = Array(new Point(1, 0), new Point(0, 1), new Point(-1, 1), new Point(-1, 0), new Point(0, -1), new Point(1, -1))

    def move(next:Point):Unit = 
    {
      if (isMoveValid(next))
      {
        Map.fromPoint(pos).entity = None
        Map.fromPoint(next).entity = Some(this)
        curAP -= pos.distance(next)
        pos.setPoint(next)
      }
    }

    def isMoveValid(next:Point):Boolean =
    {
      return (Map.isInbound(next) && Map.fromPoint(next).entity == None && Map.fromPoint(next).walkable && pos.distance(next) <= curAP)
    }


    def getInfo():String =
    {
      return "%s : %s/%s HP".format(name, curHP, maxHP)
    }

    def damage(dam:Int, from:SentientEntity):Unit=
    {
      // [from] can be used to apply a thorn-like effect
      curHP -= dam
      if(curHP <= 0)
      {
        kill()
      }
    }

    def attack(dest:Point, dir:Int):Unit =
    {
      if(curAP > 0) {
        curAP = 0
        weapon.attack(dest, this, dir)
      }
    }

    def applyEffects():Unit={
      if (poisonDuration > 0){
        curHP -= poisonDamage
        poisonDuration -= 1
      }
      if (onFireDuration > 0){
        curHP -= onFireDamage
        onFireDuration -= 1
      }
      if (frozenDuration > 0){
        curHP -= fronzenDamage
        frozenDuration -= 1
      }
      if (paralyzedDuration > 0){
        curHP -= paralyzedDamage
        paralyzedDuration -= 1
      }

      if (curHP <= 0)
        kill()
    }

    def kill():Unit =
    {
      Map.fromPoint(pos).entity=None
      Map.fromPoint(pos).item = Some(loot()) // TODO: change to not override it there is already an item
    }

    def dodge():Boolean

    def loot() // Generate loot on death
}


class Cursor(dest:GraphicsContext)
  extends Entity(Animation.load("cursor.png", 1), new Point(0,0), dest)
{
  val arrow = new GraphicEntity(Animation.load("arrow.png", 6), pos, dest)
  arrow.freeze()

  val dirArray = Array(new Point(1, 0), new Point(0, 1), new Point(-1, 1), new Point(-1, 0), new Point(0, -1), new Point(1, -1))
  var currentDir = 0
  val name = "cursor"
  var limitation = false  // indicates if the cursor is restricted to highlighted tiles
  var visible = true      // indicates if the cursor is currently visible

  def rotate(rot:Int) = 
  {
    currentDir = (currentDir + rot + dirArray.size) % dirArray.size
    arrow.currentFrame = (arrow.currentFrame + rot + dirArray.size) % dirArray.size
  }

  def getDir(dir:Int):Point = 
  {
    dir match
    {
      case 1  => return dirArray(currentDir)
      case -1 => return dirArray((currentDir + 3) % dirArray.size)
    }
  }
  override def show() = 
  {
    if(visible && Game.player.curAP > 0)
    {
      arrow.show()
      super.show()
    }
  }

  override def move(dir:Point) =
  {
    val nextX = pos.x + dir.x
    val nextY = pos.y + dir.y
    val next = new Point(nextX, nextY)
    if (Map.isInbound(next) && Map.fromPoint(next).isVisible() && (!limitation || Map.fromPoint(next).isHighlighted()))
    {
      Map.fromPoint(next).selected = false
      pos.add(dir)
      Map.fromPoint(next).selected = true
    }
    else 
      findNext(dir)
  }

  def findNext(dir:Point):Unit =
  {
    // If the highlighted zone is not connex we must try to find the next component
    var i = pos.x+dir.x
    var j = pos.y+dir.y
    var p = new Point(i, j)
    while(Map.isInbound(p))
    {
      if (Map.fromPoint(p).isVisible() && (!limitation || Map.fromPoint(p).isHighlighted()))
      {
          setPos(p)
          return
      }
      i += dir.x
      j += dir.y
      p = new Point(i ,j)
    }
  }

  def setPos(dest:Point)
  {
    Map.fromPoint(pos).selected = false
    pos.setPoint(dest)
    Map.fromPoint(pos).selected = true
  }

}

class Player()
    extends SentientEntity(Animation.load("character.png", 4, sizeY=64), new Point(0,0))
{
    val name = "Player"

    var maxHP = 100
    var curHP = 100
    
    var maxSanity = 100
    var sanity = 100
    
    var armorClass = 30
    
    var baseAP = 5
    var modifAP = 0
    var curAP = baseAP
    
    var baseStr = 5
    var modifStr = 0
    
    var baseDex = 100
    var modifDex = 0

    var basePow = 100
    var modifPow = 0
    
    var seeRange = 5
    var modifSee = 0


    var weapon:Weapon = new Weapon("Ring Weapon example", 1000000, 5, "pow", Zones.classic, 3, 0, 4, 5, 8)

    def loot()
    {

    }

    def dodge():Boolean = {return false}

    def getSeeRange():Int = 
    {
        return seeRange + modifSee
    }

    def attack(dest:Point):Unit =
    {
      super.attack(dest, Game.cursor.currentDir)
    }
}

class Inventory(val owner:SentientEntity)
{
    var inventory:Vector[Item] = Vector() // maybe move inventory into its own class/object
    var invStart = 0  // index of first element to be displayed
    var invSize = 10  // number of element to display at once
    var curInv = 0    // index of currently selected item
    var nbItem = 0    // number of item in inventory


    def display():Unit =
    {
      MessageHandler.inventory.clear()
      var i = 0
      for(j <- inventory)
      {
        if (invStart <= i && i < invStart+invSize)
        {
          if (i == curInv) 
            MessageHandler.inventory.addMessage("> "+j.getInfo()) 
          else 
            MessageHandler.inventory.addMessage(j.getInfo())
        }
        i+=1
      }
      MessageHandler.show()
    }
    def prevPage():Unit =
    {
      if (invStart != 0)
        invStart -= invSize
      display()
    }
    def nextPage():Unit =
    {
      if (invStart+invSize < nbItem)
        invStart += invSize
      display()
    }
    def moveItem(d:Int):Unit =
    {
      if (invStart <= curInv + d && curInv + d < nbItem.min(invStart + invSize))
        curInv += d
      display()
    }
    def useItem():Unit =
    {
      inventory(curInv).onUse(owner)
      display()
    }
    def remove(i:Item):Unit =
    {
      inventory = inventory.filterNot(_ == i)
      nbItem -= 1
      curInv = curInv.min(nbItem -1)
      display()
    }
    def add(i:Item):Unit=
    {
      inventory = inventory :+ i
      nbItem += 1
      display()
    }
    def drop():Unit =
    {
      inventory(curInv).pos.setPoint(owner.pos)
      Map.fromPoint(owner.pos).item = Some(inventory(curInv)) // override current item on tile
      remove(inventory(curInv))
    }
}
