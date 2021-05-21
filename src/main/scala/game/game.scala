package game

import enemy._
import entity._
import item._
import weapon._
import map._
import position._
import graphics._
import messageHandler._
import scalafx.scene.input.KeyCode
import json._
import client._
import server._
import socket._

object Game
{
    var playerVector:Vector[Player] = Vector()
    val cursor = new Cursor(GameWindow.contextGame)
    var currentPhase = ""
    var currentWeapon = Client.player.weapon
    var speakingTo:SentientEntity = Client.player

    var enemiesVector:Vector[Enemy] = Vector()

    def eventHandler(kc:KeyCode) =
    {
      kc.getName match
      {
        case "Right" | "Left" | "Up" | "Down" => handleArrow(kc.getName)
        case "A"      => setPhase("attack")
        case "I"      => setPhase("info")
        case "Space"  => handleSelection()
        case "Esc"    => setPhase("move")
        case "E"      => setPhase("inventory")
        case "F"      => Client.player.inventory.drop()
        case "G"      => pickUp()
        case "S"      => speak()
        case "T"      => trade()
        case "Enter"  => loop()
        case "F1"     => MessageHandler.setHelp()
        case _        => ()
      }
    }

    def setPhase(phase:String) = 
    {
        cursor.visible = true
        var selectionPhase = true // Are we selcting a tile on the map
        if(phase == "move")
        {
          Map.setHighlight((p:Point)=>(Client.player.pos.distance(p) <= Client.player.curAP && Client.player.curAP > 0), highlightPlayer=true)
            cursor.limitation = true
        }
        else if(phase == "attack" && Client.player.curAP > 0)
        {
            setAttackHighlight()
            val p = Map.findHighlight()
            if(p.x == -1) // if no solution is found
            {
                cursor.visible = false
            }
            else
            {
                cursor.setPos(p)
            }
            cursor.limitation = true
        }
        else if(phase == "info")
        {
            cursor.limitation = false // cursor can move freely on all visible tiles
            Map.setHighlight((p:Point)=>false)
        }
        else if(phase == "inventory" || phase == "speak" || phase == "trade")
        {
          selectionPhase = false
          speakingTo = if(phase == "inventory") Client.player else speakingTo
        }
        if(selectionPhase && phase != currentPhase)
        {
            if(!Map.fromPoint(cursor.pos).isHighlighted())
            {
              cursor.setPos(Client.player.pos)
            }
            if(currentWeapon.zone != Zones.classic _) // other weapon zone should have the cursor on the player's tile
            {
              cursor.setPos(Client.player.pos)
            }
        }
        currentPhase = phase
    }

    def handleSelection() =
    {
        currentPhase match
        {
            case "move"   => Client.player.move(cursor.pos)
                             setPhase("move")
                             //MessageHandler.clear()

            case "attack" => MessageHandler.clear()
                             Client.player.attack(cursor.pos)
                             setPhase("move")

            case "info"   => ()
            case "inventory" | "speak" => speakingTo.inventory.useItem()
            case "trade"  => sell()
            case _ => println(currentPhase)
      }
    }

    def handleArrow(event:String):Unit = 
    {
      if(currentPhase == "inventory" || currentPhase == "speak" || currentPhase == "trade")
      {
        val entity = if(currentPhase == "speak") speakingTo else Client.player
        event match
        {
          case "Right"  => entity.inventory.nextPage()
          case "Left"   => entity.inventory.prevPage()
          case "Up"     => entity.inventory.moveItem(-1)
          case "Down"   => entity.inventory.moveItem(1)
        }
      }
      else
      {
        event match
        {
          case "Right"  => cursor.rotate(1)
          case "Left"   => cursor.rotate(-1)
          case "Up"     => cursor.move(cursor.getDir(1))
          case "Down"   => cursor.move(cursor.getDir(-1))
        }
      }
      if(currentPhase == "attack")
      {
        // update attack zone when rotation
        setAttackHighlight()
      }

    }

    def setAttackHighlight():Unit =
    {

        // Zones.classic is different because it attack only on tile, but we need to select which one
        if (currentWeapon.zone == "classic")
        {
          Map.setHighlight((p:Point)=>p.distance(Client.player.pos) >= currentWeapon.innerRange && p.distance(Client.player.pos) <= currentWeapon.outerRange, true)
        }
        else
          Map.setHighlight((p:Point)=>currentWeapon.getZone()(currentWeapon.innerRange, currentWeapon.outerRange, cursor.currentDir, Client.player.pos, p), true)
    }

    def initialization() =
    {
        MessageHandler.clear()

        Client.player.pos.setPoint(new Point(4, 4))                                        /////////////////////////////////////////////////////////////!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        Map.fromPoint(new Point(4,4)).entity = Some(Client.player)
        Client.player.inventory.add(WeaponCreator.create())
        Client.player.inventory.add(WeaponCreator.create("Fire Ball"))
        Client.player.inventory.add(WeaponCreator.create("sword"))
        Client.player.inventory.add(ItemCreator.create("chainmail helmet"))
        // player.inventory.add(new Bandages)

        setPhase("move")
        Client.player.inventory.display()
        Client.player.inventory.curInv = 0
        // player.endTurn()

        // creating and placing enemies :
        enemiesVector = Map.getEnemies()

        // creating and placing items :
        MessageHandler.clear()
    }

    def loop() = 
    {
        playerVector.foreach
        {
            p => p.endTurn()
        }
        Client.player.inventory.display()
        speakingTo.inventory.display()

        changeWeapon(Client.player.weapon)

        enemiesVector = enemiesVector.filter(_.curHP > 0) // We remove enemies killed by the player
        enemiesVector.foreach
        { e =>
            e.curAP = e.baseAP + e.modifAP
            e.IA()
        }
        // We separate in case we add animation to display the damage done
        enemiesVector.foreach
        {
          e => e.applyEffects()
        }
        enemiesVector.foreach
        {
          e => e.endTurn()
        }
        enemiesVector = enemiesVector.filter(_.curHP > 0) // We remove enemies dying of other causes than the player

        setPhase(currentPhase)    // ensure highlight is up to date

        playerVector.foreach
        {
            p => if(p.curHP <= 0)
            {
                var delId = p.id
                Server.idVector = Server.idVector.filterNot(_ == delId)
                var sToClose:Socket = Server.clientsVector.find(_.jsocket.getLocalAddress().getHostAddress() == delId) match
                {
                    case Some(s) => s
                    case _ => new Socket(null)   // TODO : raise a proper exception
                }
                sToClose.close()

                Server.clientsVector = Server.clientsVector.filterNot(_.jsocket.getLocalAddress().getHostAddress() == delId)
            }
        }
        playerVector = playerVector.filter(_.curHP > 0)

        Map.update()  // We update the rooms of the map
        Client.player.displayInfo() // We update the text on screen to update the player's status
    }

    def changeWeapon(weapon:Weapon):Unit=
    {
      currentWeapon = weapon
      setPhase(currentPhase)
    }

    def pickUp():Unit =
    {
      Map.fromPoint(Client.player.pos).item match
      {
        case None    => ()
        case Some(i) => if (Client.player.curWeight + i.weight <= Client.player.maxWeight)
                        {
                          Client.player.inventory.add(i)
                          Map.fromPoint(Client.player.pos).item = None
                        }
      }
    }

    def speak():Unit =
    {
      Map.fromPoint(cursor.pos).entity match
      {
        case Some(e) => if(Client.player.pos.distance(e.pos) <= 1)
                        {
                          speakingTo = e
                          setPhase("speak")
                        }
        case _       => ()
      }
    }
    def trade():Unit =
    {
      Map.fromPoint(cursor.pos).entity match
      {
        case Some(e) => if(Client.player.pos.distance(e.pos) <= 1)
                        {
                          speakingTo = e
                          setPhase("trade")
                        }
        case _       => ()
      }
    }

    def sell():Unit =
    {
      Client.player.inventory.sell(speakingTo)
    }
}
