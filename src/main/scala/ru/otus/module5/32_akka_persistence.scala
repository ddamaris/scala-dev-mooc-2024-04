package ru.otus.module5

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.EventSourcedBehavior
import akka.persistence.typed.scaladsl.Effect



object Account{
  sealed trait AccountCommand
  case class Debit(amount: Long) extends AccountCommand
  case class Credit(amount: Long) extends AccountCommand

  case class AccountState private(amount: Long) {
    def credit(amount: Long): AccountState = this.copy(this.amount + amount)
    def debit(amount: Long): AccountState =
      if(this.amount - amount < 0) throw new IllegalStateException("Account state can't be negative")
      else this.copy(this.amount - amount)
  }
  sealed trait AccountEvent
  case class AmountCredited(amount: Long) extends AccountEvent
  case class AmountDebited(amount: Long) extends AccountEvent

  object AccountState{
    def empty = AccountState(0)
  }

  def commandHandler(accountState: AccountState,
                     accountCommand: AccountCommand)(implicit ctx: ActorContext[AccountCommand]): Effect[AccountEvent, AccountState] = accountCommand match {
    case Debit(amount) =>
      ctx.log.info(s"Command handler. Debit: ${amount}")
      Effect.persist(AmountDebited(amount))
    case Credit(amount) =>
      ctx.log.info(s"Command handler. Credit: ${amount}")
      Effect.persist(AmountCredited(amount))
  }

  def eventHandler(accountState: AccountState, accountEvent: AccountEvent)(implicit ctx: ActorContext[AccountCommand]): AccountState =
    accountEvent match {
      case AmountCredited(amount) =>
        ctx.log.info(s"Event handler. Credited: ${amount}")
        accountState.credit(amount)
      case AmountDebited(amount) =>
        ctx.log.info(s"Event handler. Debited: ${amount}")
        accountState.debit(amount)
    }

  def apply(): Behavior[AccountCommand] = Behaviors.setup{implicit ctx =>
    EventSourcedBehavior[AccountCommand, AccountEvent, AccountState](
      PersistenceId.ofUniqueId("Account"),
      AccountState.empty,
      commandHandler,
      eventHandler
    )
  }
}

object AccountApp{
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem[Account.AccountCommand] = ActorSystem(Account(), "Account")

//    system ! Account.Credit(200)
    system ! Account.Debit(100)
    system ! Account.Debit(100)
    Thread.sleep(2000)
    system.terminate()
  }

  class Counter{
    var counter: Int = 0

    def updateAndGetCounter(): Int = {
      counter += counter + 1
      counter
    }
  }


}