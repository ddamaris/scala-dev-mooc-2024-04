package ru.otus.module3.catsconcurrency.cats_effect_homework

import akka.Done
import cats.effect.{IO, Sync}
import cats.implicits._
import ru.otus.module3.catsconcurrency.cats_effect_homework.Wallet.{BalanceTooLow, WalletError, WalletId}

import java.nio.file.Files.{createFile, writeString}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  def balance: F[BigDecimal] = {
    for {
      path <- Wallet.walletFilePath(id)
      currentBalance <- Sync[F].delay(Files.readString(path)).map(BigDecimal(_))

     } yield currentBalance
  }

  def topup(amount: BigDecimal): F[Unit] = {
    for {
      currentBalance <- balance
       _ <- writeBalance(currentBalance + amount)
    } yield ()
  }

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = {
    for {
      currentBalance <- balance
      result <- if (currentBalance < amount) {
        Sync[F].pure(Left(BalanceTooLow))
      } else {
        writeBalance(currentBalance - amount).map(_.asRight)
      }
    } yield result
  }

  def writeBalance(balance: BigDecimal): F[Unit] =
    for {
      path <- Wallet.walletFilePath(id)
      _ <- Sync[F].delay(Files.writeString(path, balance.toString, StandardOpenOption.TRUNCATE_EXISTING))
    } yield ()
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов

  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = {
    walletFilePath(id) *> Sync[F].delay { new FileWallet[F](id) }
  }

  def walletFilePath[F[_]:Sync](id: WalletId): F[Path] = Sync[F].delay {
    val path: Path = Paths.get(s"./src/main/resources/wallets/$id")
    if (!Files.exists(path)) {
      createFile(path)
      writeString(path, BigDecimal(0).toString())
    }
    path
  }

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
