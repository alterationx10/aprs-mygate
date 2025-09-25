import dev.alteration.branch.lzy.Lazy
import dev.alteration.branch.veil.Veil
import models.AprsPacket

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Using

object Main {

  val port: Int =
    Veil
      .get("PORT")
      .map(_.toInt)
      .getOrElse(14580)

  val serverName: String =
    "MyGate"

  val version: String =
    "0.0.1"

  def main(args: Array[String]): Unit = {

    val serverSocket = new ServerSocket(port)
    println(s"MyGate server listening on port $port")

    Lazy
      .using(serverSocket.accept()) { clientSocket =>
        handleClient(clientSocket).ignore.runAsync
      }
      .forever
      .runSync()

  }

  def handleClient(socket: Socket): Lazy[Unit] = Lazy
    .usingManager { implicit manager =>
      {
        for {
          in  <-
            Lazy.managed(
              new BufferedReader(new InputStreamReader(socket.getInputStream))
            )
          out <-
            Lazy.managed(new PrintWriter(socket.getOutputStream, true))
          _   <- sendServerGreeting(out)
          _   <- handleAuth(in, out)
          _   <- processBody(in)
          _   <-
            Lazy.println(
              s"[${socket.getRemoteSocketAddress}] Connection closed"
            )
        } yield ()
      }.runSync()
    }
    .unit
    .tapError { e =>
      Lazy.println(s"[${socket.getRemoteSocketAddress}] Error: ${e.getMessage}")
    }

  def sendServerGreeting(pw: PrintWriter): Lazy[Unit] =
    Lazy
      .fn(
        s"# $serverName $version ${LocalDateTime.now()}"
      )
      .debug("Server Greeting")
      .map(pw.println)

  def handleAuth(in: BufferedReader, out: PrintWriter): Lazy[Unit] =
    for {
      authLine     <- Lazy
                        .fn(in.readLine())
                        .map(Option(_))
                        .someOrFail(new RuntimeException("No auth line"))
      authResponse <- Lazy
                        .fn {
                          parseAuth(authLine)
                            .map(callsign =>
                              s"# logresp $callsign verified, server $serverName"
                            )
                            .getOrElse(
                              s"# logresp unverified, server $serverName"
                            )
                        }
                        .debug("Auth Response")
    } yield ()

  def parseAuth(authLine: String): Option[String] = {
    // Parse: user CALLSIGN pass PASSCODE vers VERSION filter FILTER
    val pattern = """user\s+(\S+)\s+pass\s+(\S+).*""".r
    authLine match {
      case pattern(callsign, passcode) =>
        println(s"  Callsign: $callsign, Passcode: $passcode")
        Some(callsign)
      case _                           =>
        None
    }
  }

  def processBody(in: BufferedReader): Lazy[Int] =
    Lazy
      .fn(in.readLine())
      .flatMap {
        case null                               => Lazy.fn(-1)
        case comment if comment.startsWith("#") =>
          Lazy.fn(1)
        case packet                             =>
          parseAprsPacket(
            packet
          ).map(_.toJsonString)
            .debug("Packet")
            .as(1)
      }
      .until(_ == -1)

  def parseAprsPacket(packet: String): Lazy[AprsPacket] =
    Lazy.fn {
      // Parse TNC2 format: SENDER>DEST,PATH:payload
      val parts   = packet.split(":", 2)
      val header  = parts(0)
      val payload = if (parts.length > 1) parts(1) else ""

      val headerParts = header.split(">", 2)
      val sender      = headerParts(0)
      val destAndPath = if (headerParts.length > 1) headerParts(1) else ""

      AprsPacket(
        timestamp = LocalDateTime.now(),
        sender = sender,
        destPath = destAndPath,
        payload = payload,
        raw = packet
      )
    }
}
