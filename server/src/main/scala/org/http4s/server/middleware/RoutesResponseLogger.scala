package org.http4s
package server
package middleware

import cats.data.{Kleisli, OptionT}
import cats.effect.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import fs2.{Chunk, Stream}
import org.http4s.{Headers, HttpRoutes, Response}
import org.http4s.server.middleware.Logger
import org.http4s.util.CaseInsensitiveString
import org.log4s.getLogger

object PartialResponseLogger {
  private[this] val logger = getLogger

  def apply[F[_]](
                   logHeaders: Boolean,
                   logBody: Boolean,
                   redactHeadersWhen: CaseInsensitiveString => Boolean = Headers.SensitiveHeaders.contains,
                   logAction: String => Unit = logger.info(_))(
                   @deprecatedName('service) http: HttpRoutes[F])(
                   implicit F: Concurrent[F]
                 ): HttpRoutes[F] =
    Kleisli { req =>
      val resp : OptionT[F, Response[F]] = http(req)
        .flatMap { response =>
          OptionT.liftF {
            if (!logBody)
              Logger.logMessage[F, Response[F]](response)(logHeaders, logBody, redactHeadersWhen)(
                logAction) *> F.delay(response)
            else
              Ref[F].of(Vector.empty[Chunk[Byte]]).map { vec =>
                val newBody = Stream
                  .eval(vec.get)
                  .flatMap(v => Stream.emits(v).covary[F])
                  .flatMap(c => Stream.chunk(c).covary[F])

                response.copy(
                  body = response.body
                    // Cannot Be Done Asynchronously - Otherwise All Chunks May Not Be Appended Previous to Finalization
                    .observe(_.chunks.flatMap(c => Stream.eval_(vec.update(_ :+ c))))
                    .onFinalize {
                      Logger.logMessage[F, Response[F]](response.withBodyStream(newBody))(
                        logHeaders,
                        logBody,
                        redactHeadersWhen)(logAction)
                    }
                )
              }
          }
        }
        .handleErrorWith((t: Throwable) =>
          OptionT.liftF[F, Response[F]] {
            F.delay(logger.info(s"service raised an error: ${t.getClass}")) *>
              F.raiseError[Response[F]](t)
          })

      resp
    }
}

