package org.http4s
package server
package middleware

import cats.data.OptionT
import cats.effect._
import cats.implicits._
import scala.concurrent.duration.{FiniteDuration}

object Timeout {


  /** Transform the service to return a timeout response after the given
    * duration if the service has not yet responded.  If the timeout
    * fires, the service's response is canceled.
    *
    * @param timeout Finite duration to wait before returning a `500
    * Internal Server Error` response
    * @param service [[HttpService]] to transform
    */
  def apply[F[_]](timeout: FiniteDuration, timeoutResponse: F[Response[F]])(
      service: HttpService[F])(implicit F: Concurrent[F], T: Timer[F]): HttpService[F] = {
    val OTC = Concurrent[OptionT[F, ?]]
    service
      .mapF(respF => OTC.race(respF, OptionT.liftF(T.sleep(timeout) *> timeoutResponse)))
      .map(_.merge)
  }

  /** Transform the service to return a timeout response after the given
    * duration if the service has not yet responded.  If the timeout
    * fires, the service's response is canceled.
    *
    * @param timeout Finite duration to wait before returning a `500
    * Internal Server Error` response
    * @param service [[HttpService]] to transform
    */
  def apply[F[_]](timeout: FiniteDuration)(
      service: HttpService[F])(implicit F: Concurrent[F], T: Timer[F]): HttpService[F] =
    apply(timeout, Response[F](Status.InternalServerError).withBody("The service timed out."))(
      service)
}
