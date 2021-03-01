package de.kp.fiware.sse
/*
 * Copyright (c) 2021 Dr. Krusche & Partner PartG. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 * 
 * @author Stefan Krusche, Dr. Krusche & Partner PartG
 * 
 */

import akka.actor.{ ActorSystem, Props }
import akka.http.scaladsl.Http

import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.model._

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.headers._

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.MethodDirectives.{get, post}
import akka.http.scaladsl.server.directives.PathDirectives.path
import akka.http.scaladsl.server.directives.RouteDirectives.complete

import akka.pattern.ask

import akka.stream.scaladsl.Source

import akka.stream._
import akka.stream.scaladsl.{BroadcastHub, Keep, Source, SourceQueueWithComplete}

import akka.util.ByteString
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._

import scala.io.StdIn
import scala.util.{Failure, Success}

import de.kp.fiware.sse.OrionActor._

class OrionLauncher {
  /*
   * This method launches the Orion SSE HHTP server and also subscribes 
   * to the Orion Context Broker for receiving NGSI event notifications
   */
  def launch(config:Option[String] = None):Unit = {

    /* STEP #1: Read configuration and prepare
     * for launching the OrionSSE server
     */
    if (OrionConf.init(config) == false)
      throw new Exception("[OrionSSE] Loading configuration failed and HTTP server is not started.")
    
    throw new Exception("stop")
    /* STEP #2: Launch HHTP server */
    
    /*
     * Akka 2.6 provides a default materializer out of the box, i.e., for Scala 
     * an implicit materializer is provided if there is an implicit ActorSystem 
     * available. This avoids leaking materializers and simplifies most stream 
     * use cases somewhat.
     */
    implicit val system = ActorSystem(OrionConf.getSystemName)
    implicit lazy val context = system.dispatcher
    /*
   	 * Common timeout for all Akka connection  
     */
    implicit val timeout: Timeout = Timeout(5.seconds)

    /*
     * Define a queue for received NGSI events
     */
    import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
    
    lazy val (queue, source) = Source.queue[String](Int.MaxValue, OverflowStrategy.backpressure)
      .delay(1.seconds, DelayOverflowStrategy.backpressure)

      /* The message type is described as NGSIEvent */
      .map(message => ServerSentEvent(message, Some("NGSIEvent")))
      .keepAlive(1.second, () => ServerSentEvent.heartbeat)
      .toMat(BroadcastHub.sink[ServerSentEvent])(Keep.both)
      .run()
      
    /*
     * The OrionActor is used to receive NGSI events and send
     * them to the message queue
     */
    lazy val orionActor = system.actorOf(Props(new OrionActor(OrionConf.getSubscription, queue)), "OrionActor")
    
    def routes:Route = {
      path("events") {
        /*
         * The GET path is defined to support SSE requests; this 
         * part opens FIWARE environments to non AGPL environments 
         */
        get {
          complete {
            source
          }
        } ~ 
        post {
          /*
           * Extract (full) HTTP request from POST notification
           * of the Orion Context Broker
           */
          extractRequest { request => 
            complete {
              
              val future = orionActor ? request              
              Await.result(future, timeout.duration) match {        
                case Response(Failure(e)) => {
                  /*
                   * A failure response is sent with 500 and
                   * the respective exception message
                   */
                  val message = e.getMessage + "\n"
                  val length = message.getBytes.length
  
                  val headers = List(
                      `Content-Type`(`text/plain(UTF-8)`),
                      `Content-Length`(length)
                  )
                    
                  HttpResponse(
                      status=StatusCodes.InternalServerError, 
                      headers = headers, 
                      entity = ByteString(message), 
                      protocol = HttpProtocols.`HTTP/1.1`)
                  
                }
                case Response(Success(_)) => {
                  
                  val headers = List(
                      `Content-Type`(`text/plain(UTF-8)`),
                      `Content-Length`(0)
                  )
                    
                  HttpResponse(
                      status=StatusCodes.OK, 
                      headers = headers, 
                      entity = ByteString(), 
                      protocol = HttpProtocols.`HTTP/1.1`)
  
                }
              }            
            }            
          }
        }
      }
    }
    
    val binding = OrionConf.getHttpBinding
    /*
     * Distinguish between SSL/TLS and non-SSL/TLS requests
     */
    val serverSec = OrionConf.getServerSecurity
    val server = if (serverSec.getString("ssl") == "false")
      /*
       * The request protocol in the notification url must be
       * specified as 'http://'
       */
      Http().newServerAt(binding.host, binding.port).bind(routes)
      
    else {
      /*
       * The request protocol in the notification url must
       * be specified as 'https://'. In this case, an SSL
       * security context must be specified
       */
      val context = SslHelper.buildServerConnectionContext
      Http().newServerAt(binding.host, binding.port).enableHttps(context).bind(routes)

    }
    
    /* STEP #3: Register subscription with Orion Context Broker */
    
    val response = OrionClient.subscribe(system)
    response.onComplete {
      case Success(value) => {
        
        OrionClient.registerSubscription(value)
          
        println("[INFO] ------------------------------------------------")
        println("[INFO] Subscription successfully registered.")
        println("[INFO] ------------------------------------------------")
        
      }
      case Failure(e) => {
         server
          /* Trigger unbinding from port */ 
          .flatMap(_.unbind())
          /* Shut down application */
          .onComplete(_ => {
            system.terminate()
            throw new Exception(s"Registering subscription failed with '${e.getMessage}'.")
           })
       
      }
    }
    
  }
}