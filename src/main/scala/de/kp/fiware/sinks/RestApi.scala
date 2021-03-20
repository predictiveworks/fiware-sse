package de.kp.fiware.sinks
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

import akka.actor.ActorSystem

import akka.http.scaladsl._
import akka.http.scaladsl.model._

import akka.stream.scaladsl.{Source,Sink}
import akka.util.ByteString

import com.google.gson._

import de.kp.fiware.sse.{OrionConf, SslHelper}

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

trait RestApi {  

  protected val cfg = OrionConf.getCartoCfg
  
  private val uuid = java.util.UUID.randomUUID.toString
  implicit private val system = ActorSystem(s"carto-stream-${uuid}")

  import system.dispatcher
  
  protected val pool = buildConnectionPool  
  /*
   * Method to run a single HttpRequest leveraging
   * the generated connection pool
   * 
   * https://stackoverflow.com/questions/34872793/correct-use-of-akka-http-client-connection-pools
   */
  def doRequest(req: HttpRequest): Future[HttpResponse] = {

    Source
      .single(req → Promise[HttpResponse])
      .via(pool)
      .runWith(Sink.head)
      .flatMap {
        case (Success(response:HttpResponse), _) => 
          Future.successful(response)

        case (Failure(throwable), _) => 
          Future.failed(new Exception(throwable.getMessage))
          
        case (_, _) => 
          Future.failed(new Exception("Response does not match the request."))
       }
    
  }

  def getPayload(response:HttpResponse):JsonObject = {
    
    /* BODY */
    
    /* Extract body as String from response entity */
    val bytes = response.entity.dataBytes.runFold(ByteString(""))(_ ++ _)
    val body = bytes.value.get.get.decodeString("UTF-8")
    
    /* We expect that the Carto DB sends a JSON object */
    JsonParser.parseString(body).getAsJsonObject
    
  }

  def getEndpoint:String = {

    val ssl:Boolean = cfg.getBoolean("ssl")
    val protocol = if (ssl) "https" else "http" 

    val host = cfg.getString("host")
    val port = cfg.getInt("port")
    
    val endpoint = s"${protocol}://${host}:${port}"
    endpoint
    
  }

  private def buildConnectionPool = {

    val ssl:Boolean = cfg.getBoolean("ssl")
    
    val host = cfg.getString("host")
    val port = cfg.getInt("port")
    /*
     * Define the HTTP connection pool; note, the current
     * implementation does not use any ConnectionPoolSettings
     */
    val pool = if (ssl) {
      /*
       * The external HTTP server rquires a SSL/TLS connection      
       */
      val context:HttpsConnectionContext = SslHelper.buildCartoContext

      Http().cachedHostConnectionPoolHttps[Promise[HttpResponse]](host, port, context)
      
    } else {
      
      Http().cachedHostConnectionPool[Promise[HttpResponse]](host, port)

    }
    
    pool
    
  }

}
