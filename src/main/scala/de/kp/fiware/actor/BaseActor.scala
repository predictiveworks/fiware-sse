package de.kp.fiware.actor
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
import akka.actor.{Actor, ActorLogging, OneForOneStrategy}
import akka.actor.SupervisorStrategy._

import akka.http.scaladsl.model.HttpRequest
import akka.util.{ByteString,Timeout}

import com.google.gson._
import de.kp.fiware.sse.OrionConf

import scala.concurrent.duration._

case class OrionRequest(service:String, servicePath:String, payload:JsonObject)

abstract class BaseActor extends Actor with ActorLogging {
 
  val conf = OrionConf

  /*
   * The header positions that hold service and
   * service path information
   */
  val SERVICE_HEADER = 4
  val SERVICE_PATH_HEADER = 5
  
  /*
   * The actor system is implicitly accompanied by a materializer,
   * and this materializer is required to retrieve the bytestring
   */
  implicit val system = context.system
  implicit val ec = system.dispatcher
  /*
	 * Common timeout for all Akka connection  
   */
  val cfg = conf.getActorCfg

  implicit val timeout: Timeout = {
    val value = cfg.getInt("timeout")
    Timeout(value.seconds)
  }
  /*
   * Parameters to control the handling of failed child actors:
   * it is the number of retries within a certain time window.
   * 
   * The superviser strategy restarts a child up to 10 restarts 
   * per minute. The child actor is stopped if the restart count 
   * exceeds maxNrOfRetries during the withinTimeRange duration.    
   */
  private val maxRetries = cfg.getInt("maxRetries")
  private val timeRange = {
    val value = cfg.getInt("timeRange")
    value.minute
  }
  /*
   * Child actors are defined leveraging a RoundRobin pool with a 
   * dynamic resizer. The boundaries of the resizer are defined 
   * below
   */
  protected val lower = cfg.getInt("lower")
  protected val upper = cfg.getInt("upper")
  /*
   * The number of instances for the RoundRobin pool
   */
  protected val instances = cfg.getInt("instances")
  
  /*
   * Each actor is the supervisor of its children, and as such each 
   * actor defines fault handling supervisor strategy. This strategy 
   * cannot be changed afterwards as it is an integral part of the 
   * actor system’s structure.
   */
  override val supervisorStrategy =
    /*
     * The implemented supervisor strategy treats each child separately
     * (one-for-one). Alternatives are, e.g. all-for-one.
     * 
     */
    OneForOneStrategy(maxNrOfRetries = maxRetries, withinTimeRange = timeRange) {
      case _: ArithmeticException      => Resume
      case _: NullPointerException     => Restart
      case _: IllegalArgumentException => Stop
      case _: Exception                => Escalate
    }  
  
  def toOrionRequest(request:HttpRequest):OrionRequest = {
    
    /* HEADERS */
    
    val headers = request.headers
    
    val service = headers(SERVICE_HEADER).value()
    val servicePath = headers(SERVICE_PATH_HEADER).value()
    
    /* BODY */
    
    /* Extract body as String from request entity */
    val bytes = request.entity.dataBytes.runFold(ByteString(""))(_ ++ _)
    val body = bytes.value.get.get.decodeString("UTF-8")
    
    /* We expect that the Orion Context Broker sends a JSON object */
    val payload = JsonParser.parseString(body).getAsJsonObject

    OrionRequest(service, servicePath, payload)
    
  }
}
