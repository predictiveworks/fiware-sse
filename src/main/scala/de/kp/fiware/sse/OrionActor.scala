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

import akka.http.scaladsl.model.HttpRequest

import akka.stream.QueueOfferResult
import akka.stream.scaladsl.SourceQueueWithComplete
import akka.util.ByteString

import com.google.gson._

import scala.collection.mutable.Queue
import scala.util.{Failure, Success, Try}

import de.kp.fiware.actor.BaseActor
import de.kp.fiware.sse.OrionActor._

/*
 * This actor receives NGSI events from the Orion Context Broker
 * and sends them to the queue
 */
class OrionActor(queue:SourceQueueWithComplete[String]) extends BaseActor {
 
  private val subscription = conf.getSubscription
  /*
   * An additional queue to manage events that were dropped due
   * to a source queue overflow
   */
  val buffer = Queue[String]()
  
  override def receive: Receive = {  

    case request:HttpRequest => {    
      sender ! Response(Try({
        toNGSIEvent(request)
      })
      .recover {
        case e:Exception => {
          throw new Exception(e.getMessage)
        }
      })
    }
  
  }
  
  /*
   * Extract header and body from HTTP request
   */
  private def toNGSIEvent(request: HttpRequest):Unit = {

    val orionRequest = toOrionRequest(request)
    
    val service = orionRequest.service
    val servicePath = orionRequest.servicePath
    
    val json = orionRequest.payload
    /*
     * {
        "data": [
            {
                "id": "Room1",
                "temperature": {
                    "metadata": {},
                    "type": "Float",
                    "value": 28.5
                },
                "type": "Room"
            }
        ],
        "subscriptionId": "57458eb60962ef754e7c0998"
    }
     */
    
    val sid = json.get("subscriptionId").getAsString
    if (subscription == sid) {
      /*
       * It is an optimistic approach, and we do not 
       * throw an exception in case of the wrong sid
       */
      val data = json.get("data").getAsJsonArray
      val size = data.size
      
      val event = new JsonObject()

      event.addProperty("subscription", sid)
      event.addProperty("createdAt", System.currentTimeMillis)

      
      event.addProperty("service", service)
      event.addProperty("servicePath", servicePath)
      
      event.add("entities", data)
      val eventS = event.toString

      val message = 
        if (buffer.size == 0) eventS        
        else {
          buffer.enqueue(eventS)
          buffer.dequeue        
        }
        
      queue.offer(message).map {
        case QueueOfferResult.Enqueued => {
          /*
           * In this case the event was successfully
           * sent to the queue, i.e. there is 
           */
        }
        case QueueOfferResult.Dropped => {
          /*
           * In this case the event was dropped as
           * the queue is full
           */
          buffer.enqueue(message)
        }
        case QueueOfferResult.Failure(ex) => {}
        case QueueOfferResult.QueueClosed => {}        
      }

    }
  }

}

object OrionActor {

  case class Response(status: Try[_])

}