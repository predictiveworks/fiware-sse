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
import akka.actor.ActorSystem

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.headers._

import scala.concurrent.Future

object OrionClient {
  /*
   * {
  	 *		"description": "A subscription to get info about Room1",
  	 *  	"subject": {
   *    "entities": [
   *      {
   *        "id": "Room1",
   *        "type": "Room"
   *      }
   *    ],
   *    "condition": {
   *      "attrs": [
   *        "pressure"
   *      ]
   *    }
   *  },
   *  "notification": {
   *    "http": {
   *      "url": "http://localhost:1028/accumulate"
   *    },
   *    "attrs": [
   *      "temperature"
   *    ]
   *  },
   *  "expires": "2040-01-01T14:00:00.00Z",
   *  "throttling": 5
   * }
   * 
   */
  def subscribe(system:ActorSystem):Future[HttpResponse] = {

    try {
      
      val brokerUrl = OrionConf.getBrokerUrl
      val entity = OrionConf.getSubscriptionData.getBytes("UTF-8")
      /*
       * Build request: A subscription is registered with a POST request
       * to /v2/subscriptions
       */
      val endpoint = s"${brokerUrl}/v2/subscriptions"

      val headers = List(`Content-Type`(`text/plain(UTF-8)`))      
      val request = HttpRequest(
        HttpMethods.POST, endpoint,entity = HttpEntity(`application/json`, entity)
      ).withHeaders(headers)
      
      val response: Future[HttpResponse] = {
        /*
         * Distinguish between SSL/TLS and non-SSL/TLS requests
         */
        val clientSec = OrionConf.getFiwareSecurity
        if (clientSec.getString("ssl") == "false")
          /*
           * The request protocol in the broker url must be
           * specified as 'http://'
           */
          Http(system).singleRequest(request)
          
        else {
          /*
           * The request protocol in the broker url must be
           * specified as 'https://'. In this case, an SSL
           * security context must be specified
           */
          val context = SslHelper.buildFiwareContext
          Http(system).singleRequest(request = request, connectionContext = context)

        }
      }
      response

    } catch {
      case t:Throwable => null
    }

  }
  
  /*
   * This method validates that the response code of the Orion Context
   * Broker response is 201 (Created) and then the subscription ID is
   * extracted from the provided 'Location' header
   */
  def registerSubscription(response:HttpResponse):Unit = {
      
    var sid:String = null
    
    val statusCode = response._1
    if (statusCode == StatusCodes.Created) {
      /*
       * The Orion Context Broker respons with a 201 Created response
       * code; the subscription identifier is provided through the 
       * Location Header
       */
      val headers = response._2
      
      headers.foreach(header => {
        /* Akka HTTP requires header in lower cases */
        if (header.is("location")) {
          /*
           * Location: /v2/subscriptions/57458eb60962ef754e7c0998
           * 
           * Subscription ID: a 24 digit hexadecimal number used 
           * for updating and cancelling the subscription. It is
           * used to identify the notifications that refer to this
           * subscription  
           */
          sid = header.value().replace("/v2/subscriptions/","")
          OrionConf.setSubscription(sid)
        }
        
      })
    }

    if (sid == null)
      throw new Exception("Orion Context Broker did not respond with a subscription response.")
 
  }
}